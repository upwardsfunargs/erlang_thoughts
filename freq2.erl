%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

%%   Modifications to add buffer clearing and timeouts (experimentally) by jsm 20170419

-module(freq2).
-export([fs_start/0,allocate/0,deallocate/1,stop/0]).
-export([fs_init/0]).
-export([start_worry/1,worrisome_client/1,start_tinned_meat/1,tinned_meat_client/1]). %% for test
%% Although the export clarifies which function definitions are available to the client,
%% I prefer a more evident distinction
%% - so have added a Frequency Server prefix. Better in a separate module.

%% General definitions
%%-define( tooLong, 500). - tried to find some way of parametrizing timeout - not solved!

%% These are the start functions used to create and
%% initialize the server.

fs_start() ->
    register(freq2,
	     spawn(freq2, fs_init, [])).

fs_init() ->
  Frequencies = {fs_get_frequencies(), []},
  fs_loop(Frequencies).

% Hard Coded
fs_get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop and clear

fs_loop(Frequencies) ->
  fs_sleep(), %% add latency for testing...
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = fs_allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      fs_loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = fs_deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      fs_loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  after 5000 ->
     N = fs_clear(0),
     io:format("fs cleared ~w out-of-band messages~n",[N]),
     fs_loop (Frequencies)
  end.

fs_clear(N) ->
  %% recursively empties mailbox of pending receivable messages
  receive
    _Msg -> fs_clear(N+1)
  after 0 -> N
  end.
  
%% Functional interface (client code)
%% should only have one pending possibly meaningful message lurking
%% With 0 as timeout this never seems to get anything...
clear() ->
   receive
	{reply, Reply} ->
	io:format("Clearing ~w for ~w~n",[Reply,self()]),
	%% can handle an allocation ack with a raw deallocate here in due course, perhaps
	case Reply of
	  {ok,Freq} ->
		  freq2 ! {request, self(), {deallocate, Freq}},
    		  receive 
	            {reply, Reply2} -> io:format("Clear dealloc got ~w~n",[Reply2])
                  after 600 -> io:format("Clear dealloc times out~n",[])
		  end;
	  _Msg -> true
	end,
	true
   after 10 ->
	true
   end.
   

allocate() ->
    clear(),
    freq2 ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
    after 300 -> {timeout}
    end.

deallocate(Freq) ->
    clear(),
    freq2 ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
    after 300 -> {timeout}
    end.

stop() ->
    clear(),
    freq2 ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    after 300 -> {timeout}
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies. (server code)

fs_allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
fs_allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

fs_deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

%% Test Code =================================================================
%% to see interesting issues start the server (freq2:fs_start())
%% and 2 or 3 overlapping worriers (freq2:start_worry(30)) - two clients are handled,
%% three clients are not! It may be possible to get better behaviour by tweaking figures,
%% but the server model seems deficient!

fs_sleep() ->
  receive
  after 100 -> true
  end.

start_worry(N) ->
  spawn(freq2,worrisome_client,[N]),
  io:format("Worry started (~w)~n",[N]).
  
worrisome_client(N) ->
  case N of
  0 ->
    io:format("Worry done ~n"),
    true;
  _ ->
    Reply = allocate(),
    io:format("Worrier ~w got ~w~n",[self(),Reply]),
    case Reply of
    	 {ok,Freq} ->
            Reply2 = deallocate(Freq),
	    case Reply2 of
	      ok -> true;
	      {timeout} ->
	        io:format("Timed out in deallocate ~w~n",[self()]);
	      {ok, Freq2} ->
		io:format("Out of sequence dealloc reply f=~w for ~w~n",[Freq2,self()]);
	      {error,no_frequency} ->
	        io:format("Out of sequence dealloc error no_frequency reply for ~w~n",[self()])
	    end,
            worrisome_client(N-1);
	 {error,no_frequency} ->
	      io:format("No freq allocated ~w~n",[self()]),
	      worrisome_client(N-1);
	 {timeout} ->
	   io:format("Timed out in allocate ~w~n",[self()]),
	   worrisome_client(N-1);
	 ok ->
	   worrisome_client(N-1)
    end
  end.

start_tinned_meat(N) ->
  Pid = spawn(freq2,tinned_meat_client,[N]),
  io:format("Tinned meat started (~w from ~w)~n",[N,Pid]).
 
tinned_meat_client(N) ->
  case N of
  0 ->
    io:format("Meat delivered from ~w~n",[self()]),
    true;
  _ ->
    freq2 ! { ham, N },
    tinned_meat_client(N-1)
  end.
