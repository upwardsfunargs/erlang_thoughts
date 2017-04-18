%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson
%%   Modified 20170410 jsm

-module(frequency).
-export([startFrequencyServer/0]).

%% These are the start functions used to create and
%% initialize the server.
startFrequencyServer() ->
FrequencyServer = spawn(frequency,init,[]),
register(frequency,FrequencyServer).

init() ->
%% check we are not running already
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop
%% over paranoid version: Once we limit allocation to a single frequency per pid,
%% the Freq parameter is a symptom of paranoia.
%% It is reasonable to not require this in the message, but merely give a true/false response.

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      {NewFrequencies, Response} = deallocate(Frequencies, Freq, Pid),
      Pid ! {reply, Response },
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped} %% leaving some allocations active perhaps!
      %% registration dies automatically on process termination.
  end.

%% The internal helper functions are used to allocate and deallocate frequencies.
%% The state of frequency allocation is held in two lists in a tuple.
%% Frequencies are moved from one list to the other as they are allocated and free-ed.
%% In the allocated list they are associated with the Pids of the relevant processes.
%% There should be some retirement policy timeout - not implemented.
%% no_frequency is a domain specific unset value indicating no allocation is recorded
%% (stopping and restarting the server will potentially give a mismatch of expectations!)

allocation(Allocated,Pid) ->
 case lists:keyfind(Pid,2,Allocated) of
   {AllocatedFreq,Pid} -> AllocatedFreq;
   _ -> no_frequency
   end.
   
hasAllocation(Allocated,Freq,Pid) ->
  case lists:keyfind(Freq,1,Allocated) of
    {Freq,Pid} -> true;
    _ -> false
  end.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  case allocation(Allocated, Pid) of
    no_frequency ->
      {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}};
    _ ->
      {{[Freq|Free], Allocated}, {error}}
  end.

%% minimal, unfussy, sane version
deallocate({Free, Allocated},Pid) ->
  case allocation(Allocated,Pid) of
    no_frequency ->
      {{Free,Allocated},ok};
    Freq ->
      NewAllocated=lists:keydelete(Freq, 1, Allocated),
      {{[Freq|Free],  NewAllocated}, ok}
  end.

deallocate({Free, Allocated}, Freq, Pid) ->
  case hasAllocation(Allocated, Freq, Pid) of
  true ->
    NewAllocated=lists:keydelete(Freq, 1, Allocated),
    {{[Freq|Free],  NewAllocated}, ok};
  false ->
    {{ Free, Allocated }, not_allocated}
  end.
  
