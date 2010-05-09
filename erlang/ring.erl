-module(ring).

-export([start/2, start/0]).

ringlink(NextPid) ->
    %%io:format("ringlink with Pid: ~p awaiting orders~n", [self()]),
    receive
        {setnext, NewNextPid} ->
            %%io:format("Setting my nextpid to: ~p and I am ~p~n", [NewNextPid, self()]),
            ringlink(NewNextPid);
        {message, Message, Counter} ->
            io:format("Self: ~p, Message: ~p, Counter: ~p~n", [self(), Message, Counter]),
            case Counter of
                1 ->
                    void;
                _ ->
                    NextPid ! {message, Message, Counter-1}
            end,
            ringlink(NextPid);
        exit -> void
    end.

start(NumNodes, NumMessages) ->
    TotalMessages = NumNodes * NumMessages,
    Pids = for(1, NumNodes, fun() -> spawn(fun() -> ringlink(undefined) end) end),
    %%io:format("Pids are: ~p~n", [Pids]),
    {NextPids, _} = lists:mapfoldl(fun(_A, Idx) ->
                                           NIndex = (Idx+1) rem (NumNodes+1),
                                           NeIndex = case NIndex of
                                                         0 -> 1;
                                                         _ -> NIndex
                                                     end,
                                           {lists:nth(NeIndex, Pids), Idx+1}
                                   end, 1, Pids),
    %%io:format("NextIndexes: ~p~n", [NextPids]),
    %%io:format("NextPids: ~p~n", [NextPids]),
    PidGroups = lists:zip(Pids,NextPids),
    %%io:format("PidGroups: ~p~n", [PidGroups]),
    lists:foreach(fun({Pid, NextPid}) ->
                          Pid ! {setnext, NextPid}
                  end, PidGroups),
    FirstPid = lists:nth(1, Pids),
    FirstPid ! {message, "Hi there", TotalMessages}.

start() ->
    start(5, 5).

for(N, N, F) ->
     [F()];
for(I, N, F) ->
    [F()|for(I+1, N, F)].
