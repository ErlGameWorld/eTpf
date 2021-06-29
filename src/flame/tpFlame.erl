-module(tpFlame).          %% 火焰图分析

-export([
   pfs/2
   , pfm/3
]).

-record(state, {
   outputFile = "",
   pid,
   lastTs,
   count = 0,
   acc = []
}).

-spec pfs(file:filename_all(), file:filename_all()) -> ok.
pfs(InputFile, OutputFile) ->
   {ok, FinalState} = tpFReader:fold(fun handleEvent/2, #state{outputFile = OutputFile}, InputFile),
   flush(FinalState).

-spec pfm(file:filename(), filelib:dirname(), file:filename()) -> ok.
pfm(InputFiles, Cwd, OutputFile) ->
   PfFiles = lists:sort(filelib:wildcard(InputFiles, Cwd)),
   doPfm(PfFiles, #state{outputFile = OutputFile}).

doPfm([], State) ->
   flush(State);
doPfm([InputFile | PfFiles], State) ->
   {ok, NewState} = tpFReader:fold(fun handleEvent/2, State, InputFile),
   doPfm(PfFiles, NewState).

handleEvent(Trace, State) ->
   Pid = element(2, Trace),
   PidState = getPidState(Pid),
   NewPidState = doExpInner(Trace, PidState),
   setPidState(Pid, NewPidState),
   State.

doExpInner({call, Pid, TS, MFA, BIN}, #state{lastTs = LastTS, acc = Acc, count = Count} = PS) ->
   try
      %% Calculate time elapsed, TS-LastTs. 计算经过的时间，TS-LastTs。
      %% 0. If Acc is empty, then skip step #1.   0. 如果 Acc 为空，则跳过步骤 1。
      %% 1. Credit elapsed time to the stack on the top of Acc. 1. 将经过的时间计入 Acc 顶部的堆栈。
      %% 2. Push a 0 usec item with this stack onto Acc. 2. 将带有此堆栈的 0 usec 项目推送到 Acc。

      Stack = lists:filter(fun(<<"unknown function">>) -> false;(_) -> true end, stackToBin(BIN)),
      TrimStack = stackTrim(Stack),
      MFABin = mfaToBin(MFA),
      NewStack = [MFABin | lists:reverse(TrimStack)],
      NewAcc =
         case Acc of
            [] ->
               [{NewStack, 0}];
            [{LastStack, LastTime} | Tail] ->
               USec = TS - LastTS,
               % io:format("Stack1: ~p ~p\n", [Stack1, USec]),
               [{NewStack, 0}, {LastStack, LastTime + USec} | Tail]
         end,
      %% TODO: more state tracking here.
      PS#state{pid = Pid, lastTs = TS, count = Count + 1, acc = NewAcc}
   catch Class:Reason:StackTrace ->
      io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, Class, Reason, StackTrace]),
      PS
   end;
doExpInner({return_to, _Pid, TS, MFA}, #state{lastTs = LastTS, acc = Acc} = PS) when LastTS =/= undefined ->
   try
      %% Calculate time elapsed, TS-LastTs.
      %% 1. Credit elapsed time to the stack on the top of Acc.
      %% 2. Push a 0 usec item with the "best" stack onto Acc.
      %%    "best" = MFA exists in the middle of the stack onto Acc,
      %%    or else MFA exists at the top of a stack elsewhere in Acc.
      [{LastStack, LastTime} | Tail] = Acc,
      MFABin = mfaToBin(MFA),
      BestStack = lists:dropwhile(fun(SomeMFA) when SomeMFA /= MFABin -> true;(_) -> false end, findMatchingStack(MFABin, Acc)),
      USec = TS - LastTS,
      NewAcc = [{BestStack, 0}, {LastStack, LastTime + USec} | Tail],
      % io:format(user, "return-to: ~p\n", [lists:sublist(Acc2, 4)]),
      PS#state{lastTs = TS, acc = NewAcc}
   catch Class:Reason:StackTrace ->
      io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, Class, Reason, StackTrace]),
      PS
   end;
doExpInner({gc_start, _Pid, TS, _Info}, #state{lastTs = LastTS, acc = Acc} = PS) ->
   try
      %% Push a 0 usec item onto Acc.
      [{LastStack, LastTime} | Tail] = Acc,
      NewStack = [<<"GARBAGE-COLLECTION">> | LastStack],
      USec = TS - LastTS,
      NewAcc = [{NewStack, 0}, {LastStack, LastTime + USec} | Tail],
      % io:format(user, "GC 1: ~p\n", [lists:sublist(Acc2, 4)]),
      PS#state{lastTs = TS, acc = NewAcc}
   catch Class:Reason:StackTrace ->
      io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, Class, Reason, StackTrace]),
      PS
   end;
doExpInner({gc_end, _Pid, TS, _Info}, #state{lastTs = LastTS, acc = Acc} = PS) ->
   try
      %% Push the GC time onto Acc, then push 0 usec item from last exec
      %% stack onto Acc.
      [{GCStack, GCTime}, {LastExecStack, _} | Tail] = Acc,
      USec = TS - LastTS,
      NewAcc = [{LastExecStack, 0}, {GCStack, GCTime + USec} | Tail],
      % io:format(user, "GC 2: ~p\n", [lists:sublist(Acc2, 4)]),
      PS#state{lastTs = TS, acc = NewAcc}
   catch Class:Reason:StackTrace ->
      io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, Class, Reason, StackTrace]),
      PS
   end;
doExpInner({out, _Pid, TS, MFA}, #state{lastTs = LastTS, acc = Acc} = PS) when LastTS =/= undefined ->
   try
      %% Push a 0 usec item onto Acc.
      %% The MFA reported here probably doesn't appear in the stacktrace
      %% given to us by the last 'call', so add it here.
      [{LastStack, LastTime} | Tail] = Acc,
      MFA_bin = mfaToBin(MFA),
      NewStack = [<<"SLEEP">>, MFA_bin | LastStack],
      USec = TS - LastTS,
      NewAcc = [{NewStack, 0}, {LastStack, LastTime + USec} | Tail],
      PS#state{lastTs = TS, acc = NewAcc}
   catch Class:Reason:StackTrace ->
      io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, Class, Reason, StackTrace]),
      PS
   end;
doExpInner({in, _Pid, TS, MFA}, #state{lastTs = LastTS, acc = Acc} = PS) when LastTS =/= undefined ->
   try
      %% Push the Sleep time onto Acc, then push 0 usec item from last
      %% exec stack onto Acc.
      %% The MFA reported here probably doesn't appear in the stacktrace
      %% given to us by the last 'call', so add it here.
      MFA_bin = mfaToBin(MFA),
      [{SleepStack, SleepTime}, {LastExecStack, _} | Tail] = Acc,
      USec = TS - LastTS,
      NewAcc = [{[MFA_bin | LastExecStack], 0}, {SleepStack, SleepTime + USec} | Tail],
      PS#state{lastTs = TS, acc = NewAcc}
   catch Class:Reason:StackTrace ->
      io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, Class, Reason, StackTrace]),
      PS
   end;
doExpInner(_Else, PS) ->
   % io:format("?? ~P\n", [_Else, 10]),
   PS.

findMatchingStack(MFABin, [{H, _Time} | _] = Acc) ->
   case lists:member(MFABin, H) of
      true ->
         H;
      _ ->
         forFindMatchingStack(MFABin, Acc)
   end.

forFindMatchingStack(MFABin, [{[MFABin | _StackTail] = Stack, _Time} | _]) ->
   Stack;
forFindMatchingStack(MFABin, [_H | T]) ->
   forFindMatchingStack(MFABin, T);
forFindMatchingStack(_MFABin, []) ->
   [<<"FIND-MATCHING-FAILED">>].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

intersperse(_, []) -> [];
intersperse(_, [X]) -> [X];
intersperse(Sep, [X | Xs]) -> [X, Sep | intersperse(Sep, Xs)].

stackTrim([<<"proc_lib:init_p_do_apply/3">>, <<"gen_fsm:decode_msg/9">>, <<"gen_fsm:handle_msg/7">>, <<"gen_fsm:loop/7">> | T]) ->
   stackTrim([<<"GEN-FSM">> | T]);
stackTrim([<<"GEN-FSM">>, <<"gen_fsm:decode_msg/9">>, <<"gen_fsm:handle_msg/7">>, <<"gen_fsm:loop/7">> | T]) ->
   stackTrim([<<"GEN-FSM">> | T]);
stackTrim(Else) ->
   Else.

stackToBin(Bin) when is_binary(Bin) ->
   [list_to_binary(X) || X <- stack(Bin)];
stackToBin(X) ->
   eFmt:formatBin("~w", [X]).

mfaToBin({M, F, A}) ->
   eFmt:formatBin("~w:~w/~w", [M, F, A]);
mfaToBin(X) ->
   eFmt:formatBin("~w", [X]).

%% Borrowed from redbug.erl
stack(Bin) ->
   lists:foldl(fun munge/2, [], string:tokens(binary_to_list(Bin), "\n")).

munge(I, Out) ->
   case I of %% lists:reverse(I) of
      "..." ++ _ -> ["truncated!!!" | Out];
      _ ->
         case string:str(I, "Return addr") of
            0 ->
               case string:str(I, "cp = ") of
                  0 -> Out;
                  _ -> [mfaf(I) | Out]
               end;
            _ ->
               case string:str(I, "erminate process normal") of
                  0 -> [mfaf(I) | Out];
                  _ -> Out
               end
         end
   end.

mfaf(I) ->
   [_, C | _] = string:tokens(I, "()+"),
   string:strip(C).

flush(#state{outputFile = OutputFile}) ->
   PidStates = get(),
   {ok, FH} = file:open(OutputFile, [write, raw, binary, delayed_write]),
   io:format("\n\nWriting to ~s for ~w processes...", [OutputFile, length(PidStates)]),
   [
      [
         begin
          PidStr = eFmt:formatBin("~w", [Pid]),
          Size = byte_size(PidStr),
          TimeStr = integer_to_binary(Time),
          file:write(FH, [<<"(">>, binary:part(PidStr, 1, Size -2) , <<")">>, $;, intersperse($;, lists:reverse(Stack)), 32, TimeStr, 10])
       end || {Stack, Time} <- Acc
      ] || {Pid, #state{acc = Acc} = _PS} <- PidStates],
   ok = file:close(FH),
   io:format("finished!\n"),
   ok.

setPidState(Pid, PidState) ->
   erlang:put(Pid, PidState).

getPidState(Pid) ->
   case erlang:get(Pid) of
      undefined ->
         io:format("~p ", [Pid]),
         #state{};
      SomeState ->
         SomeState
   end.

