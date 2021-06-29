-module(tpGcColl).             %% 进程消息发送接收统计分析

-include("eTpf.hrl").

-export([
   pfs/1
   , pfm/2
]).

-record(state, {
   meta = #{} :: map(),
   senders = #{} :: #{pid() => pos_integer()},
   receivers = #{} :: #{pid() => pos_integer()},
   pairs = #{} :: #{{pid(), pid()} => pos_integer()},
   nonExisting = #{} :: #{pid() => pos_integer()},
   lastMsgs = #{} :: #{pid() => atom()}
}).

-spec pfs(file:filename_all()) -> ok.
pfs(InputFile) ->
   {ok, FinalState} = tpFReader:fold(fun handleEvent/2, #state{}, InputFile),
   flush(FinalState).

-spec pfm(file:filename(), filelib:dirname()) -> ok.
pfm(InputFiles, Cwd) ->
   PfFiles = lists:sort(filelib:wildcard(InputFiles, Cwd)),
   doPfm(PfFiles, #state{}).

doPfm([], State) ->
   flush(State);
doPfm([InputFile | PfFiles], State) ->
   {ok, NewState} = tpFReader:fold(fun handleEvent/2, State, InputFile),
   doPfm(PfFiles, NewState).

%% @todo Later we may want to look at the latency of gen_server call/reply.
%% @todo Later we may want to look at particular messages, have some sort of callback.
handleEvent({send, From, _, Info, ?eTpfHole}, State = #state{meta = Meta}) ->
   NewMeta =
      case Meta of
         #{From := OldInfo} ->
            Meta#{From => maps:merge(OldInfo, Info)};
         _ ->
            Meta#{From => Info}
      end,
   State#state{meta = NewMeta};
handleEvent({send, From, _, Msg, To}, State = #state{senders = Senders, receivers = Receivers, pairs = Pairs, lastMsgs = LastMsgs}) ->
   SendersCnt = maps:get(From, Senders, 0),
   ReceiversCnt = maps:get(To, Receivers, 0),
   PairsCnt = maps:get({From, To}, Pairs, 0),
   State#state{
      senders = Senders#{From => SendersCnt + 1},
      receivers = Receivers#{To => ReceiversCnt + 1},
      pairs = Pairs#{{From, To} => PairsCnt + 1},
      lastMsgs = LastMsgs#{From => Msg}
   };
handleEvent({send_to_non_existing_process, From, _, Msg, _}, State = #state{nonExisting = NonExisting, lastMsgs = LastMsgs}) ->
   Count = maps:get(From, NonExisting, 0),
   State#state{
      nonExisting = NonExisting#{From => Count + 1},
      lastMsgs = LastMsgs#{From => Msg}};
%% Ignore all other events. We only care about messages.
handleEvent(_, State) ->
   State.

%% Output of the profiling.

flush(State) ->
   flushMostActiveSenders(State),
   flushMostActiveReceivers(State),
   flush_most_non_existing(State),
   flushMostActivePairUnidirectional(State),
   flushMostActivePairBidirectional(State),
   io:format("~n"),
   flushDigraph(State),
   ok.

flushMostActiveSenders(State = #state{senders = Senders}) ->
   List = lists:sublist(lists:reverse(lists:keysort(2, maps:to_list(Senders))), 1, 100),
   formatByCnt(<<"They sent the most messages">>, List, State).

flushMostActiveReceivers(State = #state{receivers = Receivers}) ->
   List = lists:sublist(lists:reverse(lists:keysort(2, maps:to_list(Receivers))), 1, 100),
   formatByCnt(<<"They received the most messages">>, List, State).

flush_most_non_existing(State = #state{nonExisting = NonExisting}) ->
   List = lists:sublist(lists:reverse(lists:keysort(2, maps:to_list(NonExisting))), 1, 100),
   formatByCnt(<<"They sent the most messages to dead processes">>, List, State).

formatByCnt(Title, List, State) ->
   MsgCols = case io:columns() of {ok, Cols} -> Cols;_ -> 80 end,
   io:format(
      "~n~s~n~s~n~n"
      "Process ID      Count      (Label) OR Message sent~n"
      "----------      -----      -----------------------~n",
      [Title, lists:duplicate(byte_size(Title), $=)]
   ),
   [
      begin
         {Prefix, Label, Suffix} = labelOrMsg(P, State),
         io:format("~-15w ~-10b ~s~" ++ integer_to_list(MsgCols) ++ "P~s~n", [P, C, Prefix, Label, 5, Suffix])
      end || {P, C} <- List
   ],
   ok.

labelOrMsg(P, #state{meta = Meta, lastMsgs = LastMsgs}) ->
   case maps:get(P, Meta, #{}) of
      #{process_type := PT} ->
         {"(", PT, ")"};
      _ ->
         {"", maps:get(P, LastMsgs, '<none>'), ""}
   end.

flushMostActivePairUnidirectional(State = #state{pairs = Pairs}) ->
   List = lists:sublist(lists:reverse(lists:keysort(2, maps:to_list(Pairs))), 1, 100),
   Title = <<"They sent the most messages to one other process">>,
   MsgCols = case io:columns() of {ok, Cols} -> Cols; _ -> 80 end,
   io:format(
      "~n~s~n~s~n~n"
      "From pid        To pid          Count      (Label) OR Message sent~n"
      "--------        ------          -----      -----------------------~n",
      [Title, lists:duplicate(byte_size(Title), $=)]
   ),
   [
      begin
         {Prefix, Label, Suffix} = labelOrMsg(F, State),
         io:format("~-15w ~-15w ~-10b ~s~" ++ integer_to_list(MsgCols) ++ "P~s~n", [F, T, C, Prefix, Label, 5, Suffix])
      end || {{F, T}, C} <- List
   ],
   ok.

flushMostActivePairBidirectional(State = #state{pairs = Pairs}) ->
   TemPairs = maps:fold(fun mergePairs/3, #{}, Pairs),
   List = lists:sublist(lists:reverse(lists:keysort(2, maps:to_list(TemPairs))), 1, 100),
   Title = <<"They sent the most messages to each other">>,
   MsgCols = case io:columns() of {ok, Cols} -> Cols;_ -> 80 end,
   io:format(
      "~n~s~n~s~n~n"
      "Count      Pid 1           (Label) OR Message sent~n"
      "           Pid 2           by the corresponding process~n"
      "-----      -----           ----------------------------~n",
      [Title, lists:duplicate(byte_size(Title), $=)]
   ),
   [
      begin
         {FPrefix, FLabel, FSuffix} = labelOrMsg(F, State),
         {TPrefix, TLabel, TSuffix} = labelOrMsg(T, State),
         io:format(
            "~-10b ~-15w ~s~" ++ integer_to_list(MsgCols) ++ "P~s~n"
            "           ~-15w ~s~" ++ integer_to_list(MsgCols) ++ "P~s~n", [C, F, FPrefix, FLabel, 5, FSuffix, T, TPrefix, TLabel, 5, TSuffix])
      end || {{F, T}, C} <- List
   ],
   ok.

flushDigraph(#state{pairs = Pairs} = State) ->
   TemPairs = maps:fold(fun groupPairs/3, #{}, Pairs),
   List = maps:to_list(TemPairs),
   HeaderBin =
      <<"digraph {\n"
      "    concentrate=true;\n"
      "    splines=ortho;\n"
      "    edge [arrowhead=none, labelfontsize=12.0, minlen=3];\n"
      "\n">>,

   writeEvents(List, State, HeaderBin),

   io:format(
      "The file digraph.gv was created. Use GraphViz to make a PNG.~n"
      "$ dot -Tpng -O digraph.gv~n"
      "~n"
      "You can also edit the file to remove uninteresting processes.~n"
      "One line in the file is equal to a connection between two processes.~n"),
   ok.

writeEvents([], _State, BinAcc) ->
   LastBinAcc = <<BinAcc/binary, "}\n">>,
   ok = file:write_file(<<"digraph.gv">>, LastBinAcc);
writeEvents([{{F, T}, {FC, TC}} | List], State, BinAcc) ->
   EventBin = eFmt:formatBin(<<"    \"~w~s\" -> \"~w~s\" [taillabel=~b, headlabel=~b];~n">>, [F, label(F, State), T, label(T, State), FC, TC]),
   writeEvents(List, State, <<BinAcc/binary, EventBin/binary>>).

label(P, #state{meta = Meta}) ->
   case maps:get(P, Meta, #{}) of
      #{process_type := PT} ->
         eFmt:format(<<" (~w)">>, [PT]);
      _ ->
         <<"">>
   end.

mergePairs({From, To}, Count, Acc) ->
   Key = case From < To of true -> {From, To};_ -> {To, From} end,
   OldCount = maps:get(Key, Acc, 0),
   Acc#{Key => OldCount + Count}.

groupPairs({From, To}, Count, Acc) when From < To ->
   Key = {From, To},
   {_, AccCount} = maps:get(Key, Acc, {0, 0}),
   Acc#{Key => {Count, AccCount}};
groupPairs({From, To}, Count, Acc) ->
   Key = {To, From},
   {AccCount, _} = maps:get(Key, Acc, {0, 0}),
   Acc#{Key => {AccCount, Count}}.
