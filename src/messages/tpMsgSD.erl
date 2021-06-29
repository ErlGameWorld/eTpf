-module(tpMsgSD).             %% 进程消息时序分析

-include("eTpf.hrl").

-export([
   pfs/2
   , pfm/3
]).

-record(state, {
   meta = #{} :: map(),
   events = [],
   pids
}).

-spec pfs(file:filename_all(), list()) -> ok.
pfs(InputFile, Pids) ->
   {ok, FinalState} = tpFReader:fold(fun handleEvent/2, #state{pids = preparePids(Pids)}, InputFile),
   flush(FinalState).

-spec pfm(file:filename(), filelib:dirname(), list()) -> ok.
pfm(InputFiles, Cwd, Pids) ->
   PfFiles = lists:sort(filelib:wildcard(InputFiles, Cwd)),
   doPfm(PfFiles, #state{pids = preparePids(Pids)}).

doPfm([], State) ->
   flush(State);
doPfm([InputFile | PfFiles], State) ->
   {ok, NewState} = tpFReader:fold(fun handleEvent/2, State, InputFile),
   doPfm(PfFiles, NewState).

handleEvent({send, From, _, Info, ?eTpfHole}, State = #state{meta = Meta}) ->
   NewMeta =
      case Meta of
         #{From := OldInfo} ->
            Meta#{From => maps:merge(OldInfo, Info)};
         _ ->
            Meta#{From => Info}
      end,
   State#state{meta = NewMeta};
handleEvent({send, From, _, _, To} = Event, State) ->
   maybeKeepEvent(Event, From, To, State);
handleEvent({send_to_non_existing_process, From, _, _, To} = Event, State) ->
   maybeKeepEvent(Event, From, To, State);
handleEvent({spawn, From, _, To, _} = Event, State) ->
   maybeKeepEvent(Event, From, To, State);
handleEvent({exit, Pid0, _, _} = Event, State = #state{events = Events, pids = Pids}) ->
   Pid = hidePidNode(Pid0),
   case lists:member(Pid, Pids) of
      true ->
         State#state{events = [Event | Events]};
      _ ->
         State
   end;
%% Ignore all other events. We only care about messages and spawns/exits.
handleEvent(_, State) ->
   State.

maybeKeepEvent(Event, From0, To0, State = #state{events = Events, pids = Pids}) ->
   From = hidePidNode(From0),
   To = hidePidNode(To0),
   case {lists:member(From, Pids), lists:member(To, Pids)} of
      {true, true} -> State#state{events = [Event | Events]};
      _ -> State
   end.

preparePids(Pids) ->
   Pids.
%% [hide_pid_node(Pid) || Pid <- Pids].

hidePidNode(Pid) when is_pid(Pid) ->
   Pid;
%%hide_pid_node(pid_to_list(Pid));
hidePidNode([$<, _, $. | Tail]) -> "<***." ++ Tail;
hidePidNode([$<, _, _, $. | Tail]) -> "<***." ++ Tail;
hidePidNode([$<, _, _, _, $. | Tail]) -> "<***." ++ Tail;
hidePidNode([$<, _, _, _, _, $. | Tail]) -> "<***." ++ Tail;
hidePidNode([$<, _, _, _, _, _, $. | Tail]) -> "<***." ++ Tail;
hidePidNode(Name) -> Name.

flush(#state{events = Events} = State) ->
   %% Sort by timestamp from oldest to newest.
   SortEvents = lists:keysort(3, Events),
   %% Initialize the formatting state.
   put(num_calls, 0),
   %% Output everything.
   HeaderBin = <<"seqdiag {\n"
   "    edge_length = 300;\n"
   "    activation = none;\n"
   "\n">>,

   writeEvents(SortEvents, State, HeaderBin),

   io:format(
      "The file seq.diag was created. Use seqdiag to make a PNG.~n"
      "$ seqdiag -Tpng --no-transparency seq.diag~n"
      "~n"
      "To use a custom font, use the -f modifier:~n"
      "$ seqdiag -Tpng --no-transparency -f /usr/share/fonts/TTF/verdana.ttf seq.diag~n"
      "~n"
      "You can also edit the file to remove uninteresting messages.~n"
      "One line in the file is equal to a message sent by a process to another.~n"),
   ok.

writeEvents([], _State, BinAcc) ->
   LastBinAcc = <<BinAcc/binary, "}\n">>,
   ok = file:write_file(<<"seq.diag">>, LastBinAcc);
writeEvents([Event | SortEvents], State, BinAcc) ->
   EventBin = formatEvent(Event, State),
   writeEvents(SortEvents, State, <<BinAcc/binary, EventBin/binary>>).

formatEvent({spawn, From, _, To, MFA}, State) ->
   eFmt:formatBin(<<"    \"~w~s\" ->> \"~w~s\" [label=\"spawn ~9999P\"];~n">>, [From, label(From, State), To, label(To, State), MFA, 8]);
formatEvent({exit, Pid, _, Reason}, State) ->
   PidLabel = label(Pid, State),
   eFmt:formatBin(<<"    \"~w~s\" ->> \"~w~s\" [label=\"exit ~9999P\"];~n">>, [Pid, PidLabel, Pid, PidLabel, Reason, 8]);
formatEvent({Type, From, _, {'$gen_call', {From, Ref}, Msg}, To}, State) ->
   NumCalls = get(num_calls) + 1,
   put(num_calls, NumCalls),
   put(Ref, NumCalls),
   eFmt:formatBin(<<"    \"~w~s\" ~s \"~w~s\" [label=\"gen:call #~w ~9999P\"];~n">>, [From, label(From, State),
      case Type of send -> "->"; _ -> "-->" end, To, label(To, State), NumCalls, Msg, 8]);
formatEvent(Event = {Type, From, _, {Ref, Msg}, To}, State) ->
   case get(Ref) of
      undefined ->
         defFormatEvent(Event, State);
      NumCall ->
         eFmt:formatBin(<<"    \"~w~s\" ~s \"~w~s\" [label=\"#~w ~9999P\"];~n">>, [From, label(From, State),
            case Type of send -> "->"; _ -> "-->" end, To, label(To, State), NumCall, Msg, 8])
   end;
formatEvent(Event, State) ->
   defFormatEvent(Event, State).

defFormatEvent({Type, From, _, Msg, To}, State) ->
   eFmt:formatBin(<<"    \"~w~s\" ~s \"~w~s\" [label=\"~9999P\"];~n">>, [From, label(From, State),
      case Type of send -> "->"; _ -> "-->" end, To, label(To, State), Msg, 8]).

label(P, #state{meta = Meta}) ->
   case maps:get(P, Meta, #{}) of
      #{process_type := PT} ->
         eFmt:formatBin(" (~w)", [PT]);
      _ ->
         <<"">>
   end.
