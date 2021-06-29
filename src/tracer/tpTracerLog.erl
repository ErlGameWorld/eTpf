-module(tpTracerLog).
-include("eTpf.hrl").

-export([
   start_link/1
   , init/2
   , loop/1
]).

%% sys callbacks
-export([
   system_continue/3
   , system_terminate/4
   , system_code_change/4
]).

-record(state, {
   parent :: pid()
   , fDir :: file:filename_all()                         %% file base name
   , fBaseName :: file:filename_all()                    %% file base name
   , size = 0 :: non_neg_integer()
   , fMaxSize :: infinity | non_neg_integer()
   , ioDevice :: file:io_device()
   , tcmIsCut = false
}).

start_link(Opts) ->
   Pid = proc_lib:spawn_link(?MODULE, init, [self(), Opts]),
   {ok, Pid}.

init(Parent, TracerOpts) ->
   process_flag(message_queue_data, off_heap),
   process_flag(trap_exit, true),
   FDir = maps:get(fDir, TracerOpts, <<"./">>),
   FBaseName = maps:get(fBaseName, TracerOpts, <<"traces.log">>),
   Filename = fileName(FDir, FBaseName),
   {ok, IoDevice} = file:open(Filename, [write, raw]),

   TcmIsCut = maps:get(tcmIsCut, TracerOpts, maps:get(tcmDepth, ?defTcmMap)),
   case TcmIsCut of
      true ->
         erlang:put(tcmDepth, maps:get(tcmDepth, TracerOpts, maps:get(tcmDepth, ?defTcmMap))),
         erlang:put(tcmListSize, maps:get(tcmListSize, TracerOpts, maps:get(tcmListSize, ?defTcmMap))),
         erlang:put(tcmMapSize, maps:get(tcmMapSize, TracerOpts, maps:get(tcmMapSize, ?defTcmMap))),
         erlang:put(tcmTupleSize, maps:get(tcmTupleSize, TracerOpts, maps:get(tcmTupleSize, ?defTcmMap))),
         erlang:put(tcmBinSize, maps:get(tcmBinSize, TracerOpts, maps:get(tcmBinSize, ?defTcmMap))),
         erlang:put(tcmBitSize, maps:get(tcmBitSize, TracerOpts, maps:get(tcmBitSize, ?defTcmMap))),
         erlang:put(tcmNestStruct, maps:get(tcmNestStruct, TracerOpts, maps:get(tcmNestStruct, ?defTcmMap)));
      _ ->
         ignore
   end,

   State = #state{
      parent = Parent
      , fDir = FDir
      , fBaseName = FBaseName
      , ioDevice = IoDevice
      , fMaxSize = maps:get(fMaxSize, TracerOpts, 52428800)
      , tcmIsCut = TcmIsCut
   },
   ?MODULE:loop(State).

loop(#state{parent = Parent, fDir = FDir, fBaseName = FBaseName, size = Size, ioDevice = IoDevice, fMaxSize = MaxSize, tcmIsCut = TcmIsCut} = State) ->
   receive
      {system, From, Request} ->
         sys:handle_system_msg(Request, From, Parent, ?MODULE, [], State);
      {'EXIT', Parent, Reason} ->
         terminate(Reason, State);
      RMsg ->
         Msg = case TcmIsCut of true -> tpTermCut:cut(RMsg); _ -> RMsg end,
         MsgBin = eFmt:formatBin("~10000p\n", [Msg]),
         ok = file:write(IoDevice, MsgBin),
         NewSize = Size + byte_size(MsgBin),
         case NewSize >= MaxSize of
            true ->
               ok = file:close(IoDevice),
               Filename = fileName(FDir, FBaseName),
               {ok, NewIoDevice} = file:open(Filename, [write, raw]),
               ?MODULE:loop(State#state{size = 0, ioDevice = NewIoDevice});
            _ ->
               ?MODULE:loop(State#state{size = NewSize})
         end
   end.

system_continue(_, _, State) ->
   ?MODULE:loop(State).

-spec system_terminate(any(), _, _, #state{}) -> no_return().
system_terminate(Reason, _, _, State) ->
   terminate(Reason, State).

system_code_change(Misc, _, _, _) ->
   {ok, Misc}.

-spec terminate(any(), #state{}) -> no_return().
terminate(Reason, #state{ioDevice = IoDevice}) ->
   ok = file:close(IoDevice),
   exit(Reason).

fileName(Dir, FBaseName) ->
   {{Year, Month, Day}, {Hour, Minute, _Second}} = erlang:localtime(),
   TimeStr = eFmt:formatBin("~B~2.10.0B~2.10.0B~B~2.10.0B_", [Year, Month, Day, Hour, Minute]),
   FileName = <<TimeStr/binary, FBaseName/binary>>,
   WholeName = filename:absname(filename:join(Dir, FileName)),
   ok = filelib:ensure_dir(WholeName),
   WholeName.


