-module(tpSocketCli).
-behaviour(gen_srv).

-export([start_link/2]).
-export([stop/1]).

-export([
   init/1
   , handleCall/3
   , handleCast/2
   , handleInfo/2
   , terminate/2
   , code_change/3
]).

-record(state, {
   port :: inet:port_number(),
   fBaseName :: file:filename_all(),
   nth = 0 :: non_neg_integer(),
   socket :: inet:socket() | undefined,
   ioDevice :: file:io_device() | undefined,
   eventsPerFrame = 100000 :: pos_integer(),
   eventsThisFrame = 0 :: non_neg_integer(),
   buffer = <<>> :: binary()
}).

start_link(Port, BaseFilename) ->
   gen_srv:start_link(?MODULE, [Port, BaseFilename], []).

stop(Pid) ->
   gen_srv:stop(Pid).

init([Port, FBaseName]) ->
   process_flag(message_queue_data, off_heap),
   process_flag(trap_exit, true),
   {ok, #state{port = Port, fBaseName = FBaseName}, {nTimeout, connect, 0, doConnect}}.

handleCall(_Msg, _State, _FROM) ->
   {reply, ignored}.

handleCast(_Msg, _State) ->
   kpS.

handleInfo({tcp, Socket, Bin}, State = #state{socket = Socket, ioDevice = IoDevice, eventsPerFrame = MaxEvents, eventsThisFrame = NumEvents0, buffer = Buffer0}) ->
   BinSize = byte_size(Bin),
   Buffer = <<Buffer0/binary, BinSize:16, Bin/binary>>,
   NumEvents = NumEvents0 + 1,
   if
      MaxEvents =:= NumEvents ->
         ok = file:write(IoDevice, zip:compress_frame(Buffer)),
         {noreply, State#state{eventsThisFrame = 0, buffer = <<>>}};
      true ->
         {noreply, State#state{eventsThisFrame = NumEvents, buffer = Buffer}}
   end;
handleInfo({tcp_closed, _Socket}, State) ->
   {noreply, clearSet(State), {nTimeout, connect, 0, doConnect}};
handleInfo({tcp_error, _Socket, _}, State) ->
   {noreply, clearSet(State), {nTimeout, connect, 0, doConnect}};
handleInfo(doConnect, #state{port = Port, fBaseName = FBaseName, nth = Nth} = State) ->
   case gen_tcp:connect("localhost", Port, [binary, {packet, 2}, {active, true}]) of
      {ok, Socket} ->
         Filename = filename:flatten([FBaseName, ".", integer_to_list(Nth)]),
         {ok, IoDevice} = file:open(Filename, [write, raw]),
         {noreply, State#state{socket = Socket, nth = Nth + 1, ioDevice = IoDevice}};
      {error, _} ->
         {noreply, State, {nTimeout, connect, 1000, doConnect}}
   end;
handleInfo(_Msg, _State) ->
   kpS.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

terminate(_Reason, State) ->
   clearSet(State),
   ok.

clearSet(#state{socket = Socket, ioDevice = IoDevice, buffer = Buffer} = State) ->
   case Socket of
      undefined ->
         ignore;
      _ ->
         _ = gen_tcp:close(Socket)
   end,

   case IoDevice of
      undefined ->
         ignore;
      _ ->
         _ = file:write(IoDevice, lz4f:compress_frame(Buffer)),
         _ = file:close(IoDevice)
   end,
   State#state{socket = undefined, ioDevice = undefined, buffer = <<>>}.

