-module(tpTracerSocket).

-export([
   start_link/1
   , init/2
   , accept_loop/2
   , trace_loop/2
]).

%% sys callbacks
-export([
   system_continue/3
   , system_terminate/4
   , system_code_change/4
]).

-record(state, {
   parent :: pid(),
   lSocket :: inet:socket(),
   timerRef :: reference() | undefined
}).

start_link(TracerOpts) ->
   BasePort = maps:get(port, TracerOpts),
   Pid = proc_lib:spawn_link(?MODULE, init, [self(), BasePort]),
   {ok, Pid}.

init(Parent, Port) ->
   process_flag(message_queue_data, off_heap),
   process_flag(trap_exit, true),
   {ok, LSocket} = gen_tcp:listen(Port, [binary, {reuseaddr, true}, {nodelay, true}, {packet, 2}, {active, true}, {backlog, 1}]),
   accept(#state{parent = Parent, lSocket = LSocket}).

accept(State = #state{lSocket = LSocket}) ->
   {ok, AcceptRef} = prim_inet:async_accept(LSocket, -1),
   ?MODULE:accept_loop(State, AcceptRef).

accept_loop(State = #state{parent = Parent, lSocket = LSocket}, AcceptRef) ->
   receive
      {system, From, Request} ->
         sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {accept_loop, State, AcceptRef});
      {'EXIT', Parent, Reason} ->
         exit(Reason);
      {inet_async, LSocket, AcceptRef, {ok, CSocket}} ->
         ?MODULE:trace_loop(set_timeout(State), CSocket);
      {inet_async, LSocket, AcceptRef, Error} ->
         exit({accept_error, Error});
      _ ->
         %% We discard all trace events when no client is connected.
         %% We may also end up discarding old timeouts or TCP messages.
         ?MODULE:accept_loop(State, AcceptRef)
   end.

trace_loop(State = #state{parent = Parent, timerRef = TRef}, CSocket) ->
   receive
      {'EXIT', Parent, Reason} ->
         exit(Reason);
      {system, From, Request} ->
         sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {trace_loop, State, CSocket});
   %% Reset the timeout when we receive data.
      {tcp, CSocket, _} ->
         ?MODULE:trace_loop(reset_timeout(State), CSocket);
      {tcp_closed, CSocket} ->
         close(State, CSocket);
      {tcp_error, CSocket, _} ->
         close(State, CSocket);
      {timeout, TRef, ?MODULE} ->
         close(State, CSocket);
   %% Discard the non-blocking send reply when successful.
      {inet_reply, CSocket, ok} ->
         ?MODULE:trace_loop(State, CSocket);
   %% And close the socket when an error occured.
      {inet_reply, CSocket, _} ->
         close(State, CSocket);
   %% Discard TCP messages from closed sockets.
      {tcp, _, _} ->
         ?MODULE:trace_loop(State, CSocket);
      {tcp_closed, _} ->
         ?MODULE:trace_loop(State, CSocket);
      {tcp_error, _, _} ->
         ?MODULE:trace_loop(State, CSocket);
   %% Discard any previous timeout.
      {timeout, _, ?MODULE} ->
         ?MODULE:trace_loop(State, CSocket);
      Msg ->
         Bin = term_to_binary(Msg),
         %% _ = byte_size(Bin),
         case erlang:port_command(CSocket, <<Bin/binary>>, [nosuspend]) of
            true ->
               ?MODULE:trace_loop(State, CSocket);
            %% The send buffer is full.
            _ ->
               close(State, CSocket)
         end
   end.

close(State, CSocket) ->
   _ = gen_tcp:close(CSocket),
   accept(cancel_timeout(State)).

system_continue(_, _, {LoopTag, State, LoopArgs}) ->
   case LoopTag of
      accept_loop ->
         ?MODULE:accept_loop(State, LoopArgs);
      trace_loop ->
         ?MODULE:trace_loop(State, LoopArgs)
   end.

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, _) ->
   exit(Reason).

system_code_change(Misc, _, _, _) ->
   {ok, Misc}.

reset_timeout(State) ->
   set_timeout(cancel_timeout(State)).

set_timeout(State) ->
   TRef = erlang:start_timer(5000, self(), ?MODULE),
   State#state{timerRef = TRef}.

cancel_timeout(State = #state{timerRef = TRef}) ->
   _ = erlang:cancel_timer(TRef, [{async, true}, {info, false}]),
   State#state{timerRef = undefined}.
