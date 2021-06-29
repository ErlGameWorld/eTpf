-module(tpTracerShell).

-export([
   start_link/1
   , init/1
   , loop/1
]).

-export([
   system_continue/3
   , system_terminate/4
   , system_code_change/4
]).

start_link(_TracerOpts) ->
   Pid = proc_lib:spawn_link(?MODULE, init, [self()]),
   {ok, Pid}.

init(Parent) ->
   process_flag(message_queue_data, off_heap),
   process_flag(trap_exit, true),
   ?MODULE:loop(Parent).

loop(Parent) ->
   receive
      {system, From, Request} ->
         sys:handle_system_msg(Request, From, Parent, ?MODULE, [], Parent);
      {'EXIT', Parent, Reason} ->
         terminate(Reason);
      RMsg ->
         io:format("~10000p\n", [RMsg]),
         %erlang:display(RMsg),
         ?MODULE:loop(Parent)
   end.

system_continue(_, _, Parent) ->
   ?MODULE:loop(Parent).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, _) ->
   exit(Reason).

system_code_change(Misc, _, _, _) ->
   {ok, Misc}.

-spec terminate(any()) -> no_return().
terminate(Reason) ->
   exit(Reason).
