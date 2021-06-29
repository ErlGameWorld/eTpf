-module(tpTracerNif).

-export([
   enabled/3
   , enabled_call/3
   , enabled_procs/3
   , enabled_send/3
   , enabled_receive/3
   , enabled_running_procs/3
   , enabled_garbage_collection/3
   , trace/5
]).

-on_load(on_load/0).
on_load() ->
   SoName =
      case code:priv_dir(?MODULE) of
         {error, _} ->
            case code:which(?MODULE) of
               Filename when is_list(Filename) ->
                  filename:join([filename:dirname(Filename), "../priv", "tpTracerNif"]);
               _ ->
                  filename:join("../priv", "tpTracerNif")
            end;
         Dir ->
            filename:join(Dir, "tpTracerNif")
      end,
   erlang:load_nif(SoName, 0).

enabled(_, _, _) ->
   erlang:nif_error({not_loaded, ?MODULE}).

enabled_call(_, _, _) ->
   erlang:nif_error({not_loaded, ?MODULE}).

enabled_procs(_, _, _) ->
   erlang:nif_error({not_loaded, ?MODULE}).

enabled_send(_, _, _) ->
   erlang:nif_error({not_loaded, ?MODULE}).

enabled_receive(_, _, _) ->
   erlang:nif_error({not_loaded, ?MODULE}).

enabled_running_procs(_, _, _) ->
   erlang:nif_error({not_loaded, ?MODULE}).

enabled_garbage_collection(_, _, _) ->
   erlang:nif_error({not_loaded, ?MODULE}).

trace(_, _, _, _, _) ->
   erlang:nif_error({not_loaded, ?MODULE}).
