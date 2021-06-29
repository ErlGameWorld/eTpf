-module(eTpf).

-include("eTpf.hrl").

-export([
	%% trace函数
	trace/1
	, trace/2
	, trace/3
	, trace/6
	, stop/0
	, stop/1

	%% 记录函数
	, sts/0             %% tpTracerShell
	, stl/0           %% tpTracerLog
	, stf/0           %% tpTracerFile

	%% pt函数
	, pts/0           %% tpTracerShell
	, ptl/0           %% tpTracerLog
	, ptf/0           %% tpTracerFile

	%% pf函数
	, rsPfs/1
	, rsPfm/2

	, sdPfs/2
	, sdPfm/3

	, flPfs/2
	, flPfm/3

	, cgPfs/2
	, cgPfs/3
	, cgPfm/3
	, cgPfm/4

	, gcPfs/1
	, gcPfm/2
]).

-compile([export_all, nowarn_export_all]).

sts() ->
	trace([self(), whereis(esSyncSrv)], [call, return_to], [{test, '_', '_'}, {app, eSync}], tpTracerShell, #{}, #{}).

sts1() ->
	trace([test, {scope, [self()]}]).

stl() ->
	trace(new_processes, [call, arity, return_to, garbage_collection], [{app, eAcs}], tpTracerLog, #{}, #{stackTc => true}).

stf() ->
	trace(new_processes, [call, arity, return_to, garbage_collection], ['_'], tpTracerFile, #{}, #{stackTc => true}).

stop() ->
	stop(?eTpfTracerId).

stop(TracerId) ->
	supervisor:terminate_child(eTpf_sup, TracerId).

-spec pts() -> pattern().
pts() ->
	[{app, kernel}, {app, stdlib}, {app, looking_glass}].

ptl() ->
	ok.


ptf() ->
	spawn(eAcs, replaceSw, [<<"fdsfads拉法叶舰fds淫秽ffdsfdsffdddd"/utf8>>]),
	ok.

rsPfs(InputFile) ->
	tpMsgRS:pfs(InputFile).

rsPfm(InputFiles, Cwd) ->
	tpMsgRS:pfm(InputFiles, Cwd).

sdPfs(InputFile, Pids) ->
	tpMsgSD:pfs(InputFile, Pids).

sdPfm(InputFiles, Cwd, Pids) ->
	tpMsgSD:pfm(InputFiles, Cwd, Pids).

flPfs(InputFile, OutputPath) ->
	tpFlame:pfs(InputFile, OutputPath).

flPfm(InputFiles, Cwd, OutputPath) ->
	tpFlame:pfm(InputFiles, Cwd, OutputPath).

cgPfs(InputFile, Output) ->
	tpCallGrind:pfs(InputFile, Output).

cgPfs(InputFile, Output, Opts) ->
	tpCallGrind:pfs(InputFile, Output, Opts).

cgPfm(Wildcard, Cwd, Prefix) ->
	tpCallGrind:pfm(Wildcard, Cwd, Prefix).

cgPfm(Wildcard, Cwd, Prefix, Opts) ->
	tpCallGrind:pfm(Wildcard, Cwd, Prefix, Opts).

gcPfs(InputFile) ->
	tpGcColl:pfs(InputFile).

gcPfm(InputFiles, Cwd) ->
	tpGcColl:pfm(InputFiles, Cwd).

-spec trace(pidPortSpec()) -> ok.
trace(PidPortSpec) ->
	trace(PidPortSpec, tpTracerShell).

-spec trace(pidPortSpec(), module()) -> ok.
trace(PidPortSpec, TracerMod) ->
	trace(PidPortSpec, TracerMod, #{}).

-spec trace(pidPortSpec(), module(), tracerOpts()) -> ok.
trace(PidPortSpec, TracerMod, TracerOpts) ->
	trace(PidPortSpec, [all], [], TracerMod, TracerOpts, #{}).

-spec trace(pidPortSpec(), flagList(), pattern(), module(), tracerOpts(), traceOpts()) -> ok.
trace(PidPortSpec, FlagList, TraceMods, TracerMod, TracerOpts, TraceOpts) ->
	InputList = case is_list(PidPortSpec) of true -> PidPortSpec; _ -> [PidPortSpec] end,
	doTrace(InputList, FlagList, TraceMods, TracerMod, TracerOpts, TraceOpts).

doTrace(PidPortSpec, FlagList, TraceMods, TracerMod, TracerOpts, TraceOpts) ->
	_ = application:ensure_all_started(eTpf),

	TracerId = maps:get(tracerId, TraceOpts, ?eTpfTracerId),
	TracerSpec = #{
		id => TracerId
		, start => {TracerMod, start_link, [TracerOpts]}
		, restart => temporary
		, type => worker
		, shutdown => infinity
	},

	case supervisor:start_child(eTpf_sup, TracerSpec) of
		{ok, TracerPid} ->
			TraceMFAs = flattenMods(TraceMods, []),

			% 每个进程只能由一个tracer进行跟踪。因此，跟踪已跟踪进程会失败。
			[
				begin
					erlang:trace(OnePidPortSpec, true, [{tracer, tpTracerNif, TracerPid} | FlagList])
				end || OnePidPortSpec <- PidPortSpec
			],

			PtFlags = maps:get(FlagList, TraceOpts, [local]),
			MatchSpec = ?IIF(maps:get(stackTc, TraceOpts, false), [{'_', [], [{message, {process_dump}}]}], true),
			[
				begin
				%% The module must be loaded before we attempt to trace it.
					_ = code:ensure_loaded(M),
					_ = erlang:trace_pattern(OneTraceMFA, MatchSpec, PtFlags)
				end || {M, _F, _A} = OneTraceMFA <- TraceMFAs
			],
			ok;
		_Err ->
			io:format("trace start error ~p~n", [_Err])
	end.

flattenMods([], Acc) ->
	lists:flatten(Acc);
flattenMods([{callback, Mod, Fun} | Tail], Acc) when is_atom(Mod), is_atom(Fun) ->
	Input = flattenMods(Mod:Fun(), []),
	flattenMods(Tail, [Input | Acc]);
flattenMods([{app, App} | Tail], Acc) when is_atom(App) ->
	_ = application:load(App),
	{ok, Mods} = application:get_key(App, modules),
	MFA = [{OneMod, '_', '_'} || OneMod <- Mods],
	flattenMods(Tail, [MFA | Acc]);
flattenMods([{_, _, _} = MFA | Tail], Acc) ->
	flattenMods(Tail, [MFA | Acc]);
flattenMods([OneMod | Tail], Acc) ->
	flattenMods(Tail, [{OneMod, '_', '_'} | Acc]).