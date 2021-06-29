-module(tpCallGrind).      %% 调用堆栈和时间分析
-include("eTpf.hrl").

-export([
	pfs/2
	, pfs/3
	, pfm/3
	, pfm/4
]).

-type opts() :: #{
scope => global | per_process,                        %% Whether we filter the output per process.
running => boolean()                                  %% Whether we compute and save wait times.
}.

-record(call, {
	mfa :: atom(),                                     %% The MFA for the call.
	source :: {string(), pos_integer()},               %% The source file name.
	ts :: pos_integer(),                               %% The timestamp for the call.
	self_ts :: pos_integer(),                          %% The timestamp for when we last started executing this function.
	incl :: undefined | non_neg_integer(),             %% Execution time including subcalls.
	self = 0 :: integer(),                             %% Execution time excluding subcalls.
	count = 1 :: pos_integer(),                        %% Number of times the function was called.
	wait = 0 :: non_neg_integer(),                     %% Time when the process was not running in this function.
	waitIncl = 0 :: non_neg_integer(),                 %% Time when the process was not running in this function or any subcalls.
	waitCnt = 0 :: non_neg_integer(),                  %% Number of times the process was scheduled out.
	waitCntIncl = 0 :: non_neg_integer(),              %% Number of times the function or any subcall was scheduled out.
	calls = #{} :: #{atom() => #call{}}                %% Calls done by this MFA.
}).

-record(proc, {
	stack = [] :: [#call{}],                           %% Call stack.
	mfas = #{} :: #{atom() => #call{}},                %% Profile information waiting to be written to file.
	out = undefined :: undefined | non_neg_integer()   %% Timestamp the process got scheduled out.
}).

-record(state, {
	input :: file:filename_all(),                      %% Input file name.
	outputFile :: file:filename_all(),                     %% Output file name.
	ioDevice :: file:io_device(),                      %% Output fd.
	opts :: opts(),                                    %% Options.
	procs = #{} :: #{pid() => #proc{}},                 %% List of processes.
	sources = #{} :: #{mfa() => {string(), pos_integer()}}    %% Cache of source file information.
}).

-spec pfs(file:filename_all(), file:filename_all()) -> ok.
pfs(InputFile, OutputFile) ->
	pfs(InputFile, OutputFile, #{}).

-spec pfs(file:filename_all(), file:filename_all(), opts()) -> ok.
pfs(InputFile, OutputFile, Opts) ->
	{ok, IoDevice} = file:open(OutputFile, [write]),
	State = #state{input = InputFile, outputFile = OutputFile, ioDevice = IoDevice, opts = Opts},
	writeHeader(State),
	{ok, FinalState} = tpFReader:fold(fun handleEvent/2, State, InputFile),
	flush(FinalState),
	_ = file:close(IoDevice),
	ok.

-spec pfm(file:filename(), filelib:dirname(), file:filename()) -> ok.
pfm(Wildcard, Cwd, Prefix) ->
	pfm(Wildcard, Cwd, Prefix, #{}).

-spec pfm(file:filename(), filelib:dirname(), file:filename(), opts()) -> ok.
pfm(InputFiles, Cwd, OutputPrefix, Opts) ->
	PfFiles = lists:sort(filelib:wildcard(InputFiles, Cwd)),
	SeqList = lists:seq(1, length(PfFiles)),
	OutputFiles = [OutputPrefix ++ "." ++ integer_to_list(Index) || Index <- SeqList],
	ManyFiles = lists:zip(PfFiles, OutputFiles),
	Refs = [monitor(process, spawn_link(?MODULE, pfs, [InputFile, OutputFile, Opts])) || {InputFile, OutputFile} <- ManyFiles],
	waitForProc(Refs).

waitForProc([]) ->
	ok;
waitForProc(Refs) ->
	receive
		{'DOWN', R, process, _, _} ->
			waitForProc(Refs -- [R])
	end.

%% We handle trace events one by one, keeping track of the execution stack for each process.
%% 我们一一处理跟踪事件，跟踪每个进程的执行堆栈。

%% 我们不关心匹配规范结果 for callgrind.
handleEvent({call, Pid, Ts, MFA, _MSpec}, State) ->
	handleEvent({call, Pid, Ts, MFA}, State);
handleEvent({call, Pid, Ts, MFA}, State) ->
	case isProcProfiled(Pid, State) of
		{true, P} ->
			Proc = P;
		{empty, P} ->
			Proc = P;
		false ->
			Proc = #proc{}
	end,
	{Source, NewState} = findSource(MFA, State),
	handleCall(Pid, convertMfa(MFA), Source, Ts, Proc, NewState);
handleEvent({return_to, Pid, Ts, MFA}, State) ->
	case isProcProfiled(Pid, State) of
		{true, Proc} ->
			handleReturnTo(Pid, convertMfa(MFA), Ts, Proc, State);
		_ ->
			State
	end;
%% Process exited. Unfold the stacktrace entirely.
%%
%% We use the atom exit because we know it will not match
%% a function call and will therefore unfold everything.
handleEvent({exit, Pid, Ts, _Reason}, State0) ->
	case isProcProfiled(Pid, State0) of
		{true, Proc} ->
			State = #state{procs = Procs} = handleReturnTo(Pid, exit, Ts, Proc, State0),
			%% Remove the pid from the state to save memory.
			State#state{procs = maps:remove(Pid, Procs)};
		_ ->
			State0
	end;
handleEvent({in, Pid, Ts, _MFA}, State = #state{opts = #{running := true}}) ->
	case isProcProfiled(Pid, State) of
		{true, Proc} -> handleIn(Pid, Ts, Proc, State);
		_ -> State
	end;
handleEvent({out, Pid, Ts, _MFA}, State = #state{opts = #{running := true}}) ->
	case isProcProfiled(Pid, State) of
		{true, Proc} -> handleOut(Pid, Ts, Proc, State);
		_ -> State
	end;
%% Ignore all other events. We do not need them for building the callgrind file.
handleEvent(_, State) ->
	State.

isProcProfiled(Pid, #state{procs = Procs}) ->
	case maps:get(Pid, Procs, undefined) of
		undefined ->
			%% We never received events for this process. Ignore.
			false;
		#proc{stack = []} = Proc ->
			%% We received events but are not in a known function currently. Ignore.
			{empty, Proc};
		Proc ->
			%% All good!
			{true, Proc}
	end.

%% We track a number of different things:
%% - how much time was spent in the different function calls
%% - how much time they spent calling other functions
%% - how many times functions were called
%%
%% We track everything on a per process basis. For each process,
%% we maintain a call stack. Every time a function return, we may
%% end up saving call information to the 'mfas' map. We then write
%% this information to the disk whenever the stacktrace becomes
%% empty, or when the process terminates.

%% When we receive a call event, we add the call information
%% to the stack, regardless of what it already contains.
%% This means that recursive calls, whether tail or body,
%% will appear multiple times in the stack. And since Erlang
%% doesn't have loops, it will appear a little weird if
%% compared to an imperative language.

%% Recursive call. Just increase the call count.
handleCall(Pid, MFA, _Source, _Ts, #proc{stack = [Call = #call{mfa = MFA, count = Count} | Stack0]} = Proc, #state{procs = Procs} = State) ->
	Stack = [Call#call{count = Count + 1} | Stack0],
	NewProc = Proc#proc{stack = Stack},
	State#state{procs = Procs#{Pid => NewProc}};
%% Non-recursive call.
handleCall(Pid, MFA, Source, Ts, #proc{stack = Stack0} = Proc0, #state{procs = Procs} = State) ->
	Stack = [#call{mfa = MFA, source = Source, ts = Ts, self_ts = Ts} | Stack0],
	Proc = Proc0#proc{stack = Stack},
	State#state{procs = Procs#{Pid => Proc}}.

%% We return from the current call, so the current call
%% ends regardless of what it was doing. We stop as soon
%% as we see the caller we return to; or if we return all
%% the way up, higher than where we started (for example
%% because we were not tracing the function we actually
%% end up returning to), we get everything.
%%
%% The current call started when it was called and stopped
%% on the return_to timestamp. Therefore it is fairly simple
%% to calculate its incl/self times.
%%
%% Other calls returning at the same time are tail calls.
%% In their case, the incl time is the same as for the
%% current call. However the self time must not stop when
%% returning but rather when doing the final tail call.
%% We also update sub call times since those must be
%% maintained separately.
%%
%% NOTE: Due to how the VM works, if a function has both
%% tail and non-tail calls, it becomes impossible to know
%% what is or is not a tail call, and therefore values
%% may be wrong. Do not write such functions! For example:
%%
%%    a(true) -> 1 + b(); a(false) -> b().
%%
%% Finally we must also update the self for the call we
%% actually return to. In its case we use the time we
%% were last executing the function as a start point,
%% and the return time for the end. Here again we also
%% update the sub call times.

handleReturnTo(Pid, MFA, Ts, Proc0 = #proc{stack = [Current0 | Stack0], mfas = MFAs0}, State = #state{procs = Procs}) ->
	{Returned0, Stack1} = lists:splitwith(fun(#call{mfa = E}) -> E =/= MFA end, Stack0),
	#call{ts = CurrentTs, self_ts = CurrentSelfTs, self = CurrentSelf} = Current0,
	Current = Current0#call{incl = Ts - CurrentTs, self = CurrentSelf + Ts - CurrentSelfTs},
	Returned = updateTailCalls([Current | Returned0], Ts),
	Stack = updateStack(Returned, Stack1, Ts),
	%% Save the profile information in the state, potentially flushing it
	%% to disk if the stack is empty.
	MFAs1 = updateMfas(Returned, MFAs0),
	MFAs =
		case Stack of
			[] ->
				writeMfas(Pid, MFAs1, State),
				#{};
			_ ->
				MFAs1
		end,
	Proc = Proc0#proc{stack = Stack, mfas = MFAs},
	State#state{procs = Procs#{Pid => Proc}}.

updateTailCalls([Call], _) ->
	[Call];
updateTailCalls([Callee = #call{ts = CalleeTs}, Caller0 = #call{ts = CallerTs, self_ts = CallerSelfTs, self = CallerSelf} | Tail], ReturnToTs) ->
	Caller1 = Caller0#call{incl = ReturnToTs - CallerTs, self = CallerSelf + CalleeTs - CallerSelfTs},
	Caller = updateSubCalls(Callee, Caller1),
	[Callee | updateTailCalls([Caller | Tail], ReturnToTs)].

%% Update nothing; there's nothing in the stack.
updateStack(_, [], _) ->
	[];
%% Update the incl/self value based on the top-level function we return from,
%% but only update the sub calls based on the function we directly called.
updateStack(Returned, [Caller0 = #call{self_ts = CallerSelfTs, self = CallerSelf} | Stack], ReturnToTs) ->
	Callee = #call{ts = CalleeTs} = hd(lists:reverse(Returned)),
	Caller = Caller0#call{self_ts = ReturnToTs, self = CallerSelf + CalleeTs - CallerSelfTs},
	[updateSubCalls(Callee, Caller) | Stack].

updateSubCalls(Callee = #call{mfa = MFA, incl = CallerIncl, count = CallerCount, waitIncl = CallerWaitIncl}, Caller = #call{calls = SubCalls}) ->
	case maps:get(MFA, SubCalls, undefined) of
		%% Add the callee to the subcalls but remove the callee's subcalls
		%% since we don't need those here.
		undefined ->
			Caller#call{calls = SubCalls#{MFA => Callee#call{calls = #{}}}};
		%% Same as above, except we add to the existing values.
		Sub = #call{incl = SubIncl, count = SubCount, waitIncl = SubWaitIncl} ->
			Caller#call{calls = SubCalls#{MFA => Sub#call{
				%% We do not care about self/wait here as we will be using incl/wait_incl in the output.
				incl = SubIncl + CallerIncl,
				count = SubCount + CallerCount,
				waitIncl = SubWaitIncl + CallerWaitIncl
			}}}
	end.

%% Processes get scheduled in and out. We get the corresponding
%% in and out events when the 'running' option is set to true.
%% We keep track of how many times the process was scheduled out
%% per function, and how long.

handleIn(Pid, InTs, Proc0 = #proc{stack = [Current0 | Stack0], out = OutTs}, State = #state{procs = Procs}) ->
	#call{wait = Wait, waitIncl = WaitIncl, waitCnt = WaitCount, waitCntIncl = WaitCountIncl} = Current0,
	ThisWait = InTs - OutTs,
	%% We increase the wait time for self first.
	Current = Current0#call{wait = Wait + ThisWait, waitIncl = WaitIncl + ThisWait, waitCnt = WaitCount + 1, waitCntIncl = WaitCountIncl + 1},
	%% And then for the parent calls to include wait time of subcalls.
	Stack = [
		Call#call{waitIncl = ParentWaitIncl + ThisWait, waitCntIncl = ParentWaitCount + 1}
		|| Call = #call{waitIncl = ParentWaitIncl, waitCntIncl = ParentWaitCount} <- Stack0
	],
	Proc = Proc0#proc{stack = [Current | Stack], out = undefined},
	State#state{procs = Procs#{Pid => Proc}}.

handleOut(Pid, Ts, Proc0 = #proc{out = undefined}, State = #state{procs = Procs}) ->
	Proc = Proc0#proc{out = Ts},
	State#state{procs = Procs#{Pid => Proc}}.

%% Update the profiling information we currently hold.
updateMfas([], MFAs) ->
	MFAs;
updateMfas([Call = #call{mfa = MFA, incl = Incl, self = Self, wait = Wait, waitIncl = WaitIncl, waitCnt = WaitCount, waitCntIncl = WaitCountIncl, count = Count, calls = SubCalls} | Tail], MFAs) ->
	case MFAs of
		#{MFA := AggCall0 = #call{incl = AggIncl, self = AggSelf, wait = AggWait, waitIncl = AggWaitIncl, waitCnt = AggWaitCount, waitCntIncl = AggWaitCountIncl, count = AggCount, calls = AggSubCalls0}} ->
			AggSubCalls = updateMfas(maps:values(SubCalls), AggSubCalls0),
			AggCall = AggCall0#call{incl = Incl + AggIncl, self = Self + AggSelf,
				wait = Wait + AggWait, waitIncl = WaitIncl + AggWaitIncl,
				waitCnt = WaitCount + AggWaitCount,
				waitCntIncl = WaitCountIncl + AggWaitCountIncl,
				count = Count + AggCount, calls = AggSubCalls
			},
			updateMfas(Tail, MFAs#{MFA => AggCall});
		_ ->
			updateMfas(Tail, MFAs#{MFA => Call})
	end.

%% The callgrind format is documented at http://valgrind.org/docs/manual/cl-format.html
%%
%% We currently only store the real time spent in the calls
%% (including wait times).
%%
%% The option 'scope' can be used to enable per process tracking.

writeHeader(#state{ioDevice = OutDevice, opts = #{running := true}}) ->
	ok = file:write(OutDevice,
		<<"# callgrind format\n"
		"events: Total Active Wait WaitCount\n"
		"event: Total : Total time in microseconds\n"
		"event: Active : Active time in microseconds\n"
		"event: Wait : Wait time in microseconds (scheduled out)\n"
		"event: WaitCount : Number of times the process was scheduled out\n"
		"\n">>);
writeHeader(#state{ioDevice = OutDevice}) ->
	ok = file:write(OutDevice,
		<<"# callgrind format\n"
		"events: Total\n"
		"event: Total : Total time in microseconds\n"
		"\n">>).

flush(State = #state{procs = Procs}) ->
	maps:fold(
		fun(Pid, #proc{mfas = MFAs}, _) ->
			writeMfas(Pid, MFAs, State)
		end,
		undefined, Procs),
	ok.

writeMfas(Pid, MFAs, State) ->
	_ = [writeCall(Pid, Call, State) || Call <- maps:values(MFAs)],
	ok.

writeCall(Pid, #call{mfa = MFA, source = {Source, LN}, self = Self, wait = Wait, waitCnt = WaitCount, calls = Calls0}, #state{ioDevice = OutDevice, opts = Opts}) ->
	Calls = maps:values(Calls0),
	Ob =
		case Opts of
			#{scope := per_process} ->
				["ob=", io_lib:write(Pid), "\n"];
			_ ->
				[]
		end,
	RunningCosts =
		case Opts of
			#{running := true} ->
				[
					" ", integer_to_list(Self - Wait),
					" ", integer_to_list(Wait),
					" ", integer_to_list(WaitCount)
				];
			_ ->
				[]
		end,
	file:write(OutDevice,
		[
			Ob,
			"fl=", Source, "\n"
		   "fn=", atom_to_list(MFA), "\n",
			integer_to_list(LN), " ", integer_to_list(Self), RunningCosts, "\n",
			formatSubcalls(LN, Calls, Opts),
			"\n"
		]).

formatSubcalls(_, [], _) ->
	[];
%% @todo We don't need to write the filename for functions in the same module.
%% @todo We also don't want to put the full file name; instead we should remove
%% the prefix (path to the release).
%%
%% We only look at where the function is defined, we can't really get
%% the actual line number where the call happened, unfortunately.
formatSubcalls(LN, [#call{mfa = MFA, source = {Source, TargetLN}, incl = Incl, waitIncl = Wait, waitCntIncl = WaitCount, count = Count, calls = _Calls} | Tail], Opts) ->
	RunningCosts =
		case Opts of
			#{running := true} ->
				[
					" ", integer_to_list(Incl - Wait),
					" ", integer_to_list(Wait),
					" ", integer_to_list(WaitCount)
				];
			_ ->
				[]
		end,
	[
		[
			"cfi=", Source, "\n"
		   "cfn=", atom_to_list(MFA), "\n"
			"calls=", integer_to_list(Count), " ", integer_to_list(TargetLN), "\n",
			integer_to_list(LN), " ", integer_to_list(Incl), RunningCosts, "\n"
		] | formatSubcalls(LN, Tail, Opts)
	].

convertMfa(undefined) ->
	undefined;
convertMfa({M, F, A}) ->
	MBin = atom_to_binary(M, latin1),
	FBin = atom_to_binary(F, latin1),
	ABin = integer_to_binary(A),
	binary_to_atom(<<MBin/binary, $:, FBin/binary, $/, ABin/binary>>, latin1).

findSource(MFA, #state{sources = Cache} = State) ->
	case Cache of
		#{MFA := Source} ->
			{Source, State};
		_ ->
			NewState = #state{sources = #{MFA := Source}} = cacheModule(MFA, State),
			{Source, NewState}
	end.

%% We extract the line number of the functions by loading the
%% beam file (which is already loaded when we reach this function)
%% and looking into the abstract code directly. When something
%% goes wrong, for example the module was not compiled with
%% +debug_info, the function will return line number 1.
%%
%% Note that we can only retrieve the location of the function.
%% For functions with many clauses we are not able to properly
%% identify which clause was involved. It's probably a good
%% idea to structure your code to have more functions than
%% function clauses, especially when using behaviours.
%%
%% While this is an expensive operation, we cache the result
%% and therefore this function will only be called once per module.
cacheModule({Module, _, _} = MFA, #state{sources = Cache} = State) ->
	try
		%% If the module is in the path, we can simply query
		%% it for the source file.
		Info = Module:module_info(compile),
		%% @todo We don't want to return an absolute path,
		%% but rather the app/src/file.erl path if it's in
		%% an application, or just folder/file.erl if not.
		%% This allows different users to point to the
		%% same source at different locations on their machine.
		{_, Src} = lists:keyfind(source, 1, Info),
		cacheModule(MFA, State, Src)
	catch _:_ ->
		%% Either the module was not found, or it doesn't
		%% have a 'source' key in the compile info.
		%%
		%% We can't cache the module; on the other hand
		%% we can cache the result of this operation.
		%% Just append .erl to the module name and set the
		%% line number to 1, which is of course incorrect.
		State#state{sources = Cache#{MFA => {atom_to_list(Module) ++ ".erl", 1}}}
	end.

cacheModule({Module, _, _} = MFA, #state{sources = Cache} = State, Src) ->
	{Module, Beam, _} = code:get_object_code(Module),
	{ok, {Module, Chunks}} = beam_lib:chunks(Beam, [abstract_code]),
	[{abstract_code, {raw_abstract_v1, Forms}}] = Chunks,
	Funcs = [begin case LN of {Line, _} -> {{Module, F, A}, Line}; _ -> {{Module, F, A}, LN} end end || {function, LN, F, A, _} <- Forms],

	NewCache = lists:foldl(fun({Key, LN}, Acc) -> Acc#{Key => {Src, LN}} end, Cache, Funcs),
	%% We cannot currently retrieve line number information
	%% for list comprehensions and funs. We therefore
	%% cache the correct file with line number set to 1.
	LastCache =
		case NewCache of
			#{MFA := _} -> NewCache;
			_ -> NewCache#{MFA => {Src, 1}}
		end,
	State#state{sources = LastCache}.
