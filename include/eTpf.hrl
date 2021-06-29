%% 三目元算符
-define(IIF(Cond, Then, That), case Cond of true -> Then; _ -> That end).

%% Ignore Msg Hole Name
-define(eTpfHole, '$eTpfHole').

%% 消息截取默认配置项
-define(defTcmMap, #{
	tcmIsCut => false
	, tcmDepth => 5
	, tcmListSize => 32
	, tcmMapSize => 32
	, tcmTupleSize => 32
	, tcmBinSize => 128
	, tcmBitSize => 128 * 8
	, tcmNestStruct => 6
}).

-define(eTpfTracerId, eTpfTracerId).

%% 类型声明
-export_type([pidPortSpec/0, flagList/0, pattern/0, traceOpts/0, tracerOpts/0]).

-type pidPortSpec() :: PidPortSpec :: pid() | port()| all | processes | ports| existing | existing_processes | existing_ports| new | new_processes | new_ports.
-type flagList() :: erlang:trace_flag().
-type pattern() :: module() | {module(), atom(), atom()} | {app, atom()} | {callback, module(), atom()}.

-type traceOpts() :: #{
	tracerId => any()
	, stackTc => boolean()      %% 必需的trace选项 为了生成火焰图，我们目前在跟踪时需要使用一个附加选项。此选项将导致将堆栈跟踪信息添加到调用事件。选项为`process_dump`, 并且必须将其设置为`true`。
}.

-type tracerOpts() :: #{
	%% tracer for socket
	port => pos_integer()

	%% tracer for file
	, fDir => string()                    %% 文件目录
	, fBaseName => string()                    %% 文件base file name
	, fMaxSize => pos_integer()                %% 最大的文件字节数
	, fMaxMsg => pos_integer()                 %% 最大消息数量

	%% tracer for console

	%% trace msg term cut cfg
	, tcmIsCut => boolean()                     %% 是否cut trace msg term
	, tcmDepth => pos_integer()                 %% term cut时保留的最大深度
	, tcmListSize => pos_integer()              %% List term cut时保留的最大数量
	, tcmMapSize => pos_integer()               %% Map term cut时保留的最大数量
	, tcmTupleSize => pos_integer()             %% Tuple term cut时保留的最大数量
	, tcmBinSize => pos_integer()               %% Bin term cut时保留的最大字节数
	, tcmBitSize => pos_integer()               %% Bits term cut时保留的最大bit数
	, tcmNestStruct => pos_integer()            %% 复合结构 term cut时保留的最大数量
}.