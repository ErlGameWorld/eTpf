# PidPortSpec
    每个进程只能由一个tracer进行跟踪。因此，跟踪已跟踪进程会失败。
    all                 All currently existing processes and ports and all that will be created in the future.
    processes           All currently existing processes and all that will be created in the future.
    ports               All currently existing ports and all that will be created in the future.
    existing            All currently existing processes and ports.
    existing_processes  All currently existing processes.
    existing_ports      All currently existing ports.
    new                 All processes and ports that will be created in the future.
    new_processes       All processes that will be created in the future.
    new_ports           All ports that will be created in the future.

# FlagList
    all 设置除tracer和 cpu_timestamp之外的所有跟踪标志
    send 跟踪消息的发送 MsgTags: send and send_to_non_existing_process.
    receive  跟踪消息的接收 MsgTags: receive.
    call 跟踪某些函数调用。通过调用erlang:trace_pattern/3指定要跟踪的函数调用。 MsgTags: call and return_from.
    silent 与call跟踪标志一起使用。当着标志设置后 MsgTags: call，return_from return_to 被禁止, 但如果有match specifications时他们正常执行。通过执行erlang:trace(_, false, [silent|_])或执行函数{silent, false}的匹配规范来 禁止静默模式 。
        在silent 的跟踪标志有利于建立许多甚至在系统中的所有进程痕迹。然后可以使用匹配规范函数{silent,Bool}激活和停用跟踪，从而高度控制哪些函数使用哪些参数触发跟踪。
        MsgTags: call, return_from, and return_to. Or rather, the absence of.

    return_to 与call跟踪标志一起使用。跟踪从被跟踪函数返回到其调用者的返回. Only works for functions traced with option local to erlang:trace_pattern/3.
        语义是当调用跟踪函数返回时发送跟踪消息，即当一串尾递归调用结束时。每个尾递归调用链仅发送一条跟踪消息，因此在使用此标志进行跟踪时，函数调用的尾递归属性将被保留。一起使用call和return_to trace 可以随时准确地知道进程在哪个函数中执行。 要获取包含函数返回值的跟踪消息，请改用{return_trace}匹配规范操作。
        MsgTags: return_to.
    procs 跟踪process-related的事件 MsgTags: spawn, spawned, exit, register, unregister, link, unlink, getting_linked, and getting_unlinked.
    ports 跟踪 port-related 的事件. MsgTags: open, closed, register, unregister, getting_linked, and getting_unlinked.
    running 跟踪 processes 调度. MsgTags: in and out.
    exiting 跟踪 scheduling of exiting processes. MsgTags: in_exiting, out_exiting, and out_exited.
    running_procs 跟踪进程的调度就像 running . 但是，此选项还包括进程在端口上下文中执行而不是自行调度时的调度事件。 MsgTags: in and out.
    running_ports 跟踪 scheduling of ports. MsgTags: in and out.
    garbage_collection 跟踪 garbage collections of processes. MsgTags: gc_minor_start, gc_max_heap_size, and gc_minor_end.
    arity Used with the call trace flag. {M, F, Arity} is specified instead of {M, F, Args} in call trace messages.
    set_on_spawn 使被跟踪进程创建的任何进程继承其跟踪标志，包括标志set_on_spawn。
    set_on_first_spawn 使被跟踪进程创建的第一个进程继承其跟踪标志，不包括标志 set_on_first_spawn。
    set_on_link Makes any process linked by a traced process inherit its trace flags, including flag set_on_link.
    set_on_first_link Makes the first process linked to by a traced process inherit its trace flags, excluding flag set_on_first_link.

# 额外注释
    如果未指定跟踪器，则调用进程将接收所有跟踪消息。
    同时设置set_on_first_link与 set_on_link的时只有set_on_first_link生效 。同样对于 set_on_spawn和set_on_first_spawn。
    如果一个match specification（仅适用于call、send 和'receive'跟踪）包含具有非布尔值的{message}action function，则该值将作为额外元素添加到消息元组的最后一个位置或之前时间戳（如果存在）。

# trace msg
    {send, PidPort, Ts, Msg, To} 
        当PidPort发送消息Msg来处理To
    {send_to_non_existing_process, PidPort, Ts, Msg, To} 
        当PidPort向不存在的进程To发送消息Msg 时。
    {'receive', PidPort, Ts, Msg}
        当PidPort收到消息Msg 时。如果Msg设置为超时，则接收语句可能已超时，或者进程收到带有有效负载timeout的消息。
    {call, Pid, Ts, {M, F, Args}}
        当Pid调用跟踪函数时。从不提供调用的返回值，只提供调用及其参数。跟踪标志arity可用于更改此消息的内容，以便 指定Arity而不是Args。
    {return_to, Pid, Ts, {M, F, Arity}}
        When Pid returns to the specified function. This trace message is sent if both the flags call and return_to are set,
        and the function is set to be traced on local function calls. The message is only sent when returning from a chain of
        tail recursive function calls, where at least one call generated a call trace message (that is, the functions match 
        specification matched, and {message, false} was not an action).
    {return_from, Pid, Ts, {M, F, Arity}, ReturnValue}
        When Pid returns from the specified function. This trace message is sent if flag call is set, 
        and the function has a match specification with a return_trace or exception_trace action.
    {exception_from, Pid, Ts, {M, F, Arity}, {Class, Value}} 
        When Pid exits from the specified function because of an exception. This trace message is sent if flag call is set, 
        and the function has a match specification with an exception_trace action.
    {spawn, Pid, Ts, Pid2, {M, F, Args}} 
        When Pid spawns a new process Pid2 with the specified function call as entry point. 
        Args is supposed to be the argument list, but can be any term if the spawn is erroneous.
    {spawned, Pid, Ts, Pid2, {M, F, Args}}
        When Pid is spawned by process Pid2 with the specified function call as entry point. 
        Args is supposed to be the argument list, but can be any term if the spawn is erroneous.
    {exit, Pid, Ts, Reason} When Pid exits with reason Reason.
    {register, PidPort, Ts, RegName} When PidPort gets the name RegName registered.
    {unregister, PidPort, Ts, RegName} When PidPort gets the name RegName unregistered. This is done automatically when a registered process or port exits.
    {link, Pid, Ts, Pid2} When Pid links to a process Pid2.
    {unlink, Pid, Ts, Pid2} When Pid removes the link from a process Pid2.
    {getting_linked, PidPort, Ts, Pid2} When PidPort gets linked to a process Pid2.
    {getting_unlinked, PidPort, Ts, Pid2} When PidPort gets unlinked from a process Pid2.
    {open, Port, Ts, Pid, Driver} When Pid opens a new port Port with the running Driver. Driver is the name of the driver as an atom.
    {closed, Port, Ts, Reason} When Port closes with Reason.
    {in | in_exiting, Pid, Ts, {M, F, Arity} | 0} When Pid is scheduled to run. The process runs in function {M, F, Arity}. On some rare occasions, the current function cannot be determined, then the last element is 0.
    {out | out_exiting | out_exited, Pid, Ts, {M, F, Arity} | 0} When Pid is scheduled out. The process was running in function {M, F, Arity}. On some rare occasions, the current function cannot be determined, then the last element is 0.
    {in, Port, Ts, Command | 0} When Port is scheduled to run. Command is the first thing the port will execute, it can however run several commands before being scheduled out. On some rare occasions, the current function cannot be determined, then the last element is 0. The possible commands are call, close, command, connect, control, flush, info, link, open, and unlink.
    {out, Port, Ts, Command | 0} When Port is scheduled out. The last command run was Command. On some rare occasions, the current function cannot be determined, then the last element is 0. Command can contain the same commands as in
    {gc_minor_start, Pid, Ts, Info} Sent when a young garbage collection is about to be started. Info is a list of two-element tuples, where the first element is a key, and the second is the value. Do not depend on any order of the tuples. The following keys are defined:
        heap_size
        The size of the used part of the heap.
        heap_block_size
        The size of the memory block used for storing the heap and the stack.
        old_heap_size
        The size of the used part of the old heap.
        old_heap_block_size
        The size of the memory block used for storing the old heap.
        stack_size
        The size of the stack.
        recent_size
        The size of the data that survived the previous garbage collection.
        mbuf_size
        The combined size of message buffers associated with the process.
        bin_vheap_size
        The total size of unique off-heap binaries referenced from the process heap.
        bin_vheap_block_size
        The total size of binaries allowed in the virtual heap in the process before doing a garbage collection.
        bin_old_vheap_size
        The total size of unique off-heap binaries referenced from the process old heap.
        bin_old_vheap_block_size
        The total size of binaries allowed in the virtual old heap in the process before doing a garbage collection.
        wordsize
        For the gc_minor_start event it is the size of the need that triggered the GC. For the corresponding gc_minor_end event it is the size of reclaimed memory = start heap_size - end heap_size.
        All sizes are in words.
    {gc_max_heap_size, Pid, Ts, Info} Sent when the max_heap_size is reached during garbage collection. Info contains the same kind of list as in message gc_start, but the sizes reflect the sizes that triggered max_heap_size to be reached.
    {gc_minor_end, Pid, Ts, Info} Sent when young garbage collection is finished. Info contains the same kind of list as in message gc_minor_start, but the sizes reflect the new sizes after garbage collection.
    {gc_major_start, Pid, Ts, Info} Sent when fullsweep garbage collection is about to be started. Info contains the same kind of list as in message gc_minor_start.
    {gc_major_end, Pid, Ts, Info} Sent when fullsweep garbage collection is finished. Info contains the same kind of list as in message gc_minor_start, but the sizes reflect the new sizes after a fullsweep garbage collection. 

    
