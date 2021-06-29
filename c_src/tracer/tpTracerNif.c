#include "erl_nif.h"

#define NIF_ATOM_DECL(a) ERL_NIF_TERM atom_ ## a;
// #define NIF_ATOM_H_DECL(a) extern ERL_NIF_TERM atom_ ## a;
#define NIF_ATOM_INIT(a) atom_ ## a = enif_make_atom(env, #a);

#define NIF_ATOMS(A) \
    A(_nif_thread_ret_) \
    A(call) \
    A(closed) \
    A(cpu_timestamp) \
    A(discard) \
    A(exception_from) \
    A(exit) \
    A(extra) \
    A(gc_major_end) \
    A(gc_major_start) \
    A(gc_minor_end) \
    A(gc_minor_start) \
    A(getting_linked) \
    A(getting_unlinked) \
    A(in) \
    A(in_exiting) \
    A(link) \
    A(match_spec_result) \
    A(mode) \
    A(monotonic) \
    A(ok) \
    A(open) \
    A(out) \
    A(out_exited) \
    A(out_exiting) \
    A(percent) \
    A(profile) \
    A(receive) \
    A(register) \
    A(remove) \
    A(return_from) \
    A(return_to) \
    A(scheduler_id) \
    A(send) \
    A(send_to_non_existing_process) \
    A(spawn) \
    A(spawned) \
    A(strict_monotonic) \
    A(timestamp) \
    A(trace) \
    A(trace_status) \
    A(tracers) \
    A(unlink) \
    A(unregister)

NIF_ATOMS(NIF_ATOM_DECL)

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    NIF_ATOMS(NIF_ATOM_INIT)
    *priv_data = NULL;

    return 0;
}

static int upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info) {
    *priv_data = *old_priv_data;

    return 0;
}

static void unload(ErlNifEnv *env, void *priv_data) {

}

// enabled(TraceTag, TracerState, Tracee)
static ERL_NIF_TERM enabled(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    /* Only generate trace for when tracer != tracee */
    // enif_fprintf(stdout, "IMY************enabled 000:");
    // for (int i = 0; i <= argc - 1; i++) {
    //     if (i != 10) {
    //         enif_fprintf(stdout, " %d  %T", i, argv[i]);
    //     }
    // }
    // enif_fprintf(stdout, "\n");

    if (enif_is_identical(argv[1], argv[2])) {
        return atom_discard;
    }

    ErlNifPid tracer_pid;

    // Disable the trace when one of the tracers is not a local process.
    if (!enif_get_local_pid(env, argv[1], &tracer_pid))
        return atom_remove;

    // Disable the trace when one of the tracers is not alive.
    if (!enif_is_process_alive(env, &tracer_pid))
        return atom_remove;

    return atom_discard;
}

static ERL_NIF_TERM enabled_call(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    // We want both call and return_to when tracer != tracee.
    // enif_fprintf(stdout, "IMY************enabled_call 000:");
    // for (int i = 0; i <= argc - 1; i++) {
    //     if (i != 10) {
    //         enif_fprintf(stdout, " %d  %T", i, argv[i]);
    //     }
    // }
    // enif_fprintf(stdout, "\n");

    if (enif_is_identical(argv[1], argv[2])) {
        return atom_discard;
    }

    return atom_trace;
}

static ERL_NIF_TERM enabled_procs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    // int arity;
    // const ERL_NIF_TERM *tuple;

    // if (!enif_get_tuple(env, argv[1], &arity, &tuple)) {
    //     return atom_remove;
    // }

    // // We only want the spawn and exit events when 'profile' mode
    // // is enabled. Technically we only care about exits for callgrind,
    // // but spawn is cheap to keep and useful for message profilers.
    // if (enif_is_identical(atom_profile, tuple[1]) && !(enif_is_identical(atom_spawn, argv[0]) || enif_is_identical(atom_exit, argv[0]))) {
    //     return atom_discard;
    // }

    if (enif_is_identical(argv[1], argv[2])) {
        return atom_discard;
    }

    return atom_trace;
}

static ERL_NIF_TERM enabled_send(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    // We want both send and send_to_non_existing_process when tracer != tracee.
    if (enif_is_identical(argv[1], argv[2])) {
        return atom_discard;
    }
    return atom_trace;
}

static ERL_NIF_TERM enabled_receive(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    // We want receive when tracer != tracee.
    if (enif_is_identical(argv[1], argv[2])) {
        return atom_discard;
    }
    return atom_trace;
}

static ERL_NIF_TERM enabled_running_procs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    // We want both in and out tracer != tracee.
    if (enif_is_identical(argv[1], argv[2])) {
        return atom_discard;
     }
    return atom_trace;
}

static ERL_NIF_TERM enabled_garbage_collection(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    // We want both  gc_minor_start, gc_max_heap_size, and gc_minor_end tracer != tracee.
    if (enif_is_identical(argv[1], argv[2])) {
        return atom_discard;
    }
    return atom_trace;
}

// trace(TraceTag, TracerState, Tracee, TraceTerm, Opts)
static ERL_NIF_TERM trace(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    // enif_fprintf(stdout, "IMY************trace:");
    // for (int i = 0; i <= argc - 1; i++) {
    //     if (i != 1) {
    //         enif_fprintf(stdout, " %d  %T", i, argv[i]);
    //     }
    // }
    // enif_fprintf(stdout, "\n");

    int has_extra, has_mspec;
    ErlNifPid tracer_pid;

    // Disable the trace when one of the tracers is not a local process.
    if (!enif_get_local_pid(env, argv[1], &tracer_pid))
        return atom_ok;

    ERL_NIF_TERM ts, extra, mspec, msg;

    // Build the message. There can be two different messages
    // depending on whether the extra option was set:
    //
    // - {Tag, Tracee, Ts, Term}
    // - {Tag, Tracee, Ts, Term, Extra}
    //
    // On top of that when match specs are enabled we may have
    // one additional term at the end of the tuple containing
    // the result of the match spec function.
    //
    // - {Tag, Tracee, Ts, Term, Result}
    // - {Tag, Tracee, Ts, Term, Extra, Result}

    has_extra = enif_get_map_value(env, argv[4], atom_extra, &extra);
    has_mspec = enif_get_map_value(env, argv[4], atom_match_spec_result, &mspec);

    ErlNifTime mon = enif_monotonic_time(ERL_NIF_NSEC);
    ts = enif_make_int64(env, mon);
    //ts = enif_cpu_time(env);

    if (has_extra && has_mspec)
        msg = enif_make_tuple6(env, argv[0], argv[2], ts, argv[3], extra, mspec);
    else if (has_extra)
        msg = enif_make_tuple5(env, argv[0], argv[2], ts, argv[3], extra);
    else if (has_mspec)
        msg = enif_make_tuple5(env, argv[0], argv[2], ts, argv[3], mspec);
    else
        msg = enif_make_tuple4(env, argv[0], argv[2], ts, argv[3]);

    // Send the message to the tracer_pid.
    enif_send(env, &tracer_pid, NULL, msg);

    return atom_ok;
}

static ErlNifFunc nif_funs[] = {
        {"enabled",                    3, enabled},
        {"enabled_call",               3, enabled_call},
        {"enabled_procs",              3, enabled_procs},
        {"enabled_send",               3, enabled_send},
        {"enabled_receive",            3, enabled_receive},
        {"enabled_running_procs",      3, enabled_running_procs},
        {"enabled_garbage_collection", 3, enabled_garbage_collection},
        {"trace",                      5, trace},
};

ERL_NIF_INIT(tpTracerNif, nif_funs, load, NULL, upgrade, unload)