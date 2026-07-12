// coreutils.c — C FFI wrappers for lean-coreutils.
//
// This file is 0BSD. See LICENSE for details.
//
// File I/O and sleep have been migrated to Lean's built-in APIs.
// Only write() FFI is kept because Lean's IO.FS.Stream.write doesn't
// surface ENOSPC/EPIPE errors (no exception thrown).
// SIGPIPE handling is also kept since there's no Lean native equivalent.

#include <lean/lean.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

// ─── FFI write (kept for error detection) ─────────────────────────────────────

// Create an IO.Error from errno
static inline lean_object *io_err(void) {
    return lean_io_result_mk_error(
        lean_mk_io_error_other_error(errno, lean_mk_string(strerror(errno))));
}

// write(2): fd + ByteArray → USize bytes written (or IO.Error).
// Kept because Lean's native IO.FS.Stream.write silently swallows
// write errors like ENOSPC (/dev/full) and EPIPE.
LEAN_EXPORT lean_object *lean_coreutils_write(uint32_t fd,
                                              b_lean_obj_arg ba,
                                              lean_object *w) {
    size_t n = lean_sarray_size(ba);
    ssize_t r = write((int)fd, lean_sarray_cptr(ba), n);
    if (r < 0) return io_err();
    return lean_io_result_mk_ok(lean_box((uint32_t)r));
}

// ─── Signal handling ───────────────────────────────────────────────────────────

// Set SIGPIPE disposition to SIG_IGN so writes to closed pipes return EPIPE
// rather than killing the process.
LEAN_EXPORT lean_object *lean_coreutils_ignore_sigpipe(lean_object *w) {
    signal(SIGPIPE, SIG_IGN);
    return lean_io_result_mk_ok(lean_box(0));
}

