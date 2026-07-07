// coreutils.c — C FFI wrappers for lean-coreutils.
//
// This file is 0BSD. See LICENSE for details.
//
// Wraps Unix syscalls for use from Lean 4 via @[extern].
// All symbols are LEAN_EXPORT so the linker sees them.

#include <lean/lean.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <signal.h>

// ─── Error helper ──────────────────────────────────────────────────────────────

// Create a properly-typed IO.Error using the Lean runtime API.
// The second arg is the message string (e.g., strerror(errno)).
static inline lean_object *mk_io_error(uint32_t code, lean_object *msg) {
    // Use IO.ErrorKind.other for generic errno-based errors.
    // lean_mk_io_error_other_error(uint32_t errCode, lean_obj_arg msg)
    // returns a constructed IO.Error object.
    return lean_mk_io_error_other_error(code, msg);
}

// Shorthand: create an IO.Error from errno
static inline lean_object *io_err(void) {
    return lean_io_result_mk_error(mk_io_error(errno, lean_mk_string(strerror(errno))));
}

// ─── File descriptor operations ────────────────────────────────────────────────

// open(2): path → fd (or IO.Error)
// flags/mode are UInt32; caller passes O_RDONLY, O_WRONLY, etc. directly.
LEAN_EXPORT lean_object *lean_coreutils_open(b_lean_obj_arg path,
                                             uint32_t flags,
                                             uint32_t mode,
                                             lean_object *w) {
    int fd = open(lean_string_cstr(path), (int)flags, (mode_t)mode);
    if (fd < 0) return io_err();
    return lean_io_result_mk_ok(lean_box((uint32_t)fd));
}

// close(2): fd → Unit (or IO.Error)
LEAN_EXPORT lean_object *lean_coreutils_close(uint32_t fd, lean_object *w) {
    if (close((int)fd) < 0) return io_err();
    return lean_io_result_mk_ok(lean_box(0));
}

// read(2): fd → ByteArray in a loop until n bytes or EOF.
// Allocates a malloc buffer, reads, then copies into a Lean ByteArray.
LEAN_EXPORT lean_object *lean_coreutils_read(uint32_t fd, size_t n, lean_object *w) {
    if (n == 0)
        return lean_io_result_mk_ok(lean_alloc_sarray(1, 0, 0));
    uint8_t *tmp = (uint8_t*)malloc(n);
    if (tmp == NULL) {
        errno = ENOMEM;
        return io_err();
    }
    ssize_t r = read((int)fd, tmp, n);
    if (r < 0) {
        free(tmp);
        return io_err();
    }
    lean_object *ba = lean_alloc_sarray(1, (size_t)r, (size_t)r);
    memcpy(lean_sarray_cptr(ba), tmp, (size_t)r);
    free(tmp);
    return lean_io_result_mk_ok(ba);
}

// write(2): fd + ByteArray → USize bytes written (or IO.Error).
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

// ─── getcwd ────────────────────────────────────────────────────────────────────

// getcwd(3): returns the current working directory as a Lean String.
// Uses malloc + getcwd with dynamic allocation, then converts to Lean string.
LEAN_EXPORT lean_object *lean_coreutils_getcwd(lean_object *w) {
    size_t size = 256;
    char *buf = NULL;
    while (1) {
        char *tmp = realloc(buf, size);
        if (tmp == NULL) {
            free(buf);
            errno = ENOMEM;
            return io_err();
        }
        buf = tmp;
        if (getcwd(buf, size) != NULL) {
            lean_object *s = lean_mk_string(buf);
            free(buf);
            return lean_io_result_mk_ok(s);
        }
        if (errno != ERANGE) {
            free(buf);
            return io_err();
        }
        free(buf);
        buf = NULL;
        size *= 2;
        if (size > 65536) {
            errno = ENAMETOOLONG;
            return io_err();
        }
    }
}

// ─── nanosleep ─────────────────────────────────────────────────────────────────

#include <time.h>

// nanosleep(2): sleep for the given number of nanoseconds.
// Returns 0 on success, remaining nanoseconds on interrupt (or IO.Error).
// The Lean side passes a UInt64 of nanoseconds.
LEAN_EXPORT lean_object *lean_coreutils_nanosleep(uint64_t ns, lean_object *w) {
    struct timespec req;
    req.tv_sec  = (time_t)(ns / 1000000000ULL);
    req.tv_nsec = (long)(ns % 1000000000ULL);
    struct timespec rem;
    rem.tv_sec  = 0;
    rem.tv_nsec = 0;
    if (nanosleep(&req, &rem) < 0) {
        if (errno == EINTR) {
            // Return the remaining time as nanoseconds
            uint64_t remaining = (uint64_t)rem.tv_sec * 1000000000ULL + (uint64_t)rem.tv_nsec;
            return lean_io_result_mk_ok(lean_box_uint64(remaining));
        }
        return io_err();
    }
    return lean_io_result_mk_ok(lean_box_uint64(0));
}
