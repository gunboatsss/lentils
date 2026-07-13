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
#include <pwd.h>
#include <grp.h>
#include <utmpx.h>

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

// ─── environ (for printenv) ──────────────────────────────────────────────────

// Return the environment as a Lean Array of "KEY=VALUE" strings.
LEAN_EXPORT lean_object *lean_coreutils_environ(lean_object *w) {
    extern char **environ;
    size_t count = 0;
    while (environ[count]) count++;
    // Build a Lean list (nil is tag 0, 0 objs; cons is tag 1, 2 objs)
    lean_object *lst = lean_alloc_ctor(0, 0, 0);  // nil
    for (size_t i = count; i > 0; i--) {
        lean_object *s = lean_mk_string(environ[i-1]);
        lean_object *cons = lean_alloc_ctor(1, 2, 0);
        lean_ctor_set(cons, 0, s);       // head
        lean_ctor_set(cons, 1, lst);      // tail
        lst = cons;
    }
    lean_object *arr = lean_array_mk(lst);
    return lean_io_result_mk_ok(arr);
}

// ─── getpwuid (for id username lookup) ───────────────────────────────────────-

// Look up a username by UID. Returns empty string if not found.
LEAN_EXPORT lean_object *lean_coreutils_getpwuid(uint32_t uid,
                                                  lean_object *w) {
    struct passwd *pw = getpwuid((uid_t)uid);
    if (pw == NULL) {
        return lean_io_result_mk_ok(lean_mk_string(""));
    }
    return lean_io_result_mk_ok(lean_mk_string(pw->pw_name));
}

// ─── getgrgid (for id/groups group name lookup) ───────────────────────────────

// Look up a group name by GID. Returns empty string if not found.
LEAN_EXPORT lean_object *lean_coreutils_getgrgid(uint32_t gid,
                                                  lean_object *w) {
    struct group *gr = getgrgid((gid_t)gid);
    if (gr == NULL) {
        return lean_io_result_mk_ok(lean_mk_string(""));
    }
    return lean_io_result_mk_ok(lean_mk_string(gr->gr_name));
}

// ─── getutxent (for users logged-in list) ─────────────────────────────────────

// Get a deduplicated list of logged-in usernames from utmpx.
// Returns a Lean Array of username strings.
LEAN_EXPORT lean_object *lean_coreutils_users(lean_object *w) {
    lean_object *lst = lean_alloc_ctor(0, 0, 0);  // nil
    setutxent();
    struct utmpx *ut;
    while ((ut = getutxent()) != NULL) {
        if (ut->ut_type == USER_PROCESS && ut->ut_user[0] != '\0') {
            // Check for duplicates by iterating the list
            int dup = 0;
            for (lean_object *it = lst; lean_is_ctor(it) && lean_ptr_tag(it) == 1; it = lean_ctor_get(it, 1)) {
                lean_object *existing = lean_ctor_get(it, 0);
                const char *estr = lean_string_cstr(existing);
                if (strcmp(estr, ut->ut_user) == 0) {
                    dup = 1;
                    break;
                }
            }
            if (!dup) {
                lean_object *s = lean_mk_string(ut->ut_user);
                lean_object *cons = lean_alloc_ctor(1, 2, 0);
                lean_ctor_set(cons, 0, s);
                lean_ctor_set(cons, 1, lst);
                lst = cons;
            }
        }
    }
    endutxent();
    lean_object *arr = lean_array_mk(lst);
    return lean_io_result_mk_ok(arr);
}

// ─── access(2) for file-permission tests ─────────────────────────────────────
// Used by `test` (-r/-w/-x) to populate StatContext from the real
// filesystem permissions. mode is the POSIX access(2) bitmask:
//   R_OK = 4, W_OK = 2, X_OK = 1.
// Returns 1 (boxed) if the requested access is permitted, 0 otherwise
// (this includes EACCES and ENOENT — both mean "not accessible").
LEAN_EXPORT lean_object *lean_coreutils_access(b_lean_obj_arg path,
                                               uint32_t mode,
                                               lean_object *w) {
    const char *p = lean_string_cstr(path);
    int r = access(p, (int)mode);
    return lean_io_result_mk_ok(lean_box((uint32_t)(r == 0 ? 1 : 0)));
}

