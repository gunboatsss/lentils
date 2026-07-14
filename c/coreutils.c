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
#include <stdlib.h>
#include <sys/wait.h>
#include <limits.h>
#include <spawn.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/statvfs.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/time.h>
#include <mntent.h>

// Process-wide environment pointer (POSIX). Declared extern here so the
// fork/exec helpers can pass it to execve().
extern char **environ;

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

// ─── getpwnam (for looking up a user by name) ──────────────────────────────────

// Look up a UID by username. Returns "uid:gid" or empty string if not found.
LEAN_EXPORT lean_object *lean_coreutils_getpwnam(b_lean_obj_arg name,
                                                   lean_object *w) {
    struct passwd *pw = getpwnam(lean_string_cstr(name));
    if (pw == NULL) {
        return lean_io_result_mk_ok(lean_mk_string(""));
    }
    char buf[48];
    snprintf(buf, sizeof(buf), "%u:%u", (unsigned)pw->pw_uid, (unsigned)pw->pw_gid);
    return lean_io_result_mk_ok(lean_mk_string(buf));
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
            // Add every login session (GNU users lists all sessions, not deduplicated)
            lean_object *s = lean_mk_string(ut->ut_user);
            lean_object *cons = lean_alloc_ctor(1, 2, 0);
            lean_ctor_set(cons, 0, s);
            lean_ctor_set(cons, 1, lst);
            lst = cons;
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

// ─── readlink(2) for `readlink` utility ───────────────────────────────────────

// Read the target of a symbolic link. Returns the link target string.
// Returns an IO error if the path is not a symlink or doesn't exist.
LEAN_EXPORT lean_object *lean_coreutils_readlink(b_lean_obj_arg path,
                                                  lean_object *w) {
    const char *p = lean_string_cstr(path);
    char buf[PATH_MAX + 1];
    ssize_t r = readlink(p, buf, sizeof(buf) - 1);
    if (r < 0) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string(strerror(errno))));
    }
    buf[r] = '\0';
    return lean_io_result_mk_ok(lean_mk_string(buf));
}

// ─── realpath(3) for `realpath` utility ─────────────────────────────────────

// Resolve a path to its canonical absolute path.
// Returns an IO error if the path doesn't exist or is inaccessible.
LEAN_EXPORT lean_object *lean_coreutils_realpath(b_lean_obj_arg path,
                                                  lean_object *w) {
    const char *p = lean_string_cstr(path);
    char *resolved = realpath(p, NULL);
    if (resolved == NULL) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string(strerror(errno))));
    }
    lean_object *result = lean_mk_string(resolved);
    free(resolved);
    return lean_io_result_mk_ok(result);
}

// ─── unlink(2) for `rm` utility ───────────────────────────────────────────

// Remove a directory entry (a file or symlink). Returns an IO error on failure.
// errno is surfaced through the Lean IO error so `rm` can report the cause.
LEAN_EXPORT lean_object *lean_coreutils_unlink(b_lean_obj_arg path,
                                                lean_object *w) {
    const char *p = lean_string_cstr(path);
    if (unlink(p) != 0) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string(strerror(errno))));
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// ─── rmdir(2) for `rmdir` utility ─────────────────────────────────────────

// Remove an empty directory. Returns an IO error on failure.
LEAN_EXPORT lean_object *lean_coreutils_rmdir(b_lean_obj_arg path,
                                               lean_object *w) {
    const char *p = lean_string_cstr(path);
    if (rmdir(p) != 0) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string(strerror(errno))));
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// ─── symlink(2) for `ln -s` utility ───────────────────────────────────────

// Create a symbolic link named `linkpath` that refers to `target`.
// Returns an IO error on failure.
LEAN_EXPORT lean_object *lean_coreutils_symlink(b_lean_obj_arg target,
                                                 b_lean_obj_arg linkpath,
                                                 lean_object *w) {
    if (symlink(lean_string_cstr(target), lean_string_cstr(linkpath)) != 0) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string(strerror(errno))));
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// ─── link(2) for `ln` utility ─────────────────────────────────────────────

// Create a hard link named `newpath` referring to `oldpath`.
// Returns an IO error on failure.
LEAN_EXPORT lean_object *lean_coreutils_link(b_lean_obj_arg oldpath,
                                              b_lean_obj_arg newpath,
                                              lean_object *w) {
    if (link(lean_string_cstr(oldpath), lean_string_cstr(newpath)) != 0) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string(strerror(errno))));
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// ─── chmod(2) for `chmod` utility ─────────────────────────────────────────

// Change the mode (permission bits) of `path` to `mode`.
// Returns an IO error on failure.
LEAN_EXPORT lean_object *lean_coreutils_chmod(b_lean_obj_arg path,
                                               uint32_t mode,
                                               lean_object *w) {
    if (chmod(lean_string_cstr(path), (mode_t)mode) != 0) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string(strerror(errno))));
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// ─── stat(2) mode bits for `chmod` symbolic modes ─────────────────────────

// Return the permission (st_mode) bits of `path`. Used by the `chmod` utility
// to compute new modes for symbolic (ug+-=) mode specifications. Returns an
// IO error if the path cannot be stat'd.
LEAN_EXPORT lean_object *lean_coreutils_stat_mode(b_lean_obj_arg path,
                                                   lean_object *w) {
    struct stat st;
    if (stat(lean_string_cstr(path), &st) != 0) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string(strerror(errno))));
    }
    return lean_io_result_mk_ok(lean_box((uint32_t)(st.st_mode & 0xFFFF)));
}

// ─── env: run a command with modified environment ───────────────────────────

// Helper: build a Lean list of strings from a null-terminated char** array.
// Used to reconstruct argv after fork on error paths.
// Helper: find an executable in PATH.
// Uses manual string parsing instead of strtok_r.
static char *find_in_path(const char *name) {
    if (strchr(name, '/')) {
        return strdup(name);
    }
    const char *path = getenv("PATH");
    if (!path || *path == '\0') return NULL;
    // Duplicate PATH so we can modify it
    char *path_copy = strdup(path);
    char *result = NULL;
    char *p = path_copy;
    while (*p) {
        // Find the end of this directory entry
        char *end = strchr(p, ':');
        if (end) *end = '\0';
        // Build full path
        size_t len = strlen(p) + 1 + strlen(name) + 1;
        char *full = malloc(len);
        snprintf(full, len, "%s/%s", p, name);
        if (access(full, X_OK) == 0) {
            result = full;
            break;
        }
        free(full);
        if (!end) break;
        p = end + 1;
    }
    free(path_copy);
    return result;
}

// env utility: run a command with modified environment, returning exit code.
// Arguments are Array String for safe iteration.
LEAN_EXPORT lean_object *lean_coreutils_run_env(b_lean_obj_arg env_vars,
                                                  uint32_t clear_env,
                                                  b_lean_obj_arg cmd_argv,
                                                  lean_object *w) {
    (void)w;
    // Verify that both arrays are valid
    if (!lean_is_array(cmd_argv)) {
        return lean_io_result_mk_ok(lean_box(127));
    }
    size_t argc = lean_array_size(cmd_argv);
    if (argc == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }
    // Build argv array
    char **argv = malloc((argc + 1) * sizeof(char *));
    for (size_t i = 0; i < argc; i++) {
        lean_object *s = lean_array_get_core(cmd_argv, i);
        argv[i] = strdup(lean_string_cstr(s));
    }
    argv[argc] = NULL;

    // Build environment
    extern char **environ;
    size_t environ_count = 0;
    if (!clear_env) {
        while (environ[environ_count]) environ_count++;
    }
    // Get env vars from array
    if (!lean_is_array(env_vars)) {
        for (size_t k = 0; k < argc; k++) free(argv[k]);
        free(argv);
        return lean_io_result_mk_ok(lean_box(127));
    }
    size_t extra_count = lean_array_size(env_vars);
    // Build new environment array
    size_t new_count = environ_count + extra_count;
    char **new_environ = malloc((new_count + 1) * sizeof(char *));
    size_t j = 0;
    if (!clear_env) {
        for (size_t ei = 0; ei < environ_count; ei++) {
            new_environ[j++] = strdup(environ[ei]);
        }
    }
    for (size_t i = 0; i < extra_count; i++) {
        lean_object *s = lean_array_get_core(env_vars, i);
        new_environ[j++] = strdup(lean_string_cstr(s));
    }
    new_environ[new_count] = NULL;

    // Find the executable
    char *exe = find_in_path(argv[0]);
    if (!exe) {
        write(STDERR_FILENO, argv[0], strlen(argv[0]));
        write(STDERR_FILENO, ": command not found\n", 20);
        for (size_t k = 0; k < argc; k++) free(argv[k]);
        free(argv);
        for (size_t k = 0; k < new_count; k++) free(new_environ[k]);
        free(new_environ);
        return lean_io_result_mk_ok(lean_box(127));
    }

    // Fork and exec
    pid_t pid = fork();
    if (pid == -1) {
        free(exe);
        for (size_t k = 0; k < argc; k++) free(argv[k]);
        free(argv);
        for (size_t k = 0; k < new_count; k++) free(new_environ[k]);
        free(new_environ);
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string("fork failed")));
    }
    if (pid == 0) {
        execve(exe, argv, new_environ);
        int exit_code = (errno == ENOENT) ? 127 : 126;
        _exit(exit_code);
    }
    free(exe);
    for (size_t k = 0; k < argc; k++) free(argv[k]);
    free(argv);
    for (size_t k = 0; k < new_count; k++) free(new_environ[k]);
    free(new_environ);
    int status;
    if (waitpid(pid, &status, 0) == -1) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string("waitpid failed")));
    }
    int exit_code = WIFEXITED(status) ? WEXITSTATUS(status)
                   : WIFSIGNALED(status) ? 128 + WTERMSIG(status)
                   : 1;
    return lean_io_result_mk_ok(lean_box((uint32_t)exit_code));
}

// ─── argv helpers (for fork/exec utilities) ────────────────────────────────

// Build a NULL-terminated argv array from a Lean array of strings.
// Caller must free with free_argv().
static char **build_argv(b_lean_obj_arg cmd_argv, size_t *argc_out) {
    size_t argc = lean_array_size(cmd_argv);
    char **argv = malloc((argc + 1) * sizeof(char *));
    if (!argv) return NULL;
    for (size_t i = 0; i < argc; i++) {
        lean_object *s = lean_array_get_core(cmd_argv, i);
        argv[i] = strdup(lean_string_cstr(s));
    }
    argv[argc] = NULL;
    *argc_out = argc;
    return argv;
}

// Free an argv array produced by build_argv().
static void free_argv(char **argv, size_t argc) {
    if (!argv) return;
    for (size_t k = 0; k < argc; k++) free(argv[k]);
    free(argv);
}

// Collect a child's exit code from waitpid status.
static uint32_t child_exit_code(int status) {
    if (WIFEXITED(status))
        return (uint32_t)WEXITSTATUS(status);
    if (WIFSIGNALED(status))
        return (uint32_t)(128 + WTERMSIG(status));
    return 1;
}

// ─── nice: run a command with an adjusted niceness ──────────────────────────

// Run `cmd_argv` in a forked child after calling nice(adjustment) in the child.
// Returns the child's exit code (or 124/126/127 on failure).
LEAN_EXPORT lean_object *lean_coreutils_run_nice(int32_t adjustment,
                                                  b_lean_obj_arg cmd_argv,
                                                  lean_object *w) {
    (void)w;
    if (!lean_is_array(cmd_argv))
        return lean_io_result_mk_ok(lean_box(127));
    size_t argc;
    char **argv = build_argv(cmd_argv, &argc);
    if (!argv) return lean_io_result_mk_ok(lean_box(127));
    if (argc == 0) { free_argv(argv, argc); return lean_io_result_mk_ok(lean_box(0)); }
    char *exe = find_in_path(argv[0]);
    if (!exe) {
        write(STDERR_FILENO, argv[0], strlen(argv[0]));
        write(STDERR_FILENO, ": command not found\n", 20);
        free_argv(argv, argc);
        return lean_io_result_mk_ok(lean_box(127));
    }
    pid_t pid = fork();
    if (pid == -1) {
        free(exe); free_argv(argv, argc);
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string("fork failed")));
    }
    if (pid == 0) {
        nice((int)adjustment);
        execve(exe, argv, environ);
        int exit_code = (errno == ENOENT) ? 127 : 126;
        _exit(exit_code);
    }
    free(exe); free_argv(argv, argc);
    int status;
    if (waitpid(pid, &status, 0) == -1) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string("waitpid failed")));
    }
    return lean_io_result_mk_ok(lean_box(child_exit_code(status)));
}

// ─── nohup: run a command immune to SIGHUP ──────────────────────────────────

// Run `cmd_argv` in a forked child that ignores SIGHUP and, when stdout is a
// terminal, redirects output to nohup.out. Returns the child's exit code.
LEAN_EXPORT lean_object *lean_coreutils_run_nohup(b_lean_obj_arg cmd_argv,
                                                   lean_object *w) {
    (void)w;
    if (!lean_is_array(cmd_argv))
        return lean_io_result_mk_ok(lean_box(127));
    size_t argc;
    char **argv = build_argv(cmd_argv, &argc);
    if (!argv) return lean_io_result_mk_ok(lean_box(127));
    if (argc == 0) { free_argv(argv, argc); return lean_io_result_mk_ok(lean_box(0)); }
    char *exe = find_in_path(argv[0]);
    if (!exe) {
        write(STDERR_FILENO, argv[0], strlen(argv[0]));
        write(STDERR_FILENO, ": command not found\n", 20);
        free_argv(argv, argc);
        return lean_io_result_mk_ok(lean_box(127));
    }
    pid_t pid = fork();
    if (pid == -1) {
        free(exe); free_argv(argv, argc);
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string("fork failed")));
    }
    if (pid == 0) {
        signal(SIGHUP, SIG_IGN);
        if (isatty(STDIN_FILENO)) {
            int fd = open("/dev/null", O_RDONLY);
            if (fd != -1) { dup2(fd, STDIN_FILENO); close(fd); }
        }
        if (isatty(STDOUT_FILENO)) {
            char path[PATH_MAX + 1];
            const char *home = getenv("HOME");
            if (home)
                snprintf(path, sizeof(path), "%s/nohup.out", home);
            else
                snprintf(path, sizeof(path), "nohup.out");
            int fd = open(path, O_WRONLY | O_CREAT | O_APPEND, 0600);
            if (fd != -1) { dup2(fd, STDOUT_FILENO); close(fd); }
        }
        if (isatty(STDERR_FILENO)) {
            dup2(STDOUT_FILENO, STDERR_FILENO);
        }
        execve(exe, argv, environ);
        int exit_code = (errno == ENOENT) ? 127 : 126;
        _exit(exit_code);
    }
    free(exe); free_argv(argv, argc);
    int status;
    if (waitpid(pid, &status, 0) == -1) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string("waitpid failed")));
    }
    return lean_io_result_mk_ok(lean_box(child_exit_code(status)));
}

// ─── timeout: run a command with a time limit ───────────────────────────────

// Run `cmd_argv` in a forked child, sending `sig` after `seconds` elapsed.
// If `kill_after` > 0, SIGKILL is sent that many seconds later if the child
// is still alive. Returns the child's exit code, or 124 on timeout.
//
// Implemented with a polling waitpid loop (1-second granularity) rather than
// alarm(2): the Lean runtime manages its own timer signal, so a SIGALRM-based
// alarm would not reliably interrupt this thread's waitpid. Polling achieves
// the same timeout semantics without interfering with the runtime.
LEAN_EXPORT lean_object *lean_coreutils_run_timeout(uint32_t seconds,
                                                     uint32_t sig,
                                                     uint32_t kill_after,
                                                     b_lean_obj_arg cmd_argv,
                                                     lean_object *w) {
    (void)w;
    if (!lean_is_array(cmd_argv))
        return lean_io_result_mk_ok(lean_box(127));
    size_t argc;
    char **argv = build_argv(cmd_argv, &argc);
    if (!argv) return lean_io_result_mk_ok(lean_box(127));
    if (argc == 0) { free_argv(argv, argc); return lean_io_result_mk_ok(lean_box(0)); }
    char *exe = find_in_path(argv[0]);
    if (!exe) {
        write(STDERR_FILENO, argv[0], strlen(argv[0]));
        write(STDERR_FILENO, ": command not found\n", 20);
        free_argv(argv, argc);
        return lean_io_result_mk_ok(lean_box(127));
    }
    pid_t pid = fork();
    if (pid == -1) {
        free(exe); free_argv(argv, argc);
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string("fork failed")));
    }
    if (pid == 0) {
        execve(exe, argv, environ);
        int exit_code = (errno == ENOENT) ? 127 : 126;
        _exit(exit_code);
    }
    free(exe); free_argv(argv, argc);
    int status = 0;
    int exit_code = 0;
    int timed_out = 0;
    uint32_t elapsed = 0;
    pid_t r = 0;
    while ((r = waitpid(pid, &status, WNOHANG)) == 0) {
        if (elapsed >= seconds) {
            // Time limit reached: deliver the requested signal.
            kill(pid, (int)sig);
            timed_out = 1;
            if (kill_after > 0) {
                uint32_t k = 0;
                while ((r = waitpid(pid, &status, WNOHANG)) == 0) {
                    if (k >= kill_after) {
                        kill(pid, SIGKILL);
                        break;
                    }
                    sleep(1);
                    k++;
                }
            } else {
                // Wait for the child to die from the signal.
                waitpid(pid, &status, 0);
            }
            break;
        }
        sleep(1);
        elapsed++;
    }
    if (r == pid) {
        exit_code = (int)child_exit_code(status);
    } else if (timed_out) {
        exit_code = 124;
        if (kill_after > 0) waitpid(pid, &status, 0);  // reap if SIGKILL path
    }
    return lean_io_result_mk_ok(lean_box((uint32_t)exit_code));
}

// ─── env: list environment variables ─────────────────────────────────────────

// Return the environment as a Lean Array of "KEY=VALUE" strings.
// Duplicates lean_coreutils_environ but returns as list for env's use.
LEAN_EXPORT lean_object *lean_coreutils_list_env(lean_object *w) {
    extern char **environ;
    size_t count = 0;
    while (environ[count]) count++;
    lean_object *lst = lean_alloc_ctor(0, 0, 0);  // nil
    for (size_t i = count; i > 0; i--) {
        lean_object *s = lean_mk_string(environ[i-1]);
        lean_object *cons = lean_alloc_ctor(1, 2, 0);
        lean_ctor_set(cons, 0, s);
        lean_ctor_set(cons, 1, lst);
        lst = cons;
    }
    return lean_io_result_mk_ok(lst);
}

// ─── gettimeofday(2) for `date` utility ─────────────────────────────────────

// Return the current Unix time as (seconds, microseconds).
// Returns a Lean pair (sec : UInt64, usec : UInt64) via a boxed UInt64
// where the top 32 bits are microseconds and the bottom 32 bits are seconds.
LEAN_EXPORT lean_object *lean_coreutils_gettimeofday(lean_object *w) {
    struct timeval tv;
    if (gettimeofday(&tv, NULL) != 0) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string(strerror(errno))));
    }
    // Return as UInt64: (usec << 32) | (sec & 0xFFFFFFFF)
    uint64_t packed = ((uint64_t)(uint32_t)tv.tv_usec << 32) | (uint64_t)(uint32_t)tv.tv_sec;
    return lean_io_result_mk_ok(lean_box_uint64(packed));
}

// ─── stat(2) helpers for du, df, stat utilities ───────────────────────────

// Helper: build stat array from struct stat
static lean_object *stat_to_array(const struct stat *st) {
    lean_object *lst = lean_alloc_ctor(0, 0, 0);  // nil
    uint64_t vals[10] = {
        (uint64_t)st->st_mode,
        (uint64_t)st->st_size,
        (uint64_t)st->st_nlink,
        (uint64_t)st->st_uid,
        (uint64_t)st->st_gid,
        (uint64_t)st->st_blocks,
        (uint64_t)st->st_blksize,
        (uint64_t)st->st_dev,
        (uint64_t)st->st_ino,
        (uint64_t)st->st_rdev
    };
    for (int i = 9; i >= 0; i--) {
        lean_object *cons = lean_alloc_ctor(1, 2, 0);
        lean_ctor_set(cons, 0, lean_box_uint64(vals[i]));
        lean_ctor_set(cons, 1, lst);
        lst = cons;
    }
    return lean_array_mk(lst);
}

// Returns stat info as a Lean Array of UInt64 values using stat(2):
//   [mode, size, nlink, uid, gid, blocks, blksize, dev, ino, rdev]
// Returns an IO error if the path cannot be stat'd.
LEAN_EXPORT lean_object *lean_coreutils_stat_all(b_lean_obj_arg path,
                                               lean_object *w) {
    struct stat st;
    if (stat(lean_string_cstr(path), &st) != 0) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string(strerror(errno))));
    }
    return lean_io_result_mk_ok(stat_to_array(&st));
}

// lstat(2): same as stat_all but uses lstat (does not follow symlinks).
LEAN_EXPORT lean_object *lean_coreutils_lstat_all(b_lean_obj_arg path,
                                                lean_object *w) {
    struct stat st;
    if (lstat(lean_string_cstr(path), &st) != 0) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string(strerror(errno))));
    }
    return lean_io_result_mk_ok(stat_to_array(&st));
}

// ─── statvfs(2) for `df` utility ─────────────────────────────────────────

// Returns statvfs info as a Lean Array of UInt64 values:
//   [f_bsize, f_frsize, f_blocks, f_bfree, f_bavail, f_files, f_ffree, f_favail, f_namemax]
// Returns an IO error if the path cannot be statvfs'd.
LEAN_EXPORT lean_object *lean_coreutils_statvfs_all(b_lean_obj_arg path,
                                                    lean_object *w) {
    struct statvfs sv;
    if (statvfs(lean_string_cstr(path), &sv) != 0) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string(strerror(errno))));
    }
    lean_object *lst = lean_alloc_ctor(0, 0, 0);  // nil
    uint64_t vals[9] = {
        (uint64_t)sv.f_bsize,
        (uint64_t)sv.f_frsize,
        (uint64_t)sv.f_blocks,
        (uint64_t)sv.f_bfree,
        (uint64_t)sv.f_bavail,
        (uint64_t)sv.f_files,
        (uint64_t)sv.f_ffree,
        (uint64_t)sv.f_favail,
        (uint64_t)sv.f_namemax
    };
    for (int i = 8; i >= 0; i--) {
        lean_object *cons = lean_alloc_ctor(1, 2, 0);
        lean_ctor_set(cons, 0, lean_box_uint64(vals[i]));
        lean_ctor_set(cons, 1, lst);
        lst = cons;
    }
    lean_object *arr = lean_array_mk(lst);
    return lean_io_result_mk_ok(arr);
}

// ─── truncate(2) for `truncate` utility ───────────────────────────────────

// truncate(2) — fails on non-existent files
LEAN_EXPORT lean_object *lean_coreutils_truncate(b_lean_obj_arg path,
                                                  uint64_t size,
                                                  lean_object *w) {
    if (truncate(lean_string_cstr(path), (off_t)size) != 0) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string(strerror(errno))));
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// truncate with file creation: open(2) + ftruncate(2) to create non-existent files.
// If the file does not exist, creates it with mode 0666 (umask applies).
// If the file exists, truncates it to the given size (or extends with holes).
LEAN_EXPORT lean_object *lean_coreutils_truncate_file(b_lean_obj_arg path,
                                                       uint64_t size,
                                                       lean_object *w) {
    const char *p = lean_string_cstr(path);
    int fd = open(p, O_CREAT | O_WRONLY, 0666);
    if (fd < 0) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string(strerror(errno))));
    }
    if (ftruncate(fd, (off_t)size) != 0) {
        int saved_errno = errno;
        close(fd);
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(saved_errno, lean_mk_string(strerror(saved_errno))));
    }
    close(fd);
    return lean_io_result_mk_ok(lean_box(0));
}

// ─── kill(2) for `kill` utility ──────────────────────────────────────────────

// Send a signal to a process. pid is the process ID, sig is the signal number.
// Returns an IO error on failure (e.g., ESRCH, EPERM).
LEAN_EXPORT lean_object *lean_coreutils_kill(int32_t pid,
                                              int32_t sig,
                                              lean_object *w) {
    (void)w;
    if (kill((pid_t)pid, (int)sig) != 0) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string(strerror(errno))));
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// ─── who: list logged-in users via utmpx ──────────────────────────────────────

// Returns an Array of user entries from utmpx.
// Each entry is a string formatted as:
//   "user|line|time_sec|host"
// where time_sec is the epoch seconds as a decimal string.
// This allows the Lean side to format the output.
LEAN_EXPORT lean_object *lean_coreutils_who(lean_object *w) {
    lean_object *lst = lean_alloc_ctor(0, 0, 0);  // nil
    setutxent();
    struct utmpx *ut;
    while ((ut = getutxent()) != NULL) {
        if (ut->ut_type != USER_PROCESS) continue;
        if (ut->ut_user[0] == '\0') continue;
        // Build entry: user|line|time_sec|host
        // We need to concat 4 pieces with '|' separators.
        // First, compute total length
        size_t user_len = strnlen(ut->ut_user, sizeof(ut->ut_user));
        size_t line_len = strnlen(ut->ut_line, sizeof(ut->ut_line));
        size_t host_len = strnlen(ut->ut_host, sizeof(ut->ut_host));
        // Format the time as a decimal string
        char time_buf[24];
        int time_len = snprintf(time_buf, sizeof(time_buf), "%lld",
                                (long long)ut->ut_tv.tv_sec);
        if (time_len < 0) time_len = 0;
        // Concatenate: user|line|time|host
        size_t total = user_len + 1 + line_len + 1 + (size_t)time_len + 1 + host_len;
        char *entry = (char *)malloc(total + 1);
        if (!entry) continue;
        char *p = entry;
        memcpy(p, ut->ut_user, user_len); p += user_len; *p++ = '|';
        memcpy(p, ut->ut_line, line_len); p += line_len; *p++ = '|';
        memcpy(p, time_buf, (size_t)time_len); p += time_len; *p++ = '|';
        memcpy(p, ut->ut_host, host_len); p += host_len;
        *p = '\0';
        lean_object *s = lean_mk_string(entry);
        free(entry);
        lean_object *cons = lean_alloc_ctor(1, 2, 0);
        lean_ctor_set(cons, 0, s);
        lean_ctor_set(cons, 1, lst);
        lst = cons;
    }
    endutxent();
    lean_object *arr = lean_array_mk(lst);
    return lean_io_result_mk_ok(arr);
}

// ─── gethostid(2) for `hostid` utility ───────────────────────────────────────

// Return the 32-bit host identifier as a hex string (8 hex digits).
LEAN_EXPORT lean_object *lean_coreutils_gethostid(lean_object *w) {
    long id = gethostid();
    char buf[16];
    snprintf(buf, sizeof(buf), "%08lx", id);
    return lean_io_result_mk_ok(lean_mk_string(buf));
}

// ─── chown(2) for `install` utility ─────────────────────────────────────────

// chown(2): change owner and/or group of a file.
// If `owner` or `group` is empty string, that component is left unchanged.
LEAN_EXPORT lean_object *lean_coreutils_chown(b_lean_obj_arg path,
                                               b_lean_obj_arg owner,
                                               b_lean_obj_arg group,
                                               lean_object *w) {
    const char *p = lean_string_cstr(path);
    const char *o = lean_string_cstr(owner);
    const char *g = lean_string_cstr(group);
    uid_t uid = (uid_t)-1;
    gid_t gid = (gid_t)-1;
    
    if (o && o[0]) {
        struct passwd *pw = getpwnam(o);
        if (pw == NULL) {
            // Try numeric
            char *endptr;
            long n = strtol(o, &endptr, 10);
            if (*endptr == '\0') {
                uid = (uid_t)n;
            } else {
                return lean_io_result_mk_error(
                    lean_mk_io_error_other_error(EINVAL, lean_mk_string("invalid owner")));
            }
        } else {
            uid = pw->pw_uid;
        }
    }
    if (g && g[0]) {
        struct group *gr = getgrnam(g);
        if (gr == NULL) {
            char *endptr;
            long n = strtol(g, &endptr, 10);
            if (*endptr == '\0') {
                gid = (gid_t)n;
            } else {
                return lean_io_result_mk_error(
                    lean_mk_io_error_other_error(EINVAL, lean_mk_string("invalid group")));
            }
        } else {
            gid = gr->gr_gid;
        }
    }
    if (chown(p, uid, gid) != 0) {
        return lean_io_result_mk_error(
            lean_mk_io_error_other_error(errno, lean_mk_string(strerror(errno))));
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// ─── isatty(3) for `mesg` utility ────────────────────────────────────────────

// Check if a file descriptor refers to a terminal.
// Returns 1 if yes, 0 if no.
LEAN_EXPORT lean_object *lean_coreutils_isatty(uint32_t fd,
                                                 lean_object *w) {
    int r = isatty((int)fd);
    return lean_io_result_mk_ok(lean_box((uint32_t)(r != 0 ? 1 : 0)));
}

// ─── ttyname(3) for `mesg` utility ────────────────────────────────────────────

// Returns the terminal device path for a file descriptor.
// Returns empty string if not a tty.
LEAN_EXPORT lean_object *lean_coreutils_ttyname(uint32_t fd,
                                                  lean_object *w) {
    char *name = ttyname((int)fd);
    if (name == NULL) {
        return lean_io_result_mk_ok(lean_mk_string(""));
    }
    return lean_io_result_mk_ok(lean_mk_string(name));
}

// ─── getmntent(3) for `df` utility ──────────────────────────────────────────

// Returns a list of mounted filesystem paths as a Lean Array of strings.
// Reads /etc/mtab (or /proc/mounts) via getmntent().
// Returns empty array on error.
LEAN_EXPORT lean_object *lean_coreutils_getmounts(lean_object *w) {
    FILE *mtab = setmntent("/etc/mtab", "r");
    if (!mtab) {
        mtab = setmntent("/proc/mounts", "r");
        if (!mtab) {
            // Fallback: try reading /proc/mounts directly
            FILE *f = fopen("/proc/mounts", "r");
            if (!f) {
                return lean_io_result_mk_ok(lean_array_mk(lean_alloc_ctor(0, 0, 0)));
            }
            // Simple parsing: second field is mount point
            lean_object *lst = lean_alloc_ctor(0, 0, 0);  // nil
            char line[4096];
            while (fgets(line, sizeof(line), f)) {
                char mnt[4096] = {0};
                // Skip first field (device), get second field (mount point)
                char *p = line;
                while (*p && *p != ' ') p++;
                if (*p) p++;
                int i = 0;
                while (*p && *p != ' ' && i < 4095) {
                    mnt[i++] = *p++;
                }
                mnt[i] = '\0';
                if (mnt[0]) {
                    lean_object *s = lean_mk_string(mnt);
                    lean_object *cons = lean_alloc_ctor(1, 2, 0);
                    lean_ctor_set(cons, 0, s);
                    lean_ctor_set(cons, 1, lst);
                    lst = cons;
                }
            }
            fclose(f);
            return lean_io_result_mk_ok(lean_array_mk(lst));
        }
    }
    lean_object *lst = lean_alloc_ctor(0, 0, 0);  // nil
    struct mntent *mnt;
    while ((mnt = getmntent(mtab)) != NULL) {
        if (mnt->mnt_dir && mnt->mnt_dir[0]) {
            lean_object *s = lean_mk_string(mnt->mnt_dir);
            lean_object *cons = lean_alloc_ctor(1, 2, 0);
            lean_ctor_set(cons, 0, s);
            lean_ctor_set(cons, 1, lst);
            lst = cons;
        }
    }
    endmntent(mtab);
    return lean_io_result_mk_ok(lean_array_mk(lst));
}

