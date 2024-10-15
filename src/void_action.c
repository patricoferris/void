#define _GNU_SOURCE
#include <linux/sched.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/eventfd.h>
#if __GLIBC__ > 2 || __GLIBC_MINOR__ > 24
#include <sys/random.h>
#endif
#include <sys/syscall.h>
#include <sys/wait.h>
#include <sys/mount.h>
#include <limits.h>
#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include <string.h>


#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>

// From Eio
#include <include/fork_action.h>

// struct clone_args isn't defined in linux-lts headers, so define it here
// Note that this struct is versioned by size. See linux/sched.h for details
struct caml_void_clone_args {
  uint64_t flags;
  uint64_t pidfd;
  uint64_t child_tid;
  uint64_t parent_tid;
  uint64_t exit_signal;
  uint64_t stack;
  uint64_t stack_size;
  uint64_t tls;
};

static pid_t clone3_no_fallback(struct caml_void_clone_args *cl_args) {
  int *pidfd = (int *)(uintptr_t) cl_args->pidfd;
  pid_t child_pid = syscall(SYS_clone3, cl_args, sizeof(struct caml_void_clone_args));

  if (child_pid >= 0)
    return child_pid;		/* Success! */

  if (errno != ENOSYS && errno != EPERM) {
    uerror("clone3", Nothing);	/* Unknown error */
  }

  uerror("clone3", Nothing);
}

CAMLprim value caml_void_clone3(value v_errors, value v_flags, value v_actions) {
  CAMLparam1(v_actions);
  CAMLlocal1(v_result);
  pid_t child_pid;
  int pidfd = -1;		/* Is automatically close-on-exec */

  struct caml_void_clone_args cl_args = {
    .flags = Int_val(v_flags),
    .pidfd = (uintptr_t) &pidfd,
    .exit_signal = SIGCHLD,	/* Needed for wait4 to work if we exit before exec */
    .stack = (uintptr_t) NULL,	/* Use copy-on-write parent stack */
    .stack_size = 0,
  };

  child_pid = clone3_no_fallback(&cl_args);
  if (child_pid == 0) {
    /* Run child actions (doesn't return) */
    eio_unix_run_fork_actions(Int_val(v_errors), v_actions);
  }

  v_result = caml_alloc_tuple(2);
  Store_field(v_result, 0, Val_long(child_pid));
  Store_field(v_result, 1, Val_int(pidfd));

  CAMLreturn(v_result);
}


// Actions 

// MOUNT/UNMOUNT
static void action_mount(int errors, value v_config) {
  value v_src = Field(v_config, 1);
  value v_tgt = Field(v_config, 2);
  value v_type = Field(v_config, 3);
  value v_flags = Field(v_config, 4);
  
  mount(String_val(v_src), String_val(v_tgt), String_val(v_type), Int_val(v_flags), NULL);
  eio_unix_fork_error(errors, "mount", strerror(errno));
  _exit(1);
}

CAMLprim value void_fork_mount(value v_unit) {
  return Val_fork_fn(action_mount);
}

