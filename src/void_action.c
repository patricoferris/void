#define _GNU_SOURCE
#define _FILE_OFFSET_BITS 64
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

#ifndef SYS_pidfd_send_signal
# define SYS_pidfd_send_signal 424
#endif

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

static int pidfd_send_signal(int pidfd, int sig, siginfo_t *info, unsigned int flags) {
  return syscall(SYS_pidfd_send_signal, pidfd, sig, info, flags);
}

CAMLprim value caml_void_pidfd_send_signal(value v_pidfd, value v_signal) {
  CAMLparam0();
  int res;

  res = pidfd_send_signal(Int_val(v_pidfd), caml_convert_signal_number(Int_val(v_signal)), NULL, 0);
  if (res == -1) uerror("pidfd_send_signal", Nothing);
  CAMLreturn(Val_unit);
}

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

  int r;
  
  r = mount(String_val(v_src), String_val(v_tgt), String_val(v_type), Int_val(v_flags), NULL);
  
  if (r != 0) {
    eio_unix_fork_error(errors, "mount", strerror(errno));
    _exit(1);
  }
}

CAMLprim value void_fork_mount(value v_unit) {
  return Val_fork_fn(action_mount);
}

// Writes a single line to a file
static int put_line(const char *filename, const char *line) {
  int fd;
  int written;

  fd = open(filename, O_WRONLY | O_CLOEXEC | O_CREAT | O_TRUNC, 0644);
  
  if (fd < 0) {
    return fd;
  }

  written = write(fd, line, strlen(line));

  close(fd);

  if (written != strlen(line)) {
     return -1;
  }

  return 0;
}

// MAP UID/GID to root
static void action_map_uid_gid(int errors, value v_config) {
  value v_uid = Field(v_config, 1);
  value v_gid = Field(v_config, 2);
  int result;
  char uid_line[30];
  char gid_line[30];

  // We map root onto the calling UID
  snprintf(uid_line, sizeof(uid_line), "0 %i 1\n", Int_val(v_uid));
  result = put_line("/proc/self/uid_map", uid_line);
  
  if (result < 0) {
    eio_unix_fork_error(errors, "map_uid_gid-uid", strerror(errno));
    _exit(1);
  }

  /* From user_namespaces(7)
   *
   * Writing "deny" to the /proc/pid/setgroups file before writing to
   * /proc/pid/gid_map will permanently disable setgroups(2) in a user
   * namespace and allow writing to /proc/pid/gid_map without having
   * the CAP_SETGID capability in the parent user namespace.
   *
   * See also: https://lwn.net/Articles/626665/ */
  
  put_line("/proc/self/setgroups", "deny\n");

  if (result < 0) {
    eio_unix_fork_error(errors, "map_uid_gid-setgroups", strerror(errno));
    _exit(1);
  }

  result = snprintf(gid_line, sizeof(gid_line), "0 %i 1\n", Int_val(v_gid));
  put_line("/proc/self/gid_map", gid_line);

  if (result < 0) {
    eio_unix_fork_error(errors, "map_uid_gid-gid", strerror(errno));
    _exit(1);
  }
}


CAMLprim value void_fork_map_uid_gid(value v_unit) {
  return Val_fork_fn(action_map_uid_gid);
}

// PIVOT ROOT
//
static int pivot_root(const char *new_root, const char *put_old) {
  return syscall(SYS_pivot_root, new_root, put_old);
}

// Is there too much OCaml stuff going on here for a fork_action ?
static void action_pivot_root(int errors, value v_config) {
  value v_new_root = Field(v_config, 1);
  value v_no_root = Field(v_config, 2);
  value v_mounts = Field(v_config, 3);
  char path[PATH_MAX];
  char old_root_path[PATH_MAX];
  char *new_root = String_val(v_new_root);
  const char *put_old = ".old_root";

  // From pivot_root example: We want to change the propagation type
  // of root to be private so we can pivot it.
  if (mount(NULL, "/", NULL, MS_REC | MS_PRIVATE, NULL) == -1) {
    eio_unix_fork_error(errors, "pivot_root-private", strerror(errno));
    _exit(1);
  }

  // If no pivot_root was given, then we tmpfs the tmpdir we assume was passed.
  if (Bool_val(v_no_root)) {
    // Make a temporary directory... can't because it allocates ?
    //if (mkdtemp(new_root) != NULL) {
    //  eio_unix_fork_error(errors, new_root, strerror(errno));
    //  _exit(1);
    //}

    if (mount("tmpfs", new_root, "tmpfs", 0, NULL) <= -1) {
      eio_unix_fork_error(errors, "pivot_root-tmpfs", strerror(errno));
      _exit(1);
    }
  } else {
	// From pivot_root example: we check that new_root is indeed a mountpoint 
	if (mount(new_root, new_root, NULL, MS_BIND, NULL) <= -1) {
  	  eio_unix_fork_error(errors, "pivot_root-new_root", strerror(errno));
  	  _exit(1);
	}
  }

  // Make the place to pivot the old root too, under the new root
  snprintf(old_root_path, sizeof(path), "%s/%s", new_root, put_old);
 
  if (mkdir(old_root_path, 0777) == -1) {
    eio_unix_fork_error(errors, "pivot_root-mkdir-put_old", strerror(errno));
    _exit(1);
  }

  // Pivot the root
  if (pivot_root(new_root, old_root_path)) {
    eio_unix_fork_error(errors, "pivot_root", strerror(errno));
    _exit(1);
  }

  // Add mounts
  value current_mount = v_mounts;
  int mount_result;
  while(current_mount != Val_emptylist) {
    // TODO: Mode for mounting

    if(mkdir(String_val(Field(Field(current_mount, 0), 1)), 0777) == -1) {
      eio_unix_fork_error(errors, "pivot_root-mkdir-mount", strerror(errno));
      _exit(1);
    }

    mount_result = mount(
      String_val(Field(Field(current_mount, 0), 0)),
      String_val(Field(Field(current_mount, 0), 1)),
      NULL,
      MS_REC | MS_BIND,
      NULL
    );

    // Fail early if a mount fails...
    if (mount_result < 0) {
      char error[PATH_MAX];
      snprintf(error, sizeof(error), "mount failed: (%s->%s)",
		      String_val(Field(Field(current_mount, 0), 0)),
                      String_val(Field(Field(current_mount, 0), 1)));
      eio_unix_fork_error(errors, error, strerror(errno));
      _exit(1);
    }

    // Next mount in the list
    current_mount = Field(current_mount, 1);
  }

  // Change to the 'new' root
  if (chdir("/") == -1) {
    eio_unix_fork_error(errors, "pivot_root-chdir", strerror(errno));
    _exit(1);
  }
  
  // Unmount the old root and remove it
  if (umount2(put_old, MNT_DETACH) == -1) {
    eio_unix_fork_error(errors, put_old, strerror(errno));
    _exit(1);
  }

  // Remove the old root
  if (rmdir(put_old) == -1) {
    eio_unix_fork_error(errors, put_old, strerror(errno));
    _exit(1);
  }
}

CAMLprim value void_fork_pivot_root(value v_unit) {
  return Val_fork_fn(action_pivot_root);
}

