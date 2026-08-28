<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/real.md                  -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Numbers                                                       -->
<!--==================================================================-->

,(implementation-path "../runtime/Llib/bigloo.scm")
,(implementation-path "../runtime/Llib/os.scm")
,(implementation-path "../runtime/Llib/error.scm")
,(implementation-path "../runtime/Ieee/port.scm")
,(example-path "../test/src/os.bgl")

OS
==

Main and Exit
-------------

### command-line ###
Returns a list of strings which are the Unix command line arguments.

### executable-name ###
Returns the name of the running executable.

### exit ###
Applies all the registered exit functions then stops an execution, 
returning the integer `int`.

### register-exit-function! ###
Register `fun` as an exit functions. The `fun` argument is a procedure
accepting of one argument. This argument is the numerical value which
is the status of the exit call. The registered functions are called when the 
execution ends. 

### unregister-exit-function! ###
Unregisters a previously registered exit function.


Environment
-----------

### getenv ###
Reads the environment variable `name`. Returns its result as a string if
the variable exists. Return `#f` otherwise. If `name` is omitted, 
`getenv` returns the list of all environment variable.

### putenv ###
<!-- [:@C] -->
Assigns the environment variable.

### getrlimit ###
<!-- [:@C] -->
Get system limits.

The function `getrlimit` expects as argument a resource and it returns
two values: a soft and a hard limit. Both values are `elong`.

Applications can test the support of `getrlimit` with the `cond-expand`
feature `rlimit` (see [Conditional Execution](./condexpand.html)).


### setrlimit! ###
<!-- [:@C] -->
The function `setrlimit!` accepts a resource, a soft limit, and a
hard limit.  The soft and hard limits are either a `elong` value or
`+inf.0` to denote unlimited value. The function
returns a boolean, which is `#t` if the limit has been changed, and
`#f` otherwise.

A resource is either a fixnum, which must correspond to a native
resource identifier, or a symbol amongst:

  * `CORE`
  * `CPU`
  * `DATA`
  * `FSIZE`
  * `LOCKS`
  * `MEMLOCK`
  * `MSGQUEUE`
  * `NICE`
  * `NOFILE`
  * `NPROC`
  * `RSS`
  * `RTTIME`
  * `SIGPENDING`
  * `STACK`

All other symbols trigger an error.


File Names
----------

### file-separator ###
Gives the operating system file separator (e.g., `#\/`).

### path-separator ###
Gives the operating system file path separator (e.g., `#\:`).

### basename ###
Returns a copy of `string` where the longest prefix ending in @samp{/} is
deleted if any existed.

### dirname ###
Returns a new string which is the directory component of `string`.

### prefix ###
Returns a copy of `string` where the suffix starting by
the char `#\.` is deleted. If no prefix is found,
the result of `prefix` is a copy of `string`. For
instance:

### suffix ###
Returns a new string which is the suffix of `string`. If no
suffix is found, this function returns an empty string. For instance,

### file-name-absolute? ###
Returns `#t` if `name` is absolute file name. Returns
`#f` otherwise.

### make-file-name ###
Make an absolute file-name from a directory name `dir-name` and a relative
name `name`.

### make-file-path ###
Make an absolute file-name from a directory name `dir-name` and a relative
name `name`s.

### file-name->list ###
Explodes a file name into a list.

### unix-path->list ###
Converts a Unix path to a Bigloo list of strings.

### file-name-canonicalize ###
Canonicalizes a file name. If the file name is malformed this function
raises an `&io-malformed-url-error` exception. Returns a
fresh string.

### file-name-canonicalize! ###
As `file-name-canonicalize` but may returns its argument
if no changes in the string is needed. Otherwise, as 
`file-name-canonicalize` is returns a new string.

### file-name-unix-canonicalize ###
Similar to `file-name-unix-canonicalize` but in addition to handling
`..` directory name, the function
`file-name-unix-canonicalize` also handles the `~`
character. Returns a fresh string.

### file-name-unix-canonicalize! ###
As `file-name-unix-canonicalize` but may returns its argument.

### relative-file-name ###
Builds a file name relative to `base`.

### find-file/path ###
Search, in sequence, in the directory list `path` for the file
`name`.  If `name` is an absolute name, then `path` is not
used to find the file. If `name` is a relative name, the function
`make-file-name` is used to build absolute name from `name` and
the directories in `path`. The current path is not included
automatically in the list of `path`. In consequence, to check the
current directory one may add `"."` to the `path` list. On
success, the absolute file name is returned. On failure,
`#f` is returned. Example:

File Properties and Operations
------------------------------

### file-exists? ###
This procedure returns `#t` if the file (respectively directory, and link)
`path` exists. Otherwise it returns `#f`.

### file-gzip? ###
This procedure returns `#t` if and only if the file `path` exists
and can be unzip by Bigloo. Otherwise it returns `#f`.

### delete-file ###
Deletes the file named `path`. The result of this procedure
is `#t` is the operation succeeded. The result is `#f` otherwise.

### rename-file ###
Renames the file `from` as `to`. The two files have to
be located on the same file system. If the renaming succeeds, the result
is `#t`, otherwise it is `#f`.

### truncate-file ###
Truncates shall cause the regular file named by `path` to
have a `size` which shall be equal to length bytes.

Returns `#t` on success. Returns `#f` otherwise.

### copy-file ###
Copies the file `from` into `to`. If the copy succeeds, 
the result is `#t`, otherwise it is `#f`.

### make-symlink ###
Creates a symbolic link named `linkpath` which contains the
string `target`. Return `#t` on success.

### file-modification-time ###
Returns the file modification time.

### file-access-time ###
Returns the last file access time.

### file-change-time ###
Returns the last file  time.

### file-times-set! ###
Set the date (in second) of the last modification (respec. access) for
file `path`. The number of seconds is represented by a value
that may be converted into a date by the means of `seconds->date`
(see [Date](./date.html).

Returns `#t` if the operation succeeds.

### file-size ###
Returns the size (in bytes) for file `path`. On error, a negative size
is returned.

### file-uid ###
Returns the user id (an integer) for file `path`. On error, `-1` is returned.

### file-gid ###
The functions return the group id (an integer) 
for file `string`. On error, `-1` is returned.

### file-mode ###
Returns the file access mode (an integer). On error `-1` is returned.

### file-type ###
Returns the file type (a symbol). The possible returned values are:

  * `regular`
  * `directory`
  * `link`
  * `block`
  * `fifo`
  * `character`
  * `socket`
  * `resource`
  * `unknown`
  * `does-not-exist`

<span></span>

### chmod ###
Change the access mode of the file named `path`. The `option`
must be either a list of the following symbols `read`, `write` 
and `execute` or an integer. If the operation succeeds, `chmod` 
returns `#t`. It returns `#f` otherwise. The argument 
`option` can also be an integer that represents the native file
permission.

Directories
-----------

### directory? ###
This procedure returns `#t` if the file `string` exists and is a
directory. Otherwise it returns `#f`.

### pwd ###
Returns the current working directory.

### chdir ###
<!-- [:@C] --> 
Changes the current directory to `path`. On success, `chdir`
returns `#t`. On failure it returns `#f`.

### make-directory ###
Attempts to creae a new directory named `path`, with access mode `#o777`. It 
returns `#t` if the directory was created. It returns `#f` otherwise.

### make-directories ###
Creates a new directory named `path`, including any necessary
but nonexistent parent directories. It returns `#t` if the
directory was created. It returns `#f` otherwise. Note that 
if this operation fails it may have succeeded in creating some 
of the necessary parent directories.

### delete-directory ###
Deletes the directory named `path`. The directory must be empty
in order to be deleted. The result of this procedure is unspecified.

### delete-directories ###
Deletes recursively the directory named `path` and all the files and
subdirectories it contains.

### directory-length ###
If file `path` exists and is a directory, the function 
`directory-length` returns the number of entries contained in `string`.
If `path` is not a directory, returns `0`.

### directory->list ###
If file `path` exists and is a directory, the function 
`directory->list` returns the list of files in `path`.

### directory->path-list ###
The function `directory->path-list` returns a list of files
in `path` whose dirname are `path`.

### directory->vector ###
Similar to `directory->list` but returns a vector instead of a list.

### directory->path-vector ###
Similar to `directory->path-list` but returns a vector instead of a list.

Logs
----

### openlog ###
<!-- [:@C] --> 
Opens a system log. Wrapper to Unix syslog facilities. See the
`syslog` man page for detail.

### syslog ###
<!-- [:@C] -->
Emits a log.

### closelog ###
<!-- [:@C] -->
Close a system log.

### syslog-option ###
<!-- [:@C] -->
Set `syslog` option. The option might be a combination of

  * `LOG_CONS`
  * `LOG_NDELAY`
  * `LOG_NOWAIT`
  * `LOG_ODELAY`
  * `LOG_PID`
  
<span></span>

### syslog-level ###
<!-- [:@C] -->
The log level, which might be one of:

  * `LOG_EMERG`
  * `LOG_ALERT`
  * `LOG_CRIT`
  * `LOG_ERR`
  * `LOG_WARNING`
  * `LOG_NOTICE`
  * `LOG_INFO`
  * `LOG_DEBUG`

<span></span>


### syslog-facility ###
<!-- [:@C] -->

The log facility, which might be one of:

  * `LOG_AUTH`
  * `LOG_AUTHPRIV`
  * `LOG_CRON`
  * `LOG_DAEMON`
  * `LOG_FTP`
  * `LOG_KERN`
  * `LOG_LOCAL0`
  * `LOG_LOCAL1`
  * `LOG_LOCAL2`
  * `LOG_LOCAL3`
  * `LOG_LOCAL4`
  * `LOG_LOCAL5`
  * `LOG_LOCAL6`
  * `LOG_LOCAL7`
  * `LOG_LPR`
  * `LOG_MAIL`
  * `LOG_NEWS`
  * `LOG_SYSLOG`
  * `LOG_USER`
  * `LOG_UUCP`
      
<span></span>

Time and Sleep
--------------

### sleep ###
Sleeps for a delay during at least `micros` microseconds.

### time ###
Evaluates the `thunk` and returns four values: the result of calling
`thunk`, the actual execution time, the system time, and the user time
in millisecond.


OS Description
--------------

### os-class ###
Gives the OS class (e.g. `unix`).

### os-name ###
Gives the OS name (e.g. `Linux`).
@end deffn

### os-arch ###
Gives the host architecture (e.g. `i386`).

### os-version ###
Gives the operating system version (e.g. `RedHat 2.0.27`).

### os-tmp ###
Gives the regular temporary directory (e.g. `/tmp`).

### os-charset ###
Gives the charset used for encoding names of the file system 
(e.g. `UTF-8`).


Users and Groups
----------------

### getuid ###
The procedure `getuid` returns the UID 
of the user the current process is executed on behalf of.

### getgid ###
<!-- [:@C-wasm] -->
The procedure `getuid` returns the GID 
of the user the current process is executed on behalf of.

### setuid ###
<!-- [:@C] -->
The procedure sets the UID of the current process. In case of failure,
this procedure raises an error.

### getpid ###
<!-- [:@C-wasm] -->
Gets the current process identifier.

### setgid ###
<!-- [:@C] -->
The procedure sets the GID of the current process. In case of failure,
this procedure raises an error.

### getppid ###
<!-- [:@C] -->
Gets the parent process identifier.

### getgroups ###
<!-- [:@C] -->
Maps the Posix `getgroups` function, which returns the supplementary group
IDs of the calling process. The result is a vector of IDs. On error,
an IO exception is raised.

If the user is found, these two procedures returns a list of seven elements:

  * the user name,
  * his encrypted password,
  * his uid,
  * his group id,
  * his real name,
  * his home directory,
  * his preferred shell.

When no user is found, these procedures returns `#f`.

### getpwnam ###
<!-- [:@C] -->
Returns information about a user. The procedure 
`getpwname` accepts a string denoting the user name as argument.

### getpwuid ###
<!-- [:@C] -->
Returns information about a user.  The
procedure `getpwuid` accepts an UID as returned by the procedure
`getuid`.

If the user is found, these two procedures returns a list of seven elements:

  * the user name,
  * his encrypted password,
  * his uid,
  * his group id,
  * his real name,
  * his home directory,
  * his preferred shell.

When no user is found, these procedures returns `#f`.

Signals
-------

### signal ###
<!-- [:@C] -->
Provides a signal handler for the operating system dependent signal
`n`. The argument `proc` can either be:

  * The symbol `ignore`, ignore the signals
  * The symbol `default`, use the default handler for this signal
  * is a procedure of one argument.

Bigloo defines the following symbolic signal names:

  * `sighup`
  * `sigquit`
  * `sigill`
  * `sigabrt`
  * `sigfpe`
  * `sigkill`
  * `sigbus`
  * `sigsegv`
  * `sigpipe`
  * `sigalrm`
  * `sigterm`
  * `sigint`
  * `sigusr1`
  * `sigusr2`
  * `sigwinch`
  * `sigtrap`
        
<span></span>

### get-signal-handler ###
<!-- [:@C] -->
Returns the current handler associated with signal `n` or
`#f` if no handler is installed.

### sigsetmask ###
<!-- [:@C] -->
Sets the signal mask.


System
------

### system ###
<!-- [:@C-wasm] -->
Appends all the arguments `strings` and invokes the native host
`system` command on that new string which returns an integer.
Returns the execution status code (i.e., `0` for success).

### system->string ###
<!-- [:@C] -->
As `system` but returns a string made of the output of the
command.


