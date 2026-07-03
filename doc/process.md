<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/process.md               -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Processes                                                     -->
<!--==================================================================-->

,(implementation-path "../runtime/Llib/process.scm")
,(example-path "../test/src/process.bgl")

Child Processes
===============

Bigloo provides access to Unix-like processes as first class
objects. Basically, a process contains four informations: the standard
Unix process identification (aka PID) and the three standard files of
the process.

Predicates
----------

### process? ###
Returns `#t` if `obj` is a process, otherwise returns `#f`.

### process-alive? ###
Returns `#t` iff `process` is currently running.


Spawning New Processes
----------------------

### run-process ###

The function `run-process` creates a new process and run the
executable specified in `command`. The `arg` correspond to the command
line arguments.  When is process completes its execution, non pipe
associated ports are automatically closed. Pipe associated ports have
to be explicitly closed by the program. The following values of `p`
have a special meaning:
 
  * `input:` permits to redirect the standard input file of the process.
  Redirection can come from a file or from a pipe. To redirect the standard
  input from a file, the name of this file must be specified after `input:`.
  Use the special keyword `pipe:` to redirect the standard input 
  from a pipe.

  * `output:` permits to redirect the standard output file of the
  process.  Redirection can go to a file or to a pipe. To redirect the
  standard output to a file, the name of this file must be specified
  after `output:`. Use the special keyword `pipe:` to redirect the
  standard output to a pipe.

  * `error:` permits to redirect the standard error file of the
  process.  Redirection can go to a file or to a pipe. To redirect the
  standard error to a file, the name of this file must be specified
  after `error:`. Use the special keyword `pipe:` to redirect the
  standard error to a pipe.

  * `wait:` must be followed by a boolean value. This value
  specifies if the process must be ran asynchronously or not. By
  default, the process is run asynchronously (i.e., `wait:` if 
  `#f`).

  * `host:` must be followed by a string. This string represents the
  name of the machine on which the `command` must be executed. This
  option uses the external command `rsh`. The shell variable `PATH` 
  must be correctly set for accessing it without specifying its absolute
  path.

  * `fork:` must be followed by a boolean value. This value
  specifies if the process must substitute the current execution. That is,
  if the value is `#t` a new process is spawned otherwise, the current
  execution is stopped and replaced by the execution of `command`. It
  defaults to `#t`.

  * `env:` must be followed by a string of
  the form `var`=`val`. This will bound an environment variable
  in the spawned process. A `run-process` command may contain several 
  `env:` arguments. The current variables of the current process are
  also passed to the new process.


> [!WARNING] The wasm backend does not support `pipe:` argument.

The following example launches a process which execute the Unix
command `ls` with the arguments `-l` and `/bin`. The lines printed by
this command are stored in the file `/tmp/X`.

```bigloo
(run-process "ls" "-l" "/bin" output: "/tmp/X")
```

The same example with a pipe for output:

```bigloo
(let* ((proc (run-process "ls" "-l" "/bin" output: pipe:))
       (port (process-output-port proc)))
   (let loop ((line (read-line port)))
      (if (eof-object? line)
          (close-input-port port)
          (begin
             (print line)
             (loop (read-line port))))))
```

One should note that the same program can be written with explicit 
process handling but making use of the `|` notation for 
`open-input-file` (see chapter [Ports](./port.html)).

```bigloo
(let ((port (open-input-file "| ls -l /bin")))
   (let loop ((line (read-line port)))
      (if (eof-object? line)
          (close-input-port port)
          (begin
             (print line)
             (loop (read-line port))))))
```

Both input and output ports can be piped:

```bigloo
(let* ((proc (run-process "/usr/bin/dc" output: pipe: input: pipe:)) 
       (inport (process-input-port proc))
       (port (process-output-port proc)))
   (fprint inport "16 o")
   (fprint inport "16 i")
   (fprint inport "10")
   (fprint inport "10")
   (fprint inport "+ p")
   (flush-output-port inport)
   (let loop ((line (read-line port)))
      (if (eof-object? line)
	  (close-input-port port)
	  (begin
	     (print line)
	     (loop (read-line port)))))) &rarr; 20
```

> [!NOTE] In this last example, the call to `flush-output-port` is
mandatory in order to get the `dc` process to get its input
characters.

Process API
-----------

### close-process-ports ###
Close the three ports associated with a process. In general the ports should
not be closed before the process is terminated.

### process-pid ###
Returns an integer value which represents the Unix identification (PID) of
the `process`.
 
### process-input-port ###
Returns, the process input port, i.e., an output port open for reading.

### process-output-port ###
Returns, the process output port, i.e., a input port open for reading.

### process-error-port ###
Returns, the process error port, i.e., a input port open for reading.

### process-wait ###
This function stops the current process until `process` completion.
This function returns `#f` when `process` is already terminated. It
returns `#t` otherwise.

### process-exit-status ###
This function returns the exit status of `process` if it is has
finished its execution. It returns `#f` otherwise.
@end deffn

### process-send-signal ###
<!-- [:@C-wasm] -->
Sends the signal whose integer value is `s` to `process`. Value
of `s` is system dependent. The result of `process-send-signal`
is undefined.

### process-kill ###
This function brutally kills `process`. The result of `process-kill`
is undefined. 

### process-stop ###
<!-- [:@C] -->
Stop a process.

### process-continue ###
<!-- [:@C] -->
Returns a stopped process.

> [!NOTE] The procedures `process-stop` and `process-continue` are
> only available on systems that support job control.  The function
> `process-stop`.

### process-list ###
<!-- [:@C-jvm] -->

This function returns the list of processes which are currently running
(i.e. alive).

