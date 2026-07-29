<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/c.md                     -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Memory profiling                                              -->
<!--==================================================================-->

Memory Profiling
================

Bigloo provides a memory profiling for the C backend. It requires 
the `libbacktrace` library, which must have then been configured when
installation Bigloo. The memory profiler can be used for single- and 
multi-threaded applications.

> [!IMPORTANT] The memory profiler only runs on Linux.

The memory profiler works on C generated source files and for a better
understanding of the reports, a minimal notion of C are required.

Bglmemrun
---------

To profile memory allocations and GC activities, programs have to be
compiled with the one of:

  * `-pmem`: enables memory profiling;
  * `-pmem2`: enables memory profiling and disables inlining of user functions;
  * `-pmem3`: enables memory profiling and disables all inlining. 

> [!IMPORTANT] Memory profiling is based on the Linux `LD\_PRELOAD` facility.
> As such, it requires Bigloo programs to be linked against dynamic libraries.

Disabing function inlining improves the precision of the memory reports.

Example:

```shell
$ bigloo -Ox -unsafe maze.bgl -pmem -o maze
```

To produce a memory report, programs have to be run through the `bglmemrun`
tool. Example

```shell
$ bglmemrun maze
```

By default this produces a report displayed on the terminal such as:


```shell
Loading library /home/serrano/prgm/project/bigloo/5.0.x/lib/bigloo/5.0.0/libbigloogc-5.0.0.so...
Loading library /home/serrano/prgm/project/bigloo/5.0.x/lib/bigloo/5.0.0/libbigloo_u-5.0.0.so...
gc   1: alloc size=2.53MB, heap size=4.06MB, live size=3.95MB
gc   2: alloc size=1.90MB, heap size=7.29MB, live size=7.12MB
...
```

The two first lines enable to check that `bglmemrun` loads the correct
dynamic libraries.

The following lines are gc reports. Each line correct to a triggered 
collection. It shows the number of allocated objects since the previous
gc, the heap size, and the memory size that survived the collection.

Finally, then execution completes, `bglmemrun` produces its report about
memory allocation. For instance:

```shell
allocation size: 1599.57MB
gc count: 103

/home/serrano/trashcan/maze.c:
      447:   681.92MB 42.63% [17876100] (vector)
     2331:   411.99MB 25.76% [ 6000000] (procedure)
      484:   320.43MB 20.03% [ 6000000] (vector)
      596:    45.78MB  2.86% [     100] (vector)
      932:     1.07MB  0.07% [   69802] (pair)
     2612:     0.57MB  0.04% [   37536] (pair)
     1011:     0.37MB  0.02% [   12000] (vector)
      712:     0.15MB  0.01% [    9763] (pair)
lib/init.c:
      590:     0.05MB  0.00% [      64] (vector, keyword, input-port, output-port, string, mutex)
objs/obj_u/Ieee/port.c:
     4904:     0.81MB  0.05% [     300] (string, mutex, output-port)
objs/obj_u/Ieee/vector.c:
     1203:   136.39MB  8.53% [     111] (vector)

---------------------------------------------------
allocation count: 30007488
   vector              :    1184.94MB  74.08% [  23888883]
   procedure           :     411.99MB  25.76% [   6000100]
   pair                :       1.79MB   0.11% [    117205]
```

First the report shows how many bytes were allocated during the whole execution
and the number of collections that were triggered.

Then, C file by C file, it reports about the allocation. The first column
is the C line number where the allocations took place. The second column
is the total memory allocations that took place at that line. The third
column, show the percentage of the whole memory allocation this represents.
The fourth column shows the number of times, the line has been executed.
The last column shows the type of the values that were allocated there.

For instance, this report tells that at line 447 of the C file maze.c,
681MB where allocated in 17876100 vectors.

> [!NOTE] The current C backend is enable to map the generated C lines
> to the original Bigloo source lines so interpreting the memory allocation
> location requires reading the C files. These contain corresponding 
> Bigloo source line that should be enough to recover the source location
> of the allocations. The C identifiers can be demangled with the
> bgldemangle tool to recover original Bigloo identifiers.


The report can also be produced in textual formats easier to be read by
other programs, e.g., sexp or json.


Run 

```shell
$ bglmemrun --help
```

To see all supported options.

Debugging bglmemrun
-------------------

If for some reasons a program does not execute when running via `bglmemrun`,
debugging using `gdb` is recommended. For that, instead of running the
program with `bglmemrun`, run it with `bglmemrun-gdb`.
