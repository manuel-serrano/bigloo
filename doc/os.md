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
,(example-path "../test/src/os.bgl")

OS
==

### getenv ###
Reads the environment variable `name`. Returns its result as a string if
the variable exists. Return `#f` otherwise. If `name` is omitted, 
`getenv` returns the list of all environment variable.

### putenv ###
<!-- [:@C] -->
Assigns the environment variable.

### time ###
Evaluates the `thunk` and returns four values: the result of calling
`thunk`, the actual execution time, the system time, and the user time
in millisecond.

File Names
----------

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
