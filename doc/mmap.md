<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/object.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Objects                                                       -->
<!--==================================================================-->

,(include "head.html")
,(implementation-path "../runtime/Llib/mmap.scm")
,(example-path "../test/src/mmap.bgl")

Memory Mapped Areas
===================

The `mmap` function asks to map a file into memory. This memory area
can be randomly accessed as a string. In general using `mmap` improves
performance in comparison with equivalent code using regular ports.

Predicate
---------

### mmap? ###
Returns `#t` if and only if @var{obj} has been produced by
`open-mmap`. Otherwise, it returns `#f`.

Opening, Closing, and Properties
--------------------------------

### open-mmap ###
Maps a file `path` into memory. The optional argument @var{mode} specifies
how the file is open. The argument can be:

  * `read: #t` The memory can be read
  * `read: #f` The memory cannot be read
  * `write: #t` The memory can be written
  * `write : #f` The memory is read-only.

<span></span>

### close-mmap ###
Closes the memory mapped.

### mmap-length ###
Returns the length, an exact integer, of the memory mapped.

### mmap-name ###
Returns the file name of the memory map `mm`.

### call-with-input-mmap ###
Invokes `proc` with a mmap opened on `file`. Returns the result
of the call and closes the mmap. Triggers an error is `file` cannot
be opened.

### call-with-output-mmap ###
Invokes `proc` with a mmap opened on `file`. Returns the result
of the call and closes the mmap. Triggers an error is `file` cannot
be opened.

Conversions
-----------

### string->mmap ###
Wraps a Bigloo string into a mmap object.

### mmap->string ###
Unmap a Bigloo mmap into a native string, e.g., a C string.

### mmap->bstring ###
Unmap a Bigloo mmap into a Bigloo string.


Reading and Writing
-------------------

### mmap-ref ###
Reads the character in `mm` at `offset`.

### mmap-set! ###
Writes the character `char` in `mm` at `offset`.

### mmap-substring ###
Returns a newly allocated string made of the characters read from `mm`
starting at position `start` and ending at position `end - 1`.
If the values `start` and `end` are not ranged in
&#91;0...`(mmap-length mm)`&#93;, an error is signaled. 

### mmap-substring-set! ###
Writes the string `str` to `mm` at position `start`.
If the values `start` and `start + (string-length str)` are 
not ranged in &#91;0...`(mmap-length mm)`&#93;, an error is signaled
