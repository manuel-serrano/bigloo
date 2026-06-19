<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/error.md                 -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Errors                                                        -->
<!--==================================================================-->

,(implementation-path "../runtime/Llib/error.scm")
,(implementation-path "../runtime/Llib/object.scm")
,(example-path "../test/src/error.bgl")

Errors and Warnings
===================
Bigloo permits to signal an error via the error functions. Errors are
implemented by the means of exceptions (see `try` and `raise` forms).
Assertions allow the checking of predicates at certain points in programs.  


Classes
-------

### &condition ###

### &exception ###

### &error ###

### &warning ###


Library Functions
-----------------

### error ###
Raises an `&error` built from its arguments.

### error/location ###
Raises an `&error` built from its arguments. The `fname` designed the 
source file where the error occurred. The `loc` argument is the character
number in the source of the error. These values are used by the default
error handler to display the source location along with the reason of the
error when it is raised.

### warning ###
Emits a warning message.

### warning/location ###
Emits a warning message located at `fname` and `loc`.



