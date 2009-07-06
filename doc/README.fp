On some architecture (e.g., x86) it is possible to avoid using frame
pointer for instance, by the means of the `-fomit-frame-pointer' Gcc
compilation flags. Not using frame pointer yeild to a significant
speed improvement (about 15%). If you are concern with very fast
applications, then, I encourage using the `-fomit-frame-pointer'
option. However, it is important not to mix code compiled with and
without `-fomit-frame-pointer' option. In particular, if you decide to
install a Bigloo version compiled with `-fomit-frame-pointer' be aware
that all your Scheme programs will have to be compiled with that
option. Even debugged programs. In addition, the installation of
profile libraries will fail because they definitely need frame
pointers.

To configure Bigloo for -fomit-frame-pointer use a configure invokation 
such as:

 ./configure --cflags=-fomit-frame-pointer <the-other-options>
