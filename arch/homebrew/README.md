Bigloo Homebrew Package - 9 Nov 2020
====================================

This document explains how to use the `makebrew.sh' script to
build the Bigloo Homebrew packages.

This script creates the Bigloo formula in (use `-O` to change):

  `$HOME/prgm/utils/hopbrew`
  
The general Bigloo configure script generates a file called `makebrew.sh`
from `makebrew.sh.in`. The generate the brew fomula use:

```script
./makebrew.sh
```

It will generate two Homebrew formulas:

  * bigloo-latest.rb
  * bigloo-unstabe.rb
  
These have to be pushed on the official Bigloo homebrew git directory
(`git@gitlab.inria.fr:mserrano/hopbrew.git`).
