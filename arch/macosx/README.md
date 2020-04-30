Bigloo MacOS X Homebrew package - 18 apr 2020
=============================================

This document explains how to build a Bigloo on MacOS X using homebrew.

Here is the configuration used for the experiment

```shell
uname -a
Darwin macOSFull 17.3.0 Darwin Kernel Version 17.3.0: ... RELEASE_X86_64 x86_64
```

```shell
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
brew install automake
```

System configuration

```shell
sudo sysctl -w kern.maxproc=2500
sudo sysctl -w kern.maxprocperuid=2500
```

SSL trouble shooting
--------------------

```shell
brew link openssl --force
export PKG_CONFIG_PATH="/usr/local/opt/openssl@1.1/lib/pkgconfig"
```
