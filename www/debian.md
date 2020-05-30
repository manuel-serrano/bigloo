
Debian
------

Pre-compiled Bigloo version are available for Linux Debian
and some of its variants. This version can be installed using
the standard package managers of these distributions after
the appropriate `hop.list` file has been created in the
`/etc/apt/sources.list` directory.

The Debian packages are all installed in the `/opt/hop` directory.

### Debian

${<span class="label label-success">Debian Buster/x86_64</span>} users can 
installed pre-compiled versions with:

```
echo "deb [trusted=yes] http://hop.inria.fr/linux/Debian buster hop" > /etc/apt/sources.list.d/hop.list
```

### Raspberry

${<span class="label label-primary">Raspberry Buster/Arm</span>} users can install pre-compiled packages for with:

```
echo "deb [trusted=yes] http://hop.inria.fr/linux/Raspbian buster hop" > /etc/apt/sources.list.d/hop.list
```

### Ubuntu

${<span class="label label-info">Ubuntu LTS 20.04/x86_64</span>} users and
${<span class="label label-info">Windows WSL</span>}
can installed pre-compiled versions with:

```
echo "deb [trusted=yes] http://hop.inria.fr/linux/Ubuntu focal hop" > /etc/apt/sources.list.d/hop.list
```
