${ var doc = require( "hopdoc" ) }
${ var config = require( hop.config ) }
${ var xml = require( "./xml.js" ) }
${ var cfg = require( "./doc.json" ) }


### Homebrew

For ${<span class="label label-danger">OS X</span>} users, as well as Linux users,
Bigloo can be installed via the `homebrew` package system. For that, issue
the following shell commands:

```shell
brew tap homebrew/hop https://gitlab.inria.fr/mserrano/hopbrew.git
brew install homebrew/hop/bigloo-latest
```
