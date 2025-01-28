${ var hopdoc = require( "@hop/hopdoc" ) }
${ var config = require( hop.config ) }
${ var xml = require( "./xml.js" ) }
${ var www = require( "./www.js" ) }
${ var cfg = require( "./www.json" ) }

Bigloo Sources
--------------

${<div class="row">
  <div class="col-xs-4">
    <xml.downloadButton
       class="success"
       title="Latest"
       icon="glyphicon-download"
	   label=${"bigloo-latest.tgz (" + cfg.version + ")"}
       href="download/bigloo-latest.tar.gz"/>
  </div>
  <div class="col-xs-4">
    <xml.downloadButton
       class="warning"
       title="Unstable"
       icon="glyphicon-download"
	   label="bigloo-unstable.tgz"
       href="download/bigloo-unstable.tar.gz"/>
  </div>
  <div class="col-xs-4">
    <xml.downloadButton
       class="danger"
       title="Github"
       icon="glyphicon-cloud-download"
	   label="github"
       href=${cfg.github}/>
  </div>
</div>}

Older Bigloo source files are all available bundled in
tarball files at:
[ftp://ftp-sop.inria.fr/indes/fp/Bigloo](ftp://ftp-sop.inria.fr/indes/fp/Bigloo)


${ hopdoc.include( "../INSTALL.md" ) }

Debian/Ubuntu
-------------

On Debian and Ubuntu, Bigloo can be installed similarly to the github
[CI script](https://github.com/manuel-serrano/bigloo/blob/master/.github/workflows/bigloo.yml).


