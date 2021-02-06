${ var hopdoc = require( "hopdoc" ) }
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
       href=${cfg.bglurlbase + "/bigloo-latest.tar.gz"}/>
  </div>
  <div class="col-xs-4">
    <xml.downloadButton
       class="warning"
       title="Unstable"
       icon="glyphicon-download"
	   label="bigloo-unstable.tgz"
       href=${cfg.bglurlbase + "/bigloo-unstable.tar.gz"}/>
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


${ hopdoc.include( "debian.md" ) }

${ hopdoc.include( "homebrew.md" ) }

${ hopdoc.include( "../INSTALL.md" ) }


