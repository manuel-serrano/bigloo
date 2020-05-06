${ var doc = require( "hopdoc" ) }
${ var config = require( hop.config ) }
${ var texinfo = require( hop.texinfo ) }
${ var xml = require( "./xml.js" ) }
${ var cfg = require( "./doc.json" ) }
${ var fontifier = require( hop.fontifier ) }


Citations
---------

To cite the Bigloo software, please use the following Biblatex entry.

${<pre class="bibtex">
@software{ bigloo,
   title = {Bigloo, a Practical Scheme Compiler},
   author = {Serrano, Manuel},
   year = {1992},
   institution = {Inria},
   url = {${cfg.homepage}}
}
</pre>}

For refering to the current release, please use :

${<pre class="bibtex">
@softwareversion{ bigloo-${cfg.version},
  version = {${cfg.version}},
  year = {${cfg.date.split( " " )[ 1 ]}},
  month = {${cfg.date.split( " " )[ 0 ]}},
  file = {${cfg.urlbase}/biglo-${cfg.version}},
  crossref = {bigloo}
}
</pre>}
