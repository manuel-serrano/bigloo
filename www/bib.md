${ var doc = require( "@hop/hopdoc" ) }
${ var config = require( hop.config ) }
${ var texinfo = require( "@hop/texinfo" ) }
${ var xml = require( "./xml.js" ) }
${ var cfg = require( "./www.json" ) }
${ var fontifier = require( "@hop/fontifier" ) }
${ var bibtex = require( "./_bibtex.hop" ) }


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

For referring to the current release, please use:

${<pre class="bibtex">
@softwareversion{ bigloo-${cfg.version},
  version = {${cfg.version}},
  year = {${cfg.date.split( " " )[ 1 ]}},
  month = {${cfg.date.split( " " )[ 0 ]}},
  file = {${cfg.urlbase}/biglo-${cfg.version}},
  crossref = {bigloo}
}
</pre>}


References
----------

${ function suffix( path ) {
    if( path.match( /[.]ps[.]gz$/ ) ) {
       return "ps.gz";
    } else if( path.lastIndexOf( "." ) > 0 ) {
       return path.substring( path.lastIndexOf( "." ) + 1 );
    } else {
       return path;
    }
  }
}

${ bibtex.load( "./bigloo.bib" )
  .sort( (x, y) => x.year < y.year ? true : x.year > y.year ? false : x.month < y.month )
  .map( e => 
<div class="bibentry">
  <span class="author">${e.author}</span>
  <span class="title">${e.title}</span>
  <span class="booktitle">${e.booktitle || e.journal}</span>,
  <span class="address">${e.address}</span>,
  <span class="month">${e.month}</span>,
  <span class="year">${e.year}</span>
  <div class="download">
     <a href=${e.download}>${suffix( e.download )}</a>
  </div>
  <div class="abstract">
    ${e.abstract}
  </div>
</div> ) }
