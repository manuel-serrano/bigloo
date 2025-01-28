const doc = require( "@hop/hopdoc" );
const config = require( hop.config );
const texinfo = require( "@hop/texinfo" );
const fontifier = require( "@hop/fontifier" );

const manual = texinfo.load( "../manuals/bigloo.texi", hop.locale, fontifier );
const chapters = manual.chapters();

chapters.shift();

let i = 1;

module.exports = 
   <div>
     <div class="title"> Chapters </div>
     <ol class="nav bs-docs-sidenav chapter"> 
       ${chapters.map( el => <li> <a href=${"manual-chapter" + (i++) + ".html"}>${el.innerHTML} </a> </li> )}
     </ol>
 </div>;
   
