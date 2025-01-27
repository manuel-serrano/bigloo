${ var hopdoc = require( "@hop/hopdoc" ) }
${ var config = require( hop.config ) }
${ var texinfo = require( "@hop/texinfo" ) }
${ var xml = require( "./xml.js" ) }
${ var fontifier = require( "@hop/fontifier" ) }
${ var www = require( "./www.js" ) }
${ var fs = require( "fs" ) }

${ var manual = texinfo.load( "../manuals/bigloo.texi", hop.locale, fontifier ) }
${ var chapters = manual.chapters(); undefined; }

${ var table = [] }

${ function collectId( www, path ) {
   
     function getTags( el ) {
        if( el instanceof Array ) {
           return Array.prototype.concat.apply( [], el.map( getTags ) );
        }
      
        if( typeof( el ) == "pair" ) {
	       return Array.prototype.concat.apply( [], el.map( getTags ).toArray() );
        }

        if( typeof( el ) == "xml-element" || typeof( el ) == "xml-html" ) {
	       el.getElementsByClassName( "anchor" ).forEach( el => {
              // console.error( "collecting [#" + el.id + "] ", path );
		      table[ "#" + el.id ] = path;
		   } );
        }
     }

     return getTags( "XML" in www ? www.XML : www );
   } 
}

${ function updateId( www, path ) {
   
     function getTags( el ) {
        if( el instanceof Array ) {
           return Array.prototype.concat.apply( [], el.map( getTags ) );
        }
      
        if( typeof( el ) == "pair" ) {
	       return Array.prototype.concat.apply( [], el.map( getTags ).toArray() );
        }

        if( typeof( el ) == "xml-element" || typeof( el ) == "xml-html" ) {
	       el.getElementsByTagName( "a" ).forEach( el => {
		     // console.error( "patching [" + el.href + "] ", path, " -> ", table[ el.href ]  );
		     if( el.href in table ) el.href = table[ el.href ] + el.href;
		   } );
        }
     }

     return getTags( "XML" in www ? www.XML : www );
   } 
}

${ for( let i = 1; i < chapters.length; i++ ) {
    // collect all the identifiers
    const chap = manual.getChapterByIndex( i );
	
	const path = "manual-chapter" + i + ".html";
	collectId( chap, path );
	}
  for( let i = 1; i < chapters.length; i++ ) {
    // update the references
    const chap = manual.getChapterByIndex( i );
	
	const path = "manual-chapter" + i + ".html";
	updateId( chap, path );
	}
 }
	
${ for( let i = 1; i < chapters.length; i++ ) {
    const chap = manual.getChapterByIndex( i );
	const path = "manual-chapter" + i + ".html";
	
	if( chapters[ i ].innerHTML === "Global Index" ) {
	   fs.writeFileSync( path, "<html><script>window.location='./idx.html';</script></html>" );
    } else {
   	   www.compileXML( chap, chapters[ i ].innerHTML, "manual", path, "manual-toc.js" ); 
	}
}}

${ manual.getChapterByTitle( chapters[ 1 ].innerHTML ) }
