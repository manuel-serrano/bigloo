${ var hopdoc = require( "hopdoc" ) }
${ var config = require( hop.config ) }
${ var texinfo = require( hop.texinfo ) }
${ var xml = require( "./xml.js" ) }
${ var fontifier = require( hop.fontifier ) }
${ var doc = require( "./doc.js" ) }

${ var manual = texinfo.load( "../manuals/bigloo.texi", hop.locale, fontifier ) }
${ var chapters = manual.chapters() }

${ for( let i = 1; i < chapters.length; i++ ) {
    const chap = manual.getChapterByIndex( i );
	doc.compileXML( chap, chapters[ i ].innerHTML, "manual", "manual-chapter" + i + ".html", "manual-toc.js" ); }
	}

${ manual.getChapterByTitle( chapters[ 1 ].innerHTML ) }
