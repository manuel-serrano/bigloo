/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/www/doc.js                    */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Jul 30 17:20:13 2015                          */
/*    Last change :  Fri May  8 14:24:28 2020 (serrano)                */
/*    Copyright   :  2015-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Tools to build the Hop.js documentation.                         */
/*=====================================================================*/
"use hopscript";

/*---------------------------------------------------------------------*/
/*    module imports                                                   */
/*---------------------------------------------------------------------*/
const path = require( "path" );
const fs = require( "fs" );
const markdown = require( hop.markdown );
const texinfo = require( hop.texinfo );
const fontifier = require( hop.fontifier );
const hopdoc = require( "hopdoc" )
const docxml = require( "./xml.js" );

/*---------------------------------------------------------------------*/
/*    global parameters                                                */
/*---------------------------------------------------------------------*/
const PWD = process.cwd();
const ROOT = process.cwd();
const DOC = path.join( ROOT, "doc.json" );

const doc = fs.existsSync( DOC ) ? require( DOC ) : undefined;

const chapters = doc ?
      doc.chapters.map( function( c, idx = undefined, arr = undefined ) {
	 c.entries = chapterEntries( c );
	 return c;
      } ) : [];

function P( file ) {
   return path.normalize( "./" + file );
}
   
const css = [ P( "hss/doc.css" ),
	      P( "hss/markdown.css" ),
	      P( "hss/texinfo.css" ),
	      P( "hss/fontifier.css" ),
	      P( "lib/bootstrap/css/bootstrap.min.css" ) ];
const jscript = [ P( "lib/jquery/js/jquery.min.js" ),
		  P( "lib/bootstrap/js/bootstrap.min.js" ) ];
const favicon = P( "favicon.png" );

const alias = {
   "user.md": "api",
   "config.md": "api",
   "hss.md": "api",
   "markdown.md": "api",
   "tree.md": "widget",
   "spage.md": "widget"
}

/*---------------------------------------------------------------------*/
/*    chapterEntries ...                                               */
/*---------------------------------------------------------------------*/
function chapterEntries( chapter ) {
   
   function chapterFile( file, i = undefined, arr = undefined ) {
      var base = path.basename( file );
      return {
	 path: file.replace( /[.]md$/, ".html" ),
	 href: base.replace( /[.]md$/, ".html" ),
	 title: base.replace( /[0-9]+[-]|[.]md$/g, "" )
      };
   }
   
   function chapterEntry( file, i = false, arr = false ) {
      if( typeof file != "string" ) {
	 return [ false ];
      } else {
	 var fp = path.join( ROOT, file );
	 if( fs.lstatSync( fp ).isDirectory() ) {
	    return fs.readdirSync( fp )
	       .filter( function( e, idx = undefined, arr = undefined ) {
		  return e.match( /[.]md$/ ) && (e != "index.md");
	       } )
	       .sort( function( left, right ) {
		  return left.naturalCompare( right );
	       } )
	       .map( chapterFile );
	 } else {
	    return [ chapterFile( file ) ];
	 }
      }
   }

   if( chapter.json ) {
      var c = require( path.join( PWD, chapter.json ) );
      return Array.prototype.concat.apply( [], c.files.map( chapterEntry ) );
   } else if( chapter.files ) {
      return Array.prototype.concat.apply( [], chapter.files.map( chapterEntry ) );
   } else {
      return [];
   }
}

/*---------------------------------------------------------------------*/
/*    childrenSize ...                                                 */
/*---------------------------------------------------------------------*/
function childrenSize( children ) {
   var res = 0;
   
   for( var i = 0; i < children.length; i++ ) {
      if( children[ i ].tagName == "ul" ) {
	 res += childrenSize( children[ i ].childNodes );
      } else if( children[ i ].tagName == "li" ) {
	 res++;
      }
   }
   return res;
}

/*---------------------------------------------------------------------*/
/*    makeToc ...                                                      */
/*---------------------------------------------------------------------*/
function makeToc( els, k, proc = false ) {
   
   function _makeToc( els, k, proc, indent ) {
      if( els.length == k  ) {
	 return [];
      } else {
	 var acc = [];
	 var tag = els[ k ].tagName;

	 for( var i = k; i < els.length; ) {
	    if( els[ i ].tagName == tag ) {
	       var el = els[ i++ ];
	       var n = proc ? proc( el ) : el.childNodes;
	       acc.push( <li>
		 <a href=${"#" + el.id} role="presentation">${n}</a>
               </li> );
	    } else if( els[ i ].tagName > tag ) {
	       var children = _makeToc( els, i, proc, indent + "  " );
	       acc.push( <ul>${children}</ul> );
	       i += childrenSize( children );
	    } else {
	       i++;
	       return acc;
	    }
	 }

	 return acc;
      }
   }
   
   return _makeToc( els, k, proc, "" );
}

/*---------------------------------------------------------------------*/
/*    compileXML ...                                                   */
/*---------------------------------------------------------------------*/
function compileXML( ast, title, clazz, target, tocfile = undefined ) {
   var footer = path.join( PWD, "footer.md" );
   var toc = hopdoc.toc( ast );
   var affix = "normal";
   
   toc.shift();
   
   var document = <html>
     <head css=${css}
	   title=${doc.title + "/" + title}
           jscript=${jscript}
	   favicon=${favicon}
           rts=${false}/>

     <body data-spy="scroll" data-target="#navbar" class=${`${title} section ${clazz}`}
           onscroll=~{
	      var top = (window.pageYOffset || document.scrollTop)-(document.clientTop||0);
	      if( top > 180 ) {
		 document.body.setAttribute( "scrolled", "yes" );
	      } else {
		 document.body.setAttribute( "scrolled", "no" );
	      }
	   } >
       ~{ $('body').scrollspy( { target: '#navbar' }) }
       <docxml.navbar title=${title} key=${clazz}>
         ${chapters}
       </docxml.navbar>
       
       <docxml.title title=${doc.title}
		     version=${doc.version}
		     date=${doc.date}
		     logo=${doc.logo}
		     root=${ROOT}>
          ${title}
       </docxml.title>
       <div class="container">
	 <div class="filler">.keep</div>
            <div class="col-md-9" role="main"> ${ast} </div>
         <div class="row">
           <div id="navbar" class="col-md-3" role="complementary">
             <nav class="sidebar noaffix"
		  data-spy=${affix}
	          data-offset-top="215" data-offset-bottom="100">
               <ul class="nav bs-docs-sidenav">
                  ${makeToc( toc, 0, function( el ) {
		     if( el.childNodes[ 0 ].data.charAt( 0 ) === "(" ) {
			let m = el.childNodes[ 0 ].data.match( /[(](?:class |generic )?([^ )]*)/ );
			return m[ 1 ];
		     } else {
			return el.childNodes[ 0 ].data.replace( /[(].*$/, "");
		     }
		  } )}
	       </ul>
	     </nav>
             <nav class="sidebar noaffix toc-extra">
	       ${tocfile && fs.existsSync( path.join( PWD, tocfile ) )
		    ? require( path.join( PWD, tocfile ) )
		    : "" }
	     </nav> 
	   </div>
	 </div>
	 ${fs.existsSync( footer )
	   ? hopdoc.load( footer ).XML 
	   : <docxml.footer root=${ROOT}/>}
       </div>
     </body>
   </html>;

   fs.writeFileSync( target, hop.compileXML( document ) );
}

/*---------------------------------------------------------------------*/
/*    compileNode ...                                                  */
/*---------------------------------------------------------------------*/
function compileNode( node, title, clazz, target, tocfile = undefined ) {
   var footer = path.join( PWD, "footer.md" );
   var affix = "normal";
   
   var document = <html>
     <head css=${css}
	   title=${doc.title + "/" + title}
           jscript=${jscript}
	   favicon=${favicon}
           rts=${false}/>

     <body data-spy="scroll" data-target="#navbar" class=${`${title} section ${clazz}`}
           onscroll=~{
	      var top = (window.pageYOffset || document.scrollTop)-(document.clientTop||0);
	      if( top > 180 ) {
		 document.body.setAttribute( "scrolled", "yes" );
	      } else {
		 document.body.setAttribute( "scrolled", "no" );
	      }
	   } >
       ~{ $('body').scrollspy( { target: '#navbar' }) }
       <docxml.navbar title=${title}>
         ${chapters}
       </docxml.navbar>
       
       <docxml.title title=${doc.title}
		     version=${doc.version}
		     date=${doc.date}
		     logo=${doc.logo}
		     root=${ROOT}>
          ${title}
       </docxml.title>
       <div class="container">
	 <div class="filler">.keep</div>
	 <div class="col-md-12" role="main"> ${node} </div>
	 ${fs.existsSync( footer )
	   ? hopdoc.load( footer ).XML 
	   : <docxml.footer root=${ROOT}/>}
       </div>
     </body>
   </html>;

   fs.writeFileSync( target, hop.compileXML( document ) );
}

/*---------------------------------------------------------------------*/
/*    compileSection ...                                               */
/*---------------------------------------------------------------------*/
function compileSection( page ) {
   var footer = path.join( PWD, "footer.md" );
   var ast = hopdoc.load( path.join( PWD, page ) )
   var toc = hopdoc.toc( ast );
   var title = path.basename( page ).replace( /[0-9]+[-]|[.][^.]*$/g, "" );
   var key = path.basename( path.dirname( page ) ).toLowerCase();
   var affix = "normal";
   
   if( key == "doc" ) {
      key = alias[ path.basename( page ) ];
   } else if( key == "." ) {
      key = title;
   }
   
   var document = <html>
     <head css=${css}
	   title=${doc.title + "/" + title}
           jscript=${jscript}
	   favicon=${favicon}
           rts=${false}/>

     <body data-spy="scroll" data-target="#navbar" class=${`${title} section`}
           onscroll=~{
	      var top = (window.pageYOffset || document.scrollTop)-(document.clientTop||0);
	      if( top > 180 ) {
		 document.body.setAttribute( "scrolled", "yes" );
	      } else {
		 document.body.setAttribute( "scrolled", "no" );
	      }
	   } >
       ~{ $('body').scrollspy( { target: '#navbar' }) }
       <docxml.navbar title=${title} key=${key}>
         ${chapters}
       </docxml.navbar>
       
       <docxml.title title=${doc.title}
		     version=${doc.version}
		     date=${doc.date}
		     logo=${doc.logo}
		     root=${ROOT}>
          ${title}
       </docxml.title>
       <div class="container">
	 <div class="filler">.keep</div>
	 ${(toc.length > 0 )
	      ? <div class="col-md-9" role="main"> ${ast.XML} </div>
	      : <div class="col-md-12" role="main"> ${ast.XML} </div>}
         <div class="row">
           ${(toc.length > 0) ?
           <div id="navbar" class="col-md-3" role="complementary">
             <nav class="sidebar noaffix"
		  data-spy=${affix}
	          data-offset-top="215" data-offset-bottom="100">
               <ul class="nav bs-docs-sidenav">
                  ${makeToc( toc, 0, function( el ) {
		     if( el.childNodes[ 0 ].data.charAt( 0 ) === "(" ) {
			let m = el.childNodes[ 0 ].data.match( /[(](?:class |generic )?([^ )]*)/ );
			return m[ 1 ];
		     } else {
			return el.childNodes[ 0 ].data.replace( /[(].*$/, "");
		     }
		  } )}
	       </ul>
	     </nav>
             <nav class="sidebar noaffix toc-extra">
	       ${fs.existsSync( path.join( PWD, title + "-toc.js" ) )
		    ? require( path.join( PWD, title + "-toc.js" ) )
	            : fs.existsSync( path.join( PWD, title + "-toc.md" ) )
		        ? hopdoc.load( path.join( PWD, title + "-toc.md" ).xml )
		        : "" }
	     </nav> 
	   </div>
           : undefined}
	 </div>
	 ${fs.existsSync( footer )
	   ? hopdoc.load( footer ).XML 
	   : <docxml.footer root=${ROOT}/>}
       </div>
     </body>
   </html>;

   fs.writeSync( process.stdout.fd, hop.compileXML( document ) );
   fs.writeSync( process.stdout.fd, "\n" );
}

/*---------------------------------------------------------------------*/
/*    compileChapter ...                                               */
/*---------------------------------------------------------------------*/
function compileChapter( json ) {
   var footer = path.join( PWD, "footer.md" );
   var chapter = require( path.join( PWD, json ) );
   var toc = chapterEntries( chapter ).filter( x => x );

   var document = <html>
     <head css=${css}
	   title=${doc.title + "/" + chapter.title}
           jscript=${jscript}
	   favicon=${favicon}
           rts=${false}/>

     <body data-spy="scroll" data-target="#navbar" class="chapter">
       <docxml.navbar title=${chapter.title} key=${chapter.key}>
         ${chapters}
       </docxml.navbar>
       <docxml.title title=${doc.title}
		     version=${doc.version}
		     date=${doc.date}
		     logo=${doc.logo}
		     root=${ROOT}>
          ${chapter.title}
       </docxml.title>

       <div class="container">
	 <div class="filler">.keep</div>
         ${chapter.description ? <div class="chapter-header">
	   ${ fs.existsSync( ROOT + "/" + chapter.description ) ?
	      hopdoc.load( ROOT + "/" + chapter.description ).XML
	      : hopdoc.eval( chapter.description ).XML }
	   </div> : ""}
	 
         <h1 class="toc" id="toc">Table of Contents</h1>
         <ul class="toc">
           ${toc.map( function( el, idx = undefined, arr = undefined ) {
              return <li>
	        <a href=${el.href}>${el.title}</a>
                <span class="toc-description">
                  ${hopdoc.eval( el.description )}
                </span>
	      </li>
	   } )}
         </ul>
	 ${fs.existsSync( footer ) 
	   ? hopdoc.load( footer ).XML 
	   : <docxml.footer root=${ROOT}/>}
       </div>
     </body>
   </html>;

   console.log( hop.compileXML( document ) );
}

/*---------------------------------------------------------------------*/
/*    compileMain ...                                                  */
/*---------------------------------------------------------------------*/
function compileMain( content ) {

   var document = <html>
     <head css=${css}
	   title=${doc.title}
           jscript=${jscript}
	   favicon=${favicon}
           rts=${false}/>

     <body class="home" data-spy="scroll" data-target="#navbar"
           onscroll=~{
	      var top = (window.pageYOffset || document.scrollTop)-(document.clientTop||0);
	      if( top > 180 ) {
		 document.body.setAttribute( "scrolled", "yes" );
	      } else {
		 document.body.setAttribute( "scrolled", "no" );
	      }
	   } >
       ~{ $('body').scrollspy( { target: '#navbar' }) }
       <docxml.navbar title=${doc.title} key="home">
         ${chapters}
       </docxml.navbar>
       <docxml.title title=${doc.title}
		     version=${doc.version}
		     date=${doc.date}
		     logo=${doc.logo}
		     root=${ROOT}/>

       <div class="container home-body">
	 <div class="filler">.keep</div>
         ${hopdoc.load( content ).XML}
	 <docxml.footer root=${ROOT}/>
       </div>
     </body>
   </html>;

   console.log( hop.compileXML( document ) );
}

/*---------------------------------------------------------------------*/
/*    compileLibrary ...                                               */
/*---------------------------------------------------------------------*/
function compileLibrary( content ) {
   var footer = path.join( PWD, "footer.md" );
   
   var document = <html>
     <head css=${css}
	   title=${doc.title}
           jscript=${jscript}
	   favicon=${favicon}
           rts=${false}/>

     <body class="library" data-spy="scroll" data-target="#navbar">
       <docxml.navbar title=${doc.title} key="home">
         ${chapters}
       </docxml.navbar>
       <docxml.title title=${doc.title}
		     version=${doc.version}
		     date=${doc.date}
		     logo=${doc.logo}
		     root=${ROOT}/>

       <div class="container home-body">
	 <div class="filler">.keep</div>
         ${hopdoc.load( content ).XML}
	 ${fs.existsSync( footer )
	   ? hopdoc.load( footer ).XML
	   : ""}
       </div>
     </body>
   </html>;

   console.log( hop.compileXML( document ) );
}

/*---------------------------------------------------------------------*/
/*    compileIdx ...                                                   */
/*    -------------------------------------------------------------    */
/*    compile the HTML index page.                                     */
/*---------------------------------------------------------------------*/
function compileIdx( json ) {
   var idx = require( path.join( PWD, json ) );
   var chapter = { title: "Index", key: "manual" };
   var footer = path.join( PWD, "footer.md" );

   var document = <html>
     <head css=${css}
	   title=${doc.title + "/" + chapter.title}
           jscript=${jscript}
	   favicon=${favicon}
           rts=${false}/>

     <body data-spy="scroll" data-target="#navbar">
       <docxml.navbar title=${chapter.title}
                      key=${chapter.key}>
         ${chapters}
       </docxml.navbar>
       <docxml.title title=${doc.title}
		     version=${doc.version}
		     date=${doc.date}
		     logo=${doc.logo}
		     root=${ROOT}>
          ${chapter.title}
       </docxml.title>

       <div class="container">
	 <div class="filler">.keep</div>
	 <docxml.idx>${idx}</docxml.idx>
	 ${fs.existsSync( footer )
	   ? hopdoc.load( footer ).XML
	   : ""}
       </div>
     </body>
   </html>;

   console.log( hop.compileXML( document ) );
}

/*---------------------------------------------------------------------*/
/*    exports                                                          */
/*---------------------------------------------------------------------*/
exports.compileXML = compileXML;
exports.compileNode = compileNode;

/*---------------------------------------------------------------------*/
/*    main ...                                                         */
/*---------------------------------------------------------------------*/
function main() {
   switch( process.argv[ 2 ] ) {
      case "html-to-idx":
	 hopdoc.htmlToIdx( process.argv[ 3 ],
			   process.argv.slice( 4 ).map( function( f, _, __ ) {
			      return path.join( PWD, f );
			   } ),
 			   [ "h1", "h3", "h4" ] );
	 break;

      case "compile-idx":
	 compileIdx( process.argv[ 3 ] );
	 break;

      case "compile-main":
	 compileMain( process.argv[ 3 ] );
	 break;

      case "compile-library":
	 compileLibrary( process.argv[ 3 ] );
	 break;

      case "compile-section":
	 compileSection( process.argv[ 3 ] );
	 break;

      case "compile-chapter":
	 compileChapter( process.argv[ 3 ] );
	 break;
	 
      default:
	 throw( "Unknown command: " + process.argv[ 2 ] );
   }
}

main();
