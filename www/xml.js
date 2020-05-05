/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/www/xml.js                    */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Aug  1 10:22:56 2015                          */
/*    Last change :  Tue May  5 07:58:03 2020 (serrano)                */
/*    Copyright   :  2015-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hop.js XML extensions                                            */
/*=====================================================================*/
"use hopscript"

/*---------------------------------------------------------------------*/
/*    imports                                                          */
/*---------------------------------------------------------------------*/
const path = require( "path" );
const config = require( hop.config );

const ipath = path.join( config.iconsDir, "hop" );

/*---------------------------------------------------------------------*/
/*    title ...                                                        */
/*---------------------------------------------------------------------*/
function title( attrs, ... subtitle ) {
   return <div class="jumbotron">
     <div class="container">
       <div class="row">
	 <div class="col-md-2">
	   <div class="svg-container">
	     <svg:img
                src=${attrs.logo ? attrs.logo : path.join( ipath, "hop.svg" )}
                height="16ex" width="10em"/>
           </div>
	 </div>
	 <div class="col-md-7">
	   <h1>
              ${attrs.title ? attrs.title : "Hop.js"}
              ${subtitle.length > 0 ? <small>/${subtitle}</small> : ""}
	   </h1>
	 </div>
	 <div class="col-md-3">
	   <p class="version">
             <span class="label label-default lbl-lg">
               version ${attrs.version ? attrs.version : config.version}
	     </span>
	     <span class="label date lbl-lg"> 
	       ${attrs.date ? attrs.date : config.date}
 	     </span>
	   </p>
	 </div>
       </div>
     </div>
   </div>;
}

/*---------------------------------------------------------------------*/
/*    xmlNodes ...                                                     */
/*---------------------------------------------------------------------*/
function xmlNodes( args ) {
   var nodes = Array.prototype.slice.call( args, 1 );
   
   function pred( el ) {
      return !(typeof( el ) == "string" );
   }
   
   return Array.prototype.filter.call( nodes, pred );
}

/*---------------------------------------------------------------------*/
/*    navbut ...                                                       */
/*---------------------------------------------------------------------*/
function navbut( attrs, _ ) {
   const body = xmlNodes( arguments );

   return <div class="btn-group navbut">
       <button type="button"
               class=${"btn " + attrs.class}>
         ${body[ 0 ]}
       </button>
       <button type="button"
               class=${"btn dropdown-toggle " + attrs.class}
               data-toggle="dropdown"
               aria-haspopup="true"
               aria-expanded="false">
         <span class="caret"></span>
         <span class="sr-only">Toggle Dropdown</span>
        </button>
        ${body[ 1 ]}
     </div>
}

/*---------------------------------------------------------------------*/
/*    navbar ...                                                       */
/*---------------------------------------------------------------------*/
function navbar( attrs, chapters ) {
   if( !(chapters instanceof Array) ) { chapters = arguments[ 2 ]; }

   return <nav class="navbar navbar-inverse navbar-fixed-top">
     <div class="container">
       <!--
       <a class="navbar-brand" href="#">
	 <svg:img
             src=${attrs.logo ? attrs.logo : path.join( ipath, "hop.svg" )}
             height="3ex" width="3em"/>
       </a>
       -->
       <ul class="nav navbar-nav">
         ${chapters.map( function( p, idx = undefined, arr = undefined ) {
	    if( p.entries.length == 0 ) {
               var clazz = p.name.toLowerCase()==attrs.key
		   ? "active" : "";
               return <li class=${clazz}>
	         <a href=${p.href}>
	           <span class=${"glyphicon " + p.icon}></span>
		   ${p.name}
	         </a>
	       </li>;
	    } else {
               var clazz = p.name.toLowerCase()==attrs.key
		   ? "dropdown active" : "dropdown";
               return <li class=${clazz}>
	         <a href=${p.href}
	            class="dropdown-toggle" data-toggle="dropdown" role="button"
	            aria-haspopup="true" aria-expanded="false">
	          <span class=${"glyphicon " + p.icon}></span> ${p.name} <span class="caret"></span>
	         </a>
                 <ul class="dropdown-menu">
                   <li><a href=${p.href}>${p.name}</a></li>
                   <li role="separator" class="divider"></li>
	           ${p.entries.map( function( e, idx = undefined, arr = undefined ) {
		      if( !(e instanceof Object) ) {
			 return <li role="separator" class="divider"></li>;
		      } else {
			 return <li><a href=${e.href}>${e.title}</a></li>
		      }
		   } )}
                 </ul>
	       </li>
	    } } )}
	  </ul>
     </div>
   </nav>
}

/*---------------------------------------------------------------------*/
/*    copyrightYears ...                                               */
/*---------------------------------------------------------------------*/
function copyrightYears( iyear ) {
   var y = new Date().getFullYear();

   if( y == iyear ) {
      return iyear + "";
   } else if( y == iyear + 1 ) {
      return iyear + "-" + y;
   } else {
      return iyear + "-" + y;
   }
}
   
/*---------------------------------------------------------------------*/
/*    docfooter ...                                                    */
/*---------------------------------------------------------------------*/
function docfooter( attrs ) {
   return <footer>
     <div class="container">
       <div class="copyright col-md-2 copyright-left">
       &copy; ${copyrightYears( 1992 )}
	 <a href="http://www.inria.fr">Inria</a>
       </div>
       <div class="copyright col-md-8 copyright-middle">
         <a class="iddn" href="http://app.legalis.net/">
           IDDN.FR.001.310007.000.S.P.2018.000.31235
	 </a>
         - 
         <a class="iddn" href="http://app.legalis.net/">
	   IDDN.FR.001.310008.000.S.P.2018.000.31235
	 </a>
       </div>
       <div class="copyright copyright-right col-md-2">
	 <button type="button" class="inria btn btn-danger">
           <a href="http://www.inria.fr">
           <svg:img class="inria"
		    src=${path.join( ipath, "inria.svg" )}
		    height="1.6ex" width="4em"/>
           </a>
	 </button>
       </div>
     </div>
   </footer>
}

/*---------------------------------------------------------------------*/
/*    downloadButton ...                                               */
/*---------------------------------------------------------------------*/
function downloadButton( attrs ) {
   return  <a href=${attrs.href}>
     <button class=${"download btn btn-" + attrs.class}>
       <table>
         <tr>
           <td rowspan=2><span class=${"glyphicon " + attrs.icon}/></td>
           <td class="content">${attrs.label || path.basename(attrs.href)}</td>
         </tr>
       </table>
     </button>
   </a>;
}

/*---------------------------------------------------------------------*/
/*    entryLetter ...                                                  */
/*---------------------------------------------------------------------*/
function entryLetter( en ) {
   return en.key.charAt( 0 ).toUpperCase();
}

/*---------------------------------------------------------------------*/
/*    idxLetters ...                                                   */
/*---------------------------------------------------------------------*/
function idxLetters( es ) {
   var res = [];
   var letter = false;
   var mark = 0;

   for( var i = 0; i < es.length; i++ ) {
      var l = entryLetter( es[ i ] );
      if( l != letter ) {
	 if( i > 0 ) {
	    res = res.concat( es.slice( mark, i ) );
	 }
	 res = res.concat( [ l ] );
	 letter = l;
	 mark = i;
      }
   }

   if( mark < i ) {
      res = res.concat( es.slice( mark, i ) );
   }

   return res;
}

/*---------------------------------------------------------------------*/
/*    minIndexOf ...                                                   */
/*---------------------------------------------------------------------*/
function minIndexOf( string, ...seps ) {
   let index = string.length;
   let sep = false;
   
   seps.forEach( s => {
      const i = string.indexOf( s );
      
      if( i > -1 && i < index ) {
	 index = i;
	 sep = s;
      }
   } )
		 
   return { index, sep };
}
   
/*---------------------------------------------------------------------*/
/*    idxEntry ...                                                     */
/*---------------------------------------------------------------------*/
function idxEntry( e, idx = undefined, arr = undefined ) {
   if( typeof( e ) === "string" ) {
      return <tr class="idx-letter"><td/><th>${e}</th></tr>;
   } else {
      const { index, sep } = minIndexOf( e.proto, "[", "{", "(" );
      const title = e.proto + "..." + e.chapter;
      let lbl = index ? e.proto.substring( 0, index ) : e.proto;
      const i = lbl.lastIndexOf( "." );
      const { index: cindex, sep: csep } = minIndexOf( e.proto, "(", "{" );

      switch( csep ) {
	 case "{": lbl += "{}"; break;
	 case "(": lbl += "()"; break;
      }
      
      if( i > 0 ) {
	 return <tr>
	   <td class="idx-prefix">${lbl.substring( 0, i )}.</td>
	   <td class="idx-entry" title=${title}>
	     <a href=${e.url}>${lbl.substring( i+1 )}</a>
	   </td>
	 </tr>;
      } else {
	 if( lbl.indexOf( "&lt;" ) === 0 && lbl.lastIndexOf( "&gt;" ) === -1 ) {
	    if( lbl.charAt( lbl.length - 1 ) === " " ) {
	       lbl = lbl.substring( 0, lbl.length - 1 ) + "&gt;";
	    } else {
	       lbl += "&gt;";
	    }
	 }
	 return <tr>
	   <td/>
	   <td class="idx-entry" title=${title}>
	     <a href=${e.url}>${lbl}</a>
	   </td>
	 </tr>
      }
   }
}

/*---------------------------------------------------------------------*/
/*    idx ...                                                          */
/*---------------------------------------------------------------------*/
function idx( attrs, entries ) {
   var en = idxLetters( entries.filter( x => x ) );
   var collen = en.length / 3;
   
   return <div class="row">
     <div class="col-md-4">
       <table class="idx-col">
         ${en.slice( 0, collen ).map( idxEntry )}
       </table>
     </div>
     <div class="col-md-4">
       <table class="idx-col">
         ${en.slice( collen, collen * 2 ).map( idxEntry )}
       </table>
     </div>
     <div class="col-md-4">
       <table class="idx-col">
         ${en.slice( collen * 2, en.length ).map( idxEntry )}
       </table>
     </div>
   </div>;
}

/*---------------------------------------------------------------------*/
/*    exports                                                          */
/*---------------------------------------------------------------------*/
exports.TITLE = title;
exports.NAVBUT = navbut;
exports.NAVBAR = navbar;
exports.FOOTER = docfooter;
exports.DOWNLOADBUTTON = downloadButton;
exports.copyrightYears = copyrightYears;
exports.IDX = idx;
