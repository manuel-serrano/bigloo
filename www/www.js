/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/www/www.js                    */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Jul 30 17:20:13 2015                          */
/*    Last change :  Mon Jan 27 17:43:35 2025 (serrano)                */
/*    Copyright   :  2015-25 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Tools to build the Hop.js documentation.                         */
/*=====================================================================*/
"use hopscript";

/*---------------------------------------------------------------------*/
/*    module imports                                                   */
/*---------------------------------------------------------------------*/
import * as path from "node:path";
import * as fs from "node:fs";
import * as markdown from "@hop/markdown";
import * as texinfo from "@hop/texinfo";
import * as fontifier from "@hop/fontifier";
import * as hopdoc from "@hop/hopdoc";
import * as wwwxml from "./xml.js";

/*---------------------------------------------------------------------*/
/*    global parameters                                                */
/*---------------------------------------------------------------------*/
const PWD = process.cwd();
const ROOT = process.cwd();
const WWW = path.join(ROOT, "www.json");

const www = fs.existsSync(WWW) ? require(WWW) : undefined;

const chapters = www ?
      www.chapters.map(function(c, idx = undefined, arr = undefined) {
	 c.entries = chapterEntries(c);
	 return c;
      }) : [];

function P(file) {
   return path.normalize("./" + file);
}
   
const css = [P("hss/www.css"),
	      P("hss/markdown.css"),
	      P("hss/texinfo.css"),
	      P("hss/fontifier.css"),
	      P("hss/contribs.css"),
	      P("lib/bootstrap/css/bootstrap.min.css")];
const jscript = [P("lib/jquery/js/jquery.min.js"),
		  P("lib/bootstrap/js/bootstrap.min.js")];
const favicon = P("favicon.png");

const alias = {
   "user.md": "api",
   "config.md": "api",
   "hss.md": "api",
   "markdown.md": "api",
   "tree.md": "widget",
   "spage.md": "widget"
}

/*---------------------------------------------------------------------*/
/*    findChapter ...                                                  */
/*---------------------------------------------------------------------*/
function findChapter(key) {
   const keyhtml = key + ".html";
   return chapters.find(e => e.href === keyhtml);
}

/*---------------------------------------------------------------------*/
/*    chapterEntries ...                                               */
/*---------------------------------------------------------------------*/
function chapterEntries(chapter) {
   
   function chapterFile(file, i = undefined, arr = undefined) {
      let base = path.basename(file);
      return {
	 path: file.replace(/[.]md$/, ".html"),
	 href: base.replace(/[.]md$/, ".html"),
	 title: base.replace(/[0-9]+[-]|[.]md$/g, "")
      };
   }
   
   function chapterEntry(file, i = false, arr = false) {
      if (typeof file != "string") {
	 return [false];
      } else {
	 let fp = path.join(ROOT, file);
	 if (fs.lstatSync(fp).isDirectory()) {
	    return fs.readdirSync(fp)
	       .filter(function(e, idx = undefined, arr = undefined) {
		  return e.match(/[.]md$/) && (e != "index.md");
	       })
	       .sort(function(left, right) {
		  return left.naturalCompare(right);
	       })
	       .map(chapterFile);
	 } else {
	    return [chapterFile(file)];
	 }
      }
   }

   if (chapter.json) {
      let c = require(path.join(PWD, chapter.json));
      return Array.prototype.concat.apply([], c.files.map(chapterEntry));
   } else if (chapter.files) {
      return Array.prototype.concat.apply([], chapter.files.map(chapterEntry));
   } else {
      return [];
   }
}

/*---------------------------------------------------------------------*/
/*    childrenSize ...                                                 */
/*---------------------------------------------------------------------*/
function childrenSize(children) {
   let res = 0;
   
   for (let i = 0; i < children.length; i++) {
      if (children[i].tagName == "ul") {
	 res += childrenSize(children[i].childNodes);
      } else if (children[i].tagName == "li") {
	 res++;
      }
   }
   return res;
}

/*---------------------------------------------------------------------*/
/*    makeToc ...                                                      */
/*---------------------------------------------------------------------*/
function makeToc(els, k, proc = false) {
   
   function _makeToc(els, k, proc, indent) {
      if (els.length == k ) {
	 return [];
      } else {
	 let acc = [];
	 let tag = els[k].tagName;

	 for (let i = k; i < els.length;) {
	    if (els[i].tagName == tag) {
	       let el = els[i++];
	       let n = proc ? proc(el) : el.childNodes;
	       acc.push(<li>
		 <a href=${"#" + el.id} role="presentation">${n}</a>
               </li>);
	    } else if (els[i].tagName > tag) {
	       let children = _makeToc(els, i, proc, indent + "  ");
	       acc.push(<ul>${children}</ul>);
	       i += childrenSize(children);
	    } else {
	       i++;
	       return acc;
	    }
	 }

	 return acc;
      }
   }
   
   return _makeToc(els, k, proc, "");
}

/*---------------------------------------------------------------------*/
/*    compileXML ...                                                   */
/*---------------------------------------------------------------------*/
function compileXML(ast, title, clazz, target, tocfile = undefined) {
   let footer = path.join(PWD, "footer.md");
   let toc = hopdoc.toc(ast);
   let affix = "normal";
   
   toc.shift();
   
   let document = <html>
     <head css=${css}
	   title=${www.title + "/" + title}
           jscript=${jscript}
	   favicon=${favicon}
           rts=${false}/>

     <body data-spy="scroll" data-target="#navbar" class=${`bigloo ${title} section ${clazz}`}
           onscroll=~{
	      let top = (window.pageYOffset || document.scrollTop)-(document.clientTop||0);
	      if (top > 180) {
		 document.body.setAttribute("scrolled", "yes");
	      } else {
		 document.body.setAttribute("scrolled", "no");
	      }
	   } >
       ~{ $('body').scrollspy({ target: '#navbar' }) }
       <wwwxml.navbar title=${title} key=${clazz}>
         ${chapters}
       </wwwxml.navbar>
       
       <wwwxml.title title=${www.title}
		     version=${www.version}
		     date=${www.date}
		     logo=${www.logo}
		     root=${ROOT}>
          ${title}
       </wwwxml.title>
       <div class="container">
	 <div class="filler">.keep</div>
            <div class="col-md-9" role="main"> ${ast} </div>
         <div class="row">
           <div id="navbar" class="col-md-3" role="complementary">
             <nav class="sidebar noaffix"
		  data-spy=${affix}
	          data-offset-top="215" data-offset-bottom="100">
               <ul class="nav bs-wwws-sidenav">
                  ${makeToc(toc, 0, function(el) {
		     if (el.childNodes[0].data.charAt(0) === "(") {
			let m = el.childNodes[0].data.match(/[(](?:class |generic)?([^)]*)/);
			return m[1];
		     } else {
			return el.childNodes[0].data.replace(/[(].*$/, "");
		     }
		  })}
	       </ul>
	     </nav>
             <nav class="sidebar noaffix toc-extra">
	       ${tocfile && fs.existsSync(path.join(PWD, tocfile))
		    ? require(path.join(PWD, tocfile))
		    : "" }
	     </nav> 
	   </div>
	 </div>
	 ${fs.existsSync(footer)
	   ? hopdoc.load(footer).XML 
	   : <wwwxml.footer root=${ROOT}/>}
       </div>
     </body>
   </html>;

   fs.writeFileSync(target, hop.compileXML(document));
}

/*---------------------------------------------------------------------*/
/*    compileNode ...                                                  */
/*---------------------------------------------------------------------*/
function compileNode(node, title, clazz, target, tocfile = undefined) {
   let footer = path.join(PWD, "footer.md");
   let affix = "normal";
   
   let document = <html>
     <head css=${css}
	   title=${www.title + "/" + title}
           jscript=${jscript}
	   favicon=${favicon}
           rts=${false}/>

     <body data-spy="scroll" data-target="#navbar" class=${`bigloo ${title} section ${clazz}`}
           onscroll=~{
	      let top = (window.pageYOffset || document.scrollTop)-(document.clientTop||0);
	      if (top > 180) {
		 document.body.setAttribute("scrolled", "yes");
	      } else {
		 document.body.setAttribute("scrolled", "no");
	      }
	   } >
       ~{ $('body').scrollspy({ target: '#navbar' }) }
       <wwwxml.navbar title=${title}>
         ${chapters}
       </wwwxml.navbar>
       
       <wwwxml.title title=${www.title}
		     version=${www.version}
		     date=${www.date}
		     logo=${www.logo}
		     root=${ROOT}>
          ${title}
       </wwwxml.title>
       <div class="container">
	 <div class="filler">.keep</div>
	 <div class="col-md-12" role="main"> ${node} </div>
	 ${fs.existsSync(footer)
	   ? hopdoc.load(footer).XML 
	   : <wwwxml.footer root=${ROOT}/>}
       </div>
     </body>
   </html>;

   fs.writeFileSync(target, hop.compileXML(document));
}

/*---------------------------------------------------------------------*/
/*    compileSection ...                                               */
/*---------------------------------------------------------------------*/
function compileSection(page) {
   let footer = path.join(PWD, "footer.md");
   let ast = hopdoc.load(path.join(PWD, page))
   let toc = hopdoc.toc(ast);
   let title = path.basename(page).replace(/[0-9]+[-]|[.][^.]*$/g, "");
   let key = path.basename(path.dirname(page)).toLowerCase();
   let affix = "normal";
   let chap = findChapter(title);

   if (key == "www") {
      key = alias[path.basename(page)];
   } else if (key == ".") {
      key = title;
   }

   let document = <html>
     <head css=${css}
	   title=${www.title + "/" + title}
           jscript=${jscript}
	   favicon=${favicon}
           rts=${false}/>

     <body data-spy="scroll" data-target="#navbar" class=${`bigloo ${title} section`}
           onscroll=~{
	      let top = (window.pageYOffset || document.scrollTop)-(document.clientTop||0);
	      if (top > 180) {
		 document.body.setAttribute("scrolled", "yes");
	      } else {
		 document.body.setAttribute("scrolled", "no");
	      }
	   } >
       ~{ $('body').scrollspy({ target: '#navbar' }) }
       <wwwxml.navbar title=${title} key=${key}>
         ${chapters}
       </wwwxml.navbar>
       
       <wwwxml.title title=${www.title}
		     version=${www.version}
		     date=${www.date}
		     logo=${www.logo}
		     root=${ROOT}>
          ${title}
       </wwwxml.title>
       <div class="container">
	 <div class="filler">.keep</div>
	 ${(toc.length > 0)
	      ? <div class="col-md-9" role="main"> ${ast.XML} </div>
	      : <div class="col-md-12" role="main"> ${ast.XML} </div>}
         <div class="row">
           ${(toc.length > 0 && (!chap || !("toc" in chap) || chap.toc)) ?
           <div id="navbar" class="col-md-3" role="complementary">
	     
             <nav class="sidebar noaffix"
		  data-spy=${affix}
	          data-offset-top="215" data-offset-bottom="100">
               <ul class="nav bs-wwws-sidenav">
                  ${makeToc(toc, 0, function(el) {
		     if (el.childNodes[0].data.charAt(0) === "(") {
			let m = el.childNodes[0].data.match(/[(](?:class |generic)?([^)]*)/);
			return m[1];
		     } else {
			return el.childNodes[0].data.replace(/[(].*$/, "");
		     }
		  })}
	       </ul>
	     </nav>
             <nav class="sidebar noaffix toc-extra">
	       ${fs.existsSync(path.join(PWD, title + "-toc.js"))
		    ? require(path.join(PWD, title + "-toc.js"))
	            : fs.existsSync(path.join(PWD, title + "-toc.md"))
		        ? hopdoc.load(path.join(PWD, title + "-toc.md").xml)
		        : "" }
	     </nav> 
	   </div>
           : undefined}
	 </div>
	 ${fs.existsSync(footer)
	   ? hopdoc.load(footer).XML 
	   : <wwwxml.footer root=${ROOT}/>}
       </div>
     </body>
   </html>;

   fs.writeSync(process.stdout.fd, hop.compileXML(document));
   fs.writeSync(process.stdout.fd, "\n");
}

/*---------------------------------------------------------------------*/
/*    compileChapter ...                                               */
/*---------------------------------------------------------------------*/
function compileChapter(json) {
   let footer = path.join(PWD, "footer.md");
   let chapter = require(path.join(PWD, json));
   let toc = chapterEntries(chapter).filter(x => x);

   let document = <html>
     <head css=${css}
	   title=${www.title + "/" + chapter.title}
           jscript=${jscript}
	   favicon=${favicon}
           rts=${false}/>

     <body data-spy="scroll" data-target="#navbar" class="bigloo chapter">
       <wwwxml.navbar title=${chapter.title} key=${chapter.key}>
         ${chapters}
       </wwwxml.navbar>
       <wwwxml.title title=${www.title}
		     version=${www.version}
		     date=${www.date}
		     logo=${www.logo}
		     root=${ROOT}>
          ${chapter.title}
       </wwwxml.title>

       <div class="container">
	 <div class="filler">.keep</div>
         ${chapter.description ? <div class="chapter-header">
	   ${ fs.existsSync(ROOT + "/" + chapter.description) ?
	      hopdoc.load(ROOT + "/" + chapter.description).XML
	      : hopdoc.eval(chapter.description).XML }
	   </div> : ""}
	 
         <h1 class="toc" id="toc">Table of Contents</h1>
         <ul class="toc">
           ${toc.map(function(el, idx = undefined, arr = undefined) {
              return <li>
	        <a href=${el.href}>${el.title}</a>
                <span class="toc-description">
                  ${hopdoc.eval(el.description)}
                </span>
	      </li>
	   })}
         </ul>
	 ${fs.existsSync(footer) 
	   ? hopdoc.load(footer).XML 
	   : <wwwxml.footer root=${ROOT}/>}
       </div>
     </body>
   </html>;

   console.log(hop.compileXML(document));
}

/*---------------------------------------------------------------------*/
/*    compileMain ...                                                  */
/*---------------------------------------------------------------------*/
function compileMain(content) {

   let document = <html>
     <head css=${css}
	   title=${www.title}
           jscript=${jscript}
	   favicon=${favicon}
           rts=${false}/>

     <body class="bigloo home" data-spy="scroll" data-target="#navbar"
           onscroll=~{
	      let top = (window.pageYOffset || document.scrollTop)-(document.clientTop||0);
	      if (top > 180) {
		 document.body.setAttribute("scrolled", "yes");
	      } else {
		 document.body.setAttribute("scrolled", "no");
	      }
	   } >
       ~{ $('body').scrollspy({ target: '#navbar' }) }
       <wwwxml.navbar title=${www.title} key="home">
         ${chapters}
       </wwwxml.navbar>
       <wwwxml.title title=${www.title}
		     version=${www.version}
		     date=${www.date}
		     logo=${www.logo}
		     root=${ROOT}/>

       <div class="container home-body">
	 <div class="filler">.keep</div>
         ${hopdoc.load(content).XML}
	 <wwwxml.footer root=${ROOT}/>
       </div>
     </body>
   </html>;

   console.log(hop.compileXML(document));
}

/*---------------------------------------------------------------------*/
/*    compileLibrary ...                                               */
/*---------------------------------------------------------------------*/
function compileLibrary(content) {
   let footer = path.join(PWD, "footer.md");
   
   let document = <html>
     <head css=${css}
	   title=${www.title}
           jscript=${jscript}
	   favicon=${favicon}
           rts=${false}/>

     <body class="bigloo library" data-spy="scroll" data-target="#navbar">
       <wwwxml.navbar title=${www.title} key="home">
         ${chapters}
       </wwwxml.navbar>
       <wwwxml.title title=${www.title}
		     version=${www.version}
		     date=${www.date}
		     logo=${www.logo}
		     root=${ROOT}/>

       <div class="container home-body">
	 <div class="filler">.keep</div>
         ${hopdoc.load(content).XML}
	 ${fs.existsSync(footer)
	   ? hopdoc.load(footer).XML
	   : ""}
       </div>
     </body>
   </html>;

   console.log(hop.compileXML(document));
}

/*---------------------------------------------------------------------*/
/*    compileIdx ...                                                   */
/*    -------------------------------------------------------------    */
/*    compile the HTML index page.                                     */
/*---------------------------------------------------------------------*/
function compileIdx(json) {
   let idx = require(path.join(PWD, json));
   let chapter = { title: "Index", key: "manual" };
   let footer = path.join(PWD, "footer.md");

   let document = <html>
     <head css=${css}
	   title=${www.title + "/" + chapter.title}
           jscript=${jscript}
	   favicon=${favicon}
           rts=${false}/>

     <body data-spy="scroll" data-target="#navbar" class="bigloo">
       <wwwxml.navbar title=${chapter.title}
                      key=${chapter.key}>
         ${chapters}
       </wwwxml.navbar>
       <wwwxml.title title=${www.title}
		     version=${www.version}
		     date=${www.date}
		     logo=${www.logo}
		     root=${ROOT}>
          ${chapter.title}
       </wwwxml.title>

       <div class="container">
	 <div class="filler">.keep</div>
	 <wwwxml.idx>${idx}</wwwxml.idx>
	 ${fs.existsSync(footer)
	   ? hopdoc.load(footer).XML
	   : ""}
       </div>
     </body>
   </html>;

   console.log(hop.compileXML(document));
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
   const argv = process.argv;
   const target = argv[4] === "-o" ? argv[5]: false;
   switch(argv[2]) {
      case "html-to-idx":
	 hopdoc.htmlToIdx(argv[3],
			   argv.slice(target ? 6: 4).map(function(f, _, __) {
			      return path.join(PWD, f);
			   }),
			   target,
 			   ["h1", "h3", "h4"]);
	 break;

      case "compile-idx":
	 compileIdx(argv[3]);
	 break;

      case "compile-main":
	 compileMain(argv[3]);
	 break;

      case "compile-library":
	 compileLibrary(argv[3]);
	 break;

      case "compile-section":
	 compileSection(argv[3]);
	 break;

      case "compile-chapter":
	 compileChapter(argv[3]);
	 break;
	 
      default:
	 throw("Unknown command: " + argv[2]);
   }
}

main();
