${ var doc = require( "@hop/hopdoc" ) }
${ var config = require( hop.config ) }
${ var texinfo = require( "@hop/texinfo" ) }
${ var xml = require( "./xml.js" ) }
${ var cfg = require( "./www.json" ) }
${ var fontifier = require( "@hop/fontifier" ) }

${ function CONTRIBCARD( attrs, ...body ) {
     return <div class="card mb-4 box-shadow">
        <div class="card-body">
		   <div class="card-header">
		      <h4 class="font-weight-normal">${attrs.title}</h4>
		   </div>
		   <div class="card-description">
		      ${ body }
 		   </div>
		 </div>
    </div>;
}}

Bigloo contributions
--------------------


${<div  id="contribs" class="container">
    <div class="card-deck mb-3 text-center">
	   <contribcard title="Bigloo @ Schemers">
	   <ul>
	     <li> <a href="https://hub.docker.com/u/schemers">Schemers</a> is a big collection of containers for Scheme implementations. </li>
		 <li> 45 implementations and more are coming! </li>
		 <li> All containers boot into a REPL by default. 
		 They also offer a Linux bash shell for compiling 
		 code and exploring the system. </li>
        </ul>
	   </contribcard>

	   <contribcard title="SRFI-64">
	     <a href="https://github.com/donaldsonjw/srfi64">SRI-64</a>  --
		 The srfi library is an implementation of <a href="https://srfi.schemers.org/srfi-64/srfi-64.html">SRFI 64</a> for Bigloo.
	   </contribcard>

	   <contribcard title="Compressed I/O library">
	     The <a href="https://github.com/donaldsonjw/compressed_ports">compressed I/O library SRI-64</a> 
		 mirrors the gzip and deflate procedures included in the standard 
		 library but also supports output to compress files. The supported 
		 compression methods are gzip, bzip2, xz, lz4, and zstd. Both the 
		 native and jvm backends are supported.
	   </contribcard>
	</div>
	   
    <div class="card-deck mb-3 text-center">
	   <contribcard title="SRFI-231">
	     <a href="https://github.com/donaldsonjw/srfi231">SRFI-231</a>  --
		 The srfi library is an implementation of <a href="https://srfi.schemers.org/srfi-231/srfi-231.html">SRFI 231</a> for Bigloo. It supports both the 
		 native and jvm backends. It supports all of the procedures defined 
		 by SRFI-231 but does differ from the gambit implementation in a few 
		 ways. The first is that specialized storage classes for complex
		 numbers are not supported and that a specialized numeric storage 
		 class is only compatible with the generic storage class and itself, 
		 as opposed to all numeric storage classes with inclusive numeric 
		 ranges. 
	   </contribcard>
	   
	   <contribcard title="SRFI-42">
	     <a href="https://github.com/donaldsonjw/srfi42">SRFI-42</a>  --
   		   Eager Comprehensions.
	   </contribcard>
	   <contribcard title="SRFI-133">
	     <a href="https://github.com/donaldsonjw/srfi133">SRFI-133</a>  --
   		   Vector Library.
	   </contribcard>
	</div>
	
    <div class="card-deck mb-3 text-center">
	   <contribcard title="SRFI-196">
	     <a href="https://github.com/donaldsonjw/srfi133">SRFI-196</a>  --
   		   Range Objects
	   </contribcard>
	   
	   <contribcard title="SRFI-13">
	     <a href="https://github.com/donaldsonjw/srfi13">SRFI-13</a>  --
   		   String Library, 
	   </contribcard>
	   
	   <contribcard title="SRFI-14">
	     <a href="https://github.com/donaldsonjw/srfi14">SRFI-14</a>  --
   		   String Library, 
	   </contribcard>
    </div>
	
  </div>} 

_If you have contributions you would like
to be included here, please contact manuel serrano at inria fr._

