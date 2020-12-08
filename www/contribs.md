${ var doc = require( "hopdoc" ) }
${ var config = require( hop.config ) }
${ var texinfo = require( hop.texinfo ) }
${ var xml = require( "./xml.js" ) }
${ var cfg = require( "./www.json" ) }
${ var fontifier = require( hop.fontifier ) }

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
	     <li> <a href="https://hub.docker.com/u/schemers">Schemers</a> is a big collection of containers for Scheme 
		 implementations. </li>
		 <li> 45 implementations and more are coming! </li>
		 <li> All containers boot into a REPL by default. 
		 They also offer a Linux bash shell for compiling 
		 code and exploring the system. </li>
        </ul>
	   </contribcard>
	   <contribcard title="">
	   </contribcard>
	   <contribcard title="">
	   </contribcard>
	</div>
  </div>} 

_If you have contributions you would like
to be included here, please contact manuel serrano at inria fr._

