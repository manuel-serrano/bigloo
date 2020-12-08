/*=====================================================================*/
/*    .../project/bigloo/api/pulseaudio/src/Clib/bglpulseaudio.c       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Jun 23 18:07:00 2011                          */
/*    Last change :  Tue Jan 26 18:10:51 2016 (serrano)                */
/*    Copyright   :  2011-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo PULSEAUDIO specific functions                             */
/*=====================================================================*/
#include <pulse/simple.h>
#include <pulse/error.h>
#include <bigloo.h>
#include "bglsimple.h"

/*---------------------------------------------------------------------*/
/*    import                                                           */
/*---------------------------------------------------------------------*/
extern int bgl_pulseaudio_error( char *, char *, obj_t );

/*---------------------------------------------------------------------*/
/*    pa_simple *                                                      */
/*    bgl_pa_simple_new ...                                            */
/*---------------------------------------------------------------------*/
pa_simple *
bgl_pa_simple_new( char *srvname, char *name, char *stream, long format, long rate, long channels ) {
   int error;
   pa_sample_spec ss;
   
   ss.format = format;
   ss.rate = rate;
   ss.channels = channels;

   pa_simple *s = pa_simple_new( NULL, name,
				 PA_STREAM_PLAYBACK, NULL, stream,
				 &ss, NULL, NULL, &error );

   if( !s ) {
      bgl_pulseaudio_error( "bgl-pa-simple-new",
			    pa_strerror( error ),
			    BINT( error ) );
   }

   return s;
}
