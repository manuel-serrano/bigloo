/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/alsa/src/Clib/bglalsa.c          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Jun 23 18:07:00 2011                          */
/*    Last change :  Fri Jun 24 14:41:48 2011 (serrano)                */
/*    Copyright   :  2011 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Bigloo ALSA specific functions                                   */
/*=====================================================================*/
#include <alsa/asoundlib.h>
#include "bglalsa.h"
#include "bglpcm.h"

/*---------------------------------------------------------------------*/
/*    OBJ_TO_SND_PCM                                                   */
/*---------------------------------------------------------------------*/
#define OBJ_TO_SND_PCM( o ) \
   (((BgL_alsazd2sndzd2pcmz00_bglt)o)->BgL_z42builtinz42)

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_snd_pcm_open ...                                             */
/*---------------------------------------------------------------------*/
int
bgl_snd_pcm_open( obj_t o, char *name, snd_pcm_stream_t stream, int mode) {
   return snd_pcm_open( &(OBJ_TO_SND_PCM( o )), name, stream, mode);
}

/*---------------------------------------------------------------------*/
/*    snd_pcm_hw_params_t *                                            */
/*    bgl_snd_pcm_hw_params_malloc ...                                 */
/*---------------------------------------------------------------------*/
snd_pcm_hw_params_t *
bgl_snd_pcm_hw_params_malloc() {
   snd_pcm_hw_params_t *hw = NULL;

   snd_pcm_hw_params_malloc( &hw );

   return hw;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_snd_pcm_hw_params_free ...                                   */
/*---------------------------------------------------------------------*/
void
bgl_snd_pcm_hw_params_free( snd_pcm_hw_params_t *hw ) {
   snd_pcm_hw_params_free( hw );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_snd_pcm_hw_params_set_rate_near ...                          */
/*---------------------------------------------------------------------*/
int
bgl_snd_pcm_hw_params_set_rate_near( snd_pcm_t *pcm,
				     snd_pcm_hw_params_t *hw,
				     unsigned int rate ) {
   int err = snd_pcm_hw_params_set_rate_near( pcm, hw, &rate, 0L );

   return err < 0 ? err : rate;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_snd_pcm_hw_params_set_buffer_size_near ...                   */
/*---------------------------------------------------------------------*/
int
bgl_snd_pcm_hw_params_set_buffer_size_near( snd_pcm_t *pcm,
					    snd_pcm_hw_params_t *hw,
					    snd_pcm_uframes_t uframes ) {
   return snd_pcm_hw_params_set_buffer_size_near( pcm, hw, &uframes );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_snd_pcm_hw_params_set_period_size_near ...                   */
/*---------------------------------------------------------------------*/
int
bgl_snd_pcm_hw_params_set_period_size_near( snd_pcm_t *pcm,
					    snd_pcm_hw_params_t *hw,
					    snd_pcm_uframes_t val ) {
   return snd_pcm_hw_params_set_period_size_near( pcm, hw, &val, 0L );
}

/*---------------------------------------------------------------------*/
/*    snd_pcm_sw_params_t *                                            */
/*    bgl_snd_pcm_sw_params_malloc ...                                 */
/*---------------------------------------------------------------------*/
snd_pcm_sw_params_t *
bgl_snd_pcm_sw_params_malloc() {
   snd_pcm_sw_params_t *sw = NULL;

   snd_pcm_sw_params_malloc( &sw );

   return sw;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_snd_pcm_sw_params_free ...                                   */
/*---------------------------------------------------------------------*/
void
bgl_snd_pcm_sw_params_free( snd_pcm_sw_params_t *sw ) {
   snd_pcm_sw_params_free( sw );
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_snd_pcm_write ...                                            */
/*---------------------------------------------------------------------*/
long
bgl_snd_pcm_write( obj_t o, char *buf, long sz ) {
   snd_pcm_uframes_t frames;
   snd_pcm_sframes_t written;
   snd_pcm_t *pcm = OBJ_TO_SND_PCM( o );

   frames = snd_pcm_bytes_to_frames( pcm, sz );
   if( frames < 0 ) {
      return bgl_alsa_error(
	 "alsa-snd-pcm-write",
	 (char *)snd_strerror( written ),
	 o );
   }
   written = snd_pcm_writei( pcm, buf, frames );

   if( written == -EINTR )
       written = 0;
   else if( written == -EPIPE ) {
      if( snd_pcm_prepare( pcm ) >= 0 )
	 written = snd_pcm_writei( pcm, buf, frames );
   } else if( written >= 0 )
      return snd_pcm_frames_to_bytes( pcm, written );
   else {
      if( snd_pcm_state( pcm ) == SND_PCM_STATE_SUSPENDED ) {
	 snd_pcm_resume( pcm );
	 
	 if( snd_pcm_state( pcm ) == SND_PCM_STATE_SUSPENDED ) {
	    return bgl_alsa_error(
	       "alsa-snd-pcm-write",
	       "device suspended",
	       o );
	 }
      } else{
	 return bgl_alsa_error(
	    "alsa-snd-pcm-write",
	    (char *)snd_strerror( written ),
	    o );
      }
      return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_snd_pcm_flush ...                                            */
/*---------------------------------------------------------------------*/
void
bgl_snd_pcm_flush( obj_t o ) {
   snd_pcm_t *pcm = OBJ_TO_SND_PCM( o );
   
   snd_pcm_drop( pcm );
   snd_pcm_prepare( pcm );
}
