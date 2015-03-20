/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/alsa/src/Clib/bglalsa.h          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Jun 23 18:06:29 2011                          */
/*    Last change :  Tue Mar 17 08:36:40 2015 (serrano)                */
/*    Copyright   :  2011-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Prototype of the Bigloo specific alsa functions                  */
/*=====================================================================*/
#ifndef BGLALSA_H 
#define BGLALSA_H

#include <bigloo.h>

extern int bgl_alsa_error( char *, char *, obj_t );

extern int bgl_snd_pcm_open( obj_t, char *, snd_pcm_stream_t, int );

extern snd_pcm_hw_params_t *bgl_snd_pcm_hw_params_malloc();
extern void bgl_snd_pcm_hw_params_free( snd_pcm_hw_params_t * );
extern int bgl_snd_pcm_hw_params_set_rate_near( snd_pcm_t *, snd_pcm_hw_params_t *, unsigned int );
extern unsigned long bgl_snd_pcm_hw_params_set_buffer_size_near( snd_pcm_t *, snd_pcm_hw_params_t *, snd_pcm_uframes_t );
extern unsigned long bgl_snd_pcm_hw_params_set_period_size_near( snd_pcm_t *, snd_pcm_hw_params_t *, snd_pcm_uframes_t );
extern int bgl_snd_pcm_hw_params_get_rates( snd_pcm_t *pcm );

extern snd_pcm_sw_params_t *bgl_snd_pcm_sw_params_malloc();
extern void bgl_snd_pcm_sw_params_free( snd_pcm_sw_params_t * );

extern long bgl_snd_pcm_write( obj_t, char *, long );
extern void bgl_snd_pcm_flush( obj_t );

extern char *bgl_snd_card_get_name( int );
extern char *bgl_snd_card_get_longname( int );

extern obj_t bgl_snd_devices_list( char * );

#endif
