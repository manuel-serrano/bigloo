/*=====================================================================*/
/*    .../prgm/project/bigloo/bigloo/api/alsa/src/Clib/bglalsa.c       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Jun 23 18:07:00 2011                          */
/*    Last change :  Mon Mar  4 08:22:36 2019 (serrano)                */
/*    Copyright   :  2011-19 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo ALSA specific functions                                   */
/*=====================================================================*/
#include <alsa/asoundlib.h>
#include <bigloo.h>
#include "bglalsa.h"
#include "bglpcm.h"
#include "bglctl.h"
#include "bglmixer.h"
#include "bglrawmidi.h"

/*---------------------------------------------------------------------*/
/*    OBJ_TO_SND_PCM                                                   */
/*---------------------------------------------------------------------*/
#define OBJ_TO_SND_PCM( o ) \
   (((BgL_alsazd2sndzd2pcmz00_bglt)CREF(o))->BgL_z42builtinz42)

/*---------------------------------------------------------------------*/
/*    OBJ_TO_SND_CTL                                                   */
/*---------------------------------------------------------------------*/
#define OBJ_TO_SND_CTL( o ) \
   (((BgL_alsazd2sndzd2ctlz00_bglt)CREF(o))->BgL_z42builtinz42)

/*---------------------------------------------------------------------*/
/*    OBJ_TO_SND_MIXER                                                 */
/*---------------------------------------------------------------------*/
#define OBJ_TO_SND_MIXER( o ) \
   (((BgL_alsazd2sndzd2mixerz00_bglt)CREF(o))->BgL_z42builtinz42)

/*---------------------------------------------------------------------*/
/*    OBJ_TO_SND_RAWMIDI                                               */
/*---------------------------------------------------------------------*/
#define OBJ_TO_SND_RAWMIDI( o ) \
   (((BgL_alsazd2sndzd2rawmidiz00_bglt)CREF(o))->BgL_z42builtinz42)

/*---------------------------------------------------------------------*/
/*    alsa-snd-card-info bigloo object                                 */
/*---------------------------------------------------------------------*/
#define BGL_SND_CTL_CARD_INFO_CTL( o ) \
   ((BgL_alsazd2sndzd2ctlzd2cardzd2infoz00_bglt)CREF(o))->BgL_ctlz00
#define BGL_SND_CTL_CARD_INFO_CARD( o ) \
   ((BgL_alsazd2sndzd2ctlzd2cardzd2infoz00_bglt)CREF(o))->BgL_cardz00
#define BGL_SND_CTL_CARD_INFO_ID( o ) \
   ((BgL_alsazd2sndzd2ctlzd2cardzd2infoz00_bglt)CREF(o))->BgL_idz00
#define BGL_SND_CTL_CARD_INFO_DRIVER( o ) \
   ((BgL_alsazd2sndzd2ctlzd2cardzd2infoz00_bglt)CREF(o))->BgL_driverz00
#define BGL_SND_CTL_CARD_INFO_NAME( o ) \
   ((BgL_alsazd2sndzd2ctlzd2cardzd2infoz00_bglt)CREF(o))->BgL_namez00
#define BGL_SND_CTL_CARD_INFO_LONGNAME( o ) \
   ((BgL_alsazd2sndzd2ctlzd2cardzd2infoz00_bglt)CREF(o))->BgL_longnamez00
#define BGL_SND_CTL_CARD_INFO_MIXERNAME( o ) \
   ((BgL_alsazd2sndzd2ctlzd2cardzd2infoz00_bglt)CREF(o))->BgL_mixernamez00
#define BGL_SND_CTL_CARD_INFO_COMPONENTS( o ) \
   ((BgL_alsazd2sndzd2ctlzd2cardzd2infoz00_bglt)CREF(o))->BgL_componentsz00

/*---------------------------------------------------------------------*/
/*    alsa-snd-rawmidi-info bigloo object                              */
/*---------------------------------------------------------------------*/
#define BGL_SND_CTL_RAWMIDI_INFO_CTL( o ) \
   ((BgL_alsazd2sndzd2ctlzd2rawmidizd2infoz00_bglt)CREF(o))->BgL_ctlz00
#define BGL_SND_CTL_RAWMIDI_INFO_RAWMIDI( o ) \
   ((BgL_alsazd2sndzd2ctlzd2rawmidizd2infoz00_bglt)CREF(o))->BgL_rawmidiz00
#define BGL_SND_CTL_RAWMIDI_INFO_CARD( o ) \
   ((BgL_alsazd2sndzd2ctlzd2rawmidizd2infoz00_bglt)CREF(o))->BgL_cardz00
#define BGL_SND_CTL_RAWMIDI_INFO_ID( o ) \
   ((BgL_alsazd2sndzd2ctlzd2rawmidizd2infoz00_bglt)CREF(o))->BgL_idz00
#define BGL_SND_CTL_RAWMIDI_INFO_NAME( o ) \
   ((BgL_alsazd2sndzd2ctlzd2rawmidizd2infoz00_bglt)CREF(o))->BgL_namez00
#define BGL_SND_CTL_RAWMIDI_INFO_DEVICE( o ) \
   ((BgL_alsazd2sndzd2ctlzd2rawmidizd2infoz00_bglt)CREF(o))->BgL_devicez00
#define BGL_SND_CTL_RAWMIDI_INFO_SUBDEVICE( o ) \
   ((BgL_alsazd2sndzd2ctlzd2rawmidizd2infoz00_bglt)CREF(o))->BgL_subdevicez00
#define BGL_SND_CTL_RAWMIDI_INFO_SUBDEVICESCOUNT( o ) \
   ((BgL_alsazd2sndzd2ctlzd2rawmidizd2infoz00_bglt)CREF(o))->BgL_subdevicescountz00

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_snd_pcm_open ...                                             */
/*---------------------------------------------------------------------*/
int
bgl_snd_pcm_open( obj_t o, char *name, snd_pcm_stream_t stream, int mode ) {
   return snd_pcm_open( &(OBJ_TO_SND_PCM( o )), name, stream, mode );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_snd_pcm_reopen ...                                           */
/*---------------------------------------------------------------------*/
int
bgl_snd_pcm_reopen( obj_t o, char *name, snd_pcm_stream_t stream, int mode ) {
   if( OBJ_TO_SND_PCM( o ) ) {
      int res = snd_pcm_close( OBJ_TO_SND_PCM( o ) );

      if( res ) return res;
   }
   return snd_pcm_open( &(OBJ_TO_SND_PCM( o )), name, stream, mode );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_snd_pcm_close ...                                            */
/*---------------------------------------------------------------------*/
int
bgl_snd_pcm_close( obj_t o ) {
   int res = snd_pcm_close( OBJ_TO_SND_PCM( o ) );
   
   OBJ_TO_SND_PCM( o ) = 0L;
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_snd_ctl_open ...                                             */
/*---------------------------------------------------------------------*/
int
bgl_snd_ctl_open( obj_t o, char *card, int mode ) {
   return snd_ctl_open( &(OBJ_TO_SND_CTL( o )), card, mode );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_snd_ctl_rawmidi_next_device ...                              */
/*---------------------------------------------------------------------*/
int
bgl_snd_ctl_rawmidi_next_device( obj_t o, int dev ) {
   int device = dev;
   int err = snd_ctl_rawmidi_next_device( OBJ_TO_SND_CTL( o ), &device );

   if( err < 0 ) {
      bgl_alsa_error( "alsa-snd-ctl-rawmidi-next-device",
		      (char *)snd_strerror( err ),
		      o );
   } else {
      return device;
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_snd_mixer_open ...                                           */
/*---------------------------------------------------------------------*/
int
bgl_snd_mixer_open( obj_t o ) {
   return snd_mixer_open( &(OBJ_TO_SND_MIXER( o )), 0 );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_snd_ctl_card_info_init ...                                   */
/*---------------------------------------------------------------------*/
void
bgl_snd_ctl_card_info_init( obj_t o ) {
   int err;
   snd_ctl_card_info_t *info;
   
   snd_ctl_card_info_alloca( &info );
   snd_ctl_t *handle = OBJ_TO_SND_CTL( BGL_SND_CTL_CARD_INFO_CTL( o ) );
   
   if( (err = snd_ctl_card_info( handle, info )) < 0 ) {
      bgl_alsa_error( "alsa-snd-ctl-card-info",
		      (char *)snd_strerror( err ),
		      o );
   }
   
   BGL_SND_CTL_CARD_INFO_CARD( o ) =
      snd_ctl_card_info_get_card( info );
   BGL_SND_CTL_CARD_INFO_ID( o ) =
      string_to_bstring( (char *)snd_ctl_card_info_get_id( info ) );
   BGL_SND_CTL_CARD_INFO_DRIVER( o ) =
      string_to_bstring( (char *)snd_ctl_card_info_get_driver( info ) );
   BGL_SND_CTL_CARD_INFO_NAME( o ) =
      string_to_bstring( (char *)snd_ctl_card_info_get_name( info ) );
   BGL_SND_CTL_CARD_INFO_LONGNAME( o ) =
      string_to_bstring( (char *)snd_ctl_card_info_get_longname( info ) );
   BGL_SND_CTL_CARD_INFO_MIXERNAME( o ) =
      string_to_bstring( (char *)snd_ctl_card_info_get_mixername( info ) );
   BGL_SND_CTL_CARD_INFO_COMPONENTS( o ) =
      string_to_bstring( (char *)snd_ctl_card_info_get_components( info ) );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_snd_ctl_rawmidi_info_init ...                                */
/*---------------------------------------------------------------------*/
void
bgl_snd_ctl_rawmidi_info_init( obj_t o ) {
   int err;
   snd_rawmidi_info_t *info;
   int device = -1;

   snd_rawmidi_info_alloca( &info );
   snd_rawmidi_info_set_device( info, BGL_SND_CTL_RAWMIDI_INFO_DEVICE( o ) );
   snd_ctl_t *handle = OBJ_TO_SND_CTL( BGL_SND_CTL_RAWMIDI_INFO_CTL( o ) );
   
   if( (err = snd_ctl_rawmidi_info( handle, info )) < 0 ) {
      bgl_alsa_error( "alsa-snd-ctl-rawmidi-info",
		      (char *)snd_strerror( err ),
		      o );
   }
   
   BGL_SND_CTL_RAWMIDI_INFO_CARD( o ) =
      snd_rawmidi_info_get_card( info );
   BGL_SND_CTL_RAWMIDI_INFO_ID( o ) =
      string_to_bstring( (char *)snd_rawmidi_info_get_id( info ) );
   BGL_SND_CTL_RAWMIDI_INFO_NAME( o ) =
      string_to_bstring( (char *)snd_rawmidi_info_get_name( info ) );
   BGL_SND_CTL_RAWMIDI_INFO_DEVICE( o ) =
      snd_rawmidi_info_get_device( info );
   BGL_SND_CTL_RAWMIDI_INFO_SUBDEVICE( o ) =
      snd_rawmidi_info_get_subdevice( info );
   BGL_SND_CTL_RAWMIDI_INFO_SUBDEVICESCOUNT( o ) =
      snd_rawmidi_info_get_subdevices_count( info );

}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_snd_rawmidi_open_output ...                                  */
/*---------------------------------------------------------------------*/
int
bgl_snd_rawmidi_open_output( obj_t o, char *name, int mode ) {
   return snd_rawmidi_open( NULL, &(OBJ_TO_SND_RAWMIDI( o )), name, mode );
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
/*    unsigned long                                                    */
/*    bgl_snd_pcm_hw_params_set_buffer_size_near ...                   */
/*---------------------------------------------------------------------*/
unsigned long
bgl_snd_pcm_hw_params_set_buffer_size_near( snd_pcm_t *pcm,
					    snd_pcm_hw_params_t *hw,
					    snd_pcm_uframes_t uframes ) {
   int err = snd_pcm_hw_params_set_buffer_size_near( pcm, hw, &uframes );

   if( err < 0 ) {
      bgl_alsa_error( "snd-pcm-hw-params-set-buffer-size-near",
		      (char *)snd_strerror( err ),
		      BINT( uframes ) );
   } else {
      return (unsigned long)uframes;
   }
}

/*---------------------------------------------------------------------*/
/*    unsigned int                                                     */
/*    bgl_snd_pcm_hw_params_set_buffer_time_near ...                   */
/*---------------------------------------------------------------------*/
unsigned int
bgl_snd_pcm_hw_params_set_buffer_time_near( snd_pcm_t *pcm,
					    snd_pcm_hw_params_t *hw,
					    unsigned int val ) {
   int err = snd_pcm_hw_params_set_buffer_time_near( pcm, hw, &val, NULL );
   
   if( err < 0 ) {
      bgl_alsa_error( "snd-pcm-hw-params-set-buffer-time-near",
		      (char *)snd_strerror( err ),
		      BINT( val ) );
   } else {
      return val;
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_snd_pcm_hw_params_get_buffer_size ...                        */
/*---------------------------------------------------------------------*/
int
bgl_snd_pcm_hw_params_get_buffer_size( snd_pcm_t *pcm ) {
   snd_pcm_hw_params_t *hw;
   int err;

   snd_pcm_hw_params_alloca( &hw );

   err = snd_pcm_hw_params_any( pcm, hw );

   if( err < 0 ) {
      return err;
   } else {
      snd_pcm_uframes_t uframes;
      err = snd_pcm_hw_params_get_buffer_size( hw, &uframes );

      return err < 0 ? err : uframes;
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_snd_pcm_hw_params_get_buffer_time ...                        */
/*---------------------------------------------------------------------*/
int
bgl_snd_pcm_hw_params_get_buffer_time( snd_pcm_t *pcm ) {
   snd_pcm_hw_params_t *hw;
   int err;

   snd_pcm_hw_params_alloca( &hw );

   err = snd_pcm_hw_params_any( pcm, hw );

   if( err < 0 ) {
      return err;
   } else {
      unsigned int latency;
      err = snd_pcm_hw_params_get_buffer_time( hw, &latency, NULL );

      return err < 0 ? err : latency;
   }
}

/*---------------------------------------------------------------------*/
/*    unsigned long                                                    */
/*    bgl_snd_pcm_hw_params_set_period_size_near ...                   */
/*---------------------------------------------------------------------*/
unsigned long
bgl_snd_pcm_hw_params_set_period_size_near( snd_pcm_t *pcm,
					    snd_pcm_hw_params_t *hw,
					    snd_pcm_uframes_t val ) {
   int err = snd_pcm_hw_params_set_period_size_near( pcm, hw, &val, 0L );
   if( err < 0 ) {
      bgl_alsa_error( "snd-pcm-hw-params-set-period-size-near",
		      (char *)snd_strerror( err ),
		      BINT( val ) );
   } else {
      return (unsigned long)val;
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_snd_pcm_hw_params_get_period_size ...                        */
/*---------------------------------------------------------------------*/
int
bgl_snd_pcm_hw_params_get_period_size( snd_pcm_hw_params_t *hw ) {
   snd_pcm_uframes_t uframes;
   int err = snd_pcm_hw_params_get_period_size( hw, &uframes, NULL );

   return err < 0 ? err : uframes;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_snd_pcm_hw_params_get_rates ...                              */
/*---------------------------------------------------------------------*/
int
bgl_snd_pcm_hw_params_get_rates( snd_pcm_t *pcm ) {
   snd_pcm_hw_params_t *hw;
   int err;
   int rate_num, rate_den;

   snd_pcm_hw_params_alloca( &hw );

   err = snd_pcm_hw_params_current( pcm, hw );
/*    snd_pcm_hw_params_get_rate_numden( hw, &rate_num, &rate_den );   */
/*    snd_pcm_hw_params_get_sbits( hw );                               */
/*    fprintf( stderr, "rate_num=%d rate_den=%d\n", rate_num, rate_den ); */
/*    fprintf( stderr, "sbit=%d\n", snd_pcm_hw_params_get_sbits( hw ) ); */

   if( err < 0 ) {
      return err;
   } else {
      int cur, min, max;
      err = snd_pcm_hw_params_get_rate( hw, &cur, 0 );

      if( err ) return err;
      err = snd_pcm_hw_params_get_rate_min( hw, &min, 0 );

      if( err ) return err;
      
      err = snd_pcm_hw_params_get_rate_max( hw, &max, 0 );
   
      if( err ) {
	 return err;
      } else {
	 obj_t env = BGL_CURRENT_DYNAMIC_ENV();
   
	 BGL_ENV_MVALUES_NUMBER_SET( env, 3 );
	 BGL_ENV_MVALUES_VAL_SET( env, 1, BINT( min ) );
	 BGL_ENV_MVALUES_VAL_SET( env, 2, BINT( max ) );

	 return cur;
      }
   }
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
   long w = 0;
   
loop:   
   frames = snd_pcm_bytes_to_frames( pcm, sz - w );

   if( frames < 0 ) {
      return bgl_alsa_error(
	 "alsa-snd-pcm-write",
	 (char *)snd_strerror( written ),
	 o );
   }

   written = snd_pcm_writei( pcm, &buf[ w ], frames );

   if( written == -EINTR ) {
      fprintf( stderr, "%s:%d snd_pcm_writei %s (-EINTR)\n",
	       __FILE__, __LINE__, snd_strerror( written ) );
      written = 0;
   } else if( written == -EPIPE ) {
      fprintf( stderr, "%s:%d snd_pcm_writei( ..., %ld) %s (-EPIPE)\n",
	       __FILE__, __LINE__,
	       sz,
	       snd_strerror( written ) );
      if( snd_pcm_prepare( pcm ) >= 0 ) {
	 written = snd_pcm_writei( pcm, &buf[ w ], frames );
      }
   }

   if( written >= 0 ) {
      long bytes = snd_pcm_frames_to_bytes( pcm, written );

      w += bytes;

      if( w == sz ) {
	 return w;
      } else {
	 goto loop;
      }
   } else {
      fprintf( stderr, "%s:%d snd_pcm_writei (%ld<0) -> %s\n",
	       __FILE__, __LINE__, written, snd_strerror( written ) );
   
      if( snd_pcm_state( pcm ) == SND_PCM_STATE_SUSPENDED ) {
	 snd_pcm_resume( pcm );
	 
	 if( snd_pcm_state( pcm ) == SND_PCM_STATE_SUSPENDED ) {
	    return bgl_alsa_error(
	       "alsa-snd-pcm-write",
	       "device suspended",
	       o );
	 }
      } else {
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

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    bgl_snd_card_get_name ...                                        */
/*---------------------------------------------------------------------*/
char *
bgl_snd_card_get_name( int i ) {
   char *name;
   int err = snd_card_get_name( i, &name );

   if( !err ) {
      return name;
   } else {
      bgl_alsa_error( "alsa-get-cards",
		      (char *)snd_strerror( err ),
		      BINT( i ) );
      return 0L;
   }
}      
   
/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    bgl_snd_card_get_longname ...                                    */
/*---------------------------------------------------------------------*/
char *
bgl_snd_card_get_longname( int i ) {
   char *longname;
   int err = snd_card_get_longname( i, &longname );

   if( !err ) {
      return longname;
   } else {
      bgl_alsa_error( "alsa-get-cards",
		      (char *)snd_strerror( err ),
		      BINT( i ) );
      return 0L;
   }
}      
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_snd_devices_list ...                                         */
/*---------------------------------------------------------------------*/
obj_t
bgl_snd_devices_list( char *iface ) {
   void **hints;
   int err = snd_device_name_hint( -1, (const char*)iface, &hints );
   obj_t acc = BNIL;

   if( err >= 0 ) {
      void **h = hints;
      while( *h ) {
	 char *s = snd_device_name_get_hint( *h++, "NAME" );
	 acc = MAKE_PAIR( string_to_bstring( s ), acc );
	 free( s );
      }

      snd_device_name_free_hint( hints );
      
      return acc;
   } else {
      return BNIL;
   }
}      

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bgl_snd_rawmidi_isdir ...                                        */
/*---------------------------------------------------------------------*/
bool_t
bgl_snd_rawmidi_isdir( obj_t o, int device, int sub, int dir ) {
   snd_rawmidi_info_t *info;
   int status;

   snd_rawmidi_info_alloca( &info);
   snd_rawmidi_info_set_device( info, device );
   snd_rawmidi_info_set_subdevice( info, sub );
   snd_rawmidi_info_set_stream( info, dir );
   
   if( (status = snd_ctl_rawmidi_info(OBJ_TO_SND_CTL( o ), info)) < 0 && status != -ENXIO ) {
      return status;
   } else if( status == 0 ) {
      return 1;
   }

   return 0;
}
