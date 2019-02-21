/*=====================================================================*/
/*    .../prgm/project/bigloo/api/multimedia/src/Unix/bglmixer.c       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Feb  3 15:25:23 2000                          */
/*    Last change :  Thu Oct 29 06:49:52 2009 (serrano)                */
/*    -------------------------------------------------------------    */
/*    A control over the audio mixer for Bigloo. To a large extent     */
/*    this file is inspired of mixctl.h by Sam Hawker                  */
/*    <shawkie@geocities.com>.                                         */
/*=====================================================================*/
#include <bigloo_config.h>
#if( BGL_HAVE_MIXER && !defined( _MINGW_VER ) && !_MSC_VER )
# include <stdio.h>
# include <stdlib.h>
# include <sys/ioctl.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <fcntl.h>
# include <unistd.h>
# include <string.h>
# ifdef __NetBSD__
# include <soundcard.h>
# endif
# ifdef __FreeBSD__
# include <machine/soundcard.h>
# endif
# ifdef __linux__
# include <linux/soundcard.h>
# endif
#endif
#include <bigloo.h>

typedef int bool;

/*---------------------------------------------------------------------*/
/*    A private mixer structure                                        */
/*---------------------------------------------------------------------*/
struct mixdev {
   bool support;
   bool stereo;
   bool recsrc;
   bool records;
   char *name;
   char *label;
   int value;
   int mask;
};

typedef struct mixer {
   header_t header;
   int mixfd;
   int mixfdopen;
   char *device;

   int nrdevices;       // maximum number of devices
   int devmask;         // supported devices
   int stmask;          // stereo devices
   int recmask;         // devices which can be recorded from
   int caps;            // capabilities
   int recsrc;          // devices which are being recorded from
   struct mixdev *mixdevs;
} *mixer_t;

/*---------------------------------------------------------------------*/
/*    exported functions                                               */
/*---------------------------------------------------------------------*/
extern obj_t bgl_open_mixer( char * );
extern obj_t bgl_close_mixer( obj_t );
extern int bgl_mixer_read_vol( obj_t, int, int );
extern obj_t bgl_mixer_write_vol( obj_t, int, int );
extern char *bgl_mixer_dev_name( obj_t, int );
extern int bgl_mixer_dev_num( obj_t );
extern bool bgl_mixer_dev_p( obj_t, int );

/*---------------------------------------------------------------------*/
/*    static functions                                                 */
/*---------------------------------------------------------------------*/
static void do_status( struct mixer * );

#if( !BGL_HAVE_MIXER || !(__NetBSD__ || __FreeBSD__ || __linux__ ) )
obj_t
bgl_open_mixer( char *dname ) {
   C_SYSTEM_FAILURE( BGL_IO_ERROR,
		     "open-mixer",
		     "operation not supported on this platform",
		     BUNSPEC );
}

obj_t
bgl_close_mixer( obj_t mixerobj ) {
   return BUNSPEC;
}

int
bgl_mixer_read_vol( obj_t mixerobj, int dev, int read ) {
   return -1;
}

obj_t
bgl_mixer_write_vol( obj_t mixerobj, int dev, int value ) {
   return BUNSPEC;
}

char *
bgl_mixer_dev_name( obj_t mixerobj, int dev ) {
   C_SYSTEM_FAILURE( BGL_IO_ERROR,
		     "mixer-dev-name",
		     "operation not supported on this platform",
		     BUNSPEC );
}

int
bgl_mixer_dev_num( obj_t mixerobj ) {
   return -1;
}

bool
bgl_mixer_devp( obj_t mixerobj, int dev ) {
   return 0;
}

#else

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_open_mixer ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_open_mixer( char *dname ) {
   struct mixer *mixer = (struct mixer *)GC_MALLOC( sizeof( struct mixer ) );

   mixer->header = MAKE_HEADER( OPAQUE_TYPE, 0 );
   mixer->device = (char *)GC_MALLOC( strlen( dname ) + 1 );
   strcpy( mixer->device, dname );

   if( mixer->mixfdopen = (mixer->mixfd = open( dname, O_NONBLOCK ) )!=-1 ) {
      char *devnames[] = SOUND_DEVICE_NAMES;
      char *devlabels[] = SOUND_DEVICE_LABELS;
      int mixmask = 1, i;
      
      mixer->nrdevices = SOUND_MIXER_NRDEVICES;

      ioctl( mixer->mixfd, SOUND_MIXER_READ_DEVMASK, &(mixer->devmask) );
      ioctl( mixer->mixfd, SOUND_MIXER_READ_STEREODEVS, &(mixer->stmask) );
      ioctl( mixer->mixfd, SOUND_MIXER_READ_RECMASK, &(mixer->recmask) );
      ioctl( mixer->mixfd, SOUND_MIXER_READ_CAPS, &(mixer->caps) );

      mixer->mixdevs = (struct mixdev *)GC_MALLOC( sizeof( struct mixdev ) *
						   mixer->nrdevices );
      
      for( i = 0; i < mixer->nrdevices; i++ ) {
            mixer->mixdevs[ i ].support = mixer->devmask & mixmask;
            mixer->mixdevs[ i ].stereo = mixer->stmask & mixmask;
            mixer->mixdevs[ i ].records = mixer->recmask & mixmask;
            mixer->mixdevs[ i ].mask = mixmask;
            mixer->mixdevs[ i ].name = devnames[i];
            mixer->mixdevs[ i ].label = devlabels[i];
            mixmask *= 2;
         }

      do_status( mixer );

      return (obj_t)mixer;
   } else {
      C_SYSTEM_FAILURE( BGL_IO_ERROR,
			"open-mixer",
			strerror( errno ),
			string_to_bstring( dname ) );

      return BUNSPEC;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_close_mixer ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_close_mixer( obj_t mixerobj ) {
   int i;

   mixer_t mixer = (mixer_t)mixerobj;
   ioctl( mixer->mixfd, SOUND_MIXER_READ_RECSRC, &(mixer->recsrc) );
   for( i = 0; i < mixer->nrdevices; i++ ) {
      if( mixer->mixdevs[ i ].support )
	 ioctl( mixer->mixfd, MIXER_READ( i ), &(mixer->mixdevs[ i ].value) );
      mixer->mixdevs[ i ].recsrc = (mixer->recsrc & mixer->mixdevs[ i ].mask);
   }

   close( mixer->mixfd );
   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    do_status ...                                                    */
/*---------------------------------------------------------------------*/
static void
do_status( mixer_t mixer ) {
   int i;
   
   ioctl( mixer->mixfd, SOUND_MIXER_READ_RECSRC, &(mixer->recsrc) );
   
   for( i = 0; i< mixer->nrdevices; i++ ) {
      if( mixer->mixdevs[ i ].support )
	 ioctl( mixer->mixfd, MIXER_READ( i ),  &(mixer->mixdevs[ i ].value) );
      mixer->mixdevs[ i ].recsrc = (mixer->recsrc & mixer->mixdevs[ i ].mask );
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_mixer_read_vol ...                                           */
/*    -------------------------------------------------------------    */
/*    Read the volume for a device.                                    */
/*---------------------------------------------------------------------*/
int
bgl_mixer_read_vol( obj_t mixerobj, int dev, int read ) {
  mixer_t mixer = (mixer_t)mixerobj;
  if( read )
      ioctl( mixer->mixfd, MIXER_READ( dev ), &(mixer->mixdevs[ dev ].value) );
   return mixer->mixdevs[ dev ].value;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_mixer_write_vol ...                                          */
/*---------------------------------------------------------------------*/
obj_t
bgl_mixer_write_vol( obj_t mixerobj, int dev, int value ) {
   mixer_t mixer = (mixer_t)mixerobj;
   mixer->mixdevs[ dev ].value = value;
   ioctl( mixer->mixfd, MIXER_WRITE( dev ), &(mixer->mixdevs[ dev ].value) );

   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    bgl_mixer_dev_name ...                                           */
/*---------------------------------------------------------------------*/
char *
bgl_mixer_dev_name( obj_t mixerobj, int dev ) {
   mixer_t mixer = (mixer_t)mixerobj;
   return mixer->mixdevs[ dev ].name;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_mixer_dev_num ...                                            */
/*---------------------------------------------------------------------*/
int
bgl_mixer_dev_num( obj_t mixerobj ) {
   mixer_t mixer = (mixer_t)mixerobj;
   return mixer->nrdevices;
}

/*---------------------------------------------------------------------*/
/*    bool                                                             */
/*    bgl_mixer_devp ...                                               */
/*---------------------------------------------------------------------*/
bool
bgl_mixer_devp( obj_t mixerobj, int dev ) {
   mixer_t mixer = (mixer_t)mixerobj;
   return (dev < mixer->nrdevices) && (mixer->mixdevs[ dev ].support);
}

#endif
