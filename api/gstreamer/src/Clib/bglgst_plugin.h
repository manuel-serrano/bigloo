/*=====================================================================*/
/*    .../project/bigloo/api/gstreamer/src/Clib/bglgst_plugin.h        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Jan 11 11:28:42 2008                          */
/*    Last change :  Tue May 27 14:53:35 2008 (serrano)                */
/*    Copyright   :  2008 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    GSTREAMER plugin interface.                                      */
/*=====================================================================*/
#ifndef BGLGST_PLUGIN_H 
#define BGLGST_PLUGIN_H

/*---------------------------------------------------------------------*/
/*    Mixer                                                            */
/*---------------------------------------------------------------------*/
#if( BGL_GSTREAMER_HAVE_AUDIO )
#include <gst/interfaces/mixer.h>
#else
#  define GstMixerTrack GstObject
#  define GstMixer GstObject
#endif

extern obj_t bgl_gst_mixer_track_new( GstMixerTrack *, obj_t );
extern obj_t bgl_gst_mixer_get_track( GstElement *, char *, obj_t  );
extern obj_t bgl_gst_mixer_track_list( GstElement *, obj_t  );

extern obj_t bgl_gst_mixer_get_volume( GstElement *, GstMixerTrack * );
extern obj_t bgl_gst_mixer_set_volume( GstElement *, GstMixerTrack *, obj_t );

#endif
