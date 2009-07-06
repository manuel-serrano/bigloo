/*=====================================================================*/
/*    .../project/bigloo/api/gstreamer/src/Plugin/bglgst_port.h        */
/*    -------------------------------------------------------------    */
/*    Author      :  Cyprien Nicolas                                   */
/*    Creation    :  Tue Jul 01 15:33:12 2008                          */
/*    Last change :  Sat Sep  6 15:17:43 2008 (serrano)                */
/*    Copyright   :  2008 Cyprien Nicolas                              */
/*    -------------------------------------------------------------    */
/*    Common header file for src and sink port plugin                  */
/*=====================================================================*/
#ifndef __BGLGST_PORT_H__
#define __BGLGST_PORT_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <stdlib.h>
#include <string.h>

#include <gst/gst.h>
#include <gst/base/gstbasesrc.h>
#include <gst/base/gstbasesink.h>

#include <bigloo.h>
#include "bglgst_config.h"

#define PLUGIN_VERSION_MAJOR GST_VERSION_MAJOR
#define PLUGIN_VERSION_MINOR GST_VERSION_MINOR
#define PLUGIN_NAME "biglooports"
#define PLUGIN_DESC "GStreamer binding for bigloo's ports"
#define PLUGIN_VERSION BGL_RELEASE_NUMBER
#define PLUGIN_LICENSE "GPL"
#define PLUGIN_PACKAGE "Bigloo"
#define PLUGIN_URL BGL_HOMEURL

#define PACKAGE PLUGIN_PACKAGE

G_BEGIN_DECLS

/*---------------------------------------------------------------------*/
/*    Misc macros                                                      */
/*---------------------------------------------------------------------*/
#define GST_TYPE_BGL_GST_PORT_SRC \
  (bgl_gst_port_src_get_type())
#define BGL_GST_PORT_SRC(obj) \
  (G_TYPE_CHECK_INSTANCE_CAST((obj),GST_TYPE_BGL_GST_PORT_SRC,BglPortSrc))
#define BGL_GST_PORT_SRC_CLASS(klass) \
  (G_TYPE_CHECK_CLASS_CAST((klass),GST_TYPE_BGL_GST_PORT_SRC,BglPortSrcClass))
#define GST_IS_BGL_GST_PORT_SRC(obj) \
  (G_TYPE_CHECK_INSTANCE_TYPE((obj),GST_TYPE_BGL_GST_PORT_SRC))
#define GST_IS_BGL_GST_PORT_SRC_CLASS(klass) \
  (G_TYPE_CHECK_CLASS_TYPE((klass),GST_TYPE_BGL_GST_PORT_SRC))


#define GST_TYPE_BGL_GST_PORT_SINK \
  (bgl_gst_port_sink_get_type())
#define BGL_GST_PORT_SINK(obj) \
  (G_TYPE_CHECK_INSTANCE_CAST((obj),GST_TYPE_BGL_GST_PORT_SINK,BglPortSink))
#define BGL_GST_PROT_SINK_CLASS(klass) \
  (G_TYPE_CHECK_CLASS_CAST((klass),GST_TYPE_BGL_GST_PORT_SINK,BglPortSinkClass))
#define GST_IS_BGL_GST_PORT_SINK(obj) \
  (G_TYPE_CHECK_INSTANCE_TYPE((obj),GST_TYPE_BGL_GST_PORT_SINK))
#define GST_IS_BGL_GST_PORT_SINK_CLASS(klass) \
  (G_TYPE_CHECK_CLASS_TYPE((klass),GST_TYPE_BGL_GST_PORT_SINK))

/*---------------------------------------------------------------------*/
/*    BglPortSrc                                                       */
/*---------------------------------------------------------------------*/
typedef struct _BglPortSrc {
   GstBaseSrc element;

   /*< private >*/
   gboolean has_loop;
   gboolean has_getrange;

   guint sizemin;
   guint sizemax;
   GstBuffer *parent;
   guint parentsize;
   guint parentoffset;
   gint datarate;
   gboolean sync;
   GstClock *clock;

   gint num_buffers;
   gint rt_num_buffers; /* we are going to change this at runtime */
   gint64 buffer_count;
   gboolean silent;
   gboolean signal_handoffs;
   gboolean dump;
   gboolean can_activate_pull;
   GstFormat format;

   guint64 bytes_sent;

   gchar *last_message;

   obj_t port; /* the under or upper laying Bigloo input port */
   obj_t buffer;
   gchar *uri;
} BglPortSrc;

/*---------------------------------------------------------------------*/
/*    BglPortSrcClass                                                  */
/*---------------------------------------------------------------------*/
typedef struct _BglPortSrcClass {
   GstBaseSrcClass parent_class;
  
   /*< public >*/
   /* signals */
   void (*handoff) (GstElement *element, GstBuffer *buf, GstPad *pad);
} BglPortSrcClass;

/*---------------------------------------------------------------------*/
/*    BglPortSink                                                      */
/*---------------------------------------------------------------------*/
typedef struct _BglPortSink {
   GstBaseSink parent;
  
   /*< private >*/
   obj_t port;
   obj_t buffer;
   gchar *uri; 
   guint64 bytes_written;
   gboolean autoclose;

} BglPortSink;

/*---------------------------------------------------------------------*/
/*    BglPortSinkClass                                                 */
/*---------------------------------------------------------------------*/
typedef struct _BglPortSinkClass {
   GstBaseSinkClass parent_class;
} BglPortSinkClass;

/*---------------------------------------------------------------------*/
/*    Global procedures                                                */
/*---------------------------------------------------------------------*/
GType bgl_gst_port_src_get_type( void );
GType bgl_gst_port_sink_get_type( void );

gboolean bgl_gst_plugin_port_init();

G_END_DECLS

#endif /* __BGLGST_PORT_H__ */
