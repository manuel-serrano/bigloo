/*=====================================================================*/
/*    .../project/bigloo/api/gstreamer/src/Plugin/bglgst_port.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Cyprien Nicolas                                   */
/*    Creation    :  Tue Jul 29 10:08:12 2008                          */
/*    Last change :  Thu Mar 22 15:33:11 2012 (serrano)                */
/*    Copyright   :  2008-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo plugin declaration.                                       */
/*    -------------------------------------------------------------    */
/*    GStreamer                                                        */
/*    Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>      */
/*                       2000 Wim Taymans <wim@fluendo.com>            */
/*                                                                     */
/*    This library is free software; you can redistribute it and/or    */
/*    modify it under the terms of the GNU Library General Public      */
/*    License as published by the Free Software Foundation; either     */
/*    version 2 of the License, or (at your option) any later version. */
/*---------------------------------------------------------------------*/
#include "bglgst_port.h"

struct _elements_entry {
   const gchar *name;
   guint rank;
   GType (*type) (void);
};

static struct _elements_entry _elements[] = {
   { "bglportsrc", GST_RANK_PRIMARY, bgl_gst_port_src_get_type },
   { "bglportsink", GST_RANK_PRIMARY, bgl_gst_port_sink_get_type },
   { NULL, 0 },
};

/*---------------------------------------------------------------------*/
/*    static gboolean                                                  */
/*    plugin_init ...                                                  */
/*---------------------------------------------------------------------*/
static gboolean
plugin_init( GstPlugin * plugin ) {
   struct _elements_entry *my_elements = _elements;

   while( (*my_elements).name ) {
      if( !gst_element_register( plugin,
				 (*my_elements).name, (*my_elements).rank,
				 ((*my_elements).type) ()) )
	 return FALSE;
      my_elements++;
   }

   return TRUE;
}

/*---------------------------------------------------------------------*/
/*    gboolean                                                         */
/*    bgl_gst_plugin_port_init ...                                     */
/*---------------------------------------------------------------------*/
gboolean
bgl_gst_plugin_port_init() {
   return gst_plugin_register_static( PLUGIN_VERSION_MAJOR,
				      PLUGIN_VERSION_MINOR,
				      PLUGIN_NAME,
				      PLUGIN_DESC,
				      plugin_init,
				      PLUGIN_VERSION,
				      PLUGIN_LICENSE,
				      PACKAGE,
				      PLUGIN_PACKAGE,
				      PLUGIN_URL );
}
