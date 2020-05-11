/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/gstreamer/src/Clib/bglgst.h      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Dec 30 08:37:50 2007                          */
/*    Last change :  Mon Jul 28 09:13:38 2008 (serrano)                */
/*    Copyright   :  2007-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The source file for gst.sch.                                     */
/*=====================================================================*/
#ifndef BGLGST_H 
#define BGLGST_H

#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    Misc                                                             */
/*---------------------------------------------------------------------*/
extern void bgl_gst_init( obj_t );

extern void bgl_gst_add_finalizer( obj_t, obj_t );
extern void bgl_gst_invoke_finalizer();

extern int bgl_gst_use_threads;

/*---------------------------------------------------------------------*/
/*    Bigloo constructors (used by C code)                             */
/*---------------------------------------------------------------------*/
extern obj_t bgl_gst_element_new( GstElement *, obj_t );
extern obj_t bgl_gst_element_factory_new( GstElementFactory *, obj_t );
extern obj_t bgl_gst_plugin_new( GstPlugin *, obj_t );
extern obj_t bgl_gst_plugin_feature_new( GstPluginFeature *, obj_t );
extern obj_t bgl_gst_pad_new( GstPad *, obj_t );
extern obj_t bgl_gst_bin_new( GstBin *, obj_t );
extern obj_t bgl_gst_bus_new( GstBus *, obj_t );
extern obj_t bgl_gst_message_new( GstMessage *, obj_t );
extern obj_t bgl_gst_caps_new( GstCaps *, obj_t );
extern obj_t bgl_gst_buffer_new( GstBuffer *, obj_t );
extern obj_t bgl_gst_pipeline_new( GstPipeline *, obj_t );
extern obj_t bgl_gst_static_pad_template_new( GstStaticPadTemplate * );

/*---------------------------------------------------------------------*/
/*    Exports                                                          */
/*---------------------------------------------------------------------*/
extern bool_t bgl_gst_objectp( obj_t );
extern obj_t bgl_gst_object_to_obj( GstObject *, obj_t );
extern GstObject *bgl_gst_object_to_gstobject();
extern obj_t bgl_closure_gcmark( obj_t );
extern obj_t bgl_closure_gcunmark( obj_t );
extern obj_t bgl_gst_lock();
extern obj_t bgl_gst_unlock();
extern obj_t bgl_gst_signal();

/*---------------------------------------------------------------------*/
/*    Add Bigloo RTS functions                                         */
/*---------------------------------------------------------------------*/
extern void bgl_gst_invoke_callbacks();

/*---------------------------------------------------------------------*/
/*    Bigloo wrappers                                                  */
/*---------------------------------------------------------------------*/
extern obj_t bgl_gst_thread_init();

extern obj_t bgl_gst_registry_get_element_factory_list( GstRegistry * );
extern obj_t bgl_gst_registry_get_feature_list_by_plugin( GstRegistry *, char * );
extern obj_t bgl_gst_registry_get_plugin_list( GstRegistry * );

extern obj_t bgl_gst_element_interface_list( GstElement * );

extern obj_t bgl_gst_element_factory_get_uri_protocols( GstElementFactory * );

extern obj_t bgl_gst_element_factory_get_static_pad_templates( GstElementFactory * );

extern obj_t bgl_gst_caps_new_simple( obj_t, obj_t, obj_t );

extern obj_t bgl_gst_buffer_get_string( GstBuffer * );
extern void bgl_gst_buffer_set_string( GstBuffer *, obj_t );

extern int bgl_gst_pad_add_probe( GstPad *, GstPadProbeType, obj_t );
extern gboolean bgl_gst_pad_set_caps( GstPad *pad, GstCaps *caps );

extern obj_t bgl_gst_message_tag_list( GstMessage * );
extern char *bgl_gst_message_error_string( GstMessage * );
extern char *bgl_gst_message_info_string( GstMessage * );
extern char *bgl_gst_message_warning_string( GstMessage * );
extern GstState bgl_gst_message_new_state( GstMessage * );
extern GstState bgl_gst_message_old_state( GstMessage * );
extern GstState bgl_gst_message_pending_state( GstMessage * );
extern obj_t bgl_gst_message_get_src( GstMessage * );
extern int bgl_gst_message_stream_status_type( GstMessage * );

extern obj_t bgl_gst_object_property_list( GstObject * );
extern obj_t bgl_gst_object_get_property( GstObject *, char * );
extern obj_t bgl_gst_object_set_property( GstObject *, char *, obj_t );
extern obj_t bgl_gst_object_connect( GstObject *, char *, obj_t );

extern BGL_LONGLONG_T bgl_gst_element_query_position( GstElement * );
extern BGL_LONGLONG_T bgl_gst_element_query_duration( GstElement * );

extern obj_t bgl_gst_structure_get_property( GstStructure *, char * );
extern obj_t bgl_gst_structure_set_property( GstStructure *, char *, obj_t );
extern obj_t bgl_gst_structure_property_list( GstStructure * );

extern obj_t bgl_gst_state_to_obj( GstState );

extern obj_t bgl_gst_type_find_new( GstTypeFind * );

extern obj_t bgl_gst_parse_launch( char * );
extern obj_t bgl_gst_parse_launchv( obj_t );

extern void bgl_gst_bus_set_sync_handler( GstBus *, obj_t );

#endif
