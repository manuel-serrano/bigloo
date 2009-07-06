/*=====================================================================*/
/*    .../prgm/project/bigloo/api/gstreamer/src/Plugin/gstbigloo.h     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Feb 12 14:36:31 2008                          */
/*    Last change :  Tue Feb 12 14:38:18 2008 (serrano)                */
/*    Copyright   :  2008 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    gstbigloo.h                                                      */
/*=====================================================================*/

#ifndef __GST_BIGLOO_H__
#define __GST_BIGLOO_H__

#include <gst/gst.h>

G_BEGIN_DECLS

/*---------------------------------------------------------------------*/
/*    Misc macros                                                      */
/*---------------------------------------------------------------------*/
#define GST_TYPE_BIGLOO \
  (gst_bigloo_get_type())
#define GST_BIGLOO(obj) \
  (G_TYPE_CHECK_INSTANCE_CAST((obj),GST_TYPE_BIGLOO,Gstbigloo))
#define GST_BIGLOO_CLASS(klass) \
  (G_TYPE_CHECK_CLASS_CAST((klass),GST_TYPE_BIGLOO,GstbiglooClass))
#define GST_IS_BIGLOO(obj) \
  (G_TYPE_CHECK_INSTANCE_TYPE((obj),GST_TYPE_BIGLOO))
#define GST_IS_BIGLOO_CLASS(klass) \
  (G_TYPE_CHECK_CLASS_TYPE((klass),GST_TYPE_BIGLOO))

/*---------------------------------------------------------------------*/
/*    Gstbigloo                                                        */
/*---------------------------------------------------------------------*/
typedef struct _Gstbigloo {
   GstElement element;
   GstPad *sinkpad, *srcpad;
   gboolean silent;
   obj_t procedure;
} Gstbigloo;

/*---------------------------------------------------------------------*/
/*    GstbiglooClass                                                   */
/*---------------------------------------------------------------------*/
typedef struct _GstbiglooClass {
   GstElementClass parent_class;
} GstbiglooClass;

/*---------------------------------------------------------------------*/
/*    Global procedures                                                */
/*---------------------------------------------------------------------*/
GType gst_bigloo_get_type( void );

G_END_DECLS

#endif /* __GST_BIGLOO_H__ */
