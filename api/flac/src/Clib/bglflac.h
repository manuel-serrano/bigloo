/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/flac/src/Clib/bglflac.h          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 21 08:20:23 2011                          */
/*    Last change :  Wed Jul 13 07:58:06 2011 (serrano)                */
/*    Copyright   :  2011 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    flac C prototypes                                                */
/*=====================================================================*/
#ifndef BGL_LIBFLAC
#define BGL_LIBFLAC

extern FLAC__StreamDecoderInitStatus
bgl_FLAC__stream_decoder_init_stream( FLAC__StreamDecoder *, obj_t );

#endif
