/*=====================================================================*/
/*    .../prgm/project/bigloo/bigloo/api/flac/src/Clib/bglflac.h       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 21 08:20:23 2011                          */
/*    Last change :  Fri Dec  8 17:22:20 2023 (serrano)                */
/*    Copyright   :  2011-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    flac C prototypes                                                */
/*=====================================================================*/
#ifndef BGL_LIBFLAC
#define BGL_LIBFLAC

extern FLAC__StreamDecoderInitStatus
bgl_FLAC__stream_decoder_init_stream( FLAC__StreamDecoder *, obj_t );
extern FLAC__StreamDecoderInitStatus
bgl_FLAC__stream_decoder_init_stream16( FLAC__StreamDecoder *, obj_t );

#define BGL_FLAC_BLIT_STRING( s1, o1, s2, o2, len ) \
   memcpy( ((char *)s2) + o2, ((char *)s1) + o1, len )

#define BGL_FLAC_STRING_REF( s, o ) \
   (((unsigned char *)s)[ o ])
  
#endif
