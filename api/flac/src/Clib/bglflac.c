/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/flac/src/Clib/bglflac.c          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jun 20 14:50:56 2011                          */
/*    Last change :  Tue Jun 28 17:38:29 2011 (serrano)                */
/*    Copyright   :  2011 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    flac Bigloo binding                                              */
/*=====================================================================*/
#include <FLAC/stream_decoder.h>
#include <bigloo.h>
#include "bglflac.h"
#include "bgldecoder.h"

/*---------------------------------------------------------------------*/
/*    decoder bigloo object                                            */
/*---------------------------------------------------------------------*/
#define BGL_DECODER_READ( o ) \
   (((BgL_flaczd2decoderzd2_bglt)o)->BgL_readz00)
#define BGL_DECODER_SEEK( o ) \
   (((BgL_flaczd2decoderzd2_bglt)o)->BgL_seekz00)
#define BGL_DECODER_TELL( o ) \
   (((BgL_flaczd2decoderzd2_bglt)o)->BgL_tellz00)
#define BGL_DECODER_LENGTH( o ) \
   (((BgL_flaczd2decoderzd2_bglt)o)->BgL_lengthz00)
#define BGL_DECODER_EOF( o ) \
   (((BgL_flaczd2decoderzd2_bglt)o)->BgL_eofz00)
#define BGL_DECODER_WRITE( o ) \
   (((BgL_flaczd2decoderzd2_bglt)o)->BgL_writez00)
#define BGL_DECODER_METADATA( o ) \
   (((BgL_flaczd2decoderzd2_bglt)o)->BgL_metadataz00)
#define BGL_DECODER_ERROR( o ) \
   (((BgL_flaczd2decoderzd2_bglt)o)->BgL_errorz00)

/*---------------------------------------------------------------------*/
/*    Local declarations                                               */
/*---------------------------------------------------------------------*/
static FLAC__StreamDecoderReadStatus
bgl_read_callback( const FLAC__StreamDecoder *,
		   FLAC__byte[],
		   size_t *,
		   void * );

static FLAC__StreamDecoderSeekStatus
bgl_seek_callback( const FLAC__StreamDecoder *,
		   FLAC__uint64,
		   void * );

static FLAC__StreamDecoderTellStatus
bgl_tell_callback( const FLAC__StreamDecoder *,
		   FLAC__uint64 *,
		   void * );

static FLAC__StreamDecoderLengthStatus
bgl_length_callback( const FLAC__StreamDecoder *,
		     FLAC__uint64 *,
		     void *client_data );

static FLAC__bool
bgl_eof_callback( const FLAC__StreamDecoder *,
		  void * );

static FLAC__StreamDecoderWriteStatus
bgl_write_callback( const FLAC__StreamDecoder *,
		    const FLAC__Frame *,
		    const FLAC__int32 * const [],
		    void * );

static void
bgl_metadata_callback( const FLAC__StreamDecoder *,
		       const FLAC__StreamMetadata *,
		       void *client_data );

static void
bgl_error_callback( const FLAC__StreamDecoder *,
		    FLAC__StreamDecoderErrorStatus,
		    void *client_data );

/*---------------------------------------------------------------------*/
/*    FLAC__StreamDecoderInitStatus                                    */
/*    bgl_FLAC__stream_decoder_init_stream ...                         */
/*---------------------------------------------------------------------*/
FLAC__StreamDecoderInitStatus
bgl_FLAC__stream_decoder_init_stream( FLAC__StreamDecoder *decoder,
				      obj_t obj ) {
   return FLAC__stream_decoder_init_stream(
      decoder,
      bgl_read_callback,
      bgl_seek_callback,
      bgl_tell_callback,
      bgl_length_callback,
      bgl_eof_callback,
      bgl_write_callback,
      bgl_metadata_callback,
      bgl_error_callback,
      (void *)obj );
}

/*---------------------------------------------------------------------*/
/*    static FLAC__StreamDecoderReadStatus                             */
/*    bgl_read_callback ...                                            */
/*---------------------------------------------------------------------*/
static FLAC__StreamDecoderReadStatus
bgl_read_callback( const FLAC__StreamDecoder *decoder,
		   FLAC__byte buffer[],
		   size_t *size,
		   void *client_data ) {
   obj_t obj = (obj_t)client_data;
   obj_t proc = BGL_DECODER_READ( obj );

   if( 1 ) {
      return FLAC__STREAM_DECODER_READ_STATUS_ABORT;
   }
   if( 2 ) {
      return FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM;
   }
   if( 3 ) {
      return FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
   }

   return FLAC__STREAM_DECODER_READ_STATUS_ABORT;
}

/*---------------------------------------------------------------------*/
/*    static FLAC__StreamDecoderSeekStatus                             */
/*    bgl_seek_callback ...                                            */
/*---------------------------------------------------------------------*/
static FLAC__StreamDecoderSeekStatus
bgl_seek_callback( const FLAC__StreamDecoder *decoder,
		   FLAC__uint64 offset,
		   void *client_data ) {
   obj_t obj = (obj_t)client_data;
   obj_t proc = BGL_DECODER_SEEK( obj );

   PROCEDURE_ENTRY( proc )( proc, obj, BEOA );

   if( 1 ) return FLAC__STREAM_DECODER_SEEK_STATUS_OK;
   if( 2 ) return FLAC__STREAM_DECODER_SEEK_STATUS_ERROR;
   return FLAC__STREAM_DECODER_SEEK_STATUS_UNSUPPORTED;
}

/*---------------------------------------------------------------------*/
/*    static FLAC__StreamDecoderTellStatus                             */
/*    bgl_tell_callback ...                                            */
/*---------------------------------------------------------------------*/
static FLAC__StreamDecoderTellStatus
bgl_tell_callback( const FLAC__StreamDecoder *decoder,
		   FLAC__uint64 *offset,
		   void *client_data ) {
   if( 1 ) 
     return FLAC__STREAM_DECODER_TELL_STATUS_UNSUPPORTED;
   if( 2 )
      return FLAC__STREAM_DECODER_TELL_STATUS_ERROR;
   if( 3 ) {
      *offset = 28;
      return FLAC__STREAM_DECODER_TELL_STATUS_OK;
   }
}
   
/*---------------------------------------------------------------------*/
/*    static FLAC__StreamDecoderLengthStatus                           */
/*    bgl_length_callback ...                                          */
/*---------------------------------------------------------------------*/
static FLAC__StreamDecoderLengthStatus
bgl_length_callback( const FLAC__StreamDecoder *decoder,
		   FLAC__uint64 *offset,
		   void *client_data ) {
   if( 1 ) 
     return FLAC__STREAM_DECODER_LENGTH_STATUS_UNSUPPORTED;
   if( 2 )
      return FLAC__STREAM_DECODER_LENGTH_STATUS_ERROR;
   if( 3 ) {
      *offset = 28;
      return FLAC__STREAM_DECODER_LENGTH_STATUS_OK;
   }
}
   
/*---------------------------------------------------------------------*/
/*    static FLAC__bool                                                */
/*    bgl_eof_callback ...                                             */
/*---------------------------------------------------------------------*/
static FLAC__bool
bgl_eof_callback( const FLAC__StreamDecoder *decoder,
		  void *client_data ) {
   obj_t obj = (obj_t)client_data;
   obj_t proc = BGL_DECODER_EOF( obj );

   return CBOOL( PROCEDURE_ENTRY( proc )( proc, obj, BEOA ) ) ? true : false;
}

/*---------------------------------------------------------------------*/
/*    static FLAC__StreamDecoderWriteStatus                            */
/*    bgl_write_callback ...                                           */
/*---------------------------------------------------------------------*/
static FLAC__StreamDecoderWriteStatus
bgl_write_callback( const FLAC__StreamDecoder *decoder,
		    const FLAC__Frame *frame,
		    const FLAC__int32 *const buffer[],
		    void *client_data ) {
   obj_t obj = (obj_t)client_data;
   obj_t proc = BGL_DECODER_WRITE( obj );

   if( CBOOL( PROCEDURE_ENTRY( proc )( proc, obj, BEOA ) ) ) {
      return FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE;
   } else {
      return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_metadata_callback ...                                        */
/*---------------------------------------------------------------------*/
static void
bgl_metadata_callback( const FLAC__StreamDecoder *decoder,
		       const FLAC__StreamMetadata *metadata,
		       void *client_data ) {
   obj_t obj = (obj_t)client_data;
   obj_t proc = BGL_DECODER_METADATA( obj );

   if( PROCEDUREP( proc ) ) {
      PROCEDURE_ENTRY( proc )( proc, obj, BEOA );
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_error_callback ...                                           */
/*---------------------------------------------------------------------*/
static void
bgl_error_callback( const FLAC__StreamDecoder *decoder,
		    const FLAC__StreamDecoderErrorStatus status,
		    void *client_data ) {
   obj_t obj = (obj_t)client_data;
   obj_t proc = BGL_DECODER_ERROR( obj );

   if( PROCEDUREP( proc ) ) {
      PROCEDURE_ENTRY( proc )( proc, obj, BEOA );
   }
}
