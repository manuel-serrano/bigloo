/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/flac/src/Clib/bglflac.c          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jun 20 14:50:56 2011                          */
/*    Last change :  Mon Jul  4 08:04:55 2011 (serrano)                */
/*    Copyright   :  2011 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    flac Bigloo binding                                              */
/*=====================================================================*/
#include <FLAC/stream_decoder.h>
#include <bigloo.h>
#include "bgldecoder.h"
#include "bglflac.h"

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
extern int bgl_flac_error( char *, char *, obj_t );
extern obj_t bgl_flac_decoder_read( BgL_flaczd2decoderzd2_bglt, long );

/*---------------------------------------------------------------------*/
/*    decoder bigloo object                                            */
/*---------------------------------------------------------------------*/
#define BGL_DECODER_PORT( o ) \
   (((BgL_flaczd2decoderzd2_bglt)o)->BgL_portdz00)
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
#define BGL_DECODER_FLACBUFFER( o ) \
   (((BgL_flaczd2decoderzd2_bglt)o)->BgL_z52flacbufferz52)

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
   obj_t dec = (obj_t)client_data;
   obj_t res;
   
   res = bgl_flac_decoder_read( (BgL_flaczd2decoderzd2_bglt)dec, *size );

   if( EOF_OBJECTP( res ) ) {
      *size = 0;
      return FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM;
   } else {
      *size = CINT( res );
      return FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
   }
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
