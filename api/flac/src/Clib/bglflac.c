/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/flac/src/Clib/bglflac.c          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jun 20 14:50:56 2011                          */
/*    Last change :  Thu Nov 21 08:44:35 2013 (serrano)                */
/*    Copyright   :  2011-13 Manuel Serrano                            */
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
extern obj_t bgl_flac_error_status( FLAC__StreamDecoderErrorStatus );
extern obj_t bgl_flac_decoder_read( BgL_flaczd2decoderzd2_bglt, long );
extern obj_t bgl_flac_decoder_write( BgL_flaczd2decoderzd2_bglt, long, long, long, long );
extern obj_t bgl_flac_decoder_meta( BgL_flaczd2decoderzd2_bglt, FLAC__uint64, long, long, long );
extern obj_t bgl_flac_decoder_tell( BgL_flaczd2decoderzd2_bglt );
extern obj_t bgl_flac_decoder_seek( BgL_flaczd2decoderzd2_bglt, BGL_LONGLONG_T );
extern obj_t bgl_flac_decoder_length( BgL_flaczd2decoderzd2_bglt );
extern int bgl_flac_debug();

/*---------------------------------------------------------------------*/
/*    decoder bigloo object                                            */
/*---------------------------------------------------------------------*/
#define BGL_DECODER_PORT( o ) \
   (((BgL_flaczd2decoderzd2_bglt)o)->BgL_portdz00)
#define BGL_DECODER_EOF( o ) \
   (((BgL_flaczd2decoderzd2_bglt)o)->BgL_z52eofz52)
#define BGL_DECODER_FLACBUF( o ) \
   (((BgL_flaczd2decoderzd2_bglt)o)->BgL_z52flacbufz52)
#define BGL_DECODER_OUTBUF( o ) \
   (BSTRING_TO_STRING( (((BgL_flaczd2decoderzd2_bglt)o)->BgL_outbufz00) ))
#define BGL_DECODER_SAMPLE( o ) \
   (((BgL_flaczd2decoderzd2_bglt)o)->BgL_z52samplez52)
#define BGL_DECODER_VOLUME( o ) \
   (((BgL_flaczd2decoderzd2_bglt)o)->BgL_z52volumez52)
#define BGL_DECODER_RCHECKSUM( o ) \
   (((BgL_flaczd2decoderzd2_bglt)o)->BgL_z52rchecksumz52)
#define BGL_DECODER_BCHECKSUM( o ) \
   (((BgL_flaczd2decoderzd2_bglt)o)->BgL_z52bchecksumz52)

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

#define FLAC_DEBUG 1
#undef FLAC_DEBUG

#if( defined( FLAC_DEBUG ) )
#define DEBUG_PATH "/tmp/BGLFLAC"   
extern int bgl_flac_checksum_debug( long, char *, long, long );
FILE *dbg_file = 0L;
int dbg_index;
long dbg_countread;
long dbg_countwrite;
#endif

/*---------------------------------------------------------------------*/
/*    FLAC__StreamDecoderInitStatus                                    */
/*    bgl_FLAC__stream_decoder_init_stream ...                         */
/*---------------------------------------------------------------------*/
FLAC__StreamDecoderInitStatus
bgl_FLAC__stream_decoder_init_stream( FLAC__StreamDecoder *decoder,
				      obj_t obj ) {
#if( defined( FLAC_DEBUG ) )
   dbg_file = fopen( DEBUG_PATH, "w" );
   fprintf( dbg_file, ";; index, count-read, *size, cres, count-write, rchecksum, bchecksum\n" );
   
   dbg_index = 0;
   dbg_countread = 0;
   dbg_countwrite = 0;
#endif   
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
   obj_t res;

   CUSTOM_IDENTIFIER( BGL_DECODER_FLACBUF( obj ) ) = buffer;
   res = bgl_flac_decoder_read( (BgL_flaczd2decoderzd2_bglt)obj, *size );

   if( EOF_OBJECTP( res ) ) {
      BGL_DECODER_EOF( obj ) = true;
      *size = 0;
      
      return FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM;
   } else {
      long cres = CINT( res );
      
      if( cres >= 0 ) {
#if( defined( FLAC_DEBUG ) )
	 if( bgl_flac_debug() >= 1 ) {
	    BGL_DECODER_RCHECKSUM( obj ) =
	       bgl_flac_checksum_debug( BGL_DECODER_RCHECKSUM( obj ), buffer, 0, cres );
	    if( dbg_file ) {
	       if( (cres != *size) || (BGL_DECODER_RCHECKSUM( obj ) != BGL_DECODER_BCHECKSUM( obj ) ) ) {
		  fprintf( dbg_file, ";; === ERROR ===================\n" );
	       }

	       dbg_countread += *size;
	       fprintf( dbg_file, "%d %ld %d %d %ld %d %d\n",
			dbg_index++, dbg_countread,
			*size, cres,
			dbg_countwrite,
			(unsigned char)BGL_DECODER_RCHECKSUM( obj ),
			(unsigned char)BGL_DECODER_BCHECKSUM( obj ) );
	       fflush( dbg_file );
	    }
	 }
#endif
	 *size = cres;
	 return FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
      } else {
	 *size = 0;
	 return FLAC__STREAM_DECODER_READ_STATUS_ABORT;
      }
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
   obj_t res = bgl_flac_decoder_seek( (BgL_flaczd2decoderzd2_bglt)obj, (BGL_LONGLONG_T)offset );

#if( defined( FLAC_DEBUG ) )
   if( bgl_flac_debug() >= 1 && dbg_file ) {
      fprintf( dbg_file, ";; ### SEEK offset=%d\n", offset );
   }
#endif	    
   if( res == BTRUE ) {
      return FLAC__STREAM_DECODER_SEEK_STATUS_OK;
   } else if( res == BFALSE ) {
      return FLAC__STREAM_DECODER_SEEK_STATUS_ERROR;
   } else {
      return FLAC__STREAM_DECODER_SEEK_STATUS_UNSUPPORTED;
   }
}

/*---------------------------------------------------------------------*/
/*    static FLAC__StreamDecoderTellStatus                             */
/*    bgl_tell_callback ...                                            */
/*---------------------------------------------------------------------*/
static FLAC__StreamDecoderTellStatus
bgl_tell_callback( const FLAC__StreamDecoder *decoder,
		   FLAC__uint64 *offset,
		   void *client_data ) {
   obj_t obj = (obj_t)client_data;
   obj_t res = bgl_flac_decoder_tell( (BgL_flaczd2decoderzd2_bglt)obj );

#if( defined( FLAC_DEBUG ) )
   if( bgl_flac_debug() >= 1 && dbg_file ) {
      fprintf( dbg_file, ";; ### TELL offset=%d\n", offset );
   }
#endif	    
   if( ELONGP( res ) ) {
      *offset = BELONG_TO_LONG( res );
      return FLAC__STREAM_DECODER_TELL_STATUS_OK;
   } else if( INTEGERP( res ) ) {
      *offset = CINT( res );
      return FLAC__STREAM_DECODER_TELL_STATUS_OK;
   } else {
      *offset = -1;
      if( BOOLEANP( res ) ) {
	 return FLAC__STREAM_DECODER_TELL_STATUS_UNSUPPORTED;
      } else {
	 return FLAC__STREAM_DECODER_TELL_STATUS_ERROR;
      }
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

   obj_t obj = (obj_t)client_data;

   *offset = BELONG_TO_LONG( bgl_flac_decoder_length( (BgL_flaczd2decoderzd2_bglt)obj ) );

   return ( *offset >= 0 ) ?
      FLAC__STREAM_DECODER_LENGTH_STATUS_OK :
      FLAC__STREAM_DECODER_LENGTH_STATUS_UNSUPPORTED;
}
   
/*---------------------------------------------------------------------*/
/*    static FLAC__bool                                                */
/*    bgl_eof_callback ...                                             */
/*---------------------------------------------------------------------*/
static FLAC__bool
bgl_eof_callback( const FLAC__StreamDecoder *decoder,
		  void *client_data ) {
   obj_t obj = (obj_t)client_data;

   return BGL_DECODER_EOF( obj ) ? true : false;
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
   FLAC__FrameHeader h = frame->header;
   obj_t obj = (obj_t)client_data;
   float vol = BGL_DECODER_VOLUME( obj );
   long i = 0;
   
   switch( h.bits_per_sample ) {
      case 16: {
	 long sample;
	 char *buf = (unsigned char *)BGL_DECODER_OUTBUF( obj );

	 for( sample = 0; sample < h.blocksize; sample++ ) {
	    long channel;

	    for( channel = 0; channel < h.channels; channel++ ) {
	       FLAC__int16 v = vol * (FLAC__int16)(buffer[ channel ][ sample ]);
	       buf[ i++ ] = (unsigned char)(v & 0xff);
	       buf[ i++ ] = (unsigned char)((v >> 8) & 0xff);
	    }
	 }

	 break;
      }
      case 24: {
	 long sample;
	 unsigned char *buf = (unsigned char *)BGL_DECODER_OUTBUF( obj );

	 for( sample = 0; sample < h.blocksize; sample++ ) {
	    long channel;

	    for( channel = 0; channel < h.channels; channel++ ) {
	       FLAC__uint32 v = vol * (FLAC__uint32)buffer[ channel ][ sample ];
	       buf[ i++ ] = (v >> 0) & 0xff;
	       buf[ i++ ] = (v >> 8) & 0xff;
	       buf[ i++ ] = (v >> 16) & 0xff;
	    }
	 }
	 break;
      }
	 
      default: {
	 char msg[ 128 ];

	 sprintf( msg, "Bit rate unsupported: %d\n", h.bits_per_sample );
	 bgl_flac_error( "flac-decoder-decode", msg, obj );
      }
   }

#if( defined( FLAC_DEBUG ) )
   dbg_countwrite += i;
#endif
   
   if( h.number_type == FLAC__FRAME_NUMBER_TYPE_FRAME_NUMBER ) {
      BGL_DECODER_SAMPLE( obj ) = h.number.frame_number;
   } else {
      BGL_DECODER_SAMPLE( obj ) = h.number.sample_number;
   }
   
   if( CBOOL( bgl_flac_decoder_write( (BgL_flaczd2decoderzd2_bglt)obj,
				      i, 
				      h.sample_rate,
				      h.channels,
				      h.bits_per_sample ) ) ) {
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

   if( metadata->type == FLAC__METADATA_TYPE_STREAMINFO )  {
      bgl_flac_decoder_metadata( (BgL_flaczd2decoderzd2_bglt)obj,
				 metadata->data.stream_info.total_samples,
				 metadata->data.stream_info.sample_rate,
				 metadata->data.stream_info.channels,
				 metadata->data.stream_info.bits_per_sample );
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
   char *msg;
   obj_t res;

   switch( status ) {
      case FLAC__STREAM_DECODER_ERROR_STATUS_LOST_SYNC:
	 msg = "lost sync"; break;
      case FLAC__STREAM_DECODER_ERROR_STATUS_BAD_HEADER:
	 msg = "bad header"; break;
      case FLAC__STREAM_DECODER_ERROR_STATUS_FRAME_CRC_MISMATCH:
	 msg = "frame crc mismatch"; break;
      case FLAC__STREAM_DECODER_ERROR_STATUS_UNPARSEABLE_STREAM:
	 msg = "unparseable stream"; break;
      default:
	 msg = "unknown error";
   }
   
#if( defined( FLAC_DEBUG ) )
   if( !access( DEBUG_PATH, F_OK ) ) {
      char buf[ 100 ];

      if( dbg_file ) {
	 fprintf( dbg_file, "%d %ld %ld %s\n",
		  dbg_index, dbg_countread, dbg_countwrite,
		  msg );
	 fflush( dbg_file );
	 fclose( dbg_file );
	 dbg_file = 0L;
      }
	 
      sprintf( buf, "%s.bck", DEBUG_PATH );
      rename( DEBUG_PATH, buf );
   }
#endif   

   if( status != FLAC__STREAM_DECODER_ERROR_STATUS_FRAME_CRC_MISMATCH ) {
      bgl_flac_error( "flac-decoder", msg, obj );
   } else {
#if( defined( FLAC_DEBUG ) )
      fprintf( stderr, "flac crc mismatch: %ld\n", dbg_countread );
#else
      bgl_flac_error( "flac-decoder", msg, obj );
#endif      
   }
}
