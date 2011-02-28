/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bde/bmem/lib/thread.c                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jul  2 16:26:24 2003                          */
/*    Last change :  Fri Aug 22 08:00:17 2008 (serrano)                */
/*    Copyright   :  2003-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Threads monitoring                                               */
/*=====================================================================*/
#define THE_GC NO_GC
#include <bigloo.h>
#include <bmem.h>

/*---------------------------------------------------------------------*/
/*    Number of context switches                                       */
/*---------------------------------------------------------------------*/
static long context_switches = 0;
static long scheduler_awake = 0;

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglasync_scheduler_notify ...                                    */
/*    -------------------------------------------------------------    */
/*    Accumulate the number of times a scheduler is notified           */
/*    by an asynchronous signal.                                       */
/*---------------------------------------------------------------------*/
void
bglasync_scheduler_notify( void *scdl ) {
   scheduler_awake++;
   ____bglasync_scheduler_notify( scdl );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglthread_switch ...                                             */
/*    -------------------------------------------------------------    */
/*    Accumulates the number of fair context switches.                 */
/*---------------------------------------------------------------------*/
void
bglthread_switch( void *this, void *next ) {
   context_switches++;
   ____bglthread_switch( this, next );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    thread_dump_statistics ...                                       */
/*---------------------------------------------------------------------*/
void
thread_dump_statistics( FILE *f ) {
   if( context_switches || scheduler_awake ) {
      fprintf( f, "  (thread (context-switches %ld) (scheduler-awake %ld))\n",
	       context_switches, scheduler_awake );
      fprintf( stderr, "thread...(context switches=%ld, scheduler awake=%ld)\n",
	       context_switches,
	       scheduler_awake );
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    thread_reset_statistics ...                                      */
/*---------------------------------------------------------------------*/
void
thread_reset_statistics() {
   context_switches = 0;
   scheduler_awake = 0;
}

      

   
