;*=====================================================================*/
;*    .../prgm/project/bigloo/api/pthread/src/Llib/psemaphore.sch      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Mar  5 14:48:55 2005                          */
;*    Last change :  Wed May 17 09:26:48 2017 (serrano)                */
;*    Copyright   :  2005-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The native interfaces for semaphores                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives 
   
   (extern  (include "bglpthread.h")

	    ($psemaphore-open::semaphore (::bstring ::bool ::bool ::long ::long)
	       "bgl_open_semaphore")
	    (macro $psemaphore-close::int (::semaphore) "BGL_SEMAPHORE_CLOSE")
	    (macro $psemaphore-delete::int (::string) "BGL_SEMAPHORE_DELETE")
	    (macro $psemaphore-wait::int (::semaphore) "BGL_SEMAPHORE_WAIT")
	    ($psemaphore-timed-wait::int (::semaphore ::long) "bgl_semaphore_timed_wait")
	    (macro $psemaphore-trywait::int (::semaphore) "BGL_SEMAPHORE_TRYWAIT")
	    (macro $psemaphore-post::int (::semaphore) "BGL_SEMAPHORE_POST")
	    (macro $S_IRWXU::long "S_IRWXU")
	    (macro $S_IRUSR::long "S_IRUSR")
	    (macro $S_IWUSR::long "S_IWUSR")
	    (macro $S_IXUSR::long "S_IXUSR")
	    (macro $S_IRWXG::long "S_IRWXG")
	    (macro $S_IRGRP::long "S_IRGRP")
	    (macro $S_IWGRP::long "S_IWGRP")
	    (macro $S_IXGRP::long "S_IXGRP")
	    (macro $S_IRWXO::long "S_IRWXO")
	    (macro $S_IROTH::long "S_IROTH")
	    (macro $S_IWOTH::long "S_IWOTH")
	    (macro $S_IXOTH::long "S_IXOTH")
	    ($psemaphore-value::int (::semaphore) "bgl_semaphore_value"))
   
   (java    (class $psemaphore
	       (method static open::semaphore (::bstring ::bool ::bool ::long ::long)
		  "bgl_open_semaphore")
	       (method static close::obj (::semaphore)
		  "bgl_close_semaphore")
	       (method static delete::obj (::string)
		  "bgl_delete_semaphore")
	       (method static wait::obj (::semaphore)
		  "bgl_wait_semaphore")
	       (method static timed-wait::obj (::semaphore ::int)
		  "bgl_timed_wait_semaphore")
	       (method static trywait::obj (::semaphore)
		  "bgl_trywait_semaphore")
	       (method static post::obj (::semaphore)
		  "bgl_post_semaphore")
	       (method static value::obj (::semaphore)
		  "bgl_getvalue_semaphore")
	       "bigloo.pthread.bglpsemaphore")))
