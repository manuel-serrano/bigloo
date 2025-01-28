/*=====================================================================*/
/*    serrano/prgm/project/bigloo/flt/runtime/Clib/capply.c            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Mar 20 11:26:29 1992                          */
/*    Last change :  Tue Nov 19 07:26:00 2024 (serrano)                */
/*    Copyright   :  2006-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo APPLY                                                     */
/*=====================================================================*/
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    Les recuperations externes                                       */
/*---------------------------------------------------------------------*/
extern void c_error();
extern long bgl_list_length(obj_t);

/*---------------------------------------------------------------------*/
/*    GENERIC_VA_PROCEDURE ...                                         */
/*---------------------------------------------------------------------*/
#define GENERIC_VA_PROCEDURE(proc) \
   (PROCEDURE_VA_ENTRY(proc) == NULL)

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    opt_apply ...                                                    */
/*---------------------------------------------------------------------*/
static obj_t
opt_apply(obj_t proc, obj_t args_list) {
   int len = bgl_list_length(args_list);
   obj_t args;
   obj_t runner;
   long i;
   int byte_size;
   
   /* Stack allocated the argument vector, see         */
   /* cvector.c:create_vector for regular vector alloc */
   byte_size = VECTOR_SIZE + ((len-1) * OBJ_SIZE);
   args = (obj_t)alloca(byte_size);

#if (!defined(TAG_VECTOR))
   args->vector.header = BGL_MAKE_VECTOR_HEADER(args, VECTOR_TYPE, len);
#else   
   args->vector.length = len;
#endif		
   args = BVECTOR(args);

   /* fill the vector, up to arity argument */
   for (i = 0; i < len; i++) {
      VECTOR_SET(args, i, CAR(args_list));
      args_list = CDR(args_list);
   }

   /* jump to the function */
   return ((obj_t (*)(obj_t, obj_t))PROCEDURE_VA_ENTRY(proc))(proc, args);
}

/*---------------------------------------------------------------------*/
/*    apply ...                                                        */
/*    -------------------------------------------------------------    */
/*    Tous les tests d'arite ont ete expanses `inline'. On n'a plus    */
/*    qu'a faire l'appel.                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t apply(obj_t function, obj_t args_list) {
   long arity = PROCEDURE_ARITY(function);

   if (arity < 0) {
      if (OPT_PROCEDUREP(function)) {
	 return opt_apply(function, args_list);
      } else if (GENERIC_VA_PROCEDURE(function)) {
	 return apply(PROCEDURE_REF(function, 3), args_list);
      } else {
	 long require;
	 obj_t runner = args_list;
	 long i = 0;
	 obj_t *arg;

	 require = -arity - 1;

	 arg = alloca(sizeof(obj_t) * require);
      
	 while (i < require) {
	    arg[ i++ ] = CAR(runner);
	    runner = CDR(runner);
	 }
	 switch(arity) {
	    case -1:
	       return ((obj_t (*)(obj_t, ...))PROCEDURE_VA_ENTRY(function))(function, runner);
         
	    case -2:
	       return ((obj_t (*)(obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[ 0 ], runner);
         
	    case -3:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[ 0 ], arg[ 1 ], runner);
         
		    
	    case -4:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
												 runner);
		    
	    case -5:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
													arg[3], runner);
		    
	    case -6:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
													       arg[3], arg[4], runner);
		    
	    case -7:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
														      arg[3], arg[4], arg[5],
														      runner);
		    
	    case -8:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
															     arg[3], arg[4], arg[5],
															     arg[6], runner);
		    
	    case -9:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																    arg[3], arg[4], arg[5],
																    arg[6], arg[7], runner);
		    
	    case -10:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																	   arg[3], arg[4], arg[5],
																	   arg[6], arg[7], arg[8],
																	   runner);
		    
	    case -11:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																		  arg[3], arg[4], arg[5],
																		  arg[6], arg[7], arg[8],
																		  arg[9], runner);
		    
	    case -12:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																			 arg[3], arg[4], arg[5],
																			 arg[6], arg[7], arg[8],
																			 arg[9], arg[10], runner);
		    
	    case -13:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																				arg[3], arg[4], arg[5],
																				arg[6], arg[7], arg[8],
																				arg[9], arg[10], arg[11],
																				runner);
		    
	    case -14:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																				       arg[3], arg[4], arg[5],
																				       arg[6], arg[7], arg[8],
																				       arg[9], arg[10], arg[11],
																				       arg[12], runner);
		    
	    case -15:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																					      arg[3], arg[4], arg[5],
																					      arg[6], arg[7], arg[8],
																					      arg[9], arg[10], arg[11],
																					      arg[12], arg[13], runner);
		    
	    case -16:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																						     arg[3], arg[4], arg[5],
																						     arg[6], arg[7], arg[8],
																						     arg[9], arg[10], arg[11],
																						     arg[12], arg[13], arg[14],
																						     runner);
		    
	    case -17:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																							    arg[3], arg[4], arg[5],
																							    arg[6], arg[7], arg[8],
																							    arg[9], arg[10], arg[11],
																							    arg[12], arg[13], arg[14],
																							    arg[15], runner);

	    case -18:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																								   arg[3], arg[4], arg[5],
																								   arg[6], arg[7], arg[8],
																								   arg[9], arg[10], arg[11],
																								   arg[12], arg[13], arg[14],
																								   arg[15], arg[16],runner);
	    
	    case -19:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																									  arg[3], arg[4], arg[5],
																									  arg[6], arg[7], arg[8],
																									  arg[9], arg[10], arg[11],
																									  arg[12], arg[13], arg[14],
																									  arg[15], arg[16],arg[17],runner);

	    case -20:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																										 arg[3], arg[4], arg[5],
																										 arg[6], arg[7], arg[8],
																										 arg[9], arg[10], arg[11],
																										 arg[12], arg[13], arg[14],
																										 arg[15], arg[16],arg[17],
																										 arg[18],runner);

	    case -21:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																											arg[3], arg[4], arg[5],
																											arg[6], arg[7], arg[8],
																											arg[9], arg[10], arg[11],
																											arg[12], arg[13], arg[14],
																											arg[15], arg[16], arg[17],
																											arg[18], arg[19], runner);

	    case -22:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																											       arg[3], arg[4], arg[5],
																											       arg[6], arg[7], arg[8],
																											       arg[9], arg[10], arg[11],
																											       arg[12], arg[13], arg[14],
																											       arg[15], arg[16], arg[17],
																											       arg[18], arg[19], arg[20], runner);

	    case -23:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																												      arg[3], arg[4], arg[5],
																												      arg[6], arg[7], arg[8],
																												      arg[9], arg[10], arg[11],
																												      arg[12], arg[13], arg[14],
																												      arg[15], arg[16], arg[17],
																												      arg[18], arg[19], arg[20],
																												      arg[21], runner);

	    case -24:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																													     arg[3], arg[4], arg[5],
																													     arg[6], arg[7], arg[8],
																													     arg[9], arg[10], arg[11],
																													     arg[12], arg[13], arg[14],
																													     arg[15], arg[16], arg[17],
																													     arg[18], arg[19], arg[20],
																													     arg[21], arg[22], runner);

	    case -25:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																														    arg[3], arg[4], arg[5],
																														    arg[6], arg[7], arg[8],
																														    arg[9], arg[10], arg[11],
																														    arg[12], arg[13], arg[14],
																														    arg[15], arg[16], arg[17],
																														    arg[18], arg[19], arg[20],
																														    arg[21], arg[22], arg[23], runner);

	    case -26:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																															   arg[3], arg[4], arg[5],
																															   arg[6], arg[7], arg[8],
																															   arg[9], arg[10], arg[11],
																															   arg[12], arg[13], arg[14],
																															   arg[15], arg[16], arg[17],
																															   arg[18], arg[19], arg[20],
																															   arg[21], arg[22], arg[23],
																															   arg[24], runner);

	    case -27:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																  arg[3], arg[4], arg[5],
																																  arg[6], arg[7], arg[8],
																																  arg[9], arg[10], arg[11],
																																  arg[12], arg[13], arg[14],
																																  arg[15], arg[16], arg[17],
																																  arg[18], arg[19], arg[20],
																																  arg[21], arg[22], arg[23],
																																  arg[24], arg[25], runner);

	    case -28:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																	 arg[3], arg[4], arg[5],
																																	 arg[6], arg[7], arg[8],
																																	 arg[9], arg[10], arg[11],
																																	 arg[12], arg[13], arg[14],
																																	 arg[15], arg[16], arg[17],
																																	 arg[18], arg[19], arg[20],
																																	 arg[21], arg[22], arg[23],
																																	 arg[24], arg[25], arg[26], runner);

	    case -29:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																		arg[3], arg[4], arg[5],
																																		arg[6], arg[7], arg[8],
																																		arg[9], arg[10], arg[11],
																																		arg[12], arg[13], arg[14],
																																		arg[15], arg[16], arg[17],
																																		arg[18], arg[19], arg[20],
																																		arg[21], arg[22], arg[23],
																																		arg[24], arg[25], arg[26],
																																		arg[27], runner);

	    case -30:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																		       arg[3], arg[4], arg[5],
																																		       arg[6], arg[7], arg[8],
																																		       arg[9], arg[10], arg[11],
																																		       arg[12], arg[13], arg[14],
																																		       arg[15], arg[16], arg[17],
																																		       arg[18], arg[19], arg[20],
																																		       arg[21], arg[22], arg[23],
																																		       arg[24], arg[25], arg[26],
																																		       arg[27], arg[28], runner);

	    case -31:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																			      arg[3], arg[4], arg[5],
																																			      arg[6], arg[7], arg[8],
																																			      arg[9], arg[10], arg[11],
																																			      arg[12], arg[13], arg[14],
																																			      arg[15], arg[16], arg[17],
																																			      arg[18], arg[19], arg[20],
																																			      arg[21], arg[22], arg[23],
																																			      arg[24], arg[25], arg[26],
																																			      arg[27], arg[28], arg[29], runner);

	    case -32:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																				     arg[3], arg[4], arg[5],
																																				     arg[6], arg[7], arg[8],
																																				     arg[9], arg[10], arg[11],
																																				     arg[12], arg[13], arg[14],
																																				     arg[15], arg[16], arg[17],
																																				     arg[18], arg[19], arg[20],
																																				     arg[21], arg[22], arg[23],
																																				     arg[24], arg[25], arg[26],
																																				     arg[27], arg[28], arg[29],
																																				     arg[30], runner);

	    case -33:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																					    arg[3], arg[4], arg[5],
																																					    arg[6], arg[7], arg[8],
																																					    arg[9], arg[10], arg[11],
																																					    arg[12], arg[13], arg[14],
																																					    arg[15], arg[16], arg[17],
																																					    arg[18], arg[19], arg[20],
																																					    arg[21], arg[22], arg[23],
																																					    arg[24], arg[25], arg[26],
																																					    arg[27], arg[28], arg[29],
																																					    arg[30], arg[31], runner);

	    case -34:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																						   arg[3], arg[4], arg[5],
																																						   arg[6], arg[7], arg[8],
																																						   arg[9], arg[10], arg[11],
																																						   arg[12], arg[13], arg[14],
																																						   arg[15], arg[16], arg[17],
																																						   arg[18], arg[19], arg[20],
																																						   arg[21], arg[22], arg[23],
																																						   arg[24], arg[25], arg[26],
																																						   arg[27], arg[28], arg[29],
																																						   arg[30], arg[31], arg[32], runner);

	    case -35:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																							  arg[3], arg[4], arg[5],
																																							  arg[6], arg[7], arg[8],
																																							  arg[9], arg[10], arg[11],
																																							  arg[12], arg[13], arg[14],
																																							  arg[15], arg[16], arg[17],
																																							  arg[18], arg[19], arg[20],
																																							  arg[21], arg[22], arg[23],
																																							  arg[24], arg[25], arg[26],
																																							  arg[27], arg[28], arg[29],
																																							  arg[30], arg[31], arg[32],
																																							  arg[33], runner);

	    case -36:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																								 arg[3], arg[4], arg[5],
																																								 arg[6], arg[7], arg[8],
																																								 arg[9], arg[10], arg[11],
																																								 arg[12], arg[13], arg[14],
																																								 arg[15], arg[16], arg[17],
																																								 arg[18], arg[19], arg[20],
																																								 arg[21], arg[22], arg[23],
																																								 arg[24], arg[25], arg[26],
																																								 arg[27], arg[28], arg[29],
																																								 arg[30], arg[31], arg[32],
																																								 arg[33], arg[34], runner);

	    case -37:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																									arg[3], arg[4], arg[5],
																																									arg[6], arg[7], arg[8],
																																									arg[9], arg[10], arg[11],
																																									arg[12], arg[13], arg[14],
																																									arg[15], arg[16], arg[17],
																																									arg[18], arg[19], arg[20],
																																									arg[21], arg[22], arg[23],
																																									arg[24], arg[25], arg[26],
																																									arg[27], arg[28], arg[29],
																																									arg[30], arg[31], arg[32],
																																									arg[33], arg[34], arg[35], runner);

	    case -38:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																									       arg[3], arg[4], arg[5],
																																									       arg[6], arg[7], arg[8],
																																									       arg[9], arg[10], arg[11],
																																									       arg[12], arg[13], arg[14],
																																									       arg[15], arg[16], arg[17],
																																									       arg[18], arg[19], arg[20],
																																									       arg[21], arg[22], arg[23],
																																									       arg[24], arg[25], arg[26],
																																									       arg[27], arg[28], arg[29],
																																									       arg[30], arg[31], arg[32],
																																									       arg[33], arg[34], arg[35],
																																									       arg[36], runner);

	    case -39:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																										      arg[3], arg[4], arg[5],
																																										      arg[6], arg[7], arg[8],
																																										      arg[9], arg[10], arg[11],
																																										      arg[12], arg[13], arg[14],
																																										      arg[15], arg[16], arg[17],
																																										      arg[18], arg[19], arg[20],
																																										      arg[21], arg[22], arg[23],
																																										      arg[24], arg[25], arg[26],
																																										      arg[27], arg[28], arg[29],
																																										      arg[30], arg[31], arg[32],
																																										      arg[33], arg[34], arg[35],
																																										      arg[36], arg[37], runner);

	    case -40:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																												    arg[3], arg[4], arg[5],
																																												    arg[6], arg[7], arg[8],
																																												    arg[9], arg[10], arg[11],
																																												    arg[12], arg[13], arg[14],
																																												    arg[15], arg[16], arg[17],
																																												    arg[18], arg[19], arg[20],
																																												    arg[21], arg[22], arg[23],
																																												    arg[24], arg[25], arg[26],
																																												    arg[27], arg[28], arg[29],
																																												    arg[30], arg[31], arg[32],
																																												    arg[33], arg[34], arg[35],
																																												    arg[36], arg[37], arg[38],
																																												    runner);

	    case -41:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																												    arg[3], arg[4], arg[5],
																																												    arg[6], arg[7], arg[8],
																																												    arg[9], arg[10], arg[11],
																																												    arg[12], arg[13], arg[14],
																																												    arg[15], arg[16], arg[17],
																																												    arg[18], arg[19], arg[20],
																																												    arg[21], arg[22], arg[23],
																																												    arg[24], arg[25], arg[26],
																																												    arg[27], arg[28], arg[29],
																																												    arg[30], arg[31], arg[32],
																																												    arg[33], arg[34], arg[35],
																																												    arg[36], arg[37], arg[38],
																																												    arg[39], runner);

	    case -42:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																													   arg[3], arg[4], arg[5],
																																													   arg[6], arg[7], arg[8],
																																													   arg[9], arg[10], arg[11],
																																													   arg[12], arg[13], arg[14],
																																													   arg[15], arg[16], arg[17],
																																													   arg[18], arg[19], arg[20],
																																													   arg[21], arg[22], arg[23],
																																													   arg[24], arg[25], arg[26],
																																													   arg[27], arg[28], arg[29],
																																													   arg[30], arg[31], arg[32],
																																													   arg[33], arg[34], arg[35],
																																													   arg[36], arg[37], arg[38],
																																													   arg[39], arg[40], runner);

	    case -43:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																														  arg[3], arg[4], arg[5],
																																														  arg[6], arg[7], arg[8],
																																														  arg[9], arg[10], arg[11],
																																														  arg[12], arg[13], arg[14],
																																														  arg[15], arg[16], arg[17],
																																														  arg[18], arg[19], arg[20],
																																														  arg[21], arg[22], arg[23],
																																														  arg[24], arg[25], arg[26],
																																														  arg[27], arg[28], arg[29],
																																														  arg[30], arg[31], arg[32],
																																														  arg[33], arg[34], arg[35],
																																														  arg[36], arg[37], arg[38],
																																														  arg[39], arg[40], arg[41], runner);

	    case -44:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																															 arg[3], arg[4], arg[5],
																																															 arg[6], arg[7], arg[8],
																																															 arg[9], arg[10], arg[11],
																																															 arg[12], arg[13], arg[14],
																																															 arg[15], arg[16], arg[17],
																																															 arg[18], arg[19], arg[20],
																																															 arg[21], arg[22], arg[23],
																																															 arg[24], arg[25], arg[26],
																																															 arg[27], arg[28], arg[29],
																																															 arg[30], arg[31], arg[32],
																																															 arg[33], arg[34], arg[35],
																																															 arg[36], arg[37], arg[38],
																																															 arg[39], arg[40], arg[41],
																																															 arg[42], runner);

	    case -45:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																																arg[3], arg[4], arg[5],
																																																arg[6], arg[7], arg[8],
																																																arg[9], arg[10], arg[11],
																																																arg[12], arg[13], arg[14],
																																																arg[15], arg[16], arg[17],
																																																arg[18], arg[19], arg[20],
																																																arg[21], arg[22], arg[23],
																																																arg[24], arg[25], arg[26],
																																																arg[27], arg[28], arg[29],
																																																arg[30], arg[31], arg[32],
																																																arg[33], arg[34], arg[35],
																																																arg[36], arg[37], arg[38],
																																																arg[39], arg[40], arg[41],
																																																arg[42], arg[43], runner);

	    case -46:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																																       arg[3], arg[4], arg[5],
																																																       arg[6], arg[7], arg[8],
																																																       arg[9], arg[10], arg[11],
																																																       arg[12], arg[13], arg[14],
																																																       arg[15], arg[16], arg[17],
																																																       arg[18], arg[19], arg[20],
																																																       arg[21], arg[22], arg[23],
																																																       arg[24], arg[25], arg[26],
																																																       arg[27], arg[28], arg[29],
																																																       arg[30], arg[31], arg[32],
																																																       arg[33], arg[34], arg[35],
																																																       arg[36], arg[37], arg[38],
																																																       arg[39], arg[40], arg[41],
																																																       arg[42], arg[43], arg[44],
																																																       runner);

	    case -47:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																																	      arg[3], arg[4], arg[5],
																																																	      arg[6], arg[7], arg[8],
																																																	      arg[9], arg[10], arg[11],
																																																	      arg[12], arg[13], arg[14],
																																																	      arg[15], arg[16], arg[17],
																																																	      arg[18], arg[19], arg[20],
																																																	      arg[21], arg[22], arg[23],
																																																	      arg[24], arg[25], arg[26],
																																																	      arg[27], arg[28], arg[29],
																																																	      arg[30], arg[31], arg[32],
																																																	      arg[33], arg[34], arg[35],
																																																	      arg[36], arg[37], arg[38],
																																																	      arg[39], arg[40], arg[41],
																																																	      arg[42], arg[43], arg[44],
																																																	      arg[45], runner);

	    case -48:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																																		     arg[3], arg[4], arg[5],
																																																		     arg[6], arg[7], arg[8],
																																																		     arg[9], arg[10], arg[11],
																																																		     arg[12], arg[13], arg[14],
																																																		     arg[15], arg[16], arg[17],
																																																		     arg[18], arg[19], arg[20],
																																																		     arg[21], arg[22], arg[23],
																																																		     arg[24], arg[25], arg[26],
																																																		     arg[27], arg[28], arg[29],
																																																		     arg[30], arg[31], arg[32],
																																																		     arg[33], arg[34], arg[35],
																																																		     arg[36], arg[37], arg[38],
																																																		     arg[39], arg[40], arg[41],
																																																		     arg[42], arg[43], arg[44],
																																																		     arg[45], arg[46], runner);

	    case -49:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																																			    arg[3], arg[4], arg[5],
																																																			    arg[6], arg[7], arg[8],
																																																			    arg[9], arg[10], arg[11],
																																																			    arg[12], arg[13], arg[14],
																																																			    arg[15], arg[16], arg[17],
																																																			    arg[18], arg[19], arg[20],
																																																			    arg[21], arg[22], arg[23],
																																																			    arg[24], arg[25], arg[26],
																																																			    arg[27], arg[28], arg[29],
																																																			    arg[30], arg[31], arg[32],
																																																			    arg[33], arg[34], arg[35],
																																																			    arg[36], arg[37], arg[38],
																																																			    arg[39], arg[40], arg[41],
																																																			    arg[42], arg[43], arg[44],
																																																			    arg[45], arg[46], arg[47],
																																																			    runner);

	    case -50:
	       return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_VA_ENTRY(function))(function, arg[0], arg[1], arg[2],
																																																				   arg[3], arg[4], arg[5],
																																																				   arg[6], arg[7], arg[8],
																																																				   arg[9], arg[10], arg[11],
																																																				   arg[12], arg[13], arg[14],
																																																				   arg[15], arg[16], arg[17],
																																																				   arg[18], arg[19], arg[20],
																																																				   arg[21], arg[22], arg[23],
																																																				   arg[24], arg[25], arg[26],
																																																				   arg[27], arg[28], arg[29],
																																																				   arg[30], arg[31], arg[32],
																																																				   arg[33], arg[34], arg[35],
																																																				   arg[36], arg[37], arg[38],
																																																				   arg[39], arg[40], arg[41],
																																																				   arg[42], arg[43], arg[44],
																																																				   arg[45], arg[46], arg[47],
																																																				   arg[48], runner);

	    default: {
	       char msg[ 128 ];
	       sprintf(msg,
		       "too many arguments provided (%ld) in apply (max 50)",
		       -arity);
	    
	       C_SYSTEM_FAILURE(BGL_ERROR, "apply", msg, function);
			      
	       return BUNSPEC;
	    }
	 }
      }
   } else {
      obj_t runner = args_list;
      long i = 0;
      obj_t *arg = alloca(sizeof(obj_t) * arity);
      
      while (i < arity) {
	 arg[ i++ ] = CAR(runner);
	 runner = CDR(runner);
      }

      switch(i) {
         case 0:
	    return ((obj_t (*)(obj_t))PROCEDURE_ENTRY(function))(function);

         case 1:
	    return ((obj_t (*)(obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0]);

         case 2:
	    return ((obj_t (*)(obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1]);

         case 3:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2]);

         case 4:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3]);

         case 5:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4]);

         case 6:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5]);

         case 7:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6]);

         case 8:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7]);

         case 9:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8]);

         case 10:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9]);

         case 11:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10]);

         case 12:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11]);

         case 13:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12]);

         case 14:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13]);

         case 15:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14]);

         case 16:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15]);

         case 17:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16]);

         case 18:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17]);

         case 19:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18]);

         case 20:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19]);

         case 21:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20]);

         case 22:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21]);

         case 23:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22]);

         case 24:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23]);

         case 25:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24]);

         case 26:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25]);

         case 27:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26]);

         case 28:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27]);

         case 29:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28]);

         case 30:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29]);

         case 31:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30]);

         case 32:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31]);

         case 33:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32]);

         case 34:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32], arg[33]);

         case 35:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32], arg[33],
				    arg[34]);

         case 36:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32], arg[33],
				    arg[34], arg[35]);

         case 37:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32], arg[33],
				    arg[34], arg[35], arg[36]);

         case 38:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32], arg[33],
				    arg[34], arg[35], arg[36],
				    arg[37]);

         case 39:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32], arg[33],
				    arg[34], arg[35], arg[36],
				    arg[37], arg[38]);

         case 40:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32], arg[33],
				    arg[34], arg[35], arg[36],
				    arg[37], arg[38], arg[39]);
	    
         case 41:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32], arg[33],
				    arg[34], arg[35], arg[36],
				    arg[37], arg[38], arg[39],
				    arg[40]);
	    
         case 42:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32], arg[33],
				    arg[34], arg[35], arg[36],
				    arg[37], arg[38], arg[39],
				    arg[40], arg[41]);
	    
         case 43:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32], arg[33],
				    arg[34], arg[35], arg[36],
				    arg[37], arg[38], arg[39],
				    arg[40], arg[41], arg[42]);

         case 44:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32], arg[33],
				    arg[34], arg[35], arg[36],
				    arg[37], arg[38], arg[39],
				    arg[40], arg[41], arg[42],
	                            arg[43]);

         case 45:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32], arg[33],
				    arg[34], arg[35], arg[36],
				    arg[37], arg[38], arg[39],
				    arg[40], arg[41], arg[42],
	                            arg[43], arg[44]);

         case 46:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32], arg[33],
				    arg[34], arg[35], arg[36],
				    arg[37], arg[38], arg[39],
				    arg[40], arg[41], arg[42],
	                            arg[43], arg[44], arg[45]);

         case 47:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32], arg[33],
				    arg[34], arg[35], arg[36],
				    arg[37], arg[38], arg[39],
				    arg[40], arg[41], arg[42],
	                            arg[43], arg[44], arg[45],
	                            arg[46]);

         case 48:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32], arg[33],
				    arg[34], arg[35], arg[36],
				    arg[37], arg[38], arg[39],
				    arg[40], arg[41], arg[42],
	                            arg[43], arg[44], arg[45],
	                            arg[46], arg[47]);

         case 49:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32], arg[33],
				    arg[34], arg[35], arg[36],
				    arg[37], arg[38], arg[39],
				    arg[40], arg[41], arg[42],
	                            arg[43], arg[44], arg[45],
	                            arg[46], arg[47], arg[48]);

         case 50:
	    return ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(function))(function, arg[0],
				    arg[1], arg[2], arg[3],
				    arg[4], arg[5], arg[6],
				    arg[7], arg[8], arg[9],
				    arg[10], arg[11], arg[12],
				    arg[13], arg[14], arg[15],
				    arg[16], arg[17], arg[18],
				    arg[19], arg[20], arg[21],
				    arg[22], arg[23], arg[24],
				    arg[25], arg[26], arg[27],
				    arg[28], arg[29], arg[30],
				    arg[31], arg[32], arg[33],
				    arg[34], arg[35], arg[36],
				    arg[37], arg[38], arg[39],
				    arg[40], arg[41], arg[42],
	                            arg[43], arg[44], arg[45],
	                            arg[46], arg[47], arg[48],
	                            arg[49]);


         default: {
	    char msg[ 128 ];
	    sprintf(msg,
		     "too many arguments provided (%ld) in apply (max 50)",
		     arity);
	    
	    C_SYSTEM_FAILURE(BGL_ERROR, "apply", msg, function);
			      
	    return BUNSPEC;
	 }
      }
   }
}
