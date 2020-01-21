#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/arch/debian/Makefile          */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sun Feb  7 07:41:50 2010                          */
#*    Last change :  Tue Jan 21 11:07:00 2020 (serrano)                */
#*    Copyright   :  2010-20 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile that handles debian specific files                  */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../../Makefile.buildconfig
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    Variables                                                        */
#*---------------------------------------------------------------------*/
POPULATION = Makefile makedeb.sh.in makedebremote.sh.in \
  changelog control.in postinst.in rules.in README README.md

#*---------------------------------------------------------------------*/
#*    pop ...                                                          */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=debian/%)

