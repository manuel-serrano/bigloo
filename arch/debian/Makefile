#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/arch/debian/Makefile          */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sun Feb  7 07:41:50 2010                          */
#*    Last change :  Tue Jan 21 09:55:29 2020 (serrano)                */
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
POPULATION = Makefile makedeb.sh.in makedeb.sh changelog \
  makedebremote.sh.in makedebremote.sh
  control.in postinst.in rules.in README README.md

#*---------------------------------------------------------------------*/
#*    pop ...                                                          */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=debian/%)

