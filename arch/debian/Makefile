#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/arch/debian/Makefile          */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sun Feb  7 07:41:50 2010                          */
#*    Last change :  Wed Dec 14 13:03:20 2022 (serrano)                */
#*    Copyright   :  2010-22 Manuel Serrano                            */
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
  changelog.in control.in postinst.in rules.in README.md

#*---------------------------------------------------------------------*/
#*    pop ...                                                          */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=debian/%)

