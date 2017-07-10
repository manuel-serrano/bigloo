#*=====================================================================*/
#*    serrano/prgm/project/bigloo/arch/macosx/Makefile                 */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Feb  5 17:49:56 2001                          */
#*    Last change :  Sun Jul  9 10:19:33 2017 (serrano)                */
#*    Copyright   :  2001-17 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile that handles macosx scripts.                        */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../../Makefile.buildconfig
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    Flags                                                            */
#*---------------------------------------------------------------------*/
POPULATION = Makefile Makefile.macosx \
   dmg/Readme.txt dmg/Readme-bee.txt \
   Info.plist Info.plist.bee README

#*---------------------------------------------------------------------*/
#*    pop ...                                                          */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=macosx/%)

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean
clean:
	@ find . \( -name '*[~%]' \
                       -o -name '.??*[~%]' \
                       -o -name '#*#' \
                       -o -name '?*#' \
                       -o -name \*core \) \
                     -type f -exec $(RM) {} \;   
	@ echo "cleanup done..."
	@ echo "-------------------------------"

cleanall: clean
distclean: cleanall

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	@ if [ `pwd` = $(BOOTDIR)/etc ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; n \
             exit 1; \
          fi
	@ $(MAKE) cleanall
