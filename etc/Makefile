#*=====================================================================*/
#*    serrano/prgm/project/bigloo/5.0.x/etc/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Thu Jun 19 20:48:50 1997                          */
#*    Last change :  Wed Jul 29 12:44:55 2026 (serrano)                */
#*    Copyright   :  1997-2026 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The global etc Makefile (mainly for backuping).                  */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config

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
                     -type f -exec rm {} \;   
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

