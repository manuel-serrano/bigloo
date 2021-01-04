#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/www/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon May  4 16:13:02 2020                          */
#*    Last change :  Thu May 14 12:49:17 2020 (serrano)                */
#*    Copyright   :  2020-21 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    WWW Bigloo page                                                  */
#*=====================================================================*/
do: build

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
HOP = hop
HOPFLAGS = -q --no-autoload --no-zeroconf --no-server --so-policy none

#*---------------------------------------------------------------------*/
#*    Destination                                                      */
#*---------------------------------------------------------------------*/
INDESHTTP = www-sop.inria.fr/indes/fp
HOSTHTTP = $(INDESHTTP)
HOSTHTTPDIR = /users/serrano/public_html/bigloo
HOSTURL = http://$(HOSTHTTP)/Bigloo

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
BOOTSTRAP_POP=css/bootstrap.css css/bootstrap.min.css css/bootstrap.css.map \
  css/bootstrap-theme.css css/bootstrap-theme.min.css css/bootstrap-theme.css.map \
  js/bootstrap.js js/bootstrap.min.js  js/npm.js \
  fonts/glyphicons-halflings-regular.eot \
  fonts/glyphicons-halflings-regular.woff \
  fonts/glyphicons-halflings-regular.svg \
  fonts/glyphicons-halflings-regular.woff2 \
  fonts/glyphicons-halflings-regular.ttf

JQUERY_POP=js/jquery.min.js

POP=bib.md cross.md documentation.md homebrew.md license.md manual.md \
  contribs.md debian.md download.md _index.md \
  www.hss fontifier.css markdown.css texinfo.css \
  $(BOOTSTRAP_POP) $(JQUERY_POP) \
  favicon.png bigloo.svg \
  fib.scm fib-mt.scm flac.scm \
  Makefile.md

ALL_TARGETS=index.html license.html download.html debian.html homebrew.html \
  manual.html bib.html contribs.html cross.html

#*---------------------------------------------------------------------*/
#*    The hop executable                                               */
#*---------------------------------------------------------------------*/
.PHONY: build clean install uninstall

build: $(ALL_TARGETS) hss/www.css idx.html

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	rm -f $(ALL_TARGETS)
	rm -f manual-chapter*.html
	$(RM) idx.json idx.html

devclean: clean

distclean: clean

#*---------------------------------------------------------------------*/
#*    pop ...                                                          */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=www/%)

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES: .md .html .json

#*---------------------------------------------------------------------*/
#*    .md -> .html                                                     */
#*---------------------------------------------------------------------*/
%.html: %.md www.js xml.js bigloo.svg www.json
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          ./www.js "compile-section" $< > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    .json -> .html                                                   */
#*---------------------------------------------------------------------*/
%.html: %.json www.js xml.js bigloo.svg www.json
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          ./www.js "compile-chapter" $< > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    index.html ...                                                   */
#*---------------------------------------------------------------------*/
index.html: _index.md www.js xml.js bigloo.svg www.json \
  fib.scm fib-mt.scm flac.scm
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          ./www.js "compile-main" $< > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    idx.json ...                                                     */
#*---------------------------------------------------------------------*/
idx.json: manual.html
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          ./www.js "html-to-idx" . $^ manual-chapter*.html > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    idx.html ...                                                     */
#*---------------------------------------------------------------------*/
idx.html: idx.json
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          ./www.js "compile-idx" $^ > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    html-idx.json ...                                                */
#*---------------------------------------------------------------------*/
html-idx.json: 
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          ./html.js $(HTML) > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    mdn-idx.json ...                                                 */
#*---------------------------------------------------------------------*/
mdn-idx.json:
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          ./mdn.js > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    node-idx.json ...                                                */
#*---------------------------------------------------------------------*/
node-idx.json: node.js
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          ./node.js > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    dependencies                                                     */
#*---------------------------------------------------------------------*/
download.html: license.md ../INSTALL.md debian.md homebrew.md
lang.html: _lang.md
manual.html: manual-toc.js ../manuals/bigloo.texi ../manuals/modules.texi
bib.html: _bibtex.hop bigloo.bib
cross.html: ../arch/raspberry/README.cross.md \
  ../arch/android/README.cross.md

hss/markdown.css: ../node_modules/markdown/hss/markdown.hss
	cp $< $@

hss/texinfo.css: ../node_modules/texinfo/hss/texinfo.hss
	cp $< $@

hss/fontifier.css: ../node_modules/fontifier/hss/fontifier.hss
	cp $< $@

hss/www.css: hss/www.hss
	cp $< $@

favicon.png: ../share/icons/hop/favicon-16x16.png
	cp $< $@

LICENSE.academic: ../LICENSE.academic
	cp $< $@
