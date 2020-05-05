#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/www/Makefile.md               */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon May  4 16:13:02 2020                          */
#*    Last change :  Tue May  5 15:51:15 2020 (serrano)                */
#*    Copyright   :  2020 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    WWW Bigloo page                                                  */
#*=====================================================================*/
do: build

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
HOP=hop
HOPFLAGS=-q --no-autoload --no-zeroconf --no-server --so-policy none

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

POPULATION=$(BOOTSTRAP_POP:%=lib/bootstrap/%) $(JQUERY_POP:%=lib/jquery/%) \
  $(ICONS_POP:%/icons/%) \
  Makefile doc.js xml.js html.js ecma-262-60.js \
  doc.json.in \
  api/api.json api/00-hop.md api/10-dom.md api/20-websocket.md api/01-html.md \
  api/reqfilter.js \
  widget/widget.json \
  lang/lang.json _lang.md \
  lang/html.bnf lang/service.bnf lang/iservice.bnf \
  lang/tilde.bnf lang/syntax.bnf \
  lang/00-syntax.md \
  lang/01-es.md lang/01-service.md lang/02-worker.md \
  lang/01-module.md lang/03-react.md lang/10-xml.md \
  lang/app.js lang/stateful.js lang/ui.js \
  dev/dev.json dev/00-command.md dev/01-rcfile.md dev/10-edit.md \
  dev/30-https.md dev/90-hopc.md dev/selfsigned.sh self/redirect.js \
  dev/js2json.js intf/intf.json intf/01-nodejs.md intf/10-scheme.md \
  intf/scheme.bnf \
  intf/20-language.md license.md _index.md \
  nodejs.md download.md \
  hss/doc.hss \
  html-idx.json mdn-idx.json mdn.js node-idx.json node.js

API_TARGETS=api.html 00-hop.html 10-dom.html 20-websocket.html \
  hss.html config.html user.html 01-html.html syslog.html \
  systime.html

LANG_TARGETS=lang.html 00-syntax.html 01-service.html 02-worker.html \
  01-es.html 03-react.html 01-module.html 10-xml.html markdown.html cpp.html

WIDGET_TARGETS=widget.html tree.html spage.html

DEV_TARGETS=dev.html 00-command.html 01-rcfile.html 10-edit.html 20-cross.html \
  30-https.html 90-hopc.html

INTF_TARGETS=intf.html 01-nodejs.html 10-scheme.html 20-language.html

ALL_TARGETS=index.html license.html download.html debian.html homebrew.html \
  manual.html

HTML="/usr/local/doc/html40/index/elements.html"

#*---------------------------------------------------------------------*/
#*    Search path                                                      */
#*---------------------------------------------------------------------*/
VPATH=api lang dev widget intf \
  ../node_modules/tree/doc \
  ../node_modules/spage/doc \
  ../node_modules/hss/doc \
  ../node_modules/config/doc \
  ../node_modules/user/doc \
  ../node_modules/markdown/doc \
  ../node_modules/syslog/doc \
  ../node_modules/systime/doc \
  ../node_modules/cpp/doc

#*---------------------------------------------------------------------*/
#*    The hop executable                                               */
#*---------------------------------------------------------------------*/
.PHONY: build clean install uninstall

build: $(ALL_TARGETS) hss/doc.css

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	$(RM) $(API_TARGETS) $(LANG_TARGETS) $(WIDGET_TARGETS) $(DEV_TARGETS)
	$(RM) $(INTF_TARGETS)
	$(RM) license.html index.html download.html dev.html
	$(RM) 00-command.html 10-edit.html
	$(RM) idx.json idx.html
	$(RM) -f hss/markdown.hss hss/fontifier.hss
	$(RM) -f favicon.png
	$(RM) -f LICENSE.academic

devclean: clean

distclean: clean

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	$(MAKE) mkdir DIR=$(DESTDIR)$(HOPDOCDIR)
	$(MAKE) mkdir DIR=$(DESTDIR)$(HOPDOCDIR)/lib
	$(INSTALL) $(ALL_TARGETS) $(DESTDIR)$(HOPDOCDIR)
	$(INSTALL) idx.json $(DESTDIR)$(HOPDOCDIR)
	cp -r lib/bootstrap $(DESTDIR)$(HOPDOCDIR)/lib
	cp -r lib/jquery $(DESTDIR)$(HOPDOCDIR)/lib
	$(RM) -r -f $(DESTDIR)$(HOPDOCDIR)/hss
	cp -r hss $(DESTDIR)$(HOPDOCDIR)/hss
	$(INSTALL) doc.js $(DESTDIR)$(HOPDOCDIR)
	$(INSTALL) xml.js $(DESTDIR)$(HOPDOCDIR)
	$(INSTALL) favicon.png $(DESTDIR)$(HOPDOCDIR)
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/hss
	chmod $(MODFILE) $(DESTDIR)$(HOPDOCDIR)/hss/*
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/lib
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/lib/bootstrap
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/lib/jquery
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/lib/bootstrap/css
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/lib/bootstrap/js
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/lib/bootstrap/fonts
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/lib/jquery/js
	chmod $(MODFILE) $(ALL_TARGETS:%=$(DESTDIR)$(HOPDOCDIR)/%)
	chmod $(MODFILE) $(JQUERY_POP:%=$(DESTDIR)$(HOPDOCDIR)/lib/jquery/%)
	chmod $(MODFILE) $(BOOTSTRAP_POP:%=$(DESTDIR)$(HOPDOCDIR)/lib/bootstrap/%)
	chmod $(MODFILE) $(DESTDIR)$(HOPDOCDIR)/lib/bootstrap/css/*
	$(INSTALL) html-idx.json $(DESTDIR)$(HOPDOCDIR)
	$(INSTALL) mdn-idx.json $(DESTDIR)$(HOPDOCDIR)
	$(INSTALL) node-idx.json $(DESTDIR)$(HOPDOCDIR)

uninstall:

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES: .md .html .json

#*---------------------------------------------------------------------*/
#*    .md -> .html                                                     */
#*---------------------------------------------------------------------*/
%.html: %.md doc.js xml.js bigloo.svg doc.json
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          ./doc.js "compile-section" $< > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    .json -> .html                                                   */
#*---------------------------------------------------------------------*/
%.html: %.json doc.js xml.js bigloo.svg doc.json
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          ./doc.js "compile-chapter" $< > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    index.html ...                                                   */
#*---------------------------------------------------------------------*/
index.html: _index.md doc.js xml.js bigloo.svg doc.json
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          ./doc.js "compile-main" $< > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    idx.json ...                                                     */
#*---------------------------------------------------------------------*/
idx.json: $(API_TARGETS) $(LANG_TARGETS) $(DEV_TARGETS) $(WIDGET_TARGETS) \
  $(INTF_TARGETS)
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          ./doc.js "html-to-idx" . $^ > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    idx.html ...                                                     */
#*---------------------------------------------------------------------*/
idx.html: idx.json
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          ./doc.js "compile-idx" $^ > $@ \
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
download.html: license.md ../INSTALL.md
lang.html: _lang.md
manual.html: ../manuals/bigloo.texi

hss/markdown.css: ../node_modules/markdown/hss/markdown.hss
	cp $< $@

hss/fontifier.css: ../node_modules/fontifier/hss/fontifier.hss
	cp $< $@

hss/doc.css: hss/doc.hss
	cp $< $@

favicon.png: ../share/icons/hop/favicon-16x16.png
	cp $< $@

LICENSE.academic: ../LICENSE.academic
	cp $< $@
