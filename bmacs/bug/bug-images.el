;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug-images.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May  4 14:08:21 2002                          */
;*    Last change :  Fri May 17 10:08:32 2002 (serrano)                */
;*    Copyright   :  2002 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The images used by debugger                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bug-images)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require (if (featurep 'xemacs) 'bug-xemacs 'bug-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    bug-breakpoint-red-image ...                                     */
;*---------------------------------------------------------------------*/
(defvar bug-breakpoint-red-image
  (ude-make-glyph
   "/* XPM */
static char *mini-redlight[] = {
/* width height num_colors chars_per_pixel */
\"    21    14       13            1\",
/* colors */
\". c #000000\",
\"# c #202020\",
\"a c #303060\",
\"b c #6064c8\",
\"c c #800000\",
\"d c #808080\",
\"e c #9898f8\",
\"f c None\",
\"g c #f80000\",
\"h c #f86430\",
\"i c #f864c8\",
\"j c #f8fc00\",
\"k c #f8fcf8\",
/* pixels */
\"ffffbbbbbbbbabdffffff\",
\"ffabfkfa.afekbaa.afff\",
\"ffabkfcgccdekaee.afff\",
\"fffbkfighg.ekbfcgffff\",
\"fffbkcfggc#ekbbbcffff\",
\"ffabkeccc#eekaaaaafff\",
\"ffabkea...aekbee.afff\",
\"fffbkhddaaaekeb.aafff\",
\"fffbkbddabbekbaaaffff\",
\"ffabkedaaaeekbaaa.fff\",
\"ffdbkb#a.#aekee.aafff\",
\"fffbkbaaaa.ekdaaabdff\",
\"fffbeeeaa.dekbaabbfff\",
\"fffffaaebbbbababbdfff\",
};"
   'center))

;*---------------------------------------------------------------------*/
;*    bug-breakpoint-orange-image ...                                  */
;*---------------------------------------------------------------------*/
(defconst bug-breakpoint-orange-image
  (ude-make-glyph
   "/* XPM */
static char *mini-redlight[] = {
/* width height num_colors chars_per_pixel */
\"    21    14       13            1\",
/* colors */
\". c #000000\",
\"# c #202020\",
\"a c #303060\",
\"b c #6064c8\",
\"c c #800000\",
\"d c #808080\",
\"e c #9898f8\",
\"f c None\",
\"g c #f80000\",
\"h c #f86430\",
\"i c #f864c8\",
\"j c #f8fc00\",
\"k c #f8fcf8\",
/* pixels */
\"ffffbbbbbbbbabdffffff\",
\"ffabkea..#eekaaaaafff\",
\"fffbkdddaaaekeb.aafff\",
\"fffbkbddabbekbaaaffff\",
\"ffabfkaa.afekbaa.afff\",
\"ffabkfhhccdekaee.afff\",
\"fffbkfghhh.ekbe.h.fff\",
\"fffbkfgghh.ekbfhhffff\",
\"fffbkchhhc#ekbbbhffff\",
\"ffabkedaaaeekbaaa.fff\",
\"ffdbkb#a.#aekee.aafff\",
\"fffbkbaaa#.ekddaadbff\",
\"fffbeeeaa.dekbaabbfff\",
\"fffffaaebbbbababbdfff\",
};"
   'center))

;*---------------------------------------------------------------------*/
;*    bug-breakpoint-green-image ...                                   */
;*---------------------------------------------------------------------*/
(defconst bug-breakpoint-green-image
  (ude-make-glyph
   "/* XPM */
static char *mini-redlight[] = {
/* width height num_colors chars_per_pixel */
\"    21    14       13            1\",
/* colors */
\". c #000000\",
\"# c #202020\",
\"a c #303060\",
\"b c #6064c8\",
\"c c #008000\",
\"d c #808080\",
\"e c #9898f8\",
\"f c None\",
\"g c #00f800\",
\"h c #64f830\",
\"i c #f864c8\",
\"j c #00fc00\",
\"k c #f8fcf8\",
/* pixels */
\"ffffbbbbbbbbabdffffff\",
\"ffabkea..#eekaaaaafff\",
\"ffabkea...aekbee.afff\",
\"fffbkdddaaaekeb.aafff\",
\"fffbkbdbbaaekefaaffff\",
\"ffabkedaaaeekbaaa.fff\",
\"ffdbkb#a.#aekee.aafff\",
\"fffbkbaaaa.ekdaaabdff\",
\"ffabfafa.afekbaa.afff\",
\"ffabkfhhccdekaee.afff\",
\"fffbkhjhhh.ekbe.c.fff\",
\"fffbkhgghh.ekbfchffff\",
\"fffbkcfghc#ekbbbcffff\",
\"fffffaaebbbbababfffff\",
};"
   'center))

;*---------------------------------------------------------------------*/
;*    bug-warning-image ...                                            */
;*---------------------------------------------------------------------*/
(defconst bug-warning-image
  (ude-make-glyph
   "/* XPM */
static char *mini-redlight[] = {
/* width height num_colors chars_per_pixel */
\"    21    14       4            1\",
/* colors */
\"  c None\",
\"x c #ff2020\",
\"o c black\",
\". c white\",
/* pixels */
\"         xxx         \",
\"         xxx         \",
\"        xx.xx        \",
\"        xx.xx        \",
\"       xx...xx       \",
\"       xx.o.xx       \",
\"      xx..o..xx      \",
\"      xx..o..xx      \",
\"     xx...o...xx     \",
\"     xx...o...xx     \",
\"    xx....o....xx    \",
\"    xx.........xx    \",
\"   xxxxxxxxxxxxxxx   \",
\"    xxxxxxxxxxxxx    \",
};"
   'center))

;*---------------------------------------------------------------------*/
;*    bug-footprint-enable-image ...                                   */
;*---------------------------------------------------------------------*/
(defconst bug-footprint-enable-image
  (ude-make-glyph
   "/* XPM */
static char *footprint[] = {
/* width height num_colors chars_per_pixel */
\"    19    12        2            1\",
/* colors */
\"_ c None\",
\"d c #5848b8\",
/* pixels */
\"___________dd______\",
\"___________dd_dd___\",
\"______________dd___\",
\"__________ddd______\",
\"__________dddd_dd__\",
\"__dd______dddd_dd__\",
\"__dd_dd____ddd_____\",
\"_____dd____________\",
\"_ddd_______________\",
\"_dddd_dd___________\",
\"_dddd_dd___________\",
\"__ddd______________\",
};"
   'center))

;*---------------------------------------------------------------------*/
;*    bug-footprint-disable-image ...                                  */
;*---------------------------------------------------------------------*/
(defconst bug-footprint-disable-image
  (ude-make-glyph
   "/* XPM */
static char *footprint[] = {
/* width height num_colors chars_per_pixel */
\"    19    14        2            1\",
/* colors */
\"_ c None\",
\"d c #a0a0a0\",
/* pixels */
\"___________________\",
\"___________dd______\",
\"___________dd_dd___\",
\"______________dd___\",
\"__________ddd______\",
\"__________dddd_dd__\",
\"__dd______dddd_dd__\",
\"__dd_dd____ddd_____\",
\"_____dd____________\",
\"_ddd_______________\",
\"_dddd_dd___________\",
\"_dddd_dd___________\",
\"__ddd______________\",
\"___________________\",
};"
   'center))


