/*=====================================================================*/
/*    .../bigloo/5.0.x/api/browser/src/Wlib/bigloobrowser-web.mjs      */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Thu Sep 11 08:24:07 2025                          */
/*    Last change :  Tue Jul 28 08:47:51 2026 (serrano)                */
/*    Copyright   :  2025-26 manuel serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo-wasm Web DOM binding                                      */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    dom JS environment ...                                           */
/*---------------------------------------------------------------------*/
export function init(self) {
   function js_dom(self) {
      return {
	 getElementById: (offset, len) => {
	    const id = self.loadString(offset, len);
	    return document.getElementById(id);
	 },

	 innerHTMLset(el, addr, len) {
	    el.innerHTML = self.loadString(addr, len);
	 },
	 
	 innerHTMLget(el, addr) {
	    const s = el.innerHTML;
	    self.storeString(s, addr);
	    return s.length;
	 }
      }
   }

   self.__browser = js_dom(self);
}
