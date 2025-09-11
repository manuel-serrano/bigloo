/*=====================================================================*/
/*    .../project/bigloo/wasm/api/dom/src/Wlib/bigloodom-web.mjs       */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Thu Sep 11 08:24:07 2025                          */
/*    Last change :  Thu Sep 11 13:05:46 2025 (serrano)                */
/*    Copyright   :  2025 manuel serrano                               */
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

   self.__js_dom = js_dom(self);
}
