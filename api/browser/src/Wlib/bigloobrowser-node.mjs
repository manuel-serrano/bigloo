/*=====================================================================*/
/*    .../bigloo/5.0.x/api/browser/src/Wlib/bigloobrowser-node.mjs     */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Thu Sep 11 08:24:07 2025                          */
/*    Last change :  Tue Jul 28 08:49:44 2026 (serrano)                */
/*    Copyright   :  2025-26 manuel serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo-wasm Node DOM binding                                     */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    dom JS environment ...                                           */
/*---------------------------------------------------------------------*/
export function init(self) {
   function js_dom(self) {
      return {
	 getElementById: (offset, len) => null,
	 innerHTMLset: (el, addr, len) => undefined,
	 innerHTMLget: (el, addr) => undefined
      }
   }

   self.__browser = js_dom(self);
}
