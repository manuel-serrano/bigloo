/*=====================================================================*/
/*    .../project/bigloo/wasm/api/dom/src/Wlib/bigloodom-node.mjs      */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Thu Sep 11 08:24:07 2025                          */
/*    Last change :  Thu Sep 11 13:07:42 2025 (serrano)                */
/*    Copyright   :  2025 manuel serrano                               */
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

   self.__js_dom = js_dom(self);
}
