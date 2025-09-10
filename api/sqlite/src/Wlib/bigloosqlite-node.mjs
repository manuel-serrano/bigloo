/*=====================================================================*/
/*    .../bigloo/wasm/api/sqlite/src/Wlib/bigloosqlite-node.mjs        */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Wed Sep  4 06:42:43 2024                          */
/*    Last change :  Wed Sep 10 11:28:58 2025 (serrano)                */
/*    Copyright   :  2024-25 manuel serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo-wasm Node SQLITE binding                                  */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
import { DatabaseSync } from "node:sqlite";

/*---------------------------------------------------------------------*/
/*    sqlite JS environment.                                           */
/*---------------------------------------------------------------------*/
export function init(self) {
   function js_sqlite(self) {
      return {
	 nil: () => {
	    return null;
	 },
	 
	 open: (offset, len) => {
	    const str = self.loadString(offset, len);
	    const db = new DatabaseSync(str);
	    return db;
	 },

	 close: (db) => {
	    db.close();
	 }
      }
   }
   
   self.__js_sqlite = js_sqlite(self);
}
