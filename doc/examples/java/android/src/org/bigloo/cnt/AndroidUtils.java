package org.bigloo.cnt;

import bigloo.*;

import android.content.*;
import android.view.*;
import android.widget.*;

public class AndroidUtils {
   static public View.OnClickListener procedureToOnClickListener(bigloo.procedure proc) {
      return new View.OnClickListener() {
	 @Override
	 public void onClick(View v) {
	    proc.funcall1(v);
	 }
      };
   }
}
