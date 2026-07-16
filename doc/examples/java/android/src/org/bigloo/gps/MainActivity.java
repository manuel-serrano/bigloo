package org.bigloo.gps;

import android.os.Build;

import android.app.Activity;
import android.Manifest;
import android.content.Context;
import android.os.Bundle;

public class MainActivity extends Activity {

   static String[] cmdline = {"android"};
   
   @Override
   protected void onCreate(Bundle savedInstanceState) {
      super.onCreate(savedInstanceState);

      Activity self = this;

      ALog.init(getString(R.string.app_name));
      
      bigloo.JDK.setExit(new bigloo.JDKexit() {
	    @Override
	    public void exit(int n) {
	       // ignore normal exit calls
	    }
	    @Override
	    public void abort(int n) {
	       // true error, abort
	       ALog.v("forcing termination with status " + n + "...");
	       self.finishAffinity();
	       android.os.Process.killProcess(android.os.Process.myPid());
	       System.exit(n);
	    }
	 });
		  
      Thread.setDefaultUncaughtExceptionHandler(
	 new Thread.UncaughtExceptionHandler() {
	    @Override
	    public void uncaughtException(Thread thread, Throwable throwable) {

	       if (!main.onException(thread, throwable)) {
		  ALog.e("*** JVM ERROR: ", throwable.getMessage());
		  throwable.printStackTrace();
		     
		  Thread.UncaughtExceptionHandler defaultHandler =
		     Thread.getDefaultUncaughtExceptionHandler();

		  if (defaultHandler != null) {
		     defaultHandler.uncaughtException(thread, throwable);
		  }
	       }
		     
	       self.finishAffinity();
	       android.os.Process.killProcess(android.os.Process.myPid());
	       System.exit(1);
	    }
	 });
      
      main.main(cmdline);
      main.onCreate(this);
   }

   @Override
   protected void onResume() {
      main.main(cmdline);
      main.onResume(this);
      super.onResume();
   }

   @Override
   protected void onPause() {
      super.onPause();
   }

   @Override
   protected void onDestroy() {
      super.onDestroy();
      main.onDestroy(this);
   }

   @Override
   public void onRequestPermissionsResult(int requestCode,
					  String[] perms,
					  int[] grantResults) {
      super.onRequestPermissionsResult(requestCode, perms, grantResults);
      permissions.onRequestPermissionsResult(requestCode, perms, grantResults);
   }
}
