package org.bigloo.gps;

import android.app.Activity;
import android.Manifest;
import android.content.Context;

public class MainActivity extends Activity {

   static String[] cmdline = {"android"};
   
   @Override
   protected void onCreate(Bundle savedInstanceState) {
      super.onCreate(savedInstanceState);

      Activity self = this;

      bigloo.JDK.setExit(new bigloo.JDKexit() {
	    @Override
	    public void exit(int n) {
	    }
	    @Override
	    public void abort(int n) {
	       self.finishAffinity();
	       android.os.Process.killProcess(android.os.Process.myPid());
	       System.exit(n);
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
   protected void onDestroy() {
      super.onDestroy();
      main.onDestroy(this);
   }

   @Override
   public void onRequestPermissionsResult(int requestCode, String[] perms, int[] grantResults) {
      super.onRequestPermissionsResult(requestCode, perms, grantResults);
      permissions.onRequestPermissionsResult(requestCode, perms, grantResults);
   }
}
