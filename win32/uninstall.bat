@echo off
rem The script file that uninstalls Bigloo
set BIGLOO_VERSION=THE-VERSION
set BIGLOO_HOME=c:\\BglTHE-VERSION

del %BIGLOO_HOME%\README
del %BIGLOO_HOME%\bin\bigloo.jar
del %BIGLOO_HOME%\bin\bigloo.bat
del %BIGLOO_HOME%\bin\jigloo.class
del %BIGLOO_HOME%\bin\afile.class
del %BIGLOO_HOME%\bin\jfile.class
del %BIGLOO_HOME%\lib\bigloo\THE-VERSION\bigloo.jheap
del %BIGLOO_HOME%\lib\bigloo\THE-VERSION\bigloo_u.zip
del %BIGLOO_HOME%\lib\bigloo\THE-VERSION\bigloo.zip

del %BIGLOO_HOME%\demo\maze\maze.scm
del %BIGLOO_HOME%\demo\maze\README
del %BIGLOO_HOME%\demo\maze
del %BIGLOO_HOME%\demo\awt\awt.scm
del %BIGLOO_HOME%\demo\awt\Utils.java
del %BIGLOO_HOME%\demo\awt\README
del %BIGLOO_HOME%\demo\awt
del %BIGLOO_HOME%\demo

rmdir %BIGLOO_HOME%\lib\bigloo\THE-VERSION
rmdir %BIGLOO_HOME%\lib\bigloo
rmdir %BIGLOO_HOME%\lib
rmdir %BIGLOO_HOME%\bin
rmdir %BIGLOO_HOME%

