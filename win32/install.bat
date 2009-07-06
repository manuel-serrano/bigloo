@echo off
rem The script file that installs Bigloo
set BIGLOO_VERSION=THE-VERSION
set BIGLOO_HOME=c:\\BglTHE-VERSION
set BIGLOO_LIB=%BIGLOO_HOME%\lib
set BIGLOO_BIN=%BIGLOO_HOME%\bin

md %BIGLOO_HOME%
md %BIGLOO_HOME%\bin
md %BIGLOO_HOME%\lib
rem md %BIGLOO_HOME%\lib\bigloo
rem md %BIGLOO_HOME%\lib\bigloo\THE-VERSION

copy bigloo.pdf %BIGLOO_HOME%          
copy README %BIGLOO_HOME%
copy bin\bigloo.jar %BIGLOO_BIN%
copy bin\bigloo.bat %BIGLOO_BIN%
copy bin\jigloo.class %BIGLOO_BIN%
copy bin\afile.class %BIGLOO_BIN%
copy bin\jfile.class %BIGLOO_BIN%
copy lib\bigloo\THE-VERSION\bigloo.jheap %BIGLOO_LIB%
copy lib\bigloo\THE-VERSION\bigloo_u.zip %BIGLOO_LIB%
copy lib\bigloo\THE-VERSION\bigloo.zip %BIGLOO_LIB%
copy lib\bigloo\THE-VERSION\fthread.jheap %BIGLOO_LIB%
copy lib\bigloo\THE-VERSION\fthread.init %BIGLOO_LIB%
copy lib\bigloo\THE-VERSION\fthread_u.zip %BIGLOO_LIB%
copy lib\bigloo\THE-VERSION\fthread.zip %BIGLOO_LIB%

md %BIGLOO_HOME%\demo
md %BIGLOO_HOME%\demo\maze
md %BIGLOO_HOME%\demo\awt

copy demo\maze\README %BIGLOO_HOME%\demo\maze
copy demo\maze\maze.scm %BIGLOO_HOME%\demo\maze\maze.scm

copy demo\awt\README %BIGLOO_HOME%\demo\awt
copy demo\awt\awt.scm %BIGLOO_HOME%\demo\awt\awt.scm
copy demo\awt\Utils.java %BIGLOO_HOME%\demo\awt\Utils.java
