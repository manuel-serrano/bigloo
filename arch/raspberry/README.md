Bigloo on Raspberry PI - 27 Mar 2020
====================================

The easiest way to get Bigloo running on a Raspberry PI is to compile
and install it natively on the device. With current devices performance
(speed and stotage capacity), this is the recommended procedure.
However, if cross compilation is to be used, read the `README.cross.md`
file for the procedure.

1. Preparing Qemu
-----------------

Install Qemu has described in `README.cross.md`


2. Compiling Bigloo
-------------------

To compile Bigloo on Debian 9 (buster), install the following packages
before proceeds as on any regular Linux box

   (in guest) sudo apt update
   (in guest) sudo apt install -y dh-make libssl1.0.2 libssl-dev libsqlite3-0 libsqlite3-dev libasound2 libasound2-dev libflac8 libflac-dev libmpg123-0 libmpg123-dev libavahi-core7 libavahi-core-dev libavahi-common-dev libavahi-common3 libavahi-client3 libavahi-client-dev libunistring2 libunistring-dev libpulse-dev libpulse0 automake libtool libgmp-dev libgmp3-dev libgmp10
