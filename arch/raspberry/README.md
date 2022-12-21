Bigloo on Raspberry PI - 16 Dec 2022
====================================

The easiest way to get Bigloo running on a Raspberry PI is to compile
and install it natively on the device. With current devices performance
(speed and stotage capacity), this is the recommended procedure.
However, if cross compilation is to be used, read the `README.cross.md`
file for the procedure.


I. Preparing Qemu
-----------------

Install Qemu has described in `README.cross.md`


II. Preparing the SD card
------------------------

An SD with at *4GB* is required to install Linux + Bigloo.

  1. As of November 22, raspios images provide no default login credentials.
     It is then necessary to build a custom image with a default login already
     configured. For that it is needed to use the `rpi-imager` tool 
	 available at [Rasberrypi.org]. After choosing the 32 bit OS, click the 
	 setting button (bottom right or press ctl-shift-x) to configure a 
	 default login. In the following of this document, USER and PASSWORD 
	 refer to the default login use here.
 
III. Pre-Linux configuration
---------------------------

  4. Boot the (rpi) with the newly prepared SD card
  
  5. Log as "USER", password "PASSWORD"
  
  6. Expand the partition to use the whole SD card:
  
  
```shell
(rpi) `sudo raspi-config`, `7 Advanced Options`
```

  7. Reboot the device
  
  8. To change USER password
  
```shell
(rpi) `sudo raspi-config`, `1 Change User Password`
```

  9. Configure the network adapter (name and wifi):
  
```shell
(rpi) `sudo raspi-config`, `2 Network Options`
```

      For Raspberry model A that do not come with a builtin wifi
	  device, it might be needed to add some extra firmwares
	  (for `zd1211` firmware, see [zd1211rw]). The easiest
	  way is to copy the Debian package (for instance [zd1211.deb]) on
	  a usb key and to install it manually with `dpkg -i` on the Raspberry.
	   
  10. Configure ssh 

```shell
(rpi) `sudo raspi-config`, `5 Interfacing options`
```

From now on, the rest of the installation can be done remotely.

IV. RPI networking
------------------

  11. Update the packages
  
```shell
(rpi) sudo apt update
(rpi) sudo apt dist-upgrade
```

  12. Install avahi
  
```shell
(rpi) sudo update-rc.d avahi-daemon defaults
(rpi) sudo /etc/init.d/avahi-daemon start
```


3. Compiling Bigloo
-------------------

To compile Bigloo on Debian 10 (buster), install the following packages
before proceeds as on any regular Linux box

   (in guest) sudo apt update
   (in guest) sudo apt install -y dh-make libssl1.1 libssl-dev libsqlite3-0 libsqlite3-dev libasound2 libasound2-dev libflac8 libflac-dev libmpg123-0 libmpg123-dev libavahi-core7 libavahi-core-dev libavahi-common-dev libavahi-common3 libavahi-client3 libavahi-client-dev libunistring2 libunistring-dev libpulse-dev libpulse0 automake libtool libgmp-dev libgmp3-dev libgmp10

The follow the procedure described in [../debian/README.md](../debian/README.md).


References
----------

[Raspdoc]: https://www.raspberrypi.org/documentation/installation/installing-images/linux.md
[Raspberrypi.org]: https://www.raspberrypi.org/downloads/raspbian/
[Raspbian]: https://downloads.raspberrypi.org/raspbian_lite_latest

[zd1211rw]: https://wiki.debian.org/zd1211rw
[zd1211.deb]: https://packages.debian.org/jessie/all/firmware-zd1211/download
