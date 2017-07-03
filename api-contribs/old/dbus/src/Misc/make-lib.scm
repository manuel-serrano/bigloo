(module __dbus_makelib
	(import bigloodbus)
	(option (set! *dlopen-init* "dbus_e"))
	(eval (export-all)
	      (class %bus)
	      (class %service)
	      (class %object)
	      (class %message)))
