(module __crypto-ciphers

   (option (set! *dlopen-init-gc* #t))
   
   (import __crypto-aes
	   __crypto-des
	   __crypto-idea
	   __crypto-cast-128))
