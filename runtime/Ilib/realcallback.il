.namespace bigloo {
.class public auto ansi realcallback extends bigloo.callback {
.method public hidebysig specialname rtspecialname instance void .ctor() cil managed {
        .maxstack       1
        ldarg.0
        call    instance void 'bigloo.callback'::.ctor()
        ret
}
.method public static hidebysig specialname rtspecialname void .cctor() cil managed {
        .maxstack       1
	ret
}
.method public static hidebysig void 'init'() cil managed {
	.maxstack 1
	newobj	instance void bigloo.realcallback::.ctor()
	stsfld	class bigloo.callback bigloo.foreign::__cb__
	ret
}
.method public virtual hidebysig instance void failure(class System.Object, class System.Object, class System.Object) cil managed {
        .maxstack       3
	ldarg.1
	ldarg.2
	ldarg.3
	call class System.Object bigloo.runtime.Llib.error::the_failure(class System.Object, class System.Object, class System.Object)
	pop
	ret
}
.method public virtual hidebysig instance bool mangledp(unsigned int8[]) cil managed {
        .maxstack       1
	ldarg.1
	call bool bigloo.runtime.Llib.bigloo::bigloo_mangledp(unsigned int8[])
	ret
}
.method public virtual hidebysig instance class System.Object demangle(unsigned int8[]) cil managed {
        .maxstack       1
	ldarg.1
	call class System.Object bigloo.runtime.Llib.bigloo::bigloo_demangle(unsigned int8[])
	ret
}
.method public virtual hidebysig instance class System.Object exit_apply(class System.Object) cil managed {
        .maxstack       1
	ldarg.1
	call class System.Object bigloo.runtime.Llib.bigloo::bigloo_exit_apply(class System.Object)
	ret
}
.method public virtual hidebysig instance unsigned int8[] obj_to_string( class System.Object ) cil managed {
        .maxstack       1
	ldarg.1
	call unsigned int8[] bigloo.runtime.Unsafe.intext::obj_to_string( class System.Object )
	ret
}
.method public virtual hidebysig instance class System.Object string_to_obj( unsigned int8[] ) cil managed {
        .maxstack       1
	ldarg.1
	call class System.Object bigloo.runtime.Unsafe.intext::string_to_obj( unsigned int8[] )
	ret
}
}
}

