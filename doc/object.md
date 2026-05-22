<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/object.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Objects                                                       -->
<!--==================================================================-->

,(include "head.html")
,(implementation-path "../runtime/Llib/object.scm")
,(example-path "../test/src/object.bgl")

Object Programming
==================

Bigloo's object system is designed to be as simple as possible and
belongs to the [Clos][Bobrow88] family in that it uses _classes_,
_generic functions_ and _methods_. Its design has been strongly
influenced by C. Queinnec's [Meroon][Queinnec93]. It does not include
any meta object protocol. It only supports single inheritance and
single dispatch.

Class Declaration
-----------------

Classes are defined in a modules and their scope is the whole module,
i.e., class declarations have to be top-level. They can be exported as
functions and variables. A class declaration can take place in a
compiled or interpreted module. If a class declaration is not exported
(see [Module 5](module5.html)) its scope is limited to the current
module.  If it is exported, its scope is extended to all modules that
import it. The syntax of a class declaration is:

```bnf
<class> --> ( <class-keyword> <typed-ident> <constructor>? <field>* )
<class-keyword> --> define-class
  | define-abstract-class
  | define-final-class
  | define-wide-class
  
<constructor> --> ( <expr> )

<field> --> <typed-ident>
  | ( <typed-ident> <field-prop> )
<field-prop> --> read-only
  | ( get <expression> )
  | ( set <expression> )
  | ( default <expression> )
  | ( info expression> )
     
<typed-ident> --> <ident>
  | <ident>::<ident>
```

The name of the class is extract from the &lt;typed-ident&gt; that
follows the &lt;class-keyword&gt;. If that <typed-ident> has also a
type component, the type part of this identifier denotes the super-class
of the class. If there is no super class specified, the defined class
inherits from the root of the inheritance tree, the `object`
class.

Final classes, i.e. classes declared with `define-final-class` can
only be sub-classed by wide classes, classes defined with
`define-wide-class`.  Wide classes can only inherit from _final
classes_.  Abstract classes can't be instantiated.

The optional constructor is an expression that must evaluate to a one
argument function. This function is automatically invoked each time
a new class instance is created. The constructor is passed the fresh
instance. If a class has not defined a constructor the super class'
constructors are searched. The first constructor found is invoked. 
A constructor may be a generic function with a method specified
for one or more classes.

Fields marked with the `read-only` attributes are immutables.
The `default` field attributes enables class instantiation to omit
corresponding field initialization values.

For the means of an example, the traditional points and colored points 
can be defined as:

```bigloo
(define-abstract-class pt)
(define-class point::pt 
   x::double 
   y::double)
(define-class point-C::point 
   (color::string read-only))))
```

The second example illustrate final and wide classes:

```bigloo
(define-final-class person 
   (name::bstring (default "Jones"))
   (sex read-only)
   children::pair-nil)
(define-wide-class married-person::person
   mate::person)
```

Fields may be virtual. A field is virtual as soon as its declaration
contain a `get` attribute. Virtual fields have no physical
implementation within the instance. When defining a virtual field, the
class declaration implements a _getter_ and a _setter_ if
that field is not a read only field. Access to the virtual field will
rely on invocation of its _getter_ and _setter_. For instance:

```bigloo
(define-class complex
   mag::double
   angle::double              
   (real::double (get (lambda (p)
                         (with-access::complex p (mag angle)
                            (* mag (cos angle)))))
      read-only)
   (imag::double (get (lambda (p)
                         (with-access::complex p (mag angle)
                            (* mag (sin angle)))))
      read-only))))

(let ((p (instantiate::complex (mag 1.0) (angle 2.18))))
   (with-access::complex p (real imag)
      (print "real: " real)
      (print "imag: " imag)))
```

Virtual fields cannot be associated default values. If a virtual field
is not provided with a setter it must be annotated as read only.

Info declarations allow arbitrary user information field values.
This value can be retrieved by introspection, by the means of the
`class-field-info` introspection function.

For the means of an example, with add to information to the slot of
the point class.

```bigloo
(define-class point 
   (x::double (info '(range 0.0 10.0)))
   (y::double (info '(range -1.0 1.0))))
```

Creating and Accessing Objects
------------------------------

Objects and classes are created and manipulated via library functions
and forms created automatically by Bigloo when a new class is defined.

In this section, examples assume the declarations of a final class `Point`, 
and two side classes `ColorPoint` and `Point3d`.

### isa? ###

This function returns `#t` if `obj` is an instance
of `class` or an instance of a sub-class of `class`, otherwise,
it returns `#f`.

### (instantiate::class ...) ###
<!-- [:instantiate@NoDef] -->

This forms allocates object instance and fills the fields with
values found in the list of parameters (note that field are explicitly
named and that there is no ordering for field naming). Field values which
are not provided in the parameter list must have been declared with a
`default` value which is used to initialize the corresponding
field.

If a class declares a constructor, for instance as:

```bigloo
(define-final-class Class/ctor
  (ctor))
```

The function `ctor` is invoked on any `Class/ctor` instantiation. It
receives the newly constructor instance.
  
  
### class-nil ###

This function returns the NIL pre-existing class instance. The value
of each field is unspecified but correct with respect to the Bigloo
type system. Each call to `class-nil` returns the same
object (in the sense of `eq?}.

### (with-access::class expr ...) ###
<!-- [:with-access@NoDef] -->

A reference to any of the variables defined in as a `binding` is
replaced by the appropriate field access form. This is true for both
reference and assignment. A `binding` is either a symbol or a list
of two symbols. In the first place, it denotes a field. In the second
case, it denotes an aliases field.

### (-> expr field) ###
<!-- [:->@NoDef] -->
Class instances can be accesses using the `->1 special form. The 
the first argument must be the identifier of a local typed variable, otherwise
an error is raised. The form `->` can be used to get or set value of
an instance field. For instance:

### (co-instantiate ...) ###
<!-- [:co-instantiate@NoDef] -->
This form is only available from compiled modules. In other words, it
is not available from the interpreter. It permits the creation of
recursive instances. It is specially useful for creating instances for
which class declarations contain cyclic type references (for instance
a class @code{c1} for a which a field is declared of class @code{c2}
and a class @code{c2} for which a class is declared of type
@code{c1}). The syntax of a @code{co-instantiate} form is similar to a
@code{let} form. However the only legal `values` are
@code{instantiate} forms. The variables introduced in the binding of a
@code{co-instantiate} form are bound in `body`. In addition, they
are @emph{partially} bound in the `values` expressions. In a
`value` position, a variable `var` can only be used to set the
value of a field of an instantiated class. It cannot be used in any
calculus. Example:

### (duplicate::class ...) ###
<!-- [:duplicate@NoDef] --> 
This form allocates an instance. The field values of the new object
are picked up from the field values of the `old` object unless they
are explicitly given in the parameter list.

The new instance can be a superclass of the original instance. It can
also be a subclass of the original instance, provided that all the
fields that are specific to the subclass are passed to `duplicate`.

Generic Functions
-----------------

A generic function is a bag of specific functions known as methods. When
invoked on a Bigloo object, a generic function determines the class of the
discriminating variable (corresponding to the first argument of the generic
function) and invokes the appropriate method. Generic functions implement
single inheritance and each is defined using the @code{define-generic} 
Bigloo syntax.

### (define-generic (id o ...) ...) ###
<!-- [:generic@NoDef] --> 

A generic function can be defined with a default body which will
be evaluated if no method can be found for the discriminating
variable. The default default-body signals an error.

Example:

```bigloo
(define-generic (class-id o)
   'obj)
```
<span></span>

### (define-method (id o ...) ...)
<!-- [:method@NoDef] --> 

Methods can be defined to specialize a generic function and such
methods must have a compatible variable list. 
That is, the first argument of the method must be a sub-type (i.e. belong 
to a sub-class) of the first argument of the generic function. Other 
formal parameters must be of same types. Moreover, the result type of 
the method must be a sub-type of the result of the generic function.

```bigloo
(define-method (class-id o::Point)
   'Point)
```

<span></span>

### (call-next-method) ###
<!-- [:call-next-method@NoDef] --> 

Methods can use the form `(call-next-method)` to invoke the method
that would have been called if not present. The `(call-next-method)`
cannot be used out of method definition.

Example:

```bigloo
(define-method (class-id o::Point3d)
   (symbol-append '|Point3d::| (call-next-method)))
```
<span></span>

Widening and Shrinking
----------------------

Bigloo introduces a new kind of inheritance: _widening_. This allows an
object to be temporarily _widened_ (that is transformed into an object
of another class, a _wide-class_) and then _shrink-ed_ (that is
reshaped to its original class). This mechanism is very useful for
implementing short-term data storage. For instance, Bigloo compilation
passes are implemented using the _widening/shrinking_ mechanism. On
entry to a pass, objects are widened with the specific pass fields and, on
exit from a pass, objects are shrunk in order to forget the information
related to this pass.

Only instances of _final classes_ can be widened and objects can
only be widened in order to become instances of _wide classes_. 
Widening is performed by the `widen!` syntax:


### (widen!::class ...) ###
<!-- [:widen@NoDef] --> 

The object `obj` is widened to be instance of the wide class
`wide-class`. Fields values are either picked up from the
parameter list of the @code{widen!} form or
from the default values in the declaration of the wide class.
@end deffn

### (shrink! obj) ###
<!-- [:shrink@NoDef] -->

Objects are shrunk using the `shrink!` operator.


Widening affects the objects themselves. This is illustrated by the following
"wedding simulator". First let us define three classes, `person` (for 
man and woman), `married-woman` and `married-man`:

```bigloo
(define-final-class person 
   name::string
   fname::string
   (birth::integer read-only))
   
(define-wide-class married-man::person
   mate::person)
   
(define-wide-class married-woman::person
   maiden-name::string
   mate::person)
```

As we can see people are allowed to change their name but not their birthdate.

The identity of a person can be printed as

```bigloo
(define-method (display-object p::person #!optional (port (current-output-port)))
  (fprint port "firstname : " (-> p fname))
  (fprint port "name      : " (-> p name))
  (fprint port "birth     : " (-> p birth))
  p)
```

A married woman's identity is printed by (we suppose an equivalent method 
definition for married-man)

```bigloo
(define-method (display-object p::married-woman #!optional (port (current-output-port)))
  (call-next-method)
  (print "married to: " (-> p mate))
  p)
```

We create a person with the `birth` function:

```bigloo
(define (birth name::string fname::string birth)
   (instantiate::person 
      (name name)
      (fname fname)
      (birth birth)))
```

We celebrate a wedding using the `get-married!` function:

```bigloo
(define (get-married! woman::person man::person)
   (let* ((mname (-> woman name))
          (wife  (widen!::married-woman woman
                    (maiden-name mname)
                    (mate man))))
      (person-name-set! wife (-> man name))
      (widen!::married-man man
         (mate woman)))))
```

We can check if two people are married by

```bigloo
(define (couple? woman::person man::person)
   (and (isa? woman married-woman)
        (isa? man married-man)
        (eq? (with-access::married-woman woman (mate) mate) man)
        (eq? (with-access::married-man man (mate) mate) woman)))
```

Now let us study the life a `Junior` `Jones` and
`Pamela` `Smith`. Once upon a time...

```bigloo
(define *junior* (birth "Jones" "Junior" '2000))
(define *pamela* (birth "Smith" "Pamela" '2001))
```

Later on, they met each other and ... they got married:

```bigloo
(define *old-boy-junior* *junior*)
(define *old-girl-pamela* *pamela*)
(get-married! *pamela* *junior*)
```

This union can be checked:

```
(couple? *pamela* *junior*)
   &rarr; #t
```

We can look at the new identity of `*pamela*:

```bigloo
(print *pamela*)
   &map; name      : Jones
      firstname : Pamela
      birth     : 2001
      married to: Junior Jones
```

But `*pamela*` and `*junior*` still are the same persons as before
their wedding:

```bigloo
(print (eq? *old-boy-junior* *junior*))   &rarr; #t
(print (eq? *old-girl-pamela* *pamela*))  &rarr; #t
```

Unfortunately all days are not happy days. After having been married
`*pamela*` and `*junior*` have divorced:

```bigloo
(define (divorce! woman::person man::person)
   (if (not (couple? woman man))
       (error "divorce!" "Illegal divorce" (cons woman man))
       (with-access::married-woman woman (maiden-name)
          (begin
             (shrink! woman)
             (set! (-> woman name) maiden-name))
          (shrink! man))))

(divorce! *pamela* *junior*)
```

We can look at the new identity of `*pamela*`:

```bigloo
(print *pamela*)
   &map; name      : Smith
      firstname : Pamela
      sex       : FEMALE
```

And `*pamela*` and `*junior*` still are the same persons:

```bigloo
(print (eq? *old-boy-junior* *junior*))   &rarr; #t
(print (eq? *old-girl-pamela* *pamela*))  &rarr; #t
```
<span></span>

Object and Class library
------------------------

### find-class ###

Returns, if any, the class `symbol`.

@deffn {bigloo procedure} class? obj
Returns @code{#t} if and only if @var{obj} is a class.
@end deffn

@deffn {bigloo procedure} class-super class
Returns the @emph{super-class} of @var{class}.
@end deffn

@deffn {bigloo procedure} class-subclasses class
Returns the @emph{subclasses} of @var{class}.
@end deffn

@deffn {bigloo procedure} class-name class
Returns the name (a symbol) of @var{class}.
@end deffn

@deffn {bigloo procedure} object-constructor class
Returns @var{class}'s constructor.
@end deffn

@deffn {bigloo procedure} object-class object
Returns the class that @var{object} belongs to.
@end deffn

@subsection Object handling
@deffn {bigloo procedure} wide-object? object
Returns @code{#t} if @var{object} is a wide object otherwise it returns 
@code{#f}.
@end deffn

@deffn {bigloo generic} object->struct object
@deffnx {bigloo procedure} struct->object struct
These functions converts objects into Scheme structures and vice-versa.
@end deffn

@deffn {bigloo generic} object-equal? object obj
This generic function is invoked by @code{equal?} when the first argument
is an instance of @code{object}.
@end deffn

@deffn {bigloo generic} object-hashnumber object
This generic function returns an hash number of @var{object}.
@end deffn

@deffn {bigloo procedure} isa? obj class
Returns @code{#t} if @var{obj} belongs to @var{class} otherwise it
returns @code{#f}.
@end deffn

@c ------------------------------------------------------------------- @c
@c    Object serialization                                             @c
@c ------------------------------------------------------------------- @c
@node Object serialization, Equality, Object library, Object System
@comment  node-name,  next,  previous,  up
@section Object serialization
@cindex Object serialization

Objects can be @emph{serialized} and @emph{un-serialized} using
the regular @code{string->obj} and @code{obj->string}
functions. Objects can be stored on disk and restored from disk
by the use of the @code{output-obj} and @code{input-obj}
functions.

In addition to this standard serialization mechanism, custom object
serializers and un-serializers can be specified by the means of the
@code{register-class-serialization!} function (see Section
@ref{Serialization}.

@c ------------------------------------------------------------------- @c
@c    Equality                                                         @c
@c ------------------------------------------------------------------- @c
@node Equality, Introspection, Object serialization, Object System
@comment  node-name,  next,  previous,  up
@section Equality
@cindex Equality

Two objects can be compared with the @code{equal?} function. Two object
are equal if and only if they belong to a same class, all their field
values are equal and all their super class's field values are equal.

@c ------------------------------------------------------------------- @c
@c    Introspection                                                    @c
@c ------------------------------------------------------------------- @c
@node Introspection,  , Equality, Object System
@comment  node-name,  next,  previous,  up
@section Introspection
@cindex Introspection

Bigloo provides the programmer with some object introspection facilities.
See section @pxref{Object library} for information on classes and
objects handling. Introspection facilities are, by default, available
for all classes. However, in order to shrink the code size generation,
it may be useful to disable class introspection. This decision can be
taken on a per class basis (i.e., one class may be provided with
introspection facilities while another one is not). The compiler
option @code{-fno-reflection} 
(see Chapter @ref{Compiler Description}) prevents the
compiler to generate the code required for introspecting the classes
defined in the compiled module.

@deffn {bigloo procedure} class-fields class
Returns the a description of the fields of @var{class}. This description
is a list of field descriptions where each field description can be accessed by
the means of the following library functions. The fields are those 
@emph{directly} defined in @var{class}. That is @code{class-fields} does not
return fields defined in super classes of @var{class}.
@end deffn

@deffn {bigloo procedure} class-all-fields class
Returns the a description of the fields of @var{class}. This description
is a list of field descriptions where each field description can be accessed by
the means of the following library functions. By contrast with 
@code{class-fields}, this function returns fields that are also defined in
the super classes of @var{class}.
in th
@end deffn

@deffn {bigloo procedure} find-class-field class symbol
Returns the field named @var{symbol} from class @var{class}. Returns 
@code{#f} is such a field does not exist.
@end deffn

@deffn {bigloo procedure} class-field? obj
Returns #t if @var{obj} is a class field descriptor. Otherwise returns #f.
@end deffn

@deffn {bigloo procedure} class-field-name field
Returns the name of the @var{field}. The name is a symbol.
@end deffn

@deffn {bigloo procedure} class-field-accessor field
Returns a procedure of one argument. Applying this function to an object
returns the value of the field described by @var{field}.
@end deffn

@deffn {bigloo procedure} class-field-mutable? field
Returns @code{#t} if the described field is mutable and @code{#f} otherwise.
@end deffn

@deffn {bigloo procedure} class-field-mutator field
Returns a procedure of two arguments. Applying this function to an object
changes the value of the field described by @var{field}. It is an
error to apply @code{class-field-mutator} to an immutable field.
@end deffn

@deffn {bigloo procedure} class-field-info field
Returns the information associated to @var{field} (this the class declaration
@code{info} attribute).
@end deffn


For means of an example, here is a possible implementation of the
@code{equal?} test for objects:

@smalllisp
(define (object-equal? obj1 obj2)
   (define (class-field-equal? fd)
      (let ((get-value (class-field-accessor fd)))
          (equal? (get-value obj1) (get-value obj2))))
   (let ((class1 (object-class obj1))
         (class2 (object-class obj2)))
      (cond
         ((not (eq? class1 class2))
          #f)
         (else
          (let loop ((fields (class-fields class1))
                     (class  class1))
             (cond
                ((null? fields)
                 (let ((super (class-super class)))
                    (if (class? super)
                        (loop (class-fields super)
                              super)
                        #t)))
                ((class-field-equal? (car fields))
                 (loop (cdr fields) class))
                (else
                 #f)))))))
@end smalllisp

@deffn {bigloo procedure} class-creator class
Returns the creator for @var{class}. The creator is a function for which
the arity depends on the number of slots the class provides 
(see Section @pxref{Creating and accessing objects}).

When an instance is allocated by the means of the @code{class-creator}, as
for direct instantiation, the class constructor is 
@emph{automatically} invoked.
Example:
@smalllisp
(module foo
   (main main)
   (static (class c1 (c1-constructor))))

(define c1-constructor
   (let ((count 0))
      (lambda (inst)
	 (set! count (+ 1 count))
	 (print "creating instance: " count)
	 inst)))

(define (main argv)
   (let ((o1 (instantiate::c1))
	 (o2 (instantiate::c1))
	 (o3 ((class-creator c1))))
      'done))
   @print{} creating instance: 1
      creating instance: 2
      creating instance: 3
@end smalllisp

@end deffn

@deffn {bigloo procedure} class-predicate class
Returns the predicate for @var{class}. This predicate returns @code{#t}
when applied to object of type @var{class}. It returns @code{#f} otherwise.
@end deffn


References
----------

[Bobrow88]: gee
[Queinnec93]: foo bar
