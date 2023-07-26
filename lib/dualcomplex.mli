(** Anti-Commutative Dual Complex Numbers

    Throughout the documentation the _anti-commutative dual complex
    numbers_ are referred to as just _dual complex numbers_.

    This module provides basic operations on dual complex numbers
    including conversions from/to SE(2). The four scalar components of
    the dual complex numbers are represented in this module by
    double-precision floating-point numbers (type [float]).

    A dual complex number is a pair of complex number [(p, q)] with [p]
    the (pure) complex part and [q] the dual part. Dual complex numbers
    have the following operations:

     - Addition: [(p1, q1) + (p2, q2) = (p1 + p2, q1 + q2)].

     - Multiplication:
       [(p1, q1) * (p2, q2) = (p1 * p2, q1 * p2' + p1 * q2)], where
       [p2'] is the complex conjugate of [p2].

     - Conjugate: [(p1, q1)' = (p1', q1)].

     - Norm: [|(p1, q1)| = |p1|].

    The work in this module is partially based on:

    MATSUDA, Genki; KAJI, Shizuo; OCHIAI, Hiroyuki. Anti-commutative
    dual complex numbers and 2D rigid transformation. In: Mathematical
    Progress in _Expressive Image Synthesis I: Extended and Selected
    Results from the Symposium MEIS2013._ Springer Japan, 2014.
    p. 131-138. *)

(** The type of dual complex numbers. *)
type t

(** [zero] is the additive identity. *)
val zero : t

(** [one] is the multiplicative identity. *)
val one : t

(** [of_float_tuple (a, b, c, d)] is a dual complex number with pure
    complex part [a + b * i] and dual part [c + d * i] (with [i] being
    the imaginary unit). *)
val of_float_tuple : float * float * float * float -> t

(** [of_complex_tuple (p, q)] is a dual complex number with pure
    complex part [p] and dual part [q]. *)
val of_complex_tuple : Complex.t * Complex.t -> t

(** [of_complex p] is a dual complex number with pure complex part [p]
    and dual part [Complex.zero]. [of_complex p] is the same value as
    [of_complex_tuple p Complex.zero]. *)
val of_complex : Complex.t -> t

(** [of_dual q] is a dual complex number with dual part [q] and pure 
    complex part [Complex.zero]. [of_dual q] is the same value as
    [of_complex_tuple Complex.zero q]. *)
val of_dual : Complex.t -> t

(** [of_scalar s] is a dual complex number with scalar part [s], pure
    imaginary part [0.0], and dual part [Complex.zero]. [of_scalar s]
    is the same value as [of_float_tuple (s, 0.0, 0.0, 0.0)]. *)
val of_scalar : float -> t

(** [of_translation_rotation d r] is a (unit) dual complex number
    representing translation [d] and rotation [r]. The translation [d]
    is a 2D translation vector as a complex number, and [r] is a
    (unit) complex number representing a rotation with the argument of
    [r] being the rotation angle. If
    [to_complex_tuple (of_translation_rotation d r)] is [(p, q)], [p]
    and [q] satisfy: [p = sqrt' r], where [sqrt'] is the
    minimum-argument square root (different than [Complex.sqrt]); and
    [q = Complex.( mul { re = 0.5; im = 0.0 } (mul (inv p) d) )]. *)
val of_translation_rotation : Complex.t -> Complex.t -> t

(** [of_translation d] is a unit dual complex number representing
    translation [d] and no rotation. [of_translation d] is the same
    value as
    [of_complex_tuple Complex.(one, mul { re = 0.5; im = 0.0 } d)]. *)
val of_translation : Complex.t -> t

(** [of_rotation r] is a unit dual complex representing rotation [r]
    and no translation. *)
val of_rotation : Complex.t -> t

(** [to_float_tuple n] is a tuple containing the four elements of [n].
    If [to_float_tuple n] is [(a, b, c, d)], the pure complex part of
    [n] is [a + b * i] and the dual part is [c + d * i]. *)
val to_float_tuple : t -> float * float * float * float

(** [to_complex_tuple n] is a tuple containing the pure complex part
    and the dual part of [n], respectively. *)
val to_complex_tuple : t -> Complex.t * Complex.t

(** [to_translation_rotation n] is a tuple containing the translation
    and rotation, respectively, represented by the (unit) dual complex
    number [n]. If [to_complex_tuple n] is [(p, q)],
    [to_translation_rotation n] is
    [Complex.( mul p p, mul { re = 2.0; im = 0.0 } (mul p q) )]. *)
val to_translation_rotation : t -> Complex.t * Complex.t

(** [complex n] is the pure complex part of [n]. If
    [to_complex_tuple n] is [(p, _)], [complex n] is [p]. *)
val complex : t -> Complex.t

(** [dual n] is the dual part of [n]. If [to_complex_tuple n] is
    [(_, q)], [complex n] is [q]. *)
val dual : t -> Complex.t

(** [translation n] is the translation represented by the unit dual
    complex number [n], computed as
    [Complex.( mul {re = 2.0; im = 0.0 } (mul (real n) (dual n)) )]. *)
val translation : t -> Complex.t

(** [rotation n] is the rotation represented by the  unit dual complex
    number [n], computed as [Complex.mul (real n) (real n)]. *)
val rotation : t -> Complex.t

(** [norm2 n] is the square of the norm of [n]. [norm2 n] is the same
    values as [Complex.norm2 (complex n)]. *)
val norm2 : t -> float

(** [norm n] is the norm of [n]. [norm n] is the same value as
    [Complex.norm (complex n)]. *)
val norm : t -> float

(** [normalize n] is a dual complex proportional to [n] but with unit
    norm. *)
val normalize : t -> t

(** [neg n] is the additive inverse of [n]. If [to_complex_tuple n] is
    [(p, q)], [to_complex_tuple (neg n)] is the same value as
    [(~-. p, ~-. q)]. *)
val neg : t -> t

(** [conj n] is the conjugate of [n]. If [to_complex_tuple n] is
    [(p, q)], [to_complex_tuple (conj n)] is the same value as
    [(conj p, q)]. *)
val conj : t -> t

(** [inv n] is the multiplicative inverse of [n]. *)
val inv : t -> t

(** [exp n] is the exponential of [n]. *)
val exp : t -> t

(** [log n] is the logarithm of [n]. *)
val log : t -> t

(** [apply m n] applies [m] onto [n] by conjugate action. [apply m n]
    is the same value as [mul m (mul n (conj m)]. *)
val apply : t -> t -> t

(** [add m n] is the addition of [m] and [n]. *)
val add : t -> t -> t

(** [sub m n] is the subtraction of [n] from [m]. [sub m n] is the same
    value as [add m (neg n)]. *)
val sub : t -> t -> t

(** [mul m n] is the multiplication of [m] by [n]. Note that [mul] is
    not commutative. *)
val mul : t -> t -> t

(** [div m n] is the division of [m] by [n]. [div m n] has the same
    value as [mul m (inv n)]. *)
val div : t -> t -> t
