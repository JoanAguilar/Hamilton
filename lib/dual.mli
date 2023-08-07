(** Dual Numbers

    This module provides basic operations on dual numbers. The two
    scalar components of the dual numbers are represented in this
    module by double-precision floating-point numbers (type
    [float]). *)

(** The type of dual numbers. [re] is the real part and [du] the dual
    part. *)
type t = { re : float; du : float }

(** [zero] is the additive identity. *)
val zero : t

(** [one] is the multiplicative identity. *)
val one : t

(** [e] is the dual unit, it satisfies [mul e e = zero]. *)
val e : t

(** [norm2 n] is the square of the norm of [n]. *)
val norm2 : t -> float

(** [norm n] is the norm of [n]. *)
val norm : t -> float

(** [normalize n] is a dual number proportional to [n] but with unit
    norm. *)
val normalize : t -> t

(** [neg n] is the additive inverse of [n]. If [n] is
    [{ re = x; du = y }], [neg n] is [{ re = ~-. x; du = ~-. y }]. *)
val neg : t -> t

(** [conj n] is the conjugate of [n]. If [n] is [{ re = x; du = y }],
    [conj n] is [{ re = x; du = ~-. y }]. *)
val conj : t -> t

(** [inv n] is the multiplicative inverse of [n]. *)
val inv : t -> t

(** [sqrt n] is the square root of [n]. *)
val sqrt : t -> t

(** [exp n] is the exponential of [n]. *)
val exp : t -> t

(** [log n] is the logarithm of [n]. *)
val log : t -> t

(** [add m n] is the addition of [m] and [n]. *)
val add : t -> t -> t

(** [sub m n] is the subtraction of [n] from [m]. [sub m n] is the same
    value as [add m (neg n)]. *)
val sub : t -> t -> t

(** [mul m n] is the multiplication of [m] by [n]. *)
val mul : t -> t -> t

(** [div m n] is the division of [m] by [n]. [div m n] is the same
    value as [mul m (inv n)]. *)
val div : t -> t -> t

(** [pow m n] is [m] to the power of [n]. *)
val pow : t -> t -> t
