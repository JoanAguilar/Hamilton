(** Dual Quaternions

    This module provides basic operations on dual quaternions including
    conversions from/to different representations of 3D rigid
    transformations. The eight scalar components of dual quaternions
    are represented in this module by double-precision floating-point
    numbers (type [float]).

    A dual quaternion can be represented by a pair of quaternions
    [(r, d)] usually written in the form [r + ε d], where [ε] is the
    dual unit [ε * ε = 0]. [r] is usually referred to as the _real_
    part of the dual quaternion, and [d] as the _dual_ part.
    Alternatively, they can be represented as a single quaternion with
    dual components. *)

(** The type of dual quaternions. *)
type t

(** [zero] is the additive identity. *)
val zero : t

(** [one] is the multiplicative identity. *)
val one : t

(** [of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd)] is a dual
    quaternion with real part
    [Quaternion.of_float_tuple (sr, ir, jr, kr)] and dual part
    [Quaternion.of_float_tuple (sd, id, jd, kd)]. *)
val of_float_tuples :
  float * float * float * float ->
  float * float * float * float ->
  t

(** [of_quaternion_tuple (p, q)] is a dual quaternion with real part
    [p] and dual part [q]. *)
val of_quaternion_tuple : Quaternion.t * Quaternion.t -> t

(** [of_dual_tuple (s, i, j, k) is a dual quaternion with scalar part
    [s] (a dual number) and vector part [(i, j, k)] (a vector of dual
    numbers). *)
val of_dual_tuple : Dual.t * Dual.t * Dual.t * Dual.t -> t

(** [of_real q] is a dual quaternion with real part [q] and zero dual
    part. *)
val of_real : Quaternion.t -> t

(** [of_dual q] is a dual quaternion with dual part [q] and zero real
    part. *)
val of_dual : Quaternion.t -> t

(** [of_scalar s] is the dual quaternion with scalar part [s] and zero
    vector part. *)
val of_scalar : Dual.t -> t

(** [of_vector (i, j, k)] is a dual quaternion with vector part
    [(i, j, k)] and zero scalar part. *)
val of_vector : Dual.t * Dual.t * Dual.t -> t

(** If [d'] is a quaternion representing a translation and [r'] is a
    unit quaternion representing a rotation,
    [of_translation_rotation d' r'] is a unit dual quaternion
    representing a rigid transformation with translation [d'] and
    rotation [r']. More generally,
    [to_quaternion_tuple (of_translation_rotation d' r')] is the same
    value as [(r', Quaternion.( mul (of_scalar 0.5) (mul d' r') )]. *)
val of_translation_rotation : Quaternion.t -> Quaternion.t -> t

(** If [d'] is a quaternion representing a translation,
    [of_translation d'] is a unit dual quaternion representing a rigid
    transformation with translation [d'] and no rotation. More
    generally, [to_quaternion_tuple (of_translation d')] is the same
    value as [Quaternion.(one, mul (of_scalar 0.5) d')]. *)
val of_translation : Quaternion.t -> t

(** If [r'] is a unit quaternion representing a rotation,
    [of_rotation r'] is a unit dual quaternion representing a rigid
    transformation with rotation [r'] and no translation. More
    generally, [to_quaternion_tuple (of_rotation r')] is the same value
    as [(r', Quaternion.zero)]. In fact, [of_rotation r'] is the same
    as [of_real r']. *)
val of_rotation : Quaternion.t -> t

(** [to_float_tuples n] is a pair of float tuples, containing the
    components of the real and dual part of the dual quaternion,
    respectively. *)
val to_float_tuples :
  t ->
  ( float * float * float * float ) * ( float * float * float * float )

(** [to_quaternion_tuple n] is a tuple containing the real and dual
    part of the quaternion, respectively. *)
val to_quaternion_tuple : t -> Quaternion.t * Quaternion.t

(** [to_dual_tuple n] is a tuple of dual numbers containing the
    components of the dual quaternion. If [to_dual_tuple n] is
    [(s, i, j, k)], [s] is the scalar part of the quaternion, and
    [(i, j, k)] is the vector part. *)
val to_dual_tuple : t -> Dual.t * Dual.t * Dual.t * Dual.t

(** [real n] is the real part of [n]. If [to_quaternion_tuple n] is
    [(r, _)], [real n] is [r]. *)
val real : t -> Quaternion.t

(** [dual n] is the dual part of [n]. If [to_quaternion_tuple n] is
    [(_, d)], [dual n] is [d]. *)
val dual : t -> Quaternion.t

(** [scalar n] is the scalar part of [n]. If [to_dual_tuple n] is
    [(s, _, _, _)], [scalar n] is [s]. *)
val scalar : t -> Dual.t

(** [vector n] is the vector part of [n]. If [to_dual_tuple n] is
    [(_, i, j, k)], [vector n] is [(i, j, k)]. *)
val vector : t -> Dual.t * Dual.t * Dual.t

(** If [n] is a unit dual quaternion representing a rigid
    transformation, [to_translation_rotation n] is a tuple [(d', r')]
    with [d'] a quaternion representing the translation part of the
    rigid transformation, and [r'] a unit quaternion representing the
    rotation part. More generally, if [to_quaternion_tuple n] is
    [(p, q)], [to_translation_rotation n] is
    [(Quaternion.( mul (of_scalar 2.0) (mul q (inv p)) ), p)]. *)
val to_translation_rotation : t -> Quaternion.t * Quaternion.t

(** If [n] is a unit dual quaternion representing a rigid
    transformation, [translation n] is a quaternion representing the
    translation part of the transformation. More generally, if
    [to_quaternion_tuple n] is [(p, q)], [translation n] is
    [Quaternion.( mul (of_scalar 2.0) (mul q (inv p)) )]. *)
val translation : t -> Quaternion.t

(** If [n] is a unit dual quaternion representing a rigid
    transformation, [rotation n] is a unit quaternion representing the
    rotation part of the transformation. More generally, if
    [to_quaternion_tuple n] is [(p, _)], [rotation n] is [p]. In fact,
    [rotation n] is the same as [real n]. *)
val rotation : t -> Quaternion.t

(** [norm2 n] is the square of the norm of [n]. Note that the norm of a
    dual quaternion (and, thus, its square) is a dual number. *)
val norm2 : t -> Dual.t

(** [norm n] is the norm of [n]. Note that the norm of a dual
    quaternion is a dual number. *)
val norm : t -> Dual.t

(** [normalize n] is the normalization of [n]. *)
val normalize : t -> t

(** [neg n] is the additive inverse of [n]. If [to_quaternion_tuple n]
    is [(r, d)], [to_quaternion_tuple (neg n)] is the same value as
    [Quaternion.(neg r, neg d)]. *)
val neg : t -> t

(** [conj n] is the quaternion conjugate of [n]. If
    [to_quaternion_tuple n] is [(r, d)], [to_quaternion_tuple (conj n)]
    is the same value as [Quaternion.(conj r, conj d)]. *)
val conj : t -> t

(** [inv n] is the multiplicative inverse of [n]. *)
val inv : t -> t

(** [exp n] is the exponential of [n]. *)
val exp : t -> t

(** [log n] is the logarithm of [n]. *)
val log : t -> t

(** [apply m n] applies [m] onto [n] by inverse action. [apply m n] is
    the same value as [mul m (mul n (inv m)]. *)
val apply : t -> t -> t

(** [add m n] is the addition of [m] and [n]. *)
val add : t -> t -> t

(** [sub m n] is the subtraction of [n] from [m]. [sub m n] is the same
    value as [add m (neg n)]. *)
val sub : t -> t -> t

(** [mul m n] is the multiplication of [m] by [n]. Note that, in
    general, [mul m n] is different than [mul n m]. *)
val mul : t -> t -> t

(** [div m n] is the division of [m] by [n]. [div m n] is the same
    value as [mul m (inv n)]. *)
val div : t -> t -> t
