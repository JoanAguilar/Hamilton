(** Quaternions

    This module provides basic operations on quaternions, including
    conversions from/to different representations of 3D rotations. The
    four scalar components of a quaternion are represented in this
    module by double-precision floating-point numbers (type
    [float]). *)

(** The type of quaternions. *)
type t

(** [zero] is the additive identity. *)
val zero : t

(** [one] is the multiplicative identity. *)
val one : t

(** [of_float_tuple (s, i, j, k)] is a quaternion with scalar part [s]
    and vector part [(i, j, k)]. *)
val of_float_tuple : float * float * float * float -> t

(** [of_scalar s] is a quaternion with scalar part [s] and zero vector
    part. [of_scalar s] is the same as
    [of_float_tuple (s, 0.0, 0.0, 0.0)]. *)
val of_scalar : float -> t

(** [of_vector (i, j, k)] is a quaternion with zero scalar part and
    vector part [(i, j, k)]. [of_vector (i, j, k)] is the same as
    [of_float_tuple (0.0, i, j, k)]. *)
val of_vector : float * float * float -> t

(** [of_axis_angle ax th] is a unit quaternion representing a rotation
    of angle [th] about axis [ax].
    [to_float_tuple (of_axis_angle (x, y, z) th)] is the same value as
    [(
      cos (0.5 *. th),
      x *. (sin (0.5 *. th)) /. n,
      y *. (sin (0.5 *. th)) /. n,
      z *. (sin (0.5 *. th)) /. n
    )]
    where [n = sqrt (x *. x +. y *. y +. z *. z)]. *)
val of_axis_angle : float * float * float -> float -> t

(** [of_rotation_vector (x, y, z)] is a unit quaternion representing a
    rotation of angle [n] about axis [(x /. n, y /. n, z /. n)], where
    [n] is the L2 norm of [(x, y, z)]. *)
val of_rotation_vector : float * float * float -> t

(** [to_float_tuple q] is a tuple with the components of [q]. If
    [to_float_tuple q] is [(s, i, j, k)], the scalar part of [q] is
    [s], and the vector part is [(i, j, k)]. *)
val to_float_tuple : t -> float * float * float * float

(** [to_axis_angle q] is a tuple containing the rotation axis and the
    angle, respectively, of the rotation represented by the
    normalization of the quaternion [q]. If
    [to_float_tuple (normalize q)] is [(s, i, j, k)], [to_axis_angle q]
    is
    [(
      (
       i /. (sin (0.5 *. th)),
       j /. (sin (0.5 *. th)),
       k /. (sin (0.5 *. th))
      ),
      th
    )],
    where [th = 2.0 *. (acos s)]. Note that the rotation axis will be
    of unit norm. *)
val to_axis_angle : t -> (float * float * float) * float

(** [to_rotation_vector q] the rotation vector of the rotation
    represented by the normalization of the quaternion [q]. If
    [to_float_tuple (normalize q)] is [(s, i, j, k)],
    [to_rotation_vector q] is
    [(
      th *. i /. (sin (0.5 *. th)),
      th *. j /. (sin (0.5 *. th)),
      th *. k /. (sin (0.5 *. th))
    )],
    where [th = 2.0 *. (acos s)]. *)
val to_rotation_vector : t -> float * float * float

(** [scalar q] is the scalar part of the quaternion [q]. If
    [to_float_tuple q] is [(s, _, _, _)], [scalar q] is [s]. *)
val scalar : t -> float

(** [vector q] is the vector part of the quaternion [q]. If
    [to_float_tuple q] is [(_, i, j, k)], [vector q] is [(i, j, k)]. *)
val vector : t -> float * float * float

(** [axis q] is the axis of the rotation represented by the
    normalization of quaternion [q]. [axis q] is the same value as
    [fst (to_axis_angle q)]. *)
val axis : t -> float * float * float

(** [angle q] is the angle of the rotation represented by the
    normalization of quaternion [q]. [angle q] is the same value as
    [snd (to_axis_angle q)]. *)
val angle : t -> float

(** [norm2 q] is the square of the norm of [q]. *)
val norm2 : t -> float

(** [norm q] is the norm of [q]. *)
val norm : t -> float

(** [normalize q] is a quaternion proportional to [q] but with unit
    norm. *)
val normalize : t -> t

(** [neg q] is the additive inverse of [q]. If [to_float_tuple q] is
    [(s, i, j, k)], [to_float_tuple (neg q)] is
    [(~-. s, ~-. i, ~-. j, ~-. k)]. *)
val neg : t -> t 

(** [conj q] is the conjugate of [q]. If [to_float_tuple q] is
    [(s, i, j, k)], [to_float_tuple (conj q)] is
    [(s, ~-. i, ~-. j, ~-. k)]. *)
val conj : t -> t

(** [inv q] is the multiplicative inverse of [q]. *)
val inv : t -> t

(** [exp q] is the exponential of [q]. *)
val exp : t -> t

(** [log q] is the logarithm of [q]. *)
val log : t -> t

(** [apply p q] applies [p] onto [q] by conjugate action. [apply p q]
    is the same value as [mul p (mul q (conj p)]. *) 
val apply : t -> t -> t

(** [add p q] is the addition of [p] and [q]. *)
val add : t -> t -> t

(** [sub p q] is the subtraction of [q] from [p]. [sub p q] is the same
    value as [add p (neg q)]. *)
val sub : t -> t -> t

(** [mul p q] is the multiplication of [p] by [q]. Note that (in
    general) [mul p q] is different than [mul q p]. *)
val mul : t -> t -> t

(** [div p q] is the division of [p] by [q]. [div p q] has the same
    value as [mul p (inv q)]. *)
val div : t -> t -> t
