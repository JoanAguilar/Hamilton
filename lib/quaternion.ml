type t = float * float * float * float

let zero = (0.0, 0.0, 0.0, 0.0)

let one = (1.0, 0.0, 0.0, 0.0)

let of_float_tuple (s, i, j, k) = (s, i, j, k)

let of_scalar s = (s, 0.0, 0.0, 0.0)

let of_vector (i, j, k) = (0.0, i, j, k)

let of_axis_angle (x, y, z) th =
  (* Take care of the edge case where [th] is exactly 0. *)
  if th = 0.0 then
    one
  else
    let hth = 0.5 *. th in
    let shth, chth = (sin hth, cos hth) in
    let n = sqrt (x *. x +. y *. y +. z *. z) in
    let x', y', z' = (x /. n, y /. n, z /. n) in
    (chth, x' *. shth, y' *. shth, z' *. shth)

let to_float_tuple (s, i, j, k) = (s, i, j, k)

let scalar (s, _, _, _) = s

let vector (_, i, j, k) = (i, j, k)

let norm2 (s, i, j, k) = s *. s +. i *. i +. j *. j +. k *. k

let norm q = sqrt (norm2 q)

let normalize q =
  let n = norm q in
  let s, i, j, k = q in
  (s /. n, i /. n, j /. n, k /. n)

let to_axis_angle q =
  let q' = normalize q in
  let s, i, j, k = q' in
  let hth = acos s in
  let shth = sin hth in
  let x, y, z = (i /. shth, j /. shth, k /. shth) in
  let th = 2.0 *. hth in
  (x, y, z), th

let axis q =
  let q' = normalize q in
  let s, i, j, k = q' in
  let hth = acos s in
  let shth = sin hth in
  (i /. shth, j /. shth, k /. shth)

let angle q =
  let q' = normalize q in
  let s = scalar q' in
  2.0 *. (acos s)

let neg (s, i, j, k) = (~-. s, ~-. i, ~-. j, ~-. k)

let conj (s, i, j, k) = (s, ~-. i, ~-. j, ~-. k)

let inv q =
  let n2 = norm2 q in
  let s, i, j, k = q in
  (s /. n2, ~-. i /. n2, ~-. j /. n2, ~-. k /. n2)

let exp (s, i, j, k) =
  let es = exp s in
  let th = sqrt (i *. i +. j *. j +. k *. k) in
  (* Take care of edge case where [th] is 0. *)
  if th = 0.0 then
    (es, 0.0, 0.0, 0.0)
  else
    let sth, cth = (sin th, cos th) in (
      es *. cth,
      es *. i *. sth /. th,
      es *. j *. sth /. th,
      es *. k *. sth /. th)

let of_rotation_vector (x, y, z) =
  let x', y', z' = (0.5 *. x, 0.5 *. y, 0.5 *. z) in
  exp (0.0, x', y', z')

let log q =
  let s, i, j, k = q in
  let nv = sqrt (i *. i +. j *. j +. k *. k) in
  (* Take care of the edge case where the norm of the vector part
     ([nv]) is 0. *)
  if nv = 0.0 then
    (log s, 0.0, 0.0, 0.0)
  else
    let nq = norm q in
    let a = (acos (s /. nq)) /. nv in
    (log nq, a *. i, a *. j, a *. k)

let to_rotation_vector q =
  let q' = normalize q in
  let (_, i, j, k) = log q' in
  (2.0 *. i, 2.0 *. j, 2.0 *. k)

let add (s1, i1, j1, k1) (s2, i2, j2, k2) =
  (s1 +. s2, i1 +. i2, j1 +. j2, k1 +. k2)

let sub (s1, i1, j1, k1) (s2, i2, j2, k2) =
  (s1 -. s2, i1 -. i2, j1 -. j2, k1 -. k2)

let mul (s1, i1, j1, k1) (s2, i2, j2, k2) = (
  s1 *. s2 -. i1 *. i2 -. j1 *. j2 -. k1 *. k2,
  s1 *. i2 +. i1 *. s2 +. j1 *. k2 -. k1 *. j2,
  s1 *. j2 -. i1 *. k2 +. j1 *. s2 +. k1 *. i2,
  s1 *. k2 +. i1 *. j2 -. j1 *. i2 +. k1 *. s2)

let apply p q = mul p (mul q (inv p))

let div p q = mul p (inv q)
