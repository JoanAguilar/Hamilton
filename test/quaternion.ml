open OUnit2

open Hamilton


let tol = 1e-6

let eq p q =
  let s1, i1, j1, k1 = Quaternion.to_float_tuple p in
  let s2, i2, j2, k2 = Quaternion.to_float_tuple q in
  s1 = s2 && i1 = i2 && j1 = j2 && k1 = k2

let is_close p q =
  let s1, i1, j1, k1 = Quaternion.to_float_tuple p in
  let s2, i2, j2, k2 = Quaternion.to_float_tuple q in
  Float.(
    abs (s2 -. s1) < tol &&
    abs (i2 -. i1) < tol &&
    abs (j2 -. j1) < tol &&
    abs (k2 -. k1) < tol)



let tests = "Quaternion tests" >::: [

  "zero 1" >:: (fun _ ->
    let q = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    assert_bool
      "Addition with zero changed the value."
      ( eq q Quaternion.( add zero q ) ));

  "zero 2" >:: (fun _ ->
    let q = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    assert_bool
      "Addition with zero changed the value."
      ( eq Quaternion.( add zero q ) q));

  "zero 3" >:: (fun _ ->
    let q = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    assert_bool
      "Multiplication by zero is not zero."
      Quaternion.( eq zero (mul zero q) ));

  "zero 4" >:: (fun _ ->
    let q = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    assert_bool
      "Multiplication by zero is not zero."
      Quaternion.( eq zero (mul q zero) ));

  "one 1" >:: (fun _ ->
    let q = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    assert_bool
      "Multiplication by one changed the value."
      ( eq q Quaternion.( mul one q ) ));

  "one 2" >:: (fun _ ->
    let q = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    assert_bool
      "Multiplication by one changed the value."
      ( eq q Quaternion.( mul q one ) ));

  "of_float_tuple to_float_tuple" >:: (fun _ ->
    let s, i, j, k = (1.0, 2.0, 3.0, 4.0) in
    let q = Quaternion.of_float_tuple (s, i, j, k) in
    let s', i', j', k' = Quaternion.to_float_tuple q in
    assert_bool
      "Conversion from and to tuple failed."
      (s = s' && i = i' && j = j' && k = k'));

  "of_scalar" >:: (fun _ ->
    let s = 2.0 in
    let q = Quaternion.of_float_tuple (s, 0.0, 0.0, 0.0) in
    let q' = Quaternion.of_scalar s in
    assert_bool "of_scalar produced an incorrect value." ( eq q q' ));

  "of_vector" >:: (fun _ ->
    let i, j, k = (1.0, 2.0, 3.0) in
    let q = Quaternion.of_float_tuple (0.0, i, j, k) in
    let q' = Quaternion.of_vector (i, j, k) in
    assert_bool "of_vector produced an incorrect value." ( eq q q' ));

  "of_axis_angle zero" >:: (fun _ ->
    let q = Quaternion.one in
    let q' = Quaternion.of_axis_angle (0.0, 0.0, 0.0) 0.0 in
    assert_bool "of_axis_angle produced an incorrect value." ( eq q q' ));

  "of_axis_angle x" >:: (fun _ ->
    let th = 1.0 in
    let hth = 0.5 *. th in
    let shth, chth = (sin hth, cos hth) in
    let q = Quaternion.of_float_tuple (chth, shth, 0.0, 0.0) in
    let q' = Quaternion.of_axis_angle (1.0, 0.0, 0.0) th in
    assert_bool "of_axis_angle produced an incorrect value." ( eq q q' ));

  "of_axis_angle y" >:: (fun _ ->
    let th = 1.0 in
    let hth = 0.5 *. th in
    let shth, chth = (sin hth, cos hth) in
    let q = Quaternion.of_float_tuple (chth, 0.0, shth, 0.0) in
    let q' = Quaternion.of_axis_angle (0.0, 1.0, 0.0) th in
    assert_bool "of_axis_angle produced an incorrect value." ( eq q q' ));

  "of_axis_angle z" >:: (fun _ ->
    let th = 1.0 in
    let hth = 0.5 *. th in
    let shth, chth = (sin hth, cos hth) in
    let q = Quaternion.of_float_tuple (chth, 0.0, 0.0, shth) in
    let q' = Quaternion.of_axis_angle (0.0, 0.0, 1.0) th in
    assert_bool "of_axis_angle produced an incorrect value." ( eq q q' ));

  "of_axis_angle 5" >:: (fun _ ->
    (* Test with a non-normalized axis with non-zero components. *)
    let x, y, z = (1.0, 2.0, 3.0) in
    let th = 1.0 in
    let n = sqrt (x *. x +. y *. y +. z *. z) in
    let x', y', z' = (x /. n, y /. n, z /. n) in
    let hth = 0.5 *. th in
    let shth, chth = (sin hth, cos hth) in
    let
      q = Quaternion.of_float_tuple (chth, x' *. shth, y' *. shth, z' *. shth)
    in
    let q' = Quaternion.of_axis_angle (x, y, z) th in
    assert_bool "of_axis_angle produced an incorrect value." ( eq q q' ));

  "of_rotation_vector zero" >:: (fun _ ->
    let q = Quaternion.one in
    let q' = Quaternion.of_rotation_vector (0.0, 0.0, 0.0) in
    assert_bool "of_rotation_vector produced an incorrect value." ( eq q q' ));

  "of_rotation_vector x" >:: (fun _ ->
    let th = 2.0 in
    let hth = 0.5 *. th in
    let shth, chth = (sin hth, cos hth) in
    let q = Quaternion.of_float_tuple (chth, shth, 0.0, 0.0) in
    let q' = Quaternion.of_rotation_vector (th, 0.0, 0.0) in
    assert_bool "of_rotation_vector produced an incorrect value." ( eq q q' ));

  "of_rotation_vector y" >:: (fun _ ->
    let th = 2.0 in
    let hth = 0.5 *. th in
    let shth, chth = (sin hth, cos hth) in
    let q = Quaternion.of_float_tuple (chth, 0.0, shth, 0.0) in
    let q' = Quaternion.of_rotation_vector (0.0, th, 0.0) in
    assert_bool "of_rotation_vector produced an incorrect value." ( eq q q' ));

  "of_rotation_vector z" >:: (fun _ ->
    let th = 2.0 in
    let hth = 0.5 *. th in
    let shth, chth = (sin hth, cos hth) in
    let q = Quaternion.of_float_tuple (chth, 0.0, 0.0, shth) in
    let q' = Quaternion.of_rotation_vector (0.0, 0.0, th) in
    assert_bool "of_rotation_vector produced an incorrect value." ( eq q q' ));

  "of_rotation_vector 5" >:: (fun _ ->
    let x, y, z = (1.0, 2.0, 3.0) in
    let th = sqrt (x *. x +. y *. y +. z *. z) in
    let x', y', z' = (x /. th, y /. th, z /. th) in
    let hth = 0.5 *. th in
    let shth, chth = (sin hth, cos hth) in
    let q = Quaternion.of_float_tuple (chth, x' *. shth, y' *. shth, z' *. shth) in
    let q' = Quaternion.of_rotation_vector (x, y, z) in
    assert_bool
      "of_rotation_vector produced an incorrect value."
      ( is_close q q' ));

  "to_axis_angle zero" >:: (fun _ ->
    let q = Quaternion.one in
    let th = 0.0 in
    let (x', y', z'), th' = Quaternion.to_axis_angle q in
    assert_bool
      "to_axis_angle produced an incorrect value."
      Float.( is_nan x' && is_nan y' && is_nan z' && th = th' ));

  "to_axis_angle x" >:: (fun _ ->
    let th = 1.0 in
    let hth = 0.5 *. th in
    let shth, chth = (sin hth, cos hth) in
    let q = Quaternion.of_float_tuple (chth, shth, 0.0, 0.0) in
    let (x', y', z'), th' = Quaternion.to_axis_angle q in
    assert_bool
      "to_axis_angle produced an incorrect value."
      Float.(
        abs (x' -. 1.0) < tol &&
        abs (y' -. 0.0) < tol &&
        abs (z' -. 0.0) < tol &&
        abs (th' -. th) < tol));

  "to_axis_angle y" >:: (fun _ ->
    let th = 1.0 in
    let hth = 0.5 *. th in
    let shth, chth = (sin hth, cos hth) in
    let q = Quaternion.of_float_tuple (chth, 0.0, shth, 0.0) in
    let (x', y', z'), th' = Quaternion.to_axis_angle q in
    assert_bool
      "to_axis_angle produced an incorrect value."
      Float.(
        abs (x' -. 0.0) < tol &&
        abs (y' -. 1.0) < tol &&
        abs (z' -. 0.0) < tol &&
        abs (th' -. th) < tol));

  "to_axis_angle z" >:: (fun _ ->
    let th = 1.0 in
    let hth = 0.5 *. th in
    let shth, chth = (sin hth, cos hth) in
    let q = Quaternion.of_float_tuple (chth, 0.0, 0.0, shth) in
    let (x', y', z'), th' = Quaternion.to_axis_angle q in
    assert_bool
      "to_axis_angle produced an incorrect value."
      Float.(
        abs (x' -. 0.0) < tol &&
        abs (y' -. 0.0) < tol &&
        abs (z' -. 1.0) < tol &&
        abs (th' -. th) < tol));

  "to_axis_angle 5" >:: (fun _ ->
    let q = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    let q' = Quaternion.normalize q in
    let (s, i, j, k) = Quaternion.to_float_tuple q' in
    let hth = acos s in
    let shth = sin hth in
    let x, y, z = (i /. shth, j /. shth, k /. shth) in
    let th = 2.0 *. hth in
    let (x', y', z'), th' = Quaternion.to_axis_angle q in
    assert_bool
      "to_axis_angle produced an incorrect value."
      Float.(
        abs (x' -. x) < tol &&
        abs (y' -. y) < tol &&
        abs (z' -. z) < tol &&
        abs (th' -. th) < tol));

  "to_rotation_vector zero" >:: (fun _ ->
    let x, y, z = (0.0, 0.0, 0.0) in
    let q = Quaternion.one in
    let x', y', z' = Quaternion.to_rotation_vector q in
    assert_bool
      "to_rotation_vector produced an incorrect value."
      ( x = x' && y = y' && z = z' ));

  "to_rotation_vector x" >:: (fun _ ->
    let x, y, z = (1.0, 0.0, 0.0) in
    let shth, chth = (sin 0.5, cos 0.5) in
    let q = Quaternion.of_float_tuple (chth, shth, 0.0, 0.0) in
    let x', y', z' = Quaternion.to_rotation_vector q in
    assert_bool
      "to_axis_angle produced an incorrect value."
      Float.(
        abs (x' -. x) < tol &&
        abs (y' -. y) < tol &&
        abs (z' -. z) < tol ));

  "to_rotation_vector y" >:: (fun _ ->
    let x, y, z = (0.0, 1.0, 0.0) in
    let shth, chth = (sin 0.5, cos 0.5) in
    let q = Quaternion.of_float_tuple (chth, 0.0, shth, 0.0) in
    let x', y', z' = Quaternion.to_rotation_vector q in
    assert_bool
      "to_axis_angle produced an incorrect value."
      Float.(
        abs (x' -. x) < tol &&
        abs (y' -. y) < tol &&
        abs (z' -. z) < tol ));

  "to_rotation_vector z" >:: (fun _ ->
    let x, y, z = (0.0, 0.0, 1.0) in
    let shth, chth = (sin 0.5, cos 0.5) in
    let q = Quaternion.of_float_tuple (chth, 0.0, 0.0, shth) in
    let x', y', z' = Quaternion.to_rotation_vector q in
    assert_bool
      "to_axis_angle produced an incorrect value."
      Float.(
        abs (x' -. x) < tol &&
        abs (y' -. y) < tol &&
        abs (z' -. z) < tol ));

  "to_rotation_vector 5" >:: (fun _ ->
    let x, y, z = (1.0, 2.0, 3.0) in
    let th = sqrt (x *. x +. y *. y +. z *. z) in
    let hth = 0.5 *. th in
    let shth, chth = (sin hth, cos hth) in
    let q = Quaternion.of_float_tuple (chth, x *. shth /. th, y *. shth /. th, z *. shth /. th) in
    let x', y', z' = Quaternion.to_rotation_vector q in
    assert_bool
      "to_axis_angle produced an incorrect value."
      Float.(
        abs (x' -. x) < tol &&
        abs (y' -. y) < tol &&
        abs (z' -. z) < tol ));

  "scalar" >:: (fun _ ->
    let s, i, j, k = (4.0, 3.0, 2.0, 1.0) in
    let q = Quaternion.of_float_tuple (s, i, j, k) in
    let s' = Quaternion.scalar q in
    assert_bool "scalar produced an incorrect value." ( s = s' ));

  "vector" >:: (fun _ ->
    let s, i, j, k = (1.0, 2.0, 3.0, 4.0) in
    let q = Quaternion.of_float_tuple (s, i, j, k) in
    let i', j', k' = Quaternion.vector q in
    assert_bool
      "vector produced an incorrect value."
      ( i = i' && j = j' && k = k' ));

  "axis" >:: (fun _ ->
    let s, i, j, k = (1.0, 2.0, 3.0, 4.0) in
    let n = sqrt (s *. s +. i *. i +. j *. j +. k *. k) in
    let s', i', j', k' = (s /. n, i /. n, j /. n, k /. n) in
    let hth = acos s' in
    let shth = sin hth in
    let x, y, z = (i' /. shth, j' /. shth, k' /. shth) in
    let q = Quaternion.of_float_tuple (s, i, j, k) in
    let x', y', z' = Quaternion.axis q in
    assert_bool
      "axis produced an incorrect value."
      ( x = x' && y = y' && z = z' ));

  "angle" >:: (fun _ ->
    let s, i, j, k = (1.0, 2.0, 3.0, 4.0) in
    let n = sqrt (s *. s +. i *. i +. j *. j +. k *. k) in
    let s' = s /. n in
    let th = 2.0 *. (acos s') in
    let q = Quaternion.of_float_tuple (s, i, j, k) in
    let th' = Quaternion.angle q in
    assert_bool "angle produced an incorrect value." ( th = th' ));

  "norm2" >:: (fun _ ->
    let s, i, j, k = (1.0, 2.0, 3.0, 4.0) in
    let n2 = s *. s +. i *. i +. j *. j +. k *. k in
    let q = Quaternion.of_float_tuple (s, i, j, k) in
    let n2' = Quaternion.norm2 q in
    assert_bool "norm2 produced an incorrect value." ( n2 = n2' ));

  "norm" >:: (fun _ ->
    let s, i, j, k = (1.0, 2.0, 3.0, 4.0) in
    let n = sqrt (s *. s +. i *. i +. j *. j +. k *. k) in
    let q = Quaternion.of_float_tuple (s, i, j, k) in
    let n' = Quaternion.norm q in
    assert_bool "norm produced an incorrect value." ( n = n' ));

  "normalize" >:: (fun _ ->
    let s, i, j, k = (1.0, 2.0, 3.0, 4.0) in
    let n = sqrt (s *. s +. i *. i +. j *. j +. k *. k) in
    let q = Quaternion.of_float_tuple (s /. n, i /. n, j /. n, k /. n) in
    let p = Quaternion.of_float_tuple (s, i, j, k) in
    let q' = Quaternion.normalize p in
    assert_bool "normalize produced an incorrect value." ( eq q q' ));

  "neg 1" >:: (fun _ ->
    let s, i, j, k = (1.0, 2.0, 3.0, 4.0) in
    let q = Quaternion.of_float_tuple (~-. s, ~-. i, ~-. j, ~-. k) in
    let p = Quaternion.of_float_tuple (s, i, j, k) in
    let q' = Quaternion.neg p in
    assert_bool "neg produced an incorrect value." ( eq q q' ));

  "neg 2" >:: (fun _ ->
    let s, i, j, k = (1.0, 2.0, 3.0, 4.0) in
    let p = Quaternion.of_float_tuple (s, i, j, k) in
    let q = Quaternion.( add p (neg p) ) in
    assert_bool "neg produced an incorrect value." ( eq q Quaternion.zero ));

  "neg 3" >:: (fun _ ->
    let s, i, j, k = (1.0, 2.0, 3.0, 4.0) in
    let p = Quaternion.of_float_tuple (s, i, j, k) in
    let q = Quaternion.( add (neg p) p ) in
    assert_bool "neg produced an incorrect value." ( eq q Quaternion.zero ));

  "conj" >:: (fun _ ->
    let s, i, j, k = (1.0, 2.0, 3.0, 4.0) in
    let p = Quaternion.of_float_tuple (s, i, j, k) in
    let q = Quaternion.of_float_tuple (s, ~-. i, ~-. j, ~-. k) in
    let q' = Quaternion.conj p in
    assert_bool "conj produced an incorrect value." ( eq q q' ));

  "inv 1" >:: (fun _ ->
    let s, i, j, k = (1.0, 2.0, 3.0, 4.0) in
    let p = Quaternion.of_float_tuple (s, i, j, k) in
    let n2 = Quaternion.norm2 p in
    let
      q = Quaternion.of_float_tuple (
        s /. n2,
        ~-. i /. n2,
        ~-. j /. n2,
        ~-. k /. n2)
    in
    let q' = Quaternion.inv p in
    assert_bool "inv produced an incorrect value." ( eq q q' ));

  "inv 2" >:: (fun _ ->
    let s, i, j, k = (1.0, 2.0, 3.0, 4.0) in
    let p = Quaternion.of_float_tuple (s, i, j, k) in
    let q = Quaternion.( mul p (inv p) ) in
    assert_bool
      "inv produced an incorrect value."
      ( is_close q Quaternion.one ));

  "inv 3" >:: (fun _ ->
    let s, i, j, k = (1.0, 2.0, 3.0, 4.0) in
    let p = Quaternion.of_float_tuple (s, i, j, k) in
    let q = Quaternion.( mul (inv p) p ) in
    assert_bool
      "inv produced an incorrect value."
      ( is_close q Quaternion.one ));

  "exp 1" >:: (fun _ ->
    (* Test exp of 0. *)
    let q = Quaternion.one in
    let p = Quaternion.zero in
    let q' = Quaternion.exp p in
    assert_bool "exp produced an incorrect value." ( eq q q' ));

  "exp 2" >:: (fun _ ->
    (* Test exp of 1. *)
    let q = Quaternion.of_float_tuple (exp 1.0, 0.0, 0.0, 0.0) in
    let p = Quaternion.one in
    let q' = Quaternion.exp p in
    assert_bool "exp produced an incorrect value." ( eq q q' ));

  "exp 3" >:: (fun _ ->
    (* Test exp of a zero-scalar quaternion. *)
    let i, j, k = (1.0, 2.0, 3.0) in
    let th = sqrt (i *. i +. j *. j +. k *. k) in
    let sth, cth = (sin th, cos th) in
    let
      q = Quaternion.of_float_tuple (
        cth,
        i *. sth /. th,
        j *. sth /. th,
        k *. sth /. th)
    in
    let p = Quaternion.of_vector (i, j, k) in
    let q' = Quaternion.exp p in
    assert_bool "exp produced an incorrect value." ( eq q q' ));

  "exp 4" >:: (fun _ ->
    (* Test that log is the inverse of exp. *)
    (* Choose a small-enough, well-behaved quaternion. *)
    let s, i, j, k = (0.25, 0.5, 0.75, 1.0) in
    let q = Quaternion.of_float_tuple (s, i, j, k) in
    let q' = Quaternion.( log (exp q) ) in
    assert_bool "exp produced an incorrect value." ( is_close q q' ));

  "log 1" >:: (fun _ ->
    (* Test log of 1. *)
    let q = Quaternion.zero in
    let p = Quaternion.one in
    let q' = Quaternion.log p in
    assert_bool "log produced an incorrect value." ( eq q q' ));

  "log 2" >:: (fun _ ->
    (* Test log of a unit quaternion. *)
    let th = 1.0 in
    let x, y, z = (2.0, 3.0, 4.0) in
    let p = Quaternion.of_axis_angle (x, y, z) th in
    let s, i, j, k = Quaternion.to_float_tuple p in
    let nv = sqrt (i *. i +. j *. j +. k *. k) in
    let
      q = Quaternion.of_float_tuple (
        0.0,
        i *. (acos s) /. nv,
        j *. (acos s) /. nv,
        k *. (acos s) /. nv)
    in
    let q' = Quaternion.log p in
    assert_bool "log produced an incorrect value." ( is_close q q' ));

  "log 3" >:: (fun _ ->
    (* Test that exp is the inverse of log. *)
    let s, i, j, k = (1.0, 2.0, 3.0, 4.0) in
    let q = Quaternion.of_float_tuple (s, i, j, k) in
    let q' = Quaternion.( exp (log q) ) in
    assert_bool "log produced an incorrect value." ( is_close q q' ));

  "apply 1" >:: (fun _ ->
    let th = (2.0 /. 3.0) *. Float.pi in
    let ax = (1.0, 1.0, 1.0) in
    let x, y, z = (1.0, 2.0, 3.0) in
    let p = Quaternion.of_float_tuple (0.0, x, y, z) in
    let q = Quaternion.of_float_tuple (0.0, z, x, y) in
    let r = Quaternion.of_axis_angle ax th in
    let q' = Quaternion.apply r p in
    assert_bool "apply produced an incorrect value." ( is_close q q' ));

  "apply 2" >:: (fun _ ->
    let th = ~-. (2.0 /. 3.0) *. Float.pi in
    let ax = (1.0, 1.0, 1.0) in
    let x, y, z = (1.0, 2.0, 3.0) in
    let p = Quaternion.of_float_tuple (0.0, x, y, z) in
    let q = Quaternion.of_float_tuple (0.0, y, z, x) in
    let r = Quaternion.of_axis_angle ax th in
    let q' = Quaternion.apply r p in
    assert_bool "apply produced an incorrect value." ( is_close q q' ));

  "apply 3" >:: (fun _ ->
    let p = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    let q = Quaternion.of_float_tuple (5.0, 6.0, 7.0, 8.0) in
    let u = Quaternion.( mul p (mul q (inv p)) ) in
    let u' = Quaternion.apply p q in
    assert_bool "apply produced an incorrect value." ( eq u u' ));

  "add" >:: (fun _ ->
    let s1, i1, j1, k1 = (1.0, 2.0, 3.0, 4.0) in
    let s2, i2, j2, k2 = (5.0, 6.0, 7.0, 8.0) in
    let p = Quaternion.of_float_tuple (s1, i1, j1, k1) in
    let q = Quaternion.of_float_tuple (s2, i2, j2, k2) in
    let
      u = Quaternion.of_float_tuple (s1 +. s2, i1 +. i2, j1 +. j2, k1 +. k2)
    in
    let u' = Quaternion.add p q in
    assert_bool "add produced an incorrect value." ( eq u u' ));

  "sub" >:: (fun _ ->
    let s1, i1, j1, k1 = (1.0, 2.0, 3.0, 4.0) in
    let s2, i2, j2, k2 = (5.0, 6.0, 7.0, 8.0) in
    let p = Quaternion.of_float_tuple (s1, i1, j1, k1) in
    let q = Quaternion.of_float_tuple (s2, i2, j2, k2) in
    let
      u = Quaternion.of_float_tuple (s1 -. s2, i1 -. i2, j1 -. j2, k1 -. k2)
    in
    let u' = Quaternion.sub p q in
    assert_bool "add produced an incorrect value." ( eq u u' ));

  "mul 1" >:: (fun _ ->
    let s1, s2 = (2.0, 3.0) in
    let u = Quaternion.of_scalar (s1 *. s2) in
    let p = Quaternion.of_scalar s1 in
    let q = Quaternion.of_scalar s2 in
    let u' = Quaternion.mul p q in
    assert_bool "mul produced an incorrect value." ( eq u u' ));

  "mul 2" >:: (fun _ ->
    let a1, b1 = (1.0, 2.0) in
    let a2, b2 = (3.0, 4.0) in
    let c1 = Complex.{ re = a1; im = b1 } in
    let c2 = Complex.{ re = a2; im = b2 } in
    let c3 = Complex.mul c1 c2 in
    let u = Quaternion.of_float_tuple Complex.(c3.re, c3.im, 0.0, 0.0) in
    let p = Quaternion.of_float_tuple (a1, b1, 0.0, 0.0) in
    let q = Quaternion.of_float_tuple (a2, b2, 0.0, 0.0) in
    let u' = Quaternion.mul p q in
    assert_bool "mul produced an incorrect value." ( eq u u' ));

  "mul 3" >:: (fun _ ->
    let s1, i1, j1, k1 = (1.0, 2.0, 3.0, 4.0) in
    let s2, i2, j2, k2 = (5.0, 6.0, 7.0, 8.0) in
    let p = Quaternion.of_float_tuple (s1, i1, j1, k1) in
    let q = Quaternion.of_float_tuple (s2, i2, j2, k2) in
    let u = Quaternion.of_float_tuple (
      s1 *. s2 -. i1 *. i2 -. j1 *. j2 -. k1 *. k2,
      s1 *. i2 +. i1 *. s2 +. j1 *. k2 -. k1 *. j2,
      s1 *. j2 -. i1 *. k2 +. j1 *. s2 +. k1 *. i2,
      s1 *. k2 +. i1 *. j2 -. j1 *. i2 +. k1 *. s2)
    in
    let u' = Quaternion.mul p q in
    assert_bool "mul produced an incorrect value." ( eq u u' ));

  "div" >:: (fun _ ->
    let s1, i1, j1, k1 = (1.0, 2.0, 3.0, 4.0) in
    let s2, i2, j2, k2 = (5.0, 6.0, 7.0, 8.0) in
    let p = Quaternion.of_float_tuple (s1, i1, j1, k1) in
    let q = Quaternion.of_float_tuple (s2, i2, j2, k2) in
    let u = Quaternion.( mul p (inv q) ) in
    let u' = Quaternion.div p q in
    assert_bool "div produced an incorrect value." ( eq u u' ));

]
