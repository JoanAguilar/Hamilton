open OUnit2

open Hamilton


let tol = 1e-6

let eq_dual m n = Dual.( m.re = n.re && m.du = n.du )

let is_close_dual m n = Dual.(
  Float.abs (m.re -. n.re) < tol &&
  Float.abs (m.du -. n.du) < tol )

let eq_quat p q =
  let s1, i1, j1, k1 = Quaternion.to_float_tuple p in
  let s2, i2, j2, k2 = Quaternion.to_float_tuple q in
  s1 = s2 && i1 = i2 && j1 = j2 && k1 = k2

let is_close_quat p q = 
  let s1, i1, j1, k1 = Quaternion.to_float_tuple p in
  let s2, i2, j2, k2 = Quaternion.to_float_tuple q in
  Float.(
    abs (s1 -. s2) < tol &&
    abs (i1 -. i2) < tol &&
    abs (j1 -. j2) < tol &&
    abs (k1 -. k2) < tol)

let eq m n =
  let p1, q1 = Dualquaternion.to_quaternion_tuple m in
  let p2, q2 = Dualquaternion.to_quaternion_tuple n in
  eq_quat p1 p2 && eq_quat q1 q2

let is_close m n =
  let p1, q1 = Dualquaternion.to_quaternion_tuple m in
  let p2, q2 = Dualquaternion.to_quaternion_tuple n in
  is_close_quat p1 p2 && is_close_quat q1 q2


let tests = "Dualquaternion tests" >::: [

  "zero 1" >:: (fun _ ->
    let r = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    let d = Quaternion.of_float_tuple (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_quaternion_tuple (r, d) in
    assert_bool
      "Addition with zero changed the value."
      ( eq n Dualquaternion.( add zero n ) ));

  "zero 2" >:: (fun _ ->
    let r = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    let d = Quaternion.of_float_tuple (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_quaternion_tuple (r, d) in
    assert_bool
      "Addition with zero changed the value."
      ( eq n Dualquaternion.( add n zero ) ));

  "zero 3" >:: (fun _ ->
    let r = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    let d = Quaternion.of_float_tuple (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_quaternion_tuple (r, d) in
    assert_bool
      "Multiplication by zero is not zero."
      Dualquaternion.( eq zero (mul zero n) ));

  "zero 4" >:: (fun _ ->
    let r = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    let d = Quaternion.of_float_tuple (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_quaternion_tuple (r, d) in
    assert_bool
      "Multiplication by zero is not zero."
      Dualquaternion.( eq zero (mul n zero) ));

  "one 1" >:: (fun _ ->
    let r = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    let d = Quaternion.of_float_tuple (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_quaternion_tuple (r, d) in
    assert_bool
      "Multiplication by one changed the value."
      ( eq n Dualquaternion.( mul one n ) ));

  "one 2" >:: (fun _ ->
    let r = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    let d = Quaternion.of_float_tuple (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_quaternion_tuple (r, d) in
    assert_bool
      "Multiplication by one changed the value."
      ( eq n Dualquaternion.( mul n one ) ));

  "of_float_tuples to_float_tuples" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let r = (sr, ir, jr, kr) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let d = (sd, id, jd, kd) in
    let n = Dualquaternion.of_float_tuples r d in
    let r', d' = Dualquaternion.to_float_tuples n in
    let sr', ir', jr', kr' = r' in
    let sd', id', jd', kd' = d' in
    assert_bool
      "Conversion from and to tuple failed."
      (sr = sr' && ir = ir' && jr = jr' && kr = kr' &&
      sd = sd' && id = id' && jd = jd' && kd = kd'));

  "of_quaternion_tuple" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let r = Quaternion.of_float_tuple (sr, ir, jr, kr) in
    let d = Quaternion.of_float_tuple (sd, id, jd, kd) in
    let n' = Dualquaternion.of_quaternion_tuple (r, d) in
    assert_bool
      "of_quaternion_tuple produced an incorrect value."
      ( eq n n' ));

  "of_dual_tuple" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let s = Dual.{ re = sr; du = sd } in
    let i = Dual.{ re = ir; du = id } in
    let j = Dual.{ re = jr; du = jd } in
    let k = Dual.{ re = kr; du = kd } in
    let n' = Dualquaternion.of_dual_tuple (s, i, j, k) in
    assert_bool
      "of_dual_tuple produced an incorrect value."
      ( eq n n' ));

  "of_real" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualquaternion.of_float_tuples
      (sr, ir, jr, kr)
      (0.0, 0.0, 0.0, 0.0)
    in
    let r = Quaternion.of_float_tuple (sr, ir, jr, kr) in
    let n' = Dualquaternion.of_real r in
    assert_bool
      "of_real produced an incorrect value."
      ( eq n n' ));

  "of_dual" >:: (fun _ ->
    let sd, id, jd, kd = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualquaternion.of_float_tuples
      (0.0, 0.0, 0.0, 0.0)
      (sd, id, jd, kd)
    in
    let d = Quaternion.of_float_tuple (sd, id, jd, kd) in
    let n' = Dualquaternion.of_dual d in
    assert_bool
      "of_dual produced an incorrect value."
      ( eq n n' ));

  "of_scalar" >:: (fun _ ->
    let sr, sd = (1.0, 2.0) in
    let
      n = Dualquaternion.of_float_tuples
        (sr, 0.0, 0.0, 0.0)
        (sd, 0.0, 0.0, 0.0)
    in
    let s = Dual.{ re = sr; du = sd } in
    let n' = Dualquaternion.of_scalar s in
    assert_bool
      "of_scalar produced an incorrect value."
      ( eq n n' ));

  "of_vector" >:: (fun _ ->
    let ir, jr, kr = (1.0, 2.0, 3.0) in
    let id, jd, kd = (4.0, 5.0, 6.0) in
    let n = Dualquaternion.of_float_tuples
      (0.0, ir, jr, kr)
      (0.0, id, jd, kd)
    in
    let i, j, k = Dual.(
      { re = ir; du = id },
      { re = jr; du = jd },
      { re = kr; du = kd }
    ) in
    let n' = Dualquaternion.of_vector (i, j, k) in
    assert_bool
      "of_vector produced an incorrect value."
      ( eq n n' ));

  "of_translation_rotation" >:: (fun _ ->
    let x, y, z = (1.0, 2.0, 3.0) in
    let th = 1.0 in
    let ax = (4.0, 5.0, 6.0) in
    let d' = Quaternion.of_vector (x, y, z) in
    let r' = Quaternion.of_axis_angle ax th in
    let d = Quaternion.( mul (of_scalar 0.5) (mul d' r') ) in
    let r = r' in
    let n = Dualquaternion.of_quaternion_tuple (r, d) in
    let n' = Dualquaternion.of_translation_rotation d' r' in
    assert_bool
      "of_translation_rotation produced an incorrect value."
      ( eq n n' ));

  "of_translation" >:: (fun _ ->
    let x, y, z = (1.0, 2.0, 3.0) in
    let d' = Quaternion.of_vector (x, y, z) in
    let n = Dualquaternion.of_quaternion_tuple Quaternion.(
      one,
      mul (of_scalar 0.5) d'
    ) in
    let n' = Dualquaternion.of_translation d' in
    assert_bool "of_translation produced an incorrect value." ( eq n n' ));

  "of_rotation" >:: (fun _ ->
    let ax = (1.0, 2.0, 3.0) in
    let th = 1.0 in
    let r' = Quaternion.of_axis_angle ax th in
    let n = Dualquaternion.of_real r' in
    let n'= Dualquaternion.of_rotation r' in
    assert_bool "of_rotation produced an incorrect value." ( eq n n' ));

  "to_quaternion_tuple" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let r = Quaternion.of_float_tuple (sr, ir, jr, kr) in
    let d = Quaternion.of_float_tuple (sd, id, jd, kd) in
    let r', d' = Dualquaternion.to_quaternion_tuple n in
    assert_bool
      "to_quaternion_tuple produced an incorrect value."
      ( eq_quat r r' && eq_quat d d' ));

  "to_dual_tuple" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let s, i, j, k = Dual.(
      { re = sr; du = sd },
      { re = ir; du = id },
      { re = jr; du = jd },
      { re = kr; du = kd }
    ) in
    let s', i', j', k' = Dualquaternion.to_dual_tuple n in
    assert_bool
      "to_dual produced an incorrect value."
      ( eq_dual s s' && eq_dual i i' && eq_dual j j' && eq_dual k k' ));

  "real" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let r = Quaternion.of_float_tuple (sr, ir, jr, kr) in
    let r'= Dualquaternion.real n in
    assert_bool "real produced an incorrect value." ( eq_quat r r' ));

  "dual" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let d = Quaternion.of_float_tuple (sd, id, jd, kd) in
    let d' = Dualquaternion.dual n in
    assert_bool "dual produced an incorrect value." ( eq_quat d d' ));

  "scalar" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let s = Dual.{ re = sr; du = sd } in
    let s' = Dualquaternion.scalar n in
    assert_bool "scalar produced an incorrect value." ( eq_dual s s' ));

  "vector" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let i, j, k = Dual.(
      { re = ir; du = id },
      { re = jr; du = jd },
      { re = kr; du = kd }
    ) in
    let i', j', k' = Dualquaternion.vector n in
    assert_bool
      "vector produced an incorrect value."
      ( eq_dual i i' && eq_dual j j' && eq_dual k k' ));

  "to_translation_rotation" >:: (fun _ ->
    let ax = (1.0, 2.0, 3.0) in
    let th = 1.0 in
    let p = Quaternion.of_axis_angle ax th in
    let q = Quaternion.of_float_tuple (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_quaternion_tuple (p, q) in
    let d = Quaternion.( mul (of_scalar 2.0) (mul q (inv p)) ) in
    let r = p in
    let d', r' = Dualquaternion.to_translation_rotation n in
    assert_bool
      "to_translation_rotation produced an incorrect value."
      ( eq_quat r r' && eq_quat d d' ));

  "translation" >:: (fun _ ->
    let ax = (1.0, 2.0, 3.0) in
    let th = 1.0 in
    let p = Quaternion.of_axis_angle ax th in
    let q = Quaternion.of_float_tuple (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_quaternion_tuple (p, q) in
    let d = Quaternion.( mul (of_scalar 2.0) (mul q (inv p)) ) in
    let d' = Dualquaternion.translation n in
    assert_bool "translation produced an incorrect value." ( eq_quat d d' ));

  "rotation" >:: (fun _ ->
    let ax = (1.0, 2.0, 3.0) in
    let th = 1.0 in
    let p = Quaternion.of_axis_angle ax th in
    let q = Quaternion.of_float_tuple (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_quaternion_tuple (p, q) in
    let r = p in
    let r' = Dualquaternion.rotation n in
    assert_bool "rotation produced an incorrect value." ( eq_quat r r' ));

  "norm2" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let x = Dualquaternion.( n |> conj |> mul n |> scalar ) in
    let x' = Dualquaternion.norm2 n in
    assert_bool "norm2 produced an incorrect value." ( eq_dual x x' ));

  "norm" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let x = Dualquaternion.( n |> conj |> mul n |> scalar |> Dual.sqrt ) in
    let x' = Dualquaternion.norm n in
    assert_bool "norm produced an incorrect value." ( eq_dual x x' ));

  "normalize 1" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let m = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let x = Dualquaternion.norm m in
    let n = Dualquaternion.( x |> Dual.inv |> of_scalar |> mul m ) in
    let n' = Dualquaternion.normalize m in
    assert_bool "normalize produced an incorrect value." ( eq n n' ));

  "normalize 2" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let m = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let n = Dualquaternion.normalize m in
    let x = Dualquaternion.norm n in
    assert_bool
      "normalize resulted in a norm different than 1."
      ( is_close_dual x Dual.one ));

  "neg 1" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let m = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let n = Dualquaternion.of_float_tuples
      (~-. sr, ~-. ir, ~-. jr, ~-. kr)
      (~-. sd, ~-. id, ~-. jd, ~-. kd)
    in
    let n' = Dualquaternion.neg m in
    assert_bool "neg produced an incorrect value." ( eq n n' ));

  "neg 2" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let m = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let n = Dualquaternion.( add m (neg m) ) in
    assert_bool
      "neg produced an incorrect value."
      ( eq n Dualquaternion.zero ));

  "neg 3" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let m = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let n = Dualquaternion.( add (neg m) m ) in
    assert_bool
      "neg produced an incorrect value."
      ( eq n Dualquaternion.zero ));

  "conj" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let m = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let n = Dualquaternion.of_float_tuples
      (sr, ~-. ir, ~-. jr, ~-. kr)
      (sd, ~-. id, ~-. jd, ~-. kd)
    in
    let n' = Dualquaternion.conj m in
    assert_bool "conj produced an incorrect value." ( eq n n' ));

  "inv 1" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let r = Quaternion.of_float_tuple (sr, ir, jr, kr) in
    let d = Quaternion.of_float_tuple (sd, id, jd, kd) in
    let m = Dualquaternion.of_quaternion_tuple (r, d) in
    let n = Dualquaternion.of_quaternion_tuple Quaternion.(
      inv r,
      neg (mul (inv r) (mul d (inv r)))
    ) in
    let n' = Dualquaternion.inv m in
    assert_bool "inv produced an incorrect value." ( eq n n' ));

  "inv 2" >:: (fun _ ->
    let s = Dual.{ re = 2.0; du = 3.0 } in
    let m = Dualquaternion.of_scalar s in
    let n = Dualquaternion.of_scalar (Dual.inv s) in
    let n' = Dualquaternion.inv m in
    assert_bool "inv produced an incorrect value." ( eq n n' ));

  "inv 3" >:: (fun _ ->
    let r = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    let m = Dualquaternion.of_real r in
    let n = Dualquaternion.of_real (Quaternion.inv r) in
    let n' = Dualquaternion.inv m in
    assert_bool "inv produced an incorrect value." ( eq n n' ));

  "inv 4" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let m = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let n = Dualquaternion.( mul m (inv m) ) in
    assert_bool
      "inv produced an incorrect value."
      ( is_close n Dualquaternion.one ));

  "inv 5" >:: (fun _ ->
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let m = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let n = Dualquaternion.( mul (inv m) m ) in
    assert_bool
      "inv produced an incorrect value."
      ( is_close n Dualquaternion.one ));

  "exp 1" >:: (fun _ ->
    (* Test exp of 0. *)
    let n = Dualquaternion.one in
    let n' = Dualquaternion.( exp zero ) in
    assert_bool "exp produced an incorrect value." ( eq n n' ));

  "exp 2" >:: (fun _ ->
    (* Test exp of 1. *)
    let n = Dualquaternion.of_real Quaternion.( exp one ) in
    let n' = Dualquaternion.( exp one ) in
    assert_bool "exp produced an incorrect value." ( eq n n' ));

  "exp 3" >:: (fun _ ->
    (* Test exp of a dual number. *)
    let s = Dual.{ re = 2.0; du = 3.0 } in
    let n = Dualquaternion.of_scalar Dual.( exp s ) in
    let m = Dualquaternion.of_scalar s in
    let n' = Dualquaternion.exp m in
    assert_bool "exp produced an incorrect value." ( eq n n' ));

  "exp 4" >:: (fun _ ->
    (* Test exp of a quaternion. *)
    let q = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    let n = Dualquaternion.of_real Quaternion.( exp q ) in
    let m = Dualquaternion.of_real q in
    let n' = Dualquaternion.exp m in
    assert_bool "exp produced an incorrect value." ( eq n n' ));

  "exp 5" >:: (fun _ ->
    (* Test exp of a pure dual quaternion. *)
    let sd, id, jd, kd = (2.0, 3.0, 4.0, 5.0) in
    let q = Quaternion.of_float_tuple (sd, id, jd, kd) in
    let n = Dualquaternion.of_quaternion_tuple (Quaternion.one, q) in
    let m = Dualquaternion.of_dual q in
    let n' = Dualquaternion.exp m in
    assert_bool "exp produced an incorrect value." ( eq n n' ));

  "exp 6" >:: (fun _ ->
    (* Test that log is the inverse of exp. *)
    (* Choose a small-enough well-behaved dual quaternion. *)
    let sr, ir, jr, kr = (0.1, 0.2, 0.3, 0.4) in
    let sd, id, jd, kd = (0.5, 0.6, 0.7, 0.8) in
    let n = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let n' = Dualquaternion.( log (exp n) ) in
    assert_bool "exp produced an incorrect value." ( is_close n n' ));

  "log 1" >:: (fun _ ->
    (* Test log of 1. *)
    let n = Dualquaternion.zero in
    let n' = Dualquaternion.( log one ) in
    assert_bool "log produced an incorrect value." ( eq n n' ));

  "log 2" >:: (fun _ ->
    (* Test log of a dual number. *)
    let s = Dual.{ re = 2.0; du = 3.0 } in
    let n = Dualquaternion.of_scalar Dual.( log s ) in
    let m = Dualquaternion.of_scalar s in
    let n' = Dualquaternion.log m in
    assert_bool "log produced an incorrect value." ( eq n n' ));

  "log 3" >:: (fun _ ->
    (* Test log of a quaternion. *)
    let q = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    let n = Dualquaternion.of_real Quaternion.( log q ) in
    let m = Dualquaternion.of_real q in
    let n' = Dualquaternion.log m in
    assert_bool "log produced an incorrect value." ( eq n n' ));

  "log 4" >:: (fun _ ->
    (* Test log of a dual quaternion with [one] as real part. *)
    let sd, id, jd, kd = (2.0, 3.0, 4.0, 5.0) in
    let d = Quaternion.of_float_tuple (sd, id, jd, kd) in
    let n = Dualquaternion.of_quaternion_tuple (Quaternion.zero, d) in
    let m = Dualquaternion.of_quaternion_tuple (Quaternion.one, d) in
    let n' = Dualquaternion.log m in
    assert_bool "log produced an incorrect value." ( eq n n' ));

  "log 5" >:: (fun _ ->
    (* Test that log is the inverse of exp. *)
    let sr, ir, jr, kr = (1.0, 2.0, 3.0, 4.0) in
    let sd, id, jd, kd = (5.0, 6.0, 7.0, 8.0) in
    let n = Dualquaternion.of_float_tuples (sr, ir, jr, kr) (sd, id, jd, kd) in
    let n' = Dualquaternion.( exp (log n) ) in
    assert_bool "log produced an incorrect value." ( is_close n n' ));

  "apply" >:: (fun _ ->
    let m = Dualquaternion.of_float_tuples
      (1.0, 2.0, 3.0, 4.0)
      (5.0, 6.0, 7.0, 8.0)
    in
    let n = Dualquaternion.of_float_tuples
      (9.0, 0.0, 1.0, 2.0)
      (3.0, 4.0, 5.0, 6.0)
    in
    let p = Dualquaternion.( mul m (mul n (inv m)) ) in
    let p' = Dualquaternion.apply m n in
    assert_bool "apply produced an incorrect value." ( eq p p' ));

  "add" >:: (fun _ ->
    let sr1, ir1, jr1, kr1 = (1.0, 2.0, 3.0, 4.0) in
    let sd1, id1, jd1, kd1 = (5.0, 6.0, 7.0, 8.0) in
    let sr2, ir2, jr2, kr2 = (9.0, 0.0, 1.0, 2.0) in
    let sd2, id2, jd2, kd2 = (3.0, 4.0, 5.0, 6.0) in
    let m = Dualquaternion.of_float_tuples
      (sr1, ir1, jr1, kr1)
      (sd1, id1, jd1, kd1)
    in
    let n = Dualquaternion.of_float_tuples
      (sr2, ir2, jr2, kr2)
      (sd2, id2, jd2, kd2)
    in
    let p = Dualquaternion.of_float_tuples
      (sr1 +. sr2, ir1 +. ir2, jr1 +. jr2, kr1 +. kr2)
      (sd1 +. sd2, id1 +. id2, jd1 +. jd2, kd1 +. kd2)
    in
    let p' = Dualquaternion.add m n in
    assert_bool "add produced an incorrect value." ( eq p p' ));

  "sub" >:: (fun _ ->
    let sr1, ir1, jr1, kr1 = (1.0, 2.0, 3.0, 4.0) in
    let sd1, id1, jd1, kd1 = (5.0, 6.0, 7.0, 8.0) in
    let sr2, ir2, jr2, kr2 = (9.0, 0.0, 1.0, 2.0) in
    let sd2, id2, jd2, kd2 = (3.0, 4.0, 5.0, 6.0) in
    let m = Dualquaternion.of_float_tuples
      (sr1, ir1, jr1, kr1)
      (sd1, id1, jd1, kd1)
    in
    let n = Dualquaternion.of_float_tuples
      (sr2, ir2, jr2, kr2)
      (sd2, id2, jd2, kd2)
    in
    let p = Dualquaternion.of_float_tuples
      (sr1 -. sr2, ir1 -. ir2, jr1 -. jr2, kr1 -. kr2)
      (sd1 -. sd2, id1 -. id2, jd1 -. jd2, kd1 -. kd2)
    in
    let p' = Dualquaternion.sub m n in
    assert_bool "sub produced an incorrect value." ( eq p p' ));

  "mul" >:: (fun _ ->
    let r1 = Quaternion.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    let d1 = Quaternion.of_float_tuple (5.0, 6.0, 7.0, 8.0) in
    let r2 = Quaternion.of_float_tuple (9.0, 0.0, 1.0, 2.0) in
    let d2 = Quaternion.of_float_tuple (3.0, 4.0, 5.0, 6.0) in
    let m = Dualquaternion.of_quaternion_tuple (r1, d1) in
    let n = Dualquaternion.of_quaternion_tuple (r2, d2) in
    let p = Dualquaternion.of_quaternion_tuple Quaternion.(
      mul r1 r2,
      add (mul r1 d2) (mul d1 r2)
    ) in
    let p' = Dualquaternion.mul m n in
    assert_bool "mul produced an incorrect value." ( eq p p' ));

  "div" >:: (fun _ ->
    let m = Dualquaternion.of_float_tuples
      (1.0, 2.0, 3.0, 4.0)
      (5.0, 6.0, 7.0, 8.0)
    in
    let n = Dualquaternion.of_float_tuples
      (9.0, 0.0, 1.0, 2.0)
      (3.0, 4.0, 5.0, 6.0)
    in
    let p = Dualquaternion.( mul m (inv n) ) in
    let p' = Dualquaternion.div m n in
    assert_bool "div produced an incorrect value." ( eq p p' ));
]
