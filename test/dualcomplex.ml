open OUnit2

open Hamilton


let tol = 1e-6

let sqrt' x =
  let n, a = Complex.(norm x, arg x) in
  Complex.polar (sqrt n) (0.5 *. a)

let eq m n =
  let a1, b1, c1, d1 = Dualcomplex.to_float_tuple m in
  let a2, b2, c2, d2 = Dualcomplex.to_float_tuple n in
  a1 = a2 && b1 = b2 && c1 = c2 && d1 = d2

let is_close m n =
  let a1, b1, c1, d1 = Dualcomplex.to_float_tuple m in
  let a2, b2, c2, d2 = Dualcomplex.to_float_tuple n in
  Float.(
    abs (a2 -. a1) < tol &&
    abs (b2 -. b1) < tol &&
    abs (c2 -. c1) < tol &&
    abs (d2 -. d1) < tol )


let tests = "Dualcomplex tests" >::: [

  "zero 1" >:: (fun _ ->
    let n = Dualcomplex.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    assert_bool
      "Addition with zero changed the value."
      ( eq n Dualcomplex.( add zero n ) ));

  "zero 2" >:: (fun _ ->
    let n = Dualcomplex.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    assert_bool
      "Addition with zero changed the value."
      ( eq n Dualcomplex.( add n zero )));

  "zero 3" >:: (fun _ ->
    let n = Dualcomplex.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    assert_bool
      "Multiplication by zero is not zero."
      Dualcomplex.( eq zero (mul zero n) ));

  "zero 4" >:: (fun _ ->
    let n = Dualcomplex.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    assert_bool
      "Multiplication by zero is not zero."
      Dualcomplex.( eq zero (mul n zero) ));

  "one 1" >:: (fun _ ->
    let n = Dualcomplex.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    assert_bool
      "Multiplication by one changed the value."
      ( eq n Dualcomplex.( mul one n ) ));

  "one 2" >:: (fun _ ->
    let n = Dualcomplex.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    assert_bool
      "Multiplication by one changed the value."
      ( eq n Dualcomplex.( mul n one ) ));

  "of_float_tuple to_float_tuple" >:: (fun _ ->
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualcomplex.of_float_tuple (a, b, c, d) in
    let a', b', c', d' = Dualcomplex.to_float_tuple n in
    assert_bool
      "Conversion from and to tuple failed."
      (a = a' && b = b' && c = c' && d = d'));

  "of_complex_tuple" >:: (fun _ ->
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let p = Complex.{ re = a; im = b } in
    let q = Complex.{ re = c; im = d } in
    let n = Dualcomplex.of_complex_tuple (p, q) in
    let n' = Dualcomplex.of_float_tuple (a, b, c, d) in
    assert_bool "of_complex_tuple produced an incorrect value." ( eq n n' ));

  "of_complex" >:: (fun _ ->
    let a, b = (1.0, 2.0) in
    let p = Complex.{ re = a; im = b } in
    let n = Dualcomplex.of_complex p in
    let n' = Dualcomplex.of_float_tuple (a, b, 0.0, 0.0) in
    assert_bool "of_complex produced an incorrect value." ( eq n n' ));

  "of_dual" >:: (fun _ ->
    let a, b = (1.0, 2.0) in
    let q = Complex.{ re = a; im = b } in
    let n = Dualcomplex.of_dual q in
    let n' = Dualcomplex.of_float_tuple (0.0, 0.0, a, b) in
    assert_bool "of_dual produced an incorrect value." ( eq n n' ));

  "of_scalar" >:: (fun _ ->
    let s = 2.0 in
    let n = Dualcomplex.of_scalar s in
    let n' = Dualcomplex.of_float_tuple (s, 0.0, 0.0, 0.0) in
    assert_bool "of_scalar produced an incorrect value." ( eq n n' ));

  "of_translation_rotation" >:: (fun _ ->
    let a, b, c = (1.0, 2.0, 3.0) in
    let d = Complex.{ re = a; im = b } in
    let r = Complex.polar 1.0 c in
    let n = Dualcomplex.of_translation_rotation d r in
    let p = sqrt' r in
    let q = Complex.( mul { re = 0.5; im = 0.0 } (mul (inv p) d) ) in
    let n' = Dualcomplex.of_complex_tuple (p, q) in
    assert_bool
      "of_translation_rotation produced an incorrect value."
      ( eq n n' ));

  "of_translation" >:: (fun _ ->
    let a, b = (1.0, 2.0) in
    let d = Complex.{ re = a; im = b } in
    let n = Dualcomplex.of_translation d in
    let n' = Dualcomplex.of_translation_rotation d Complex.one in
    assert_bool "of_translation produced an incorrect value." ( eq n n' ));

  "of_rotation" >:: (fun _ ->
    let a = 1.0 in
    let r = Complex.polar 1.0 a in
    let n = Dualcomplex.of_rotation r in
    let n' = Dualcomplex.of_translation_rotation Complex.zero r in
    assert_bool "of_rotation produced an incorrect value." ( eq n n' ));

  "to_complex_tuple" >:: (fun _ ->
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualcomplex.of_float_tuple (a, b, c, d) in
    let p, q = Dualcomplex.to_complex_tuple n in
    let a', b', c', d' = Complex.(p.re, p.im, q.re, q.im) in
    assert_bool
      "to_complex_tuple produced an incorrect value."
      (a = a' && b = b' && c = c' && d = d'));

  "to_translation_rotation" >:: (fun _ ->
    let p, q = Complex.( { re = 0.0; im = 1.0 }, { re = 2.0; im = 3.0 } ) in
    let n = Dualcomplex.of_complex_tuple (p, q) in
    let d, r = Dualcomplex.to_translation_rotation n in
    let r' = Complex.mul p p in
    let d' = Complex.( mul { re = 2.0; im = 0.0 } (mul p q) ) in
    assert_bool
      "to_translation_rotation produced an incorrect value."
      Complex.(
        d.re = d'.re && d.im = d'.im && r.re = r'.re && r.im = r'.im
      ));

  "complex" >:: (fun _ ->
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualcomplex.of_float_tuple (a, b, c, d) in
    let p = Dualcomplex.complex n in
    let a', b' = Complex.(p.re, p.im) in
    assert_bool
      "to_complex_tuple produced an incorrect value."
      (a = a' && b = b'));

  "dual" >:: (fun _ ->
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualcomplex.of_float_tuple (a, b, c, d) in
    let q = Dualcomplex.dual n in
    let c', d' = Complex.(q.re, q.im) in
    assert_bool
      "to_complex_tuple produced an incorrect value."
      (c = c' && d = d'));

  "translation" >:: (fun _ ->
    let p, q = Complex.( { re = 0.0; im = 1.0 }, { re = 2.0; im = 3.0 } ) in
    let n = Dualcomplex.of_complex_tuple (p, q) in
    let d = Dualcomplex.translation n in
    let d' = Complex.( mul { re = 2.0; im = 0.0 } (mul p q) ) in
    assert_bool
      "translation produced an incorrect value."
      Complex.( d.re = d'.re && d.im = d'.im ));

  "rotation" >:: (fun _ ->
    let p, q = Complex.( { re = 0.0; im = 1.0 }, { re = 2.0; im = 3.0 } ) in
    let n = Dualcomplex.of_complex_tuple (p, q) in
    let r = Dualcomplex.rotation n in
    let r' = Complex.mul p p in
    assert_bool
      "rotation produced an incorrect value."
      Complex.( r.re = r'.re && r.im = r'.im ));

  "norm2" >:: (fun _ ->
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualcomplex.of_float_tuple (a, b, c, d) in
    let x = Dualcomplex.norm2 n in
    let x' = Complex.norm2 (Dualcomplex.complex n) in
    assert_bool "norm2 produced an incorrect value." ( x = x' ));

  "norm" >:: (fun _ ->
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualcomplex.of_float_tuple (a, b, c, d) in
    let x = Dualcomplex.norm n in
    let x' = Complex.norm (Dualcomplex.complex n) in
    assert_bool "norm produced an incorrect value." ( x = x' ));

  "normalize 1" >:: (fun _ ->
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualcomplex.of_float_tuple (a, b, c, d) in
    let x = Complex.norm (Dualcomplex.complex n) in
    let m = Dualcomplex.of_float_tuple (a /. x, b /. x, c /. x, d /. x) in
    let m' = Dualcomplex.normalize n in
    assert_bool "normalize produced an incorrect value." ( eq m m' ));

  "normalize 2" >:: (fun _ ->
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let m = Dualcomplex.of_float_tuple (a, b, c, d) in
    let n' = Dualcomplex.normalize m in
    let n = Dualcomplex.norm n' in
    assert_bool "normalize resulted in a norm different than 1." ( n = 1.0 ));

  "neg 1" >:: (fun _ ->
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualcomplex.of_float_tuple (a, b, c, d) in
    let m = Dualcomplex.of_float_tuple (~-. a, ~-. b, ~-. c, ~-. d) in
    let m' = Dualcomplex.neg n in
    assert_bool "neg produced an incorrect value." ( eq m m' ));

  "neg 2" >:: (fun _ ->
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualcomplex.of_float_tuple (a, b, c, d) in
    let m = Dualcomplex.( add n (neg n) ) in
    assert_bool "neg produced an incorrect value." ( eq m Dualcomplex.zero ));

  "neg 3" >:: (fun _ ->
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualcomplex.of_float_tuple (a, b, c, d) in
    let m = Dualcomplex.( add (neg n) n ) in
    assert_bool "neg produced an incorrect value." ( eq m Dualcomplex.zero ));

  "conj" >:: (fun _ ->
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualcomplex.of_float_tuple (a, b, c, d) in
    let m = Dualcomplex.of_float_tuple (a, ~-. b, c, d) in
    let m' = Dualcomplex.conj n in
    assert_bool "conj produced an incorrect value." ( eq m m' ));

  "inv 1" >:: (fun _ ->
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualcomplex.of_float_tuple (a, b, c, d) in
    let x2 = Dualcomplex.norm2 n in
    let
      m = Dualcomplex.of_float_tuple (
        a /. x2,
        ~-. b /. x2,
        ~-. c /. x2,
        ~-. d /. x2)
    in
    let m' = Dualcomplex.inv n in
    assert_bool "inv produced an incorrect value." ( eq m m' ));

  "inv 2" >:: (fun _ ->
    let s = 2.0 in
    let n = Dualcomplex.of_scalar s in
    let m = Dualcomplex.of_scalar (1.0 /. s) in
    let m' = Dualcomplex.inv n in
    assert_bool "inv produced an incorrect value." ( eq m m' ));

  "inv 3" >:: (fun _ ->
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualcomplex.of_float_tuple (a, b, c, d) in
    let m = Dualcomplex.( mul n (inv n) ) in
    assert_bool
      "inv produced an incorrect value."
      ( is_close m Dualcomplex.one ));

  "inv 4" >:: (fun _ ->
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualcomplex.of_float_tuple (a, b, c, d) in
    let m = Dualcomplex.( mul (inv n) n ) in
    assert_bool
      "inv produced an incorrect value."
      ( is_close m Dualcomplex.one ));

  "exp 1" >:: (fun _ ->
    (* Test exp of 0. *)
    let n = Dualcomplex.zero in
    let m = Dualcomplex.one in
    let m' = Dualcomplex.exp n in
    assert_bool "exp produced an incorrect value." ( eq m m' ));

  "exp 2" >:: (fun _ ->
    (* Test exp of 1. *)
    let n = Dualcomplex.one in
    let m = Dualcomplex.of_complex Complex.( exp one ) in
    let m' = Dualcomplex.exp n in
    assert_bool "exp produced an incorrect value." ( eq m m' ));

  "exp 3" >:: (fun _ ->
    (* Test exp of a zero-scalar-part dual complex. *)
    let a, b, c = (1.0, 2.0, 3.0) in
    let p, q = Complex.( { re = 0.0; im = a }, { re = b; im = c }) in
    let n = Dualcomplex.of_complex_tuple (p, q) in
    let
      m = Dualcomplex.of_complex_tuple Complex.(
        polar 1.0 a,
        mul { re = (sin a) /. a; im = 0.0 } q)
    in
    let m' = Dualcomplex.exp n in
    assert_bool "exp produced an incorrect value." ( eq m m' ));

  "exp 4" >:: (fun _ ->
    (* Test implementation againts the formula in the article. *)
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let p, q = Complex.( { re = a; im = b }, { re = c; im = d }) in
    let n = Dualcomplex.of_complex_tuple (p, q) in
    let
      m = Dualcomplex.of_complex_tuple Complex.(
        exp p,
        mul
          (div
            (sub (exp p) (exp (conj p)))
            (sub p (conj p)))
        q)
    in
    let m' = Dualcomplex.exp n in
    assert_bool "exp produced an incorrect value." ( eq m m' ));

  "exp 5" >:: (fun _ ->
    (* Test that log is the inverse of exp. *)
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualcomplex.of_float_tuple (a, b, c, d) in
    let n' = Dualcomplex.( log (exp n) ) in
    assert_bool "exp produced an incorrect value." ( eq n n' ));

  "log 1" >:: (fun _ ->
    (* Test log of 1. *)
    let n = Dualcomplex.one in
    let m = Dualcomplex.zero in
    let m' = Dualcomplex.log n in
    assert_bool "log produced an incorrect value." ( eq m m' ));

  "log 2" >:: (fun _ ->
    (* Test log of a unit-norm dual complex. *)
    let a = 1.0 in
    let b, c = (2.0, 3.0) in
    let p = Complex.polar 1.0 a in
    let q = Complex.( { re = b; im = c } ) in
    let n = Dualcomplex.of_complex_tuple (p, q) in
    let
      m = Dualcomplex.of_complex_tuple Complex.(
        { re = 0.0; im = a },
        mul { re = a /. (sin a); im = 0.0 } q)
    in
    let m' = Dualcomplex.log n in
    assert_bool "log produced an incorrect value." ( eq m m' ));

  "log 3" >:: (fun _ ->
    (* Test the implementation against the inverse of the formula in
       the article. *)
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let p, q = Complex.( { re = a; im = b }, { re = c; im = d }) in
    let n = Dualcomplex.of_complex_tuple (p, q) in
    let
      m = Dualcomplex.of_complex_tuple Complex.(
        log p,
        mul
          (div
            (sub (log p) (log (conj p)))
            (sub p (conj p)))
          q)
    in
    let m' = Dualcomplex.log n in
    assert_bool "log produced an incorrect value." ( eq m m' ));

  "log 4" >:: (fun _ ->
    (* Test that exp is the inverse of log. *)
    let a, b, c, d = (1.0, 2.0, 3.0, 4.0) in
    let n = Dualcomplex.of_float_tuple (a, b, c, d) in
    let n' = Dualcomplex.( exp (log n) ) in
    assert_bool "log produced an incorrect value." ( is_close n n' ));

  "apply" >:: (fun _ ->
    let m = Dualcomplex.of_float_tuple (1.0, 2.0, 3.0, 4.0) in
    let n = Dualcomplex.of_float_tuple (5.0, 6.0, 7.0, 8.0) in
    let p = Dualcomplex.( mul m (mul n (inv m)) ) in
    let p' = Dualcomplex.apply m n in
    assert_bool "apply produced an incorrect value." ( eq p p' ));

  "add" >:: (fun _ ->
    let a1, b1, c1, d1 = (1.0, 2.0, 3.0, 4.0) in
    let a2, b2, c2, d2 = (5.0, 6.0, 7.0, 8.0) in
    let x = Dualcomplex.of_float_tuple (a1, b1, c1, d1) in
    let y = Dualcomplex.of_float_tuple (a2, b2, c2, d2) in
    let
      z = Dualcomplex.of_float_tuple (a1 +. a2, b1 +. b2, c1 +. c2, d1 +. d2)
    in
    let z' = Dualcomplex.add x y in
    assert_bool "add produced an incorrect value." ( eq z z' ));

  "sub" >:: (fun _ ->
    let a1, b1, c1, d1 = (1.0, 2.0, 3.0, 4.0) in
    let a2, b2, c2, d2 = (5.0, 6.0, 7.0, 8.0) in
    let x = Dualcomplex.of_float_tuple (a1, b1, c1, d1) in
    let y = Dualcomplex.of_float_tuple (a2, b2, c2, d2) in
    let
      z = Dualcomplex.of_float_tuple (a1 -. a2, b1 -. b2, c1 -. c2, d1 -. d2)
    in
    let z' = Dualcomplex.sub x y in
    assert_bool "sub produced an incorrect value." ( eq z z' ));

  "mul" >:: (fun _ ->
    let p1, q1 = Complex.({ re = 1.0; im = 2.0 }, { re = 3.0; im = 4.0 }) in
    let p2, q2 = Complex.({ re = 5.0; im = 6.0 }, { re = 7.0; im = 8.0 }) in
    let x = Dualcomplex.of_complex_tuple (p1, q1) in
    let y = Dualcomplex.of_complex_tuple (p2, q2) in
    let z = Dualcomplex.of_complex_tuple Complex.(
      mul p1 p2,
      add (mul q1 (conj p2)) (mul p1 q2))
    in
    let z' = Dualcomplex.mul x y in
    assert_bool "mul produced an incorrect value." ( eq z z' ));

  "div" >:: (fun _ ->
    let a1, b1, c1, d1 = (1.0, 2.0, 3.0, 4.0) in
    let a2, b2, c2, d2 = (5.0, 6.0, 7.0, 8.0) in
    let x = Dualcomplex.of_float_tuple (a1, b1, c1, d1) in
    let y = Dualcomplex.of_float_tuple (a2, b2, c2, d2) in
    let z = Dualcomplex.( mul x (inv y) ) in
    let z' = Dualcomplex.div x y in
    assert_bool "div produced an incorrect value." ( eq z z' ));
]
