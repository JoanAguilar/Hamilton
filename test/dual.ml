open OUnit2

open Hamilton


let tol = 1e-6

let eq m n = Dual.( m.re = n.re && m.du = n.du )

let is_close m n =
  Dual.(
    Float.abs (m.re -. n.re) < tol &&
    Float.abs (m.du -. n.du) < tol )


let tests = "Dual numbers tests" >::: [

  "zero 1" >:: (fun _ ->
    let n = Dual.{ re = 1.0; du = 2.0 } in
    assert_bool
      "Addition with zero changed the value."
      ( eq n Dual.( add zero n ) ));

  "zero 2" >:: (fun _ ->
    let n = Dual.{ re = 1.0; du = 2.0 } in
    assert_bool
      "Addition with zero changed the value."
      ( eq n Dual.( add n zero ) ));

  "zero 3" >:: (fun _ ->
    let n = Dual.{ re = 1.0; du = 2.0 } in
    assert_bool
      "Multiplication by zero is not zero."
      Dual.( eq zero (mul zero n) ));

  "zero 4" >:: (fun _ ->
    let n = Dual.{ re = 1.0; du = 2.0 } in
    assert_bool
      "Multiplication by zero is not zero."
      Dual.( eq zero (mul n zero) ));

  "one 1" >:: (fun _ ->
    let n = Dual.{ re = 1.0; du = 2.0 } in
    assert_bool
      "Multiplication by one changed the value."
      ( eq n Dual.( mul one n ) ));

  "one 2" >:: (fun _ ->
    let n = Dual.{ re = 1.0; du = 2.0 } in
    assert_bool
      "Multiplication by one changed the value."
      ( eq n Dual.( mul n one ) ));

  "dual unit" >:: (fun _ ->
    let n = Dual.{ re = 0.0; du = 1.0 } in
    assert_bool
      "Wrong value for the dual unit (e)."
      ( eq n Dual.e ));

  "norm2" >:: (fun _ ->
    let n = Dual.{ re = 2.0; du = 1.0 } in
    let n2 = Dual.( n.re *. n.re ) in
    let n2' = Dual.norm2 n in
    assert_bool "norm2 produced an incorrect value." ( n2 = n2' ));

  "norm" >:: (fun _ ->
    let m = Dual.{ re = 2.0; du = 1.0 } in
    let n = Float.abs Dual.( m.re ) in
    let n' = Dual.norm m in
    assert_bool "norm produced an incorrect value." ( n = n' ));

  "normalize 1" >:: (fun _ ->
    let m = Dual.{ re = 2.0; du = 3.0 } in
    let a = Dual.norm m in
    let n = Dual.{ re = m.re /. a; du = m.du /. a } in
    let n' = Dual.normalize m in
    assert_bool "normalize produced an incorrect value." ( eq n n' ));

  "normalize 2" >:: (fun _ ->
    let m = Dual.{ re = 2.0; du = 3.0 } in
    let n' = Dual.normalize m in
    let n = Dual.norm n' in
    assert_bool "normalize resulted in a norm different than 1." ( n = 1.0 ));

  "neg 1" >:: (fun _ ->
    let m = Dual.{ re = 1.0; du = 2.0 } in
    let n = Dual.{ re = ~-. (m.re); du = ~-. (m.du) } in
    let n' = Dual.neg m in
    assert_bool "neg produced an incorrect value." ( eq n n' ));

  "neg 2" >:: (fun _ ->
    let m = Dual.{ re = 1.0; du = 2.0 } in
    let n = Dual.( add m (neg m) ) in
    assert_bool "neg produced an incorrect value." ( eq n Dual.zero ));

  "neg 3" >:: (fun _ ->
    let m = Dual.{ re = 1.0; du = 2.0 } in
    let n = Dual.( add (neg m) m ) in
    assert_bool "neg produced an incorrect value." ( eq n Dual.zero ));

  "conj" >:: (fun _ ->
    let m = Dual.{ re = 1.0; du = 2.0 } in
    let n = Dual.{ re = m.re; du = ~-. (m.du) } in
    let n' = Dual.conj m in
    assert_bool "conj produced an incorrect value." ( eq n n' ));

  "inv 1" >:: (fun _ ->
    let m = Dual.{ re = 2.0; du = 3.0 } in
    let n = Dual.{ re = 1.0 /. m.re; du = ~-. (m.du) /. (m.re *. m.re) } in
    let n' = Dual.inv m in
    assert_bool "inv produced an incorrect value." ( eq n n' ));

  "inv 2" >:: (fun _ ->
    let s = 2.0 in
    let m = Dual.{ re = s; du = 0.0 } in
    let n = Dual.{ re = 1.0 /. s; du = 0.0 } in
    let n' = Dual.inv m in
    assert_bool "inv produced an incorrect value." ( eq n n' ));

  "inv 3" >:: (fun _ ->
    let m = Dual.{ re = 2.0; du = 3.0 } in
    let n = Dual.( mul m (inv m) ) in
    assert_bool "inv produced an incorrect value." ( eq Dual.one n ));

  "inv 4" >:: (fun _ ->
    let m = Dual.{ re = 2.0; du = 3.0 } in
    let n = Dual.( mul (inv m) m ) in
    assert_bool "inv produced an incorrect value." ( eq Dual.one n ));

  "sqrt 1" >:: (fun _ ->
    let m = Dual.{ re = 2.0; du = 3.0 } in
    let
      n = Dual.{
        re = Float.sqrt (m.re);
        du = m.du /. (2.0 *. (Float.sqrt (m.re))) }
    in
    let n' = Dual.sqrt m in
    assert_bool "sqrt produced an incorrect value." ( eq n n' ));

  "sqrt 2" >:: (fun _ ->
    let s = 2.0 in
    let m = Dual.{ re = s; du = 0.0 } in
    let n = Dual.{ re = Float.sqrt s; du = 0.0 } in
    let n' = Dual.sqrt m in
    assert_bool "sqrt produced an incorrect value." ( eq n n' ));

  "sqrt 3" >:: (fun _ ->
    let n = Dual.{ re = 2.0; du = 3.0 } in
    let m = Dual.sqrt n in
    let n' = Dual.mul m m in
    assert_bool "sqrt produced an incorrect value." ( is_close n n' ));

  "exp 1" >:: (fun _ ->
    (* Test exp of 0. *)
    let n = Dual.one in
    let n' = Dual.( exp zero ) in
    assert_bool "exp produced an incorrect value." ( eq n n' ));

  "exp 2" >:: (fun _ ->
    (* Test exp of 1. *)
    let n = Dual.{ re = Float.exp 1.0; du = 0.0 } in
    let n' = Dual.( exp one ) in
    assert_bool "exp produced an incorrect value." ( eq n n' ));

  "exp 3" >:: (fun _ ->
    (* Test exp of a pure dual number. *)
    let m = Dual.{ re = 0.0; du = 2.0 } in
    let n = Dual.{ re = 1.0; du = m.du } in
    let n' = Dual.exp m in
    assert_bool "exp produced an incorrect value." ( eq n n' ));

  "exp 4" >:: (fun _ ->
    (* Test that log is the inverse of exp. *)
    let n = Dual.{ re = 2.0; du = 3.0 } in
    let n' = Dual.( log (exp n) ) in
    assert_bool "exp produced an incorrect value." ( is_close n n' ));

  "log 1" >:: (fun _ ->
    (* Test log of 1. *)
    let n = Dual.zero in
    let n' = Dual.( log one ) in
    assert_bool "log produced an incorrect value." ( eq n n' ));

  "log 2" >:: (fun _ ->
    (* Test log of a dual number with unit real part. *)
    let m = Dual.{ re = 1.0; du = 2.0 } in
    let n = Dual.{ re = 0.0; du = m.du } in
    let n' = Dual.log m in
    assert_bool "log produced an incorrect value." ( eq n n' ));

  "log 3" >:: (fun _ ->
    (* Test that exp is the inverse of log. *)
    let n = Dual.{ re = 2.0; du = 3.0 } in
    let n' = Dual.( exp (log n) ) in
    assert_bool "log produced an incorrect value." ( eq n n' ));

  "add" >:: (fun _ ->
    let m = Dual.{ re = 1.0; du = 2.0 } in
    let n = Dual.{ re = 3.0; du = 4.0 } in
    let p = Dual.{ re = m.re +. n.re; du = m.du +. n.du } in
    let p' = Dual.add m n in
    assert_bool "add produced an incorrect value." ( eq p p' ));

  "sub" >:: (fun _ ->
    let m = Dual.{ re = 1.0; du = 2.0 } in
    let n = Dual.{ re = 3.0; du = 4.0 } in
    let p = Dual.{ re = m.re -. n.re; du = m.du -. n.du } in
    let p' = Dual.sub m n in
    assert_bool "sub produced an incorrect value." ( eq p p' ));

  "mul" >:: (fun _ ->
    let m = Dual.{ re = 1.0; du = 2.0 } in
    let n = Dual.{ re = 3.0; du = 4.0 } in
    let p = Dual.{ re = m.re *. n.re; du = m.re *. n.du +. m.du *. n.re} in
    let p' = Dual.mul m n in
    assert_bool "mul produced an incorrect value." ( eq p p' ));

  "div" >:: (fun _ ->
    let m = Dual.{ re = 1.0; du = 2.0 } in
    let n = Dual.{ re = 3.0; du = 4.0 } in
    let p = Dual.( mul m (inv n) ) in
    let p' = Dual.div m n in
    assert_bool "div produced an incorrect value." ( eq p p' ));

  "pow 1" >:: (fun _ ->
    let m = Dual.{ re = 2.0; du = 0.0 } in
    let n = Dual.{ re = 3.0; du = 0.0 } in
    let p = Dual.{ re = m.re ** n.re; du = 0.0 } in
    let p' = Dual.pow m n in
    assert_bool "pow produced an incorrect value." ( eq p p' ));

  "pow 2" >:: (fun _ ->
    let m = Dual.{ re = 2.0; du = 0.0 } in
    let n = Dual.{ re = 0.5; du = 0.0 } in
    let p = Dual.sqrt m  in
    let p' = Dual.pow m n in
    assert_bool "pow produced an incorrect value." ( eq p p' ));

  "pow 3" >:: (fun _ ->
    let m = Dual.{ re = 2.0; du = 3.0 } in
    let n = Dual.{ re = 2.0; du = 0.0 } in
    let p = Dual.mul m m  in
    let p' = Dual.pow m n in
    assert_bool "pow produced an incorrect value." ( eq p p' ));

  "pow 4" >:: (fun _ ->
    let m = Dual.{ re = 4.0; du = 5.0 } in
    let n = Dual.{ re = 2.0; du = 3.0 } in
    let p' = Dual.( mul (pow m (neg n)) (pow m n) )  in
    assert_bool "pow produced an incorrect value." ( eq Dual.one p' ));

]
