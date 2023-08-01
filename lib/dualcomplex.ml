type t = Complex.t * Complex.t

let zero = Complex.(zero, zero)

let one = Complex.(one, zero)

let of_float_tuple (a, b, c, d) =
  Complex.( { re = a; im = b }, { re = c; im = d } )

let of_complex_tuple (p, q) = (p, q)

let of_complex p = (p, Complex.zero)

let of_dual q = (Complex.zero, q)

let of_scalar s = (Complex.{ re = s; im = 0.0 }, Complex.zero)

let of_translation_rotation d r =

  (* Avoid using [Complex.sqrt] to compute the pure complex part as it
     only produces complex numbers with arguments between 0 and
     [0.5 *. pi]. *)
  let n, a = Complex.(norm r, arg r) in
  let p = Complex.polar (sqrt n) (0.5 *. a) in

  let q = Complex.( mul {re = 0.5; im = 0.0 } (mul (inv p) d) ) in

  (p, q)

let of_translation d = Complex.(one, mul { re = 0.5; im = 0.0 } d)

let of_rotation r =
  (* Avoid using [Complex.sqrt] to compute the pure complex part as it
     only produces complex numbers with arguments between 0 and
     [0.5 *. pi]. *)
  let n, a = Complex.(norm r, arg r) in
  Complex.(polar (Float.sqrt n) (0.5 *. a), zero)

let to_float_tuple (p, q) = Complex.(p.re, p.im, q.re, q.im)

let to_complex_tuple (p, q) = (p, q)

let to_translation_rotation (p, q) =
  let d = Complex.( mul { re = 2.0; im = 0.0 } (mul p q) ) in
  let r = Complex.mul p p in
  (d, r)

let complex (p, _) = p

let dual (_, q) = q

let translation (p, q) = Complex.( mul { re = 2.0; im = 0.0 } (mul p q) )

let rotation (p, _) = Complex.mul p p

let norm2 (p, _) = Complex.norm2 p

let norm (p, _) = Complex.norm p

let normalize (p, q) =
  let n = norm (p, q) in
  Complex.(
    { re = p.re /. n; im = p.im /. n },
    { re = q.re /. n; im = q.im /. n })

let neg (p, q) = Complex.(neg p, neg q)

let conj (p, q) = Complex.(conj p, q)

let inv (p, q) =
  let norm2_p = Complex.( mul p (conj p) ) in
  Complex.( inv p, neg (div q norm2_p) )

let exp (p, q) =
  let ea = exp Complex.( p.re ) in
  (* Take care of edge case where [p.im = 0.0]. *)
  if Complex.( p.im = 0.0 ) then
    of_float_tuple Complex.(ea, 0.0, ea *. q.re, ea *. q.im)
  else
    let sb = sin Complex.( p.im ) in
    Complex.( exp p, mul { re = ea *. sb /. p.im; im = 0.0 } q )

let log (p, q) =
  (* Take care of the edge case where [(p, q)] is a scalar. *)
  if Complex.( p.im = 0.0 && q.re = 0.0 && q.im = 0.0 ) then
    Complex.({ re = Float.log p.re; im = 0.0 }, zero)
  else
    let lp = Complex.log p in
    Complex.(lp, mul { re = lp.im /. p.im; im = 0.0 } q )

let add (p1, q1) (p2, q2) = Complex.( add p1 p2, add q1 q2 )

let sub (p1, q1) (p2, q2) = Complex.( sub p1 p2, sub q1 q2 )

let mul (p1, q1) (p2, q2) = Complex.(
  mul p1 p2,
  add (mul q1 (conj p2) ) (mul p1 q2) )

let apply m n = mul m (mul n (conj m))

let div x y = mul x (inv y)
