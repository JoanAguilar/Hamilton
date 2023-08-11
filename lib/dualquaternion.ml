type t = Quaternion.t * Quaternion.t

let zero = Quaternion.(zero, zero)

let one = Quaternion.(one, zero)

let of_float_tuples r d = Quaternion.(of_float_tuple r, of_float_tuple d)

let of_quaternion_tuple (r, d) = (r, d)

let of_dual_tuple (s, i, j, k) =
  let r = Quaternion.of_float_tuple Dual.(s.re, i.re, j.re, k.re) in
  let d = Quaternion.of_float_tuple Dual.(s.du, i.du, j.du, k.du) in
  (r, d)

let of_real r = (r, Quaternion.zero)

let of_dual d = (Quaternion.zero, d)

let of_scalar s =
  let r = Quaternion.of_scalar Dual.( s.re ) in
  let d = Quaternion.of_scalar Dual.( s.du ) in
  (r, d) 

let of_vector (i, j, k) = 
  let r = Quaternion.of_vector Dual.(i.re, j.re, k.re) in
  let d = Quaternion.of_vector Dual.(i.du, j.du, k.du) in
  (r, d)

let of_translation_rotation d' r' =
  (r', Quaternion.( mul (of_scalar 0.5) (mul d' r') ))

let of_translation d' = Quaternion.(one, mul (of_scalar 0.5) d')

let of_rotation r' = (r', Quaternion.zero)

let to_float_tuples (r, d) = Quaternion.(to_float_tuple r, to_float_tuple d)

let to_quaternion_tuple (r, d) = (r, d)

let to_dual_tuple (r, d) =
  let sr, ir, jr, kr = Quaternion.to_float_tuple r in
  let sd, id, jd, kd = Quaternion.to_float_tuple d in
  Dual.(
    { re = sr; du = sd },
    { re = ir; du = id },
    { re = jr; du = jd },
    { re = kr; du = kd })

let real (r, _) = r

let dual (_, d) = d

let scalar (r, d) = Dual.{ re = Quaternion.scalar r; du = Quaternion.scalar d }

let vector (r, d) =
  let ir, jr, kr = Quaternion.vector r in
  let id, jd, kd = Quaternion.vector d in
  Dual.(
    { re = ir; du = id },
    { re = jr; du = jd },
    { re = kr; du = kd })

let to_translation_rotation (p, q) =
  (Quaternion.( mul (of_scalar 2.0) (mul q (inv p)) ), p)

let translation (p, q) = Quaternion.( mul (of_scalar 2.0) (mul q (inv p)) )

let rotation (p, _) = p

let neg (r, d) = Quaternion.(neg r, neg d)

let conj (r, d) = Quaternion.(conj r, conj d)

let inv (r, d) = 
  let ir = Quaternion.inv r in
  (ir, Quaternion.( neg (mul ir (mul d ir)) ))

let exp (r, d) =
  (* See: Dantam, Neil T. "Practical exponential coordinates using
     implicit dual quaternions." _Algorithmic Foundations of Robotics
     XIII: Proceedings of the 13th Workshop on the Algorithmic
     Foundations of Robotics 13._ Springer International Publishing,
     2020. *)

  (* Extract the components of the quaternions. *)
  let sr, ir, jr, kr = Quaternion.to_float_tuple r in
  let sd, id, jd, kd = Quaternion.to_float_tuple d in

  (* Some coefficients that show up in the exponential. *)
  let psi = sqrt (ir *. ir +. jr *. jr +. kr *. kr) in
  let gamma = ir *. id +. jr *. jd +. kr *. kd in
  let sinc_psi = if psi = 0.0 then 1.0 else (sin psi) /. psi in
  let cosinc_psi =
    if psi = 0.0 then
      ~-. (1.0 /. 3.0)
    else
      ((cos psi) -. sinc_psi) /. (psi *. psi)
  in
  let esr = Float.exp sr in
  let es' = Dual.{ re = esr; du = sd *. esr } in

  (* The exponential for each (dual) component of the quaternion. *)
  let es = Dual.( mul es' { re = cos psi; du = ~-. sinc_psi *. gamma } ) in
  let
    ei = Dual.( mul
      es'
      { re = sinc_psi *. ir;
        du = sinc_psi *. id +. cosinc_psi *. gamma *. ir } )
  in
  let
    ej = Dual.( mul
      es'
      { re = sinc_psi *. jr;
        du = sinc_psi *. jd +. cosinc_psi *. gamma *. jr } )
  in
  let
    ek = Dual.( mul
      es'
      { re = sinc_psi *. kr;
        du = sinc_psi *. kd +. cosinc_psi *. gamma *. kr } )
  in

  (* Convert the 4 dual numbers from the exponential to a dual
     quaternion. *)
  of_dual_tuple (es, ei, ej, ek)


let log (r, d) =
  (* See: Dantam, Neil T. "Practical exponential coordinates using
     implicit dual quaternions." _Algorithmic Foundations of Robotics
     XIII: Proceedings of the 13th Workshop on the Algorithmic
     Foundations of Robotics 13._ Springer International Publishing,
     2020. *)

  (* Extract the components of the quaternions. *)
  let sr, ir, jr, kr = Quaternion.to_float_tuple r in
  let sd, id, jd, kd = Quaternion.to_float_tuple d in

  (* Some coefficients that show up in the logarithm. *)
  let nrv = sqrt (ir *. ir +. jr *. jr +. kr *. kr) in
  let psi = atan2 nrv sr in
  let nr = Quaternion.norm r in
  let psi_vec = if psi = 0.0 then 1.0 /. nr else psi /. nrv in
  let nr2 = Quaternion.norm2 r in
  let alpha =
    if nrv = 0.0 then
      ~-. 2.0 /. (3.0 *. nr)
    else
      (sr -. psi_vec *. nr2) /. (nrv *. nrv)
  in
  let beta = ((ir *. id +. jr *. jd +. kr *. kd) *. alpha -. sd) /. nr2 in
  let rd = sr *. sd +. ir *. id +. jr *. jd +. kr *. kd in

  (* The logarithm for each (dual) component of the quaternion. *)
  let ls = Dual.{ re = Float.log nr; du = rd /. nr2 } in
  let li = Dual.{ re = psi_vec *. ir; du = beta *. ir +. psi_vec *. id } in
  let lj = Dual.{ re = psi_vec *. jr; du = beta *. jr +. psi_vec *. jd } in
  let lk = Dual.{ re = psi_vec *. kr; du = beta *. kr +. psi_vec *. kd } in

  (* Convert the 4 dual numbers from the exponential to a dual
     quaternion. *)
  of_dual_tuple (ls, li, lj, lk)

let add (r1, d1) (r2, d2) = Quaternion.(add r1 r2, add d1 d2)

let sub (r1, d1) (r2, d2) = Quaternion.(sub r1 r2, sub d1 d2)

let mul (r1, d1) (r2, d2) = Quaternion.(mul r1 r2, add (mul r1 d2) (mul d1 r2))

let norm2 n =
  let n2 = mul n (conj n) in
  (* The square of the norm of a quaternion is guaranteed to be a dual
     scalar. *)
  Dual.{ re = Quaternion.scalar (real n2); du = Quaternion.scalar (dual n2) }

let norm n = Dual.sqrt (norm2 n)

let normalize n = n |> norm |> Dual.inv |> of_scalar |> mul n

let apply m n = mul m (mul n (inv m))

let div m n = mul m (inv n)
