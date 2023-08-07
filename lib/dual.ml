type t = { re : float; du : float }

let zero = { re = 0.0; du = 0.0 }

let one = { re = 1.0; du = 0.0 }

let e = { re = 0.0; du = 1.0 }

let norm2 { re = x; du = _ } = x *. x

let norm { re = x; du = _ } = Float.abs x

let normalize { re = x; du = y } =
  let n = Float.abs x in
  { re = x /. n; du = y /. n }

let neg { re = x; du = y } = { re = ~-. x; du = ~-. y }

let conj { re = x; du = y } = { re = x; du = ~-. y }

let inv { re = x; du = y } = { re = 1.0 /. x; du = ~-. y /. (x *. x) }

let sqrt { re = x; du = y } =
  let sx = sqrt x in
  { re = sx; du = y /. (2.0 *. sx) }

let exp { re = x; du = y } =
  let ex = exp x in
  { re = ex; du = ex *. y }

let log { re = x; du = y } = { re = log x; du = y /. x }

let add { re = x1; du = y1 } { re = x2; du = y2 } =
  { re = x1 +. x2; du = y1 +. y2 }

let sub { re = x1; du = y1 } { re = x2; du = y2 } =
  { re = x1 -. x2; du = y1 -. y2 }

let mul { re = x1; du = y1 } { re = x2; du = y2 } =
  { re = x1 *. x2; du = x1 *. y2 +. y1 *. x2 }

let div m n = mul m (inv n)

let pow { re = x1; du = y1 } { re = x2; du = y2 } =
  let a = x1 ** x2 in
  { re = a; du = a *. (y2 *. (Float.log x1) +. y1 *. x2 /. x1) }
