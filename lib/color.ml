type t = int * int * int * int

let fail_if_invalid c x =
  let invalid_int x = x < 0x00 || x > 0xff in
  if invalid_int x then
    failwith (Printf.sprintf "color component %c has invalid value %d" c x)

let create ~r ~g ~b ~a =
  fail_if_invalid 'r' r;
  fail_if_invalid 'g' g;
  fail_if_invalid 'b' b;
  fail_if_invalid 'a' a;
  (r, g, b, a)

let r (r, _, _, _) = r
let g (_, g, _, _) = g
let b (_, _, b, _) = b
let a (_, _, _, a) = a
