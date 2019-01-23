(* ===== PRVA NALOGA ===== *)

type izraz = 
  | Samo of int
  | Plus of izraz * izraz
  | Krat of izraz * izraz

let primer = Krat (Plus (Samo 1, Samo 2), Krat (Samo 3, Samo 4))


(* a *)
let rec izracunaj = function
  | Samo(x) -> x
  | Plus (x, y) -> (izracunaj x) + (izracunaj y)
  | Krat (x, y) -> (izracunaj x) * (izracunaj y)

