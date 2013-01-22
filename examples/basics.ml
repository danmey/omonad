module Exception = struct

  exception Error of string

  type 'a t =
    | Continue of 'a
    | Exception of string

  let return x = Continue x

  let fail str = Exception str

  let bind x f = match x with
  | Exception str -> x
  | Continue x -> f x

  let run = function
  | Continue x -> x
  | Exception str -> raise (Error str)

end

let () =
  let compute c =
    Exception.(perform begin
      a <-- return (1+2);
      b <-- return (a+4);
      return (b + a * c)
    end)
  in
  let computation =
    Exception.(perform begin
      a <-- compute 10;
      b <-- return (a - 37);
      c <-- if b = 0 then fail "Division by zero!" else return (80 / b);
      return (a+b)
    end)
  in
  print_int (Exception.run computation)
