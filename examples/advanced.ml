(* Based on Xavier Leroy's lectures about monads *)
(* Converted from http://gallium.inria.fr/~xleroy/mpri/2-4/monads.ml *)

(* Not every example has been converted *)
(*********************************************************************)
(* The Caml code for the various monads of lecture 4 *)

(* The State monad, specialized to references over integers *)

module Store = struct
  module IntMap = Map.Make(struct type t = int let compare = compare end)
  type t = { next: int; contents: int IntMap.t }
  type ref = int
  let empty = { next = 0; contents = IntMap.empty }
  let alloc x s =
      (s.next,
       { next = s.next + 1; contents = IntMap.add s.next x s.contents })
  let read r s = IntMap.find r s.contents
  let write r x s =
     { next = s.next; contents = IntMap.add r x s.contents }
end

module State = struct
  type 'a mon = Store.t -> 'a * Store.t

  let return (x: 'a) : 'a mon = fun v -> (x, v)
  let bind (x: 'a mon) (f: 'a -> 'b mon) : 'b mon =
    fun v -> let (x', v') = x v in f x' v'
  let (>>=) = bind

  type 'a result = 'a
  let run (c: 'a mon) : 'a result = fst (c Store.empty)

  let ref (n : int) : Store.ref mon =
    fun s -> Store.alloc n s
  let deref (r: Store.ref) : int mon =
    fun s -> (Store.read r s, s)
  let assign (r: Store.ref) (x: int) : unit mon =
    fun s -> ((), Store.write r x s)
end

let () =
  let computation =
    State.(perform begin
      a <-- ref 42;
      b <-- ref 124;
      assign a 56;
      a <-- deref a;
      b <-- deref b;
      return (a+b)
  end)
  in
  assert (State.run computation = 180)

let () =
  let sumlist l =
    State.(run (perform begin
      r <-- ref 0;
      let rec sum = function
      | [] -> deref r
      | hd :: tl ->
        n <-- deref r;
        assign r (n + hd);
        sum tl
      in sum l
    end))
  in
  assert (sumlist [1;2;3;4;5;6;7] = 28)

module Cont = struct

  type answer = int
  type 'a mon = ('a -> answer) -> answer

  let return (x: 'a) : 'a mon = fun k -> k x
  let bind (m: 'a mon) (f: 'a -> 'b mon) : 'b mon =
    fun k -> m (fun x -> f x k)
  let (>>=) = bind

  let run (m: answer mon) = m (fun x -> x)

  let callcc (f: ('a -> answer) -> 'a mon) : 'a mon =
    fun k -> f k k
  let throw (k: 'a -> answer) (x: 'a) : 'a mon =
    fun _ -> k x
end

let exple_cont n =
  Cont.(run (perform begin
    (callcc (fun k ->
      x <-- (if n < 0 then throw k n else return n);
      return (x + 1)))
  end))

let () =
  assert (exple_cont 13 = 14)

(* The Counting monad *)

module Count = struct
    type 'a mon = int -> 'a * int

    let ret (x: 'a) : 'a mon = fun n -> (x, n)
    let bind (m: 'a mon) (f: 'a -> 'b mon) =
      fun n -> match m n with (x, n') -> f x n'
    let (>>=) = bind

    let run (m: 'a mon) : 'a * int = m 0

    let tick (m: 'a mon) : 'a mon = fun n -> m (n+1)
end

let rec insert x l =
  Count.(perform begin
    match l with
    | [] -> ret [x]
    | h :: t ->
      b <-- tick (ret (x < h));
      if b
      then ret (x::l)
      else begin
        r <-- insert x t;
        ret (h::r)
      end
  end)

let test_count =
  assert (snd (Count.run (insert 3 [1;2;3;4;5;6])) = 4)

(* The Logging monad, a.k.a the Writer monad *)

module Log = struct
  type log = string list
  type 'a mon = log -> 'a * log

  let ret (x: 'a) : 'a mon = fun l -> (x, l)
  let bind (m: 'a mon) (f: 'a -> 'b mon): 'b mon =
    fun l -> match m l with (x, l') -> f x l'
  let (>>=) = bind

  type 'a result = 'a * log
  let run (m: 'a mon): 'a result =
    match m [] with (x, l) -> (x, List.rev l)

  let log msg : unit mon = fun l -> ((), msg :: l)
end

let log_abs n =
  Log.(run (perform (
    (if n >= 0
     then begin
       log "positive";
       ret n
     end
     else begin
       log "negative";
       ret (-n)
     end))))

let () =
  let (_,log) = log_abs (1000) in
  assert (log =  ["positive"]);
  let (_,log) = log_abs (-1000) in
  assert (log =  ["negative"])

(* The Environment monad, a.k.a the Reader monad *)

module Env = struct
  module StringMap = Map.Make(String)
  type env = int StringMap.t
  type 'a mon = env -> 'a

  let ret (x: 'a) : 'a mon = fun e -> x
  let bind (m: 'a mon) (f: 'a -> 'b mon) : 'b mon =
    fun e -> f (m e) e
  let (>>=) = bind

  type 'a result = 'a
  let run (m: 'a mon) : 'a result = m StringMap.empty

  let getenv (v: string) : int mon =
    fun e -> try StringMap.find v e with Not_found -> 0
  let putenv (v: string) (n: int) (m: 'a mon): 'a mon =
    fun e -> m (StringMap.add v n e)
end

type expr =
  | Const of int
  | Var of string
  | Plus of expr * expr
  | Let of string * expr * expr

let rec eval_expr (a: expr) : int Env.mon =
  Env.(perform(
    match a with
    | Const n -> ret n
    | Var v -> getenv v
    | Plus(a1, a2) ->
      n1 <-- eval_expr a1;
      n2 <-- eval_expr a2;
      ret (n1 + n2)
  | Let(v, a1, a2) ->
      n1 <-- eval_expr a1;
    putenv v n1 (eval_expr a2)))

let test_eval_expr =
  assert (Env.run (eval_expr (Let ("x", Const 1, Plus(Var "x", Const 2)))) = 3)

let () = print_endline "Advanced tests passed"
