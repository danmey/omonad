(* A fairly direct translation of "Monadic Parsing in Haskell" (Graham Hutton
   & Erik Meijer) *)

type 'a parser_ = Parser of (char list -> ('a * char list) list)

let parse (Parser p) = p

let explode s =
  let l = ref [] in
  begin
    String.iter (fun c -> l := c :: !l) s;
    List.rev !l
  end

let implode s = String.concat "" (List.map (String.make 1) s)
    
module ListMonad = struct
  type 'a t = 'a list

  let return x = [x]
  let bind m k = List.concat (List.map k m)
  let fail _ = []
end
module Parser =
struct
  type 'a t = 'a parser_

  let return a = Parser (fun cs -> [(a, cs)])

  let bind p f = Parser (fun cs -> List.concat 
    ListMonad.(perform
                 ((a, cs') <-- parse p cs;
                  return (parse (f a) cs'))))

  let zero = Parser (fun cs -> [])

  let (++) p q = Parser (fun cs -> parse p cs @ parse q cs)

  let item : char parser_ =
    Parser (function
             []    -> []
           | c::cs -> [(c, cs)])
      
  let (+++) p q = Parser (fun cs ->
                             match parse (p ++ q) cs with
                              | []    -> []
                              | x::xs -> [x])

  let sat : (char -> bool) -> char parser_ =
    fun p -> perform (c <-- item;
                      if p c then return c
                      else zero)

  let char : char -> char parser_
    = fun c -> sat ((=)c)

  let string : string -> string parser_
    = let rec char_list : char list -> char list parser_ = function
      | []    -> return []
      | c::cs -> perform (char c;
                          char_list cs;
                          return (c::cs))
      in fun s -> perform (cl <-- char_list (explode s);
                           return (implode cl))

  let rec many : 'a. 'a parser_ -> 'a list parser_
    = fun p -> many1 p +++ return []

  and many1 : 'a. 'a parser_ -> 'a list parser_
    = fun p -> perform (x <-- p; xs <-- many p; return (x::xs))

  let rec sepby : 'a 'b. 'a parser_ -> 'b parser_ -> 'a list parser_
    = fun p sep -> (sepby1 p sep) +++ return []

  and sepby1 : 'a 'b. 'a parser_ -> 'b parser_ -> 'a list parser_
    = fun p sep -> perform (x <-- p;
			    xs <-- many (perform (sep; p));
			    return (x::xs))

  let rec chainl : 'a. 'a parser_ -> ('a -> 'a -> 'a) parser_ -> 'a -> 'a parser_
    = fun p op a -> chainl1 p op +++ return a

  and chainl1 : 'a. 'a parser_ -> ('a -> 'a -> 'a) parser_ -> 'a parser_
    = fun p op -> 
      let rec rest a = perform (f <-- op;
				b <-- p;
				rest (f a b))
	+++ return a
      in
      perform (a <-- p; rest a)

  let is_space c = List.mem c [' '; '\t'; '\n'; '\r']
  let space : string parser_ = perform (cs <-- many (sat is_space); return (implode cs))

  let token : 'a. 'a parser_ -> 'a parser_
    = fun p -> perform (a <-- p;
			space;
			return a)

  let symb : string -> string parser_
    = fun cs -> token (string cs)

  let apply : 'a. 'a parser_ -> string -> ('a * string) list
    = fun p s -> ListLabels.map (parse (perform (space; p)) (explode s))
      ~f:(fun (x, s) -> (x, implode s))
end

open Parser

let is_digit c = List.mem c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

let digit = perform (x <-- token (sat is_digit);
		     return (Char.code x - Char.code '0'))

let mulop = (perform (symb "*";
		      return ( * ))
	     +++ 
	     perform (symb "/";
		      return ( / )))

let addop = (perform (symb "+";
		      return ( + ))
	     +++ 
	     perform (symb "-";
		      return ( - )))

let rec factor' = lazy (digit +++ perform (symb "(";
	                                   n <-- Lazy.force expr';
		                           symb ")";
 		                           return n))

and term' = lazy (chainl1 (Lazy.force factor') mulop)

and expr' = lazy (chainl1 (Lazy.force term') addop)

let factor, term, expr = Lazy.force factor', Lazy.force term', Lazy.force expr'

let () = begin
  assert ((apply expr " 1 - 2 * 3 + 4 ") = [(-1, "")]);
  print_endline "Parsing succeeded"
end
