module ListMonad = struct
  type 'a t = 'a list

  let return x = [x]
  let bind m k = List.concat (List.map k m)
  let fail _ = []
end

let tests () = ListMonad.(begin
  let vs = perform (`A x <-- [`A 1; `B '2'; `C true; `A 4; `A 5; `B '6'];
                    return (x * 2)) in
    assert (vs = [2; 8; 10]);

  let vs = perform (`A x <-- [`A 1; `B '2'; `C true; `A 4; `A 5; `B '6'];
                    (y, Some z) <-- [(1, None); (2, Some 3); (4, Some 5)];
                    return (x + y + z)) in
    assert (vs = [1 + 2 + 3;
                  1 + 4 + 5;
                  4 + 2 + 3;
                  4 + 4 + 5;
                  5 + 2 + 3;
                  5 + 4 + 5]);

  let vs = perform ([|x; y|] <-- [[|1; 2|]; [||]; [|3; 4; 5|]; [|6; 7|]];
                    return (x + y)) in
    assert (vs = [1 + 2;
                  6 + 7]);
end)

let () = begin
  tests ();
  print_endline "Pattern-matching tests passed."
end
