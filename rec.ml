let rec contain_zero lst =
  match lst with
  | [] -> false
  | first :: rest -> if first = 0 then true else contain_zero rest

let test1 = contain_zero [] = false

let test2 = contain_zero [ 0; 2 ] = true

let test3 = contain_zero [ 1; 2 ] = false

let test4 = contain_zero [ 1; 2; 3; 0; 5; 6; 6 ] = true

let test5 = contain_zero [ 1; 2; 3; 4; 5; 6; 6 ] = false

let rec sum lst = match lst with [] -> 0 | first :: rest -> first + sum rest

let rec add_to_each n lst =
  match lst with
  | [] -> []
  | first :: rest -> (n :: first) :: add_to_each n rest

let test1 = add_to_each 1 []

let test2 = add_to_each 1.1 []
