(* Matthew Tan
* mxtan
* cs112
* asg2: bigint.ml
*)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let revmap    = List.rev_map
    let reverse   = List.rev
    let listlen   = List.length
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

    (* stripzeroes: strip leading zeroes *)
    let rec stripzeroes value =
        if value = [] then []
        else let rev_value = reverse value in
            if (car rev_value) = 0 
            then stripzeroes (reverse (cdr rev_value))
            else value

   (* cmp: returns a comparison value
      similar to strcmp in C *)
   let rec cmp list1 list2 = 
        if listlen list1 > listlen list2 
        then 1
        else if listlen list1 < listlen list2 
        then -1
        else match (list1, list2) with
            | [], [] -> 0
            | list1, list2 ->  
                let rev_list1 = reverse list1 in
                let rev_list2 = reverse list2 in
                    if car rev_list1 > car rev_list2 
                    then 1
                    else if car rev_list1 < car rev_list2 
                    then -1
                    else cmp (reverse (cdr rev_list1)) 
                        (reverse (cdr rev_list2))

    (* sub': helper function for sub function *)
    let rec sub' list1 list2 borrow = 
        match (list1, list2, borrow) with
        | list1, [], false -> list1
        | list1, [], true  ->
          let result = (car list1) - 1
          in if result < 0 
          then (result + 10)::(sub' (cdr list1) [] true)
          else result::(cdr list1)
        | [], list2, true  -> []
        | [], list2, false -> []
        | car1::cdr1, car2::cdr2, borrow -> 
            let result = car1 - car2 in
                if borrow 
                then sub' ((car1 - 1)::cdr1) list2 false
                else if result < 0 
                then (result + 10)::(sub' cdr1 cdr2 true)
                else result::(sub' cdr1 cdr2 false) 

    (*rev_list: reverses a list *)
    let rev_list list = 
        float_of_string (strcat "" 
            (revmap string_of_int list))


    (* add': helper function for add *)
    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    (* add: add function *)
    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else if rev_list value1 > rev_list value2
           then Bigint(neg1, sub' value1 value2 false)
           else Bigint(neg2, sub' value2 value1 false)


    (* sub: sub function *)
    let sub (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
        if neg1 = neg2 then 
            let result = cmp val1 val2 in
                 if result > 0 
                    then Bigint (neg1, 
                        stripzeroes (sub' val1 val2 false))
                 else if result < 0 then 
                     let sign = if neg1 = Pos then Neg else Pos
                     in  Bigint (sign, 
                         stripzeroes (sub' val2 val1 false))
                 else zero
        else Bigint (neg1, add' val1 val2 0)

    (* mul_help: helper function for mul' *)
    let rec mul_help list1 num carry =
        if list1 = [] then [carry]
        else let prod = (car list1) * num + carry in
            (prod mod 10)::(mul_help (cdr list1) num (prod / 10))

    (* mul': helper for mul *)
    let rec mul' list1 list2 =
        if (list1 = [] || list2 = []) then []
        else add' (mul_help list1 (car list2) 0)
                    (0::(mul' list1 (cdr list2))) 0

    (* mul: mul function *)
    let mul (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
        if neg1 = neg2 then Bigint (Pos, stripzeroes (mul' val1 val2))
        else Bigint (Neg, stripzeroes (mul' val1 val2))

    let double number = add' number number 0

    (* divrem': helper for divrem *)
    let rec divrem' list1 power list2 =
        if rev_list list2 > rev_list list1
        then [0], list1
        else let quotient, remainder =
            divrem' list1 (double power) (double list2) in
            if rev_list remainder < rev_list list2
            then quotient, remainder
            else (add' quotient power 0), (sub' remainder list2 false)

    (* divrem: helper for div *)
    let divrem list1 list2 = divrem' list1 [1] list2

    (* div: div function *)
    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then let quotient, _ = 
            divrem value1 value2 in Bigint(Pos, quotient)
        else let quotient, _ = 
            divrem value1 value2 in Bigint(Neg, quotient)

    (* rem: remainder function *)
    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then let _, remainder = 
            divrem value1 value2 in Bigint(Pos, remainder)
        else let _, remainder = 
            divrem value1 value2 in Bigint(Neg, remainder)

    (* pow': helper function for pow *)
    let rec pow' list1 list2 list3 =
        let list2' = (stripzeroes (sub' list2 [1] false)) in
            if list2' = [] 
            then list1
            else pow' (mul' list1 list3) list2' list3

    (* pow: pow function *)
    let pow (Bigint (neg1, list1)) (Bigint (neg2, list2)) =
        if neg2 = Neg 
        then zero
        else match (list1, list2) with
            | [], list2     -> zero
            | list1, []     -> Bigint (Pos, [1])
            | list1, list2 -> Bigint (neg1, 
                (stripzeroes (pow' list1 list2 list1)))

end
