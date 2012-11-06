let f = ref (fun y -> y + 1)
let funny_fact x = if x = 1 then 1 else x * (!f (x - 1));;

f := funny_fact;;
funny_fact 5;;

let rec funny_fact' x = if x = 1 then 1 else x * (funny_fact' (x - 1));;
funny_fact' 5;;

(*
一度、fに適当な無名関数の参照を代入し、これを利用しfunny_factを定義する。
その直後に、f の参照先を funny_fact (自己の関数)に変更することで、funny_fact内で自己参照と同等に呼び出すようにしている。
*)
