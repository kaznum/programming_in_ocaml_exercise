(* マス目の状態 *)
type state = Pressed | NotPressed

(* 問題となる行・列のあるべき黒マス状態 *)
type spec = int list

(* 盤面 *)
type board = {
  width : int;
  height : int;
  h_spec : spec list;
  v_spec : spec list;
  body : state ref list list; (* マス目の状態 *)
}

(* マス目が解となっているかどうかチェック *)
val is_solved :
  h_spec:spec list -> v_spec:spec_list -> state ref list list -> bool

(* 問題から初期画面を作成する *)
val board_of_spec : h_spec:spec list -> v_spec:spec list -> board

