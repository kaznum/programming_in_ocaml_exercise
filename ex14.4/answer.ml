(*
[> ‘Can of int | ‘Can of bool] listは、リストの要素としては、`Can 1も`Can trueも良い、という宣言になるが、コンストラクタの`Canは最初に適用された時の値の型で決定するため、`Can 1が呼ばれたあとに`Can trueを呼び出すようなケースもリストとしては許容することになる。しかし、多相バリアントはコンストラクタの値に複数の型を割り当てるのは許容していないため、矛盾が発生する。
*)
