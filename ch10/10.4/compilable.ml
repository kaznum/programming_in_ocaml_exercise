let x = ref []
let f a = x := (a + 1) :: !x

let y : int list ref = ref []
