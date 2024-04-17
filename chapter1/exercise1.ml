type id = string

type binop = Plus | Minus | Times | Div

type stm = Compound of stm * stm
         | Assign of id * exp
         | Print of exp list

 and exp = Id of id
         | Num of int
         | Op of exp * binop * exp
         | Eseq of stm * exp
