{--------------------------------------
  Fun: a minimal functional language
  -------------------------------------
 
  This module contains some example programs.
  Pedro Vasconcelos, 2008--2009.
-}
module Examples where
import Fun
import SECD2

-- simple computations
ex1 = (Const 42 :+ Const 23) :* Const 5

-- the identity function
ex2 = Lambda "x" (Var "x")

-- the sucessor function
ex3 = Lambda "x" (Var "x" :+ Const 1)

-- one function between two integers
ex4 = Lambda "x" 
      (Lambda "y"
       (IfZero (Var "x" :- Var "y") 
        (Var "y") (Var "x")))
                       
-- an example that builds a closure with free vars
ex5 = Let "x" (Const 42) (Lambda "y" (Var "x" :+ Var "y"))

ex5b = Let "x" (Const 23) (Lambda "y" (Var "x" :+ Var "y"))

-- a recursive function (factorial)
ex6 = Fix 
      (Lambda "f" 
       (Lambda "n"
        (IfZero (Var "n")
         (Const 1)
         ((App (Var "f") (Var "n" :- Const 1)) :* Var "n")
        )))


-- compute the factorial of 10
ex7 = App ex6 (Const 10)

-- factorial of a negative number (diverges)
ex9 = App ex6  (Const (-1))

-- recursive sum 0^2 + 1^2 + 2^2 + ... + n^2
ex8 = Fix 
      (Lambda "f"
       (Lambda "n"
         (IfZero (Var "n")
          (Const 0)
           ((Var "n" :* Var "n") :+ 
            App (Var "f") (Var "n" :- Const 1)))))

-- buggy expressions (type errors)

bug1 = Const 42 :+ Lambda "x" (Var "x")

bug2 = App (Const 42) (Const 1)

----------------------------------
-- assignment examples

-- pairs
pairex1 = compileBytecode(fstt(Pair (Const 2) (Const 3)))

pairex2 = compileBytecode(sndd(Pair (Const 2) (Const 3)))

pairex3 = runCodeGen(compileExpr(Pair (Const 2) (Const 3)))

-- lists
listex1 = List [Const 2, Const 3, Var "4"]

listex2 = compileBytecode(List [])

listex3 = runCodeGen(compileExpr(List [Const 2, Const 3]))
    
listex4 = runCodeGen(compileExpr(headd(List [Const 2, Const 3])))

listex5 = runCodeGen(compileExpr(taill(List [Const 2, Const 3])))

listex6 = nulll(List [])

listex7 = nulll(List [Const 2, Const 3])















