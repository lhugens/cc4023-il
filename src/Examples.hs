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
-- assignment examples --
----------------------------------

fl1 = Cons (Const 1) Nil

fl2 = Cons (Const 76) (Cons (Const 34) Nil)

funlength = Fix (Lambda "f" (Lambda "ls" (IfZero (Null (Var "ls")) (Const 0) ((Const 1) :+ (App (Var "f") (Tail (Var "ls")))))))

fl3 = Cons funlength Nil

funappend = Fix (Lambda "f" (Lambda "ls1" (Lambda "ls2" (IfZero (Null (Var "ls1"))
    (Var "ls2") 
    (Cons (Head (Var "ls1")) (App (App (Var "f") (Tail (Var "ls1" ))) (Var "ls2")))))))

fl4 = App funlength (App funappend (Cons (Cons (Const 1) fl1) fl2))

-- define the list [24, 32, 56], and get SECD to print the first, second, and third element to the console

exel1 = compileToFile "temp1" (Head(Cons (Const 24) (Cons (Const 32) (Cons (Const 56) Nil))))

exel2 = compileToFile "temp2" (Head(Tail(Cons (Const 24) (Cons (Const 32) (Cons (Const 56) Nil)))))

exel3 = compileToFile "temp3" (Head(Tail(Tail(Cons (Const 24) (Cons (Const 32) (Cons (Const 56) Nil))))))











