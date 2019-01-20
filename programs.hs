module Programs where

import Whilelang
import qualified Data.Map as M

------ FACTORIAL OF 5 ------
factorial = (Comp[
                (Assign(Vrb "n")(Val 5)),
                (Assign(Vrb "r")(Val 1)),
                (While(Bin Grt(Vrb "n")(Val 0)) (Comp[
                    (Assign(Vrb "r")(Bin Mul(Vrb "r")(Vrb "n"))),
                    (Assign(Vrb "n")(Bin Sub(Vrb "n")(Val 1)))]))
            ])

factOutput = (Out (Vrb "r"))

------ 10TH FIBONACCI NUMBER -------
fibonacci =  (Comp[
                (Assign(Vrb "n")(Val 0)),
                (Assign(Vrb "r")(Val 1)),
                (Assign(Vrb "i")(Val 0)),
                (While(Bin Lss(Vrb "i")(Val 10)) (Comp[
                    (Assign(Vrb "s")(Bin Add(Vrb "n")(Vrb "r"))),
                    (Assign(Vrb "n")(Vrb "r")),
                    (Assign(Vrb "r")(Vrb "s")),
                    (Assign(Vrb "i")(Bin Add(Vrb "i")(Val 1)))]))
            ])

fibOutput = (Out (Vrb "n")) 

-- Interpret Programs
intpfact = intpOut factOutput $intp factorial M.empty
intpfib = intpOut fibOutput $intp fibonacci M.empty

-- Pretty Print Programs
ppfact = putStr $pps 0 factorial
ppfib = putStr $pps 0 fibonacci
