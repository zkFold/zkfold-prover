module Main (main) where

import           RustFunctions                               (rustMultiScalarMultiplication, rustScalarSum)

import           ZkFold.Base.Algebra.Basic.Field             (toZp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1)
import           ZkFold.Base.Algebra.EllipticCurve.Class     (EllipticCurve (ScalarField, add, mul), Point (Inf, Point))

main :: IO ()
main = do
    putStrLn "\n# Getting started\n"
    let p1 = Point @BLS12_381_G1 2559712313477264959172148753206065810530485631723838699245332881708990728203874839151608166824090730325662475859717 3282735722178278861620513589250581354175673581769008136942324935535780642053629018389076743284875675335525385192198
        p2 = Inf @BLS12_381_G1
        s1 = toZp 32984311834479214614449262496445865980857193015750714262712443632514467389033 :: ScalarField BLS12_381_G1
        s2 = toZp 18466831675457810771569545241519055162047500441467738241139670919171195908980 :: ScalarField BLS12_381_G1

    res <- rustMultiScalarMultiplication [p1, p2] [s1, s2]
    print res
    print $ if res == s1 `mul` p1 then "Equals" else "Not equals"
