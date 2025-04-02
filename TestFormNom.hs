module TestFormNorm (testsFormNorm) where

import FormulasProposicionales
import FormasNormales
import Test.HUnit

-- variables
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"

-- formulas
pq  = (Neg((p --> q) /\ (Neg (p \/ r))))
pq' = (p /\ (Neg q)) \/ (p \/ r)

rs  = (Neg (p <--> (r \/ (Neg s))))
rs' = ((p /\ ((Neg r) /\ s)) \/ ((r \/ (Neg s)) /\ (Neg p)))

tu  = ((p --> q) /\ (r \/ (Neg q)))
tu' = ((Neg p) \/ q) /\ (r \/ (Neg q))

vw  = ((Neg (p \/ q)) \/ (Neg (r /\ q)))
vw' =  Conj (Disy (Neg (Var "p")) (Disy (Neg (Var "r")) (Neg (Var "q")))) (Disy (Neg (Var "q")) (Disy (Neg (Var "r")) (Neg (Var "q"))))

xy  = (p /\ ((r \/ s) \/ q))
xy' = Disy (Disy (Conj (Var "p") (Var "r")) (Conj (Var "p") (Var "s"))) (Conj (Var "p") (Var "q"))

za  = (r <--> s)
za' = Disy (Disy (Conj (Neg (Var "r")) (Neg (Var "s"))) (Conj (Neg (Var "r")) (Var "r"))) (Disy (Conj (Var "s") (Neg (Var "s"))) (Conj (Var "s") (Var "r")))


testFNN1 :: Test
testFNN1 = TestCase $ assertEqual "for (fnn pq)" pq' (fnn pq)

testFNN2 :: Test
testFNN2 = TestCase $ assertEqual "for (fnn rs)" rs' (fnn rs)

testFNC1 :: Test
testFNC1 = TestCase $ assertEqual "for (fnc tu)" tu' (fnc tu)

testFNC2 :: Test
testFNC2 = TestCase $ assertEqual "for (fnc vw)" vw' (fnc vw)

testFND1 :: Test
testFND1 = TestCase $ assertEqual "for (fnd xy)" xy' (fnd xy)

testFND2 :: Test
testFND2 = TestCase $ assertEqual "for (fnd za)" za' (fnd za)

testsFormNorm :: [Test]
testsFormNorm = [
                testFNN1,
                testFNN2,
                testFNC1,
                testFNC2,
                testFND1,
                testFND2
                ]