import VecTest
import ParticleTest
import MCTest
import ProblemTest

main :: IO ()
main = do
    _ <- VecTest.runTests
    _ <- ParticleTest.runTests
    _ <- MCTest.runTests
    _ <- ProblemTest.runTests
    return ()
