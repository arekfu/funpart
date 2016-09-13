import VecTest
import ParticleTest
import MCTest
import ProblemTest
import StatTest

main :: IO ()
main = do
    _ <- VecTest.runTests
    _ <- ParticleTest.runTests
    _ <- MCTest.runTests
    _ <- ProblemTest.runTests
    _ <- StatTest.runTests
    return ()
