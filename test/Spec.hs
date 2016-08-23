import VecTest
import ParticleTest
import MCTest

main :: IO ()
main = do
    _ <- VecTest.runTests
    _ <- ParticleTest.runTests
    _ <- MCTest.runTests
    return ()
