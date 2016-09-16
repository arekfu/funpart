import qualified Physics.FunPart.VecTest as VecTest
import qualified Physics.FunPart.ParticleTest as ParticleTest
import qualified Physics.FunPart.MCTest as MCTest
import qualified Physics.FunPart.MeshTest as MeshTest
import qualified Physics.FunPart.ProblemTest as ProblemTest
import qualified Physics.FunPart.StatTest as StatTest

main :: IO ()
main = do
    _ <- VecTest.runTests
    _ <- ParticleTest.runTests
    _ <- MeshTest.runTests
    _ <- MCTest.runTests
    _ <- ProblemTest.runTests
    _ <- StatTest.runTests
    return ()
