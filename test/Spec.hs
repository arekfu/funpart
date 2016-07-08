import VecTest

main :: IO ()
main = do
    _ <- VecTest.runTests
    return ()
