import Test.Test
import Test.MTRNG as M

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Test Begins"
  reportTests $ M.fastTests
