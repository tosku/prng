import Test.Test
import Test.MTRNG as MT
import Test.MWC as MWC

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Test Begins"
  {-reportTests $ MT.fastTests -}
  reportTests $ MT.fastTests ++ MWC.fastTests
