import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Char
import           Data.IORef
import           Data.List
import           Text.Pretty.Simple         (pPrint)


main :: IO ()
main = print "nice"



fun1 :: Either Int Int
fun1 = foldM f 0 [1..1000]
  where f b a | a >100 = Left b
              | otherwise = Right (b +a)


f :: Int -> Int ->  ExceptT Int IO Int
f b a | a > 10 = t $ Left b
      | otherwise = do
          liftIO $ print $ "total val: " ++ show b
          t $ Right (b +a)
      where t = ExceptT . return

fun2 :: ExceptT Int IO Int
fun2 =  foldM f 0 [1..1000]

-- validate :: String -> Bool
-- validates ::  [String -> Bool]

isHaveAnyone :: (Char -> Bool) -> String -> Bool
isHaveAnyone f = any id . map f

isHaveNumber :: String -> Bool
isHaveNumber  = isHaveAnyone (`elem` "123456789")

isHaveUpper :: String -> Bool
isHaveUpper  = isHaveAnyone isUpper  -- any id $ map isUpper st

isHaveLower :: String -> Bool
isHaveLower  = isHaveAnyone isLower  -- any id $ map isLower st

isHaveLessChar :: String -> Bool
isHaveLessChar st  = length st > 7 && length st < 30


validatePassword :: String -> Maybe String
validatePassword st | all id $ map ( $ st)
                      [isHaveLessChar,isHaveNumber,isHaveUpper,isHaveLower] = Just st
                    | otherwise = Nothing

getPassword' :: MaybeT IO String
getPassword' = do
  st <- liftIO getLine
  MaybeT $ return $ validatePassword st


getPassword =runMaybeT $ msum $ repeat getPassword'

tmain :: IO ()
tmain = do
  password <- getPassword
  print password

