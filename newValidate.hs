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
main = print "hello"


-- validate :: String -> Either (Error ) String
-- validates [String -> Either (Error ) String]

data Error = E String

instance Semigroup Error where
  (E a) <> (E b) = E (a ++ b)

instance Monoid Error where
  mempty = E ""

type ES = Either Error String


isHaveAnyone :: (Char -> Bool) -> String -> String -> ES
isHaveAnyone f errorInfo s | any id $ map f s = Right s
                           | otherwise = Left $ E errorInfo


-- isHaveNumber :: String -> ES
isHaveNumber  = isHaveAnyone (`elem` "123456789") "没有数字"

-- isHaveUpper :: String -> Bool
isHaveUpper  = isHaveAnyone isUpper "没有大写字母"  -- any id $ map isUpper st
            --
-- isHaveLower :: String -> Bool
isHaveLower  = isHaveAnyone isLower "没有小写字母" -- any id $ map isLower st

-- isHaveLessChar :: String -> Bool
isHaveLessChar st  | length st > 7 && length st < 30 = Right st
                   | otherwise = Left $ E "长度不符合标准，应该在7-13个字符之间"


-- validatePassword :: String -
validatePassword st = sequence $ map  ( $ st) [isHaveLessChar,isHaveNumber,isHaveUpper,isHaveLower]

getPassword' :: ExceptT Error IO String
getPassword' = do
  st <- liftIO getLine
  case (validatePassword st)  of
    Left (E s) -> do
      liftIO $ pPrint s
      ExceptT $ return $ Left $ E st
    Right _ -> do
      ExceptT $ return $ Right st

getPassword =runExceptT $ msum $ repeat getPassword'

-- tmain :: IO ()
-- tmain = do
--   password <- getPassword
--   print password
tmain :: IO ()
tmain = do
  Right password <- getPassword
  print password












