{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Aeson                 (FromJSON, ToJSON,encode,decode)
-- import qualified Data.Aeson                 as A
import           Data.Char
import           Data.IORef
import           Data.List
import           GHC.Generics
import           Text.Pretty.Simple         (pPrint)


-- validate :: String ->
-- validate   success  --------- Nothing
-- validate   fail     --------- Just (faild Reason)
-- ValidateResult
data VR a =
    Success
  | E a deriving (Show,Eq,Generic,FromJSON,ToJSON)

data ErrorType =
    NotHaveNumber
  | NotHaveUpper
  | NotHaveLower
  | CharIsNotEnought Int String
  deriving (Show,Eq,Generic,FromJSON,ToJSON)


isSuccess :: VR a -> Bool
isSuccess Success = True
isSuccess _       = False

vr2str :: VR a -> a
vr2str Success = error "这不会发生"
vr2str (E a)   = a

isHaveAnyone :: (Char -> Bool) -> ErrorType -> String -> VR ErrorType
isHaveAnyone f errorInfo s | any id $ map f s = Success
                           | otherwise =  E errorInfo

isHaveNumber  = isHaveAnyone (`elem` "123456789") NotHaveNumber

isHaveUpper  = isHaveAnyone isUpper NotHaveUpper  -- any id $ map isUpper st
            --
isHaveLower  = isHaveAnyone isLower NotHaveLower -- any id $ map isLower st

isHaveLessChar st  | length st > 7 && length st < 30 = Success
                   | otherwise =  E $ CharIsNotEnought (length st) "长度不符合标准，应该在7-13个字符之间"

validatePassword :: String -> VR [ErrorType]
validatePassword st | all isSuccess res = Success
                    | otherwise = E $ map vr2str $ filter (not . isSuccess) res
      where res = map ($ st) [isHaveLessChar,isHaveNumber,isHaveUpper,isHaveLower]


getPassword' :: MaybeT IO String
getPassword' = do
  st <- liftIO getLine
  case validatePassword st of
    Success -> MaybeT $ return $ Just st
    E r -> do
      liftIO $ pPrint r
      liftIO $ pPrint $ encode r
      liftIO $ pPrint $ (decode $ encode r :: Maybe [ErrorType])
      MaybeT  $ return $ Nothing

getPassword =runMaybeT $ msum $ repeat getPassword'

tmain :: IO ()
tmain = do
  password <- getPassword
  print password

