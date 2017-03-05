{-# language OverloadedStrings #-}
module AllTests where
import GHC.Exts
import Data.Maybe
import Data.Text
import Data.Either
import Control.Foldl as F (list, head)
import Turtle hiding (options)
import Distribution.TestSuite


tests :: IO [Test]
tests = do pos <- fold runPosTests list
           dls <- fold runDLTests list
           return (pos ++ dls)


runDLTests :: Shell Test
runDLTests = runTests "Deadlock Tests" checkPos dlTests
  where
    checkPos (ExitFailure 4) = Nothing
    checkPos ExitSuccess     = Just "Unexpected success"
    checkPos (ExitFailure c) = Just ("Unexpected Status: " ++ show c)

runPosTests = runTests "Positive Tests" checkPos posTests
  where
    checkPos ExitSuccess     = Nothing
    checkPos (ExitFailure c) = Just ("Unexpected Status: " ++ show c)

runTests :: String -> (ExitCode -> Maybe String) -> Shell Turtle.FilePath -> Shell Test
runTests group check findTests
  = do test    <- fold findTests list
       let tests = [ Test (mkInstance f) | f <- test ]
       return $ Group { groupName    = group
                      , concurrently = True
                      , groupTests   = tests
                      }
       where
         mkInstance f = TestInstance { run     = fromJust <$> fold (mkRunCmd f) F.head
                                     , name    = mkName f
                                     , tags    = []
                                     , options = []
                                     , setOption = \_ _ -> Left "No Options"
                                     }
         mkName f = case toText f of
                      Left s -> unpack s
                      Right s -> unpack s
         mkRunCmd f = do
           exit <- shell (format ("stack exec -- brisk "%fp%" main 1>/dev/null 2>/dev/null") f) Turtle.empty
           case check exit of
             Nothing -> 
               return $ Finished Pass
             Just msg ->
               return $ Finished (Fail msg)


posTests :: Shell Turtle.FilePath
posTests = haskellFiles "tests/pos"

dlTests :: Shell Turtle.FilePath
dlTests = haskellFiles "tests/dl"

haskellFiles :: Turtle.FilePath -> Shell Turtle.FilePath
haskellFiles = Turtle.find (star dot >> text ".hs")
