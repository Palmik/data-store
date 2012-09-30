module Main
( main
) where

------------------------------------------------------------------------------
import           Test.Framework (defaultMain, testGroup)
------------------------------------------------------------------------------
import qualified Test.Data.Store 
------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
    [ testGroup "Data.Store" Test.Data.Store.tests
    ]

