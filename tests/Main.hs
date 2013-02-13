module Main
( main
) where

------------------------------------------------------------------------------
import Test.Framework (defaultMain, testGroup)
------------------------------------------------------------------------------
import qualified Test.Data.Store01
------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
    [ testGroup "Data.Store01" Test.Data.Store01.tests
    ]
