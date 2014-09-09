module Main where

import           Test.Tasty

import qualified Test.Actor.Clerk.Implementation as Clerk

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Clerk.tests]
