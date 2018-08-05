{-# language DataKinds #-}
module Main where

import Control.Lens
import Data.Text.Lazy(unpack)
import Programs
import FixMutableDefaultArguments
import OptimizeTailRecursion
import Indentation

import Language.Python.Internal.Render
import Language.Python.Internal.Syntax

section a = do
  putStrLn "**********"
  a
  putStrLn "\n**********\n"

main = do
  section $ do
    putStrLn "Before\n"
    putStrLn $ unpack $ showModule everything

  section $ do
    putStrLn "Spaced\n"
    putStrLn . unpack .
      showModule $
      everything & _Statements %~ indentSpaces 2

  section $ do
    putStrLn "Tabbed\n"
    putStrLn . unpack .
      showModule $
      everything & _Statements %~ indentTabs

  section $ do
    putStrLn "Refactored\n"
    putStrLn . unpack .
      showModule .
      rewriteOn _Statements fixMutableDefaultArguments .
      rewriteOn _Statements optimizeTailRecursion $
      everything
