{-# OPTIONS_GHC -F -pgmF htfpp #-}

module CIsCweb.HelloTest (htf_thisModulesTests) where

import Test.Framework
import CIsCweb.Hello (greet)

test_greet =
     do assertEqual "Hello, Alice." (greet "Alice")
