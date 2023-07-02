module VariablesBroken where

message = "Hello George"
message = "Hiya Porter" -- variables can't be reassigned
                        -- in  a haskell file, only in ghci

main = print message
