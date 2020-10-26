module Test where

f x = case x of
    _ -> let y = 4 in f y
    _ -> 4
