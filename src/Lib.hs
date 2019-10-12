module Lib
    ( safeHead
    ) where

safeHead :: [a] -> Maybe a 
safeHead (x:_) = Just x
safeHead _     = Nothing 
        
