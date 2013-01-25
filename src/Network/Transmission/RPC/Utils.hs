module Network.Transmission.RPC.Utils (ffmap) where

ffmap :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
ffmap = fmap . fmap
