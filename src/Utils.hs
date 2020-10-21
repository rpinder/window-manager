module Utils where

import Control.Monad.IO.Class

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

io :: MonadIO m => IO a -> m a
io = liftIO

(.+) :: (Integral a, Integral b, Num c) => a -> b -> c
a .+ b = fi a + fi b

(.-) :: (Integral a, Integral b, Num c) => a -> b -> c
a .- b = fi a - fi b
  
