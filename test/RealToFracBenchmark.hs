{- Contributed by Liyang HU <haskell.org@liyang.hu> -}
module Main where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Time
import Data.Time.Clock.POSIX
import Prelude
import System.Random

main :: IO ()
main = do
    ts <-
        replicateM 100000 $ do
            t <-
                posixSecondsToUTCTime . realToFrac <$>
                ((*) . fromInteger <$> randomRIO (-15 * 10 ^ 21, 15 * 10 ^ 21) <*> randomIO :: IO Double) :: IO UTCTime
            rnf t `seq` return t
    now <- getCurrentTime
    print . sum $ map (diffUTCTime now) ts
    print =<< flip diffUTCTime now <$> getCurrentTime
