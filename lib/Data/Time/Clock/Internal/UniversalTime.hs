module Data.Time.Clock.Internal.UniversalTime
    (
    -- * Universal Time
    -- | Time as measured by the Earth.
      UniversalTime(..)
    ) where

import Control.DeepSeq
import Data.Data

-- | The Modified Julian Date is the day with the fraction of the day, measured from UT midnight.
-- It's used to represent UT1, which is time as measured by the earth's rotation, adjusted for various wobbles.
newtype UniversalTime = ModJulianDate
    { getModJulianDate :: Rational
    } deriving (Eq, Ord, Data, Typeable)

-- necessary because H98 doesn't have "cunning newtype" derivation
instance NFData UniversalTime where
    rnf (ModJulianDate a) = rnf a
