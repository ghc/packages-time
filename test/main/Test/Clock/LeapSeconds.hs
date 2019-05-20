{-# OPTIONS -fno-warn-orphans #-}
module Test.Clock.LeapSeconds(testLeapSeconds) where
{
    import Test.Tasty;
    import Test.Tasty.HUnit;
    import Data.Time;
    import Data.Time.Clock.TAI;
    import Data.Time.Clock.LeapSeconds;


    testParseNISTLeapSecondList :: TestTree;
    testParseNISTLeapSecondList = testCase "parseNISTLeapSecondList" $ let
    {
        jan y = fromGregorian y 1 1;
        jul y = fromGregorian y 7 1;

        refList :: LeapSecondList;
        refList = MkLeapSecondList
        {
            lslVersion = fromGregorian 2016 7 8,
            lslExpiration = fromGregorian 2017 6 28,
            lslTransitions =
            zip
            [
                jan 1972,
                jul 1972,
                jan 1973,
                jan 1974,
                jan 1975,
                jan 1976,
                jan 1977,
                jan 1978,
                jan 1979,
                jan 1980,
                jul 1981,
                jul 1982,
                jul 1983,
                jul 1985,
                jan 1988,
                jan 1990,
                jan 1991,
                jul 1992,
                jul 1993,
                jul 1994,
                jan 1996,
                jul 1997,
                jan 1999,
                jan 2006,
                jan 2009,
                jul 2012,
                jul 2015,
                jan 2017
            ]
            [10..]
        };
    } in do
    {
        fileString <- readFile "test/leap-seconds.list";
        assertEqual "LeapSecondList" (Just refList) $ parseNISTLeapSecondList fileString;
    };

    deriving instance Show LeapSecondList;

    testLeapSecondListToMap :: TestTree;
    testLeapSecondListToMap = testCase "leapSecondListToMap" $ let
    {
        sampleLeapSecondList :: LeapSecondList;
        sampleLeapSecondList = MkLeapSecondList
        {
            lslVersion = fromGregorian 1974 1 1,
            lslExpiration = fromGregorian 1975 1 1,
            lslTransitions =
            [
                (fromGregorian 1972 1 1,10),
                (fromGregorian 1972 7 1,11)
            ]
        };

        sampleLeapSecondMap :: LeapSecondMap;
        sampleLeapSecondMap = leapSecondListToMap sampleLeapSecondList;

        assertVal expected day = assertEqual (show day) expected $ sampleLeapSecondMap day;
    } in do
    {
        assertVal Nothing (fromGregorian 1971 12 31);
        assertVal (Just 10) (fromGregorian 1972 1 1);
        assertVal (Just 10) (fromGregorian 1972 6 31);
        assertVal (Just 11) (fromGregorian 1972 7 1);
        assertVal (Just 11) (fromGregorian 1974 12 31);
        assertVal Nothing (fromGregorian 1975 1 1);
    };

    testLeapSeconds :: TestTree;
    testLeapSeconds = testGroup "leapseconds" $
    [
        testParseNISTLeapSecondList,
        testLeapSecondListToMap
    ];
}
