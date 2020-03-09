-- You are given the following information, but you may prefer to do some research for yourself.

-- 1 Jan 1900 was a Monday.
-- Thirty days has September,
-- April, June and November.
-- All the rest have thirty-one,
-- Saving February alone,
-- Which has twenty-eight, rain or shine.
-- And on leap years, twenty-nine.
-- A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

data Date = Date {
    year  :: Int,
    month :: Int,
    day   :: Int
}

isEarlierMonth :: Date -> Date -> Bool
isEarlierMonth (Date y1 m1 d1) (Date y2 m2 d2) | y1 < y2   = True
                                               | y1 > y2   = False
                                               | m1 < m2   = True
                                               | m1 > m2   = False
                                               | d1 < d2   = True
                                               | otherwise = False

subtract1Month :: Date -> Date
subtract1Month (Date y 1 d) = Date (y - 1) 12 d
subtract1Month (Date y m d) = Date y (m - 1) d

isLeapYear :: Date -> Bool
isLeapYear (Date x _ _) | mod x 4 /= 0   = False
                        | mod x 100 /= 0 = True
                        | mod x 400 /= 0 = False
                        | otherwise      = True

getMonthLength :: Date -> Int
getMonthLength x@(Date y 2 _) | isLeapYear x = 29
                              | otherwise    = 28
getMonthLength (Date _ 4 _)  = 30
getMonthLength (Date _ 6 _)  = 30
getMonthLength (Date _ 9 _)  = 30
getMonthLength (Date _ 11 _) = 30
getMonthLength _             = 31

getStartingWeekday :: Date -> Int
getStartingWeekday (Date 1900 1 1) = 1
getStartingWeekday x@(Date _ _ 1) = let prev = subtract1Month x
                                    in mod (getStartingWeekday prev + getMonthLength prev) 7
getStartingWeekday (Date y m d)   = getStartingWeekday (Date y m 1)

getInitialSundays :: Date -> Date -> Int
getInitialSundays x y | isEarlierMonth y x        = 0
                      | getStartingWeekday y == 0 = 1 + (getInitialSundays x $ subtract1Month y)
                      | otherwise                 = getInitialSundays x $ subtract1Month y

p019 = do
    print $ getInitialSundays (Date 1901 1 1) (Date 2000 12 31)