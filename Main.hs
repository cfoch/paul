import Data.Time
import Data.Time.Parse
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar

import Data.List
import Data.List.Split

import Data.Maybe

data DayWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving ( Show )
data StockDay = StockDay {
  date  :: Day,
  open  :: Float,
  high  :: Float,
  low   :: Float,
  close :: Float
} deriving ( Show )

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct |
             Nov | Dec deriving ( Show )

monthToNumber :: Month -> Int
monthToNumber month = case month of Jan -> 1
                                    Feb -> 2
                                    Mar -> 3
                                    Apr -> 4
                                    May -> 5
                                    Jun -> 6
                                    Jul -> 7
                                    Aug -> 8
                                    Sep -> 9
                                    Oct -> 10
                                    Nov -> 11
                                    Dec -> 12

strMonthToMonth :: String -> Month
strMonthToMonth strMonth
  | strMonth == "Jan" = Jan
  | strMonth == "Feb" = Feb
  | strMonth == "Mar" = Mar
  | strMonth == "Apr" = Apr
  | strMonth == "May" = May
  | strMonth == "Jun" = Jun
  | strMonth == "Jul" = Jul
  | strMonth == "Aug" = Aug
  | strMonth == "Sep" = Sep
  | strMonth == "Oct" = Oct
  | strMonth == "Nov" = Nov
  | strMonth == "Dec" = Dec

parseGoogleFinanceDate :: String -> Maybe Day
parseGoogleFinanceDate strDate = case maybeDate of
                                  Nothing -> Nothing
                                  (Just x) -> Just (localDay $ fst x)
  where
    spl = splitOn "-" strDate
    strDay = spl !! 0
    strMonth = show $ monthToNumber $ strMonthToMonth (spl !! 1)
    strYear = spl !! 2
    strFormatDate = intercalate "-" [strMonth, strDay, strYear]
    maybeDate = strptime "%m-%d-%y" strFormatDate
    
  

data StockHistory = StockHistory {
  tickerSymbol  :: String,
  companyName   :: String,
  stockHistory  :: [StockDay]
} deriving ( Show )

getDayWeek :: Day -> DayWeek
getDayWeek day = case (toWeekDate day) of (_, _, 0) -> Sun
                                          (_, _, 1) -> Mon
                                          (_, _, 2) -> Tue
                                          (_, _, 3) -> Wed
                                          (_, _, 4) -> Thu
                                          (_, _, 5) -> Fri
                                          (_, _, 6) -> Sat

daysWeek :: [DayWeek]
daysWeek = [Sun, Mon, Tue, Wed, Thu, Fri, Sat]

weekDays :: [DayWeek]
weekDays = [Mon, Tue, Wed, Thu, Fri]

dayWeekToNumber :: DayWeek -> Int
dayWeekToNumber day = case day of Sun -> 0
                                  Mon -> 1
                                  Tue -> 2
                                  Wed -> 3
                                  Thu -> 4
                                  Fri -> 5
                                  Sat -> 6



getHighestStockDay' :: [StockDay] -> StockDay
getHighestStockDay' [x] = x
getHighestStockDay' (x:xs)
  | high x > high x' = x
  | otherwise = x'
  where
    x' = getHighestStockDay' xs

getLowestStockDay' :: [StockDay] -> StockDay
getLowestStockDay' [x] = x
getLowestStockDay' (x:xs)
  | low x < low x' = x
  | otherwise = x'
  where
    x' = getLowestStockDay' xs

getLowestCloseStockDay' :: [StockDay] -> StockDay
getLowestCloseStockDay' [x] = x
getLowestCloseStockDay' (x:xs)
  | close x < close x' = x
  | otherwise = x'
  where
    x' = getLowestCloseStockDay' xs

getHighestCloseStockDay' :: [StockDay] -> StockDay
getHighestCloseStockDay' [x] = x
getHighestCloseStockDay' (x:xs)
  | close x > close x' = x
  | otherwise = x'
  where
    x' = getHighestCloseStockDay' xs


stockDayMatchDayWeek :: DayWeek -> StockDay -> Bool
stockDayMatchDayWeek dayWeek stockDay = dayWeekNumber (toWeekDate $ date stockDay) == dayWeekToNumber dayWeek
  where
    dayWeekNumber (_, _, d) = d


getCloseAverageStockDay' :: [StockDay] -> Float
getCloseAverageStockDay' xs = sum xs' / fromIntegral (length xs)
  where
    xs' = [close stockDay | stockDay <- xs]


getCloseAverageDayWeekStockDay' :: [StockDay] -> DayWeek -> Float
getCloseAverageDayWeekStockDay' xs dayWeek = getCloseAverageStockDay' xs'
  where
    xs' = filter (stockDayMatchDayWeek dayWeek) xs


getHighestCloseAverageDayWeek' :: [StockDay] -> DayWeek
getHighestCloseAverageDayWeek' xs = fst $ dayMaxPrice prices
  where
    prices = [(d, getCloseAverageDayWeekStockDay' xs d) | d <- weekDays]
    dayMaxPrice [x]  = x
    dayMaxPrice ((d, p):xs')
      | p > snd x' = (d, p)
      | otherwise = x'
      where
        x' = dayMaxPrice xs'

{-
sortStockDayByDate :: []

getBestDayToBuy' :: [StockDay] -> Int -> DayWeek
getBestDayToBuy' xs daysToHold = 
  where
    dayMatchConditions (x:x':xs) = if (date x 
-}


readGoogleFinanceHistoryCV :: FilePath -> String -> String -> IO (Maybe StockHistory)
readGoogleFinanceHistoryCV filePath tickerSymbol companyName = do
  content <- readFile filePath
  let uglyHistory = map (parseFixedLine . (splitOn ",")) $ tail $ lines content
  return $ makeStockHistory uglyHistory
  where
    maybeDay date = parseGoogleFinanceDate date
    parseFixedLine [date, open, high, low, close, _] =
                    case (parseGoogleFinanceDate date) of
                          Nothing -> Nothing
                          (Just day) -> Just StockDay {
                                        date = day,
                                        open = read open :: Float,
                                        high = read high :: Float,
                                        low = read low :: Float,
                                        close = read close :: Float}
    makeStockHistory uglyHistory = Just StockHistory {
                                          tickerSymbol = tickerSymbol,
                                          companyName = companyName,
                                          stockHistory = catMaybes uglyHistory}


main = print "Hello world!"
