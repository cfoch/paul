import Data.Time.Calendar.WeekDate
import Data.Time.Calendar
import Data.Time

data DayWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving ( Show )
data StockDay = StockDay {
  date  :: Day,
  open  :: Float,
  high  :: Float,
  low   :: Float,
  close :: Float
} deriving ( Show )

data StockHistory = StockHistory {
  tickerSymbol  :: String,
  companyName   :: String,
  history       :: [StockDay]
} deriving ( Show )

getDayWeek :: Day -> DayWeek
getDayWeek day = case (toWeekDate day) of (_, _, 1) -> Sun
                                          (_, _, 2) -> Mon
                                          (_, _, 3) -> Tue
                                          (_, _, 4) -> Wed
                                          (_, _, 5) -> Thu
                                          (_, _, 6) -> Fri
                                          (_, _, 7) -> Sat

readGoogleFinanceHistoryCV :: String -> IO (String)
readGoogleFinanceHistoryCV filePath = do
  content <- readFile filePath
  let fixLine = tail . init
  let uglyHistory = map fixLine (lines content)
  return $ show (uglyHistory !! 1)

