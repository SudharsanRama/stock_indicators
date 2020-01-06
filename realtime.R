library(knitr)
library(lubridate)
library(plyr)

function_name <- "TIME_SERIES_INTRADAY"
stock_ticker <- "TRENT.NS"
period <- "1min"
api_key <- "FZR4L3DSIDMJ7G6F"
data_type <- "csv"
output_size <- "full"
api_call <- paste0("https://www.alphavantage.co/query?function=", function_name, "&symbol=", stock_ticker, "&interval=", period, "&apikey=", api_key, "&datatype=", data_type, "&outputsize=", output_size)
df <- read.csv(url(api_call))

df$timestamp <- as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
df$timestamp <- with_tz(df$timestamp, tz = "Asia/Kolkata")

trade_start <- as.POSIXct(paste0(Sys.Date()," 09:15:00"), format="%Y-%m-%d %H:%M:%S", tz="Asia/Kolkata")
df <- df[which(df$timestamp >= trade_start), ]
df <- df[order(df$timestamp),]
df$timestamp5min <- cut(df$timestamp, "5 min")
df <- ddply(df, .(timestamp5min), summarize, open = open[1], high = max(high), low = min(low), close = tail(close, n=1), volume = sum(volume))