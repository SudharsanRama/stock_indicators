# https://www.pushbullet.com/channel?tag=262c8964f87fba3a44c861b7a26d53f7

library(knitr)
library(lubridate)
library(plyr)
library(RPushbullet)
library(config)
source('/Users/sr1000266884/Documents/repositories/stock_indicators/custom_indicators.R')

config <- config::get(file='/Users/sr1000266884/Documents/repositories/stock_indicators/config.yml')

agg_timestamp <- function(df, duration = '15 min') {
    df$timestamp <- cut(df$timestamp, duration)
    df <- ddply(df, .(timestamp), summarize, open = open[1], high = max(high), low = min(low), close = tail(close, n=1), volume = sum(volume))
    return (df)
}

get_df <- function(stock_ticker) {
    function_name <- "TIME_SERIES_INTRADAY"
    period <- "1min"
    api_key <- config$alphavantage_apikey
    data_type <- "csv"
    output_size <- "full"
    api_call <- paste0("https://www.alphavantage.co/query?function=", function_name, "&symbol=", stock_ticker, "&interval=", period, "&apikey=", api_key, "&datatype=", data_type, "&outputsize=", output_size)
    df <- read.csv(url(api_call))
    df$timestamp <- as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
    df$timestamp <- with_tz(df$timestamp, tz = "Asia/Kolkata")
    return (df)
}

indices <- c('^NSEI','^NSEBANK')
for (index in indices) {
    df <- get_df(index)
    df <- df[which(date(df$timestamp) %in% head(unique(date(df$timestamp)), n=2)), ]
    df <- df[order(df$timestamp),]
    df <- agg_timestamp(df, '5 min')
    colnames(df) <- toupper(colnames(df))
    df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP)
    indicators <- get_results(df)
    if(length(indicators)>0) {
        pbPost("note", index, paste(indicators, collapse=', '), channel = config$pb_channel, apikey = config$pb_apikey)
    }
}


# trade_start <- as.POSIXct(paste0(Sys.Date()," 09:15:00"), format="%Y-%m-%d %H:%M:%S", tz="Asia/Kolkata")
# df <- df[which(df$timestamp >= trade_start), ]

