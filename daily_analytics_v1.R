library(lubridate)
library(RMariaDB)
library(candlesticks)
library(xts)
library(rjson)
library(googledrive)

stockDb <- dbConnect(RMariaDB::MariaDB(), user='stockuser', password='stockuser@123', dbname='stockdb', host='localhost')

# select symbols
query <- "select DISTINCT SYMBOL from stock_history where timestamp = (select max(timestamp) from stock_history) and close >=50 and TOTALTRADES >= 10000"
symbols <- dbFetch(dbSendQuery(stockDb, query))$SYMBOL

query <- paste0("select * from stock_history where timestamp >= str_to_date('",Sys.Date() %m+% months(-3),"','%Y-%m-%d')")
main_df <- dbFetch(dbSendQuery(stockDb, query))

dbDisconnect(stockDb)

# Technical Analysis

get_tail_except <- function(df, count ,skip){
    return(head(tail(df,n=count),n=-1*skip))
}

# DOJI
doji_symbols <- c()
for (symbol in symbols){
    df <- main_df[which(main_df$SYMBOL == symbol),]
    close <- xts(df$CLOSE, order.by = df$TIMESTAMP)
    dfxts <- merge(close, df$OPEN, df$HIGH, df$LOW)
    doji <- CSPDoji(dfxts)

    if(tail(coredata(doji),n=1)[1]){
        if(tail(coredata(doji),n=2)[1,1]){
            if((max(tail(df,n=2)$HIGH) >= max(get_tail_except(df,12,2)$HIGH)) || (min(tail(df,n=2)$LOW) <= min(get_tail_except(df,12,2)$LOW))){
                doji_symbols <- c(doji_symbols, symbol)
                next
            }
        }
        if((tail(df,n=1)$HIGH >= max(get_tail_except(df,11,1)$HIGH)) || (tail(df,n=1)$LOW <= min(get_tail_except(df,11,1)$LOW))){
            doji_symbols <- c(doji_symbols, symbol)
        }
    }

}

# HAMMER
hammer_symbols <- c() # Bullish
hanging_man_symbols <- c() # Bearish
for(symbol in symbols){
    df <- main_df[which(main_df$SYMBOL == symbol),]
    if(length(row.names(df)) <= 10){
        next
    }
    close <- xts(df$CLOSE, order.by = df$TIMESTAMP)
    dfxts <- merge(close, df$OPEN, df$HIGH, df$LOW)
    hammer <- CSPHammer(dfxts)
    trend <- TrendDetectionChannel(dfxts, n=10)

    if(tail(coredata(hammer),n=1)[1]){
        if((tail(coredata(trend),n=1)[4] == -1) && (tail(df,n=1)$CLOSE > get_tail_except(df,2,1)$CLOSE)){
            hammer_symbols <- c(hammer_symbols, symbol)
        }
        if((tail(coredata(trend),n=1)[4] == 1) && (tail(df,n=1)$CLOSE < get_tail_except(df,2,1)$CLOSE)){
            hanging_man_symbols <- c(hanging_man_symbols, symbol)
        }
    }
}

# INVERTED-HAMMER
inv_hammer_symbols <- c() # Bullish
shooting_star_symbols <- c() # Bearish
for(symbol in symbols){
    df <- main_df[which(main_df$SYMBOL == symbol),]
    if(length(row.names(df)) <= 10){
        next
    }
    close <- xts(df$CLOSE, order.by = df$TIMESTAMP)
    dfxts <- merge(close, df$OPEN, df$HIGH, df$LOW)
    inv_hammer <- CSPInvertedHammer(dfxts)
    trend <- TrendDetectionChannel(dfxts, n=10)

    if(tail(coredata(inv_hammer),n=1)[1]){
        if((tail(coredata(trend),n=1)[4] == -1) && (tail(df,n=1)$CLOSE > get_tail_except(df,2,1)$CLOSE)){
            inv_hammer_symbols <- c(inv_hammer_symbols, symbol)
        }
        if((tail(coredata(trend),n=1)[4] == 1) && (tail(df,n=1)$CLOSE < get_tail_except(df,2,1)$CLOSE)){
            shooting_star_symbols <- c(shooting_star_symbols, symbol)
        }
    }
}

# STAR
morn_star_symbols <- c() # Bullish
eve_star_symbols <- c() # Bearish
for(symbol in symbols){
    df <- main_df[which(main_df$SYMBOL == symbol),]
    if(length(row.names(df)) <= 23){
        next
    }
    close <- xts(df$CLOSE, order.by = df$TIMESTAMP)
    dfxts <- merge(close, df$OPEN, df$HIGH, df$LOW)
    star <- CSPStar(dfxts)

    if((tail(coredata(star),n=1)[1,1]) && (min(tail(df,n=3)$LOW) <= min(get_tail_except(df,13,3)$LOW))){
        morn_star_symbols <- c(morn_star_symbols, symbol)
    }
    if((tail(coredata(star),n=1)[1,2]) && (max(tail(df,n=3)$HIGH) >= max(get_tail_except(df,13,3)$HIGH))){
        eve_star_symbols <- c(eve_star_symbols, symbol)
    }
}

result_file <- paste0("/Users/sr1000266884/Documents/repositories/stock_indicators/results/indicators_",Sys.Date(),".json")
write(toJSON(list(
    continuous = list(doji = doji_symbols),
    bullish = list(
        hammer = hammer_symbols,
        inv_hammer = inv_hammer_symbols,
        morn_star = morn_star_symbols
    ),
    bearish = list(
        hanging_man = hanging_man_symbols,
        shooting_star = shooting_star_symbols,
        eve_star = eve_star_symbols
    )
), indent = 4),
    file(result_file,'w')
)

drive_upload(result_file, path = as_id('1n6eCiYwneU5FUOqhHKRsQXXVJJlUfWjn'))

# https://bookdown.org/kochiuyu/Technical-Analysis-with-R/quantmod.html
# https://rdrr.io/rforge/candlesticks/
# https://github.com/joshuaulrich/TTR/blob/master/R/pivots.R