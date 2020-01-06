library(lubridate)
library(RMariaDB)
library(candlesticks)
library(xts)
library(rjson)

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
    close <- xts(df$CLOSE, order.by = df$TIMESTAMP)
    dfxts <- merge(close, df$OPEN, df$HIGH, df$LOW)
    hammer <- CSPHammer(dfxts)

    if((tail(coredata(hammer),n=1)[1]) && (tail(df,n=1)$LOW <= min(get_tail_except(df,11,1)$LOW))){
        hammer_symbols <- c(hammer_symbols, symbol)
    }
    if((tail(coredata(hammer),n=1)[1]) && (tail(df,n=1)$HIGH >= max(get_tail_except(df,11,1)$HIGH))){
        hanging_man_symbols <- c(hanging_man_symbols, symbol)
    }
}

# INVERTED-HAMMER
inv_hammer_symbols <- c() # Bullish
shooting_star_symbols <- c() # Bearish
for(symbol in symbols){
    df <- main_df[which(main_df$SYMBOL == symbol),]
    close <- xts(df$CLOSE, order.by = df$TIMESTAMP)
    dfxts <- merge(close, df$OPEN, df$HIGH, df$LOW)
    inv_hammer <- CSPInvertedHammer(dfxts)

    if((tail(coredata(inv_hammer),n=1)[1]) && (tail(df,n=1)$LOW <= min(get_tail_except(df,11,1)$LOW))){
        inv_hammer_symbols <- c(inv_hammer_symbols, symbol)
    }
    if((tail(coredata(inv_hammer),n=1)[1]) && (tail(df,n=1)$HIGH >= max(get_tail_except(df,11,1)$HIGH))){
        shooting_star_symbols <- c(shooting_star_symbols, symbol)
    }
}

# STAR
morn_star_symbols <- c() # Bullish
eve_star_symbols <- c() # Bearish
for(symbol in symbols){
    df <- main_df[which(main_df$SYMBOL == symbol),]
    if(length(row.names(df)) < 20){
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

write(toJSON(list(
    doji = doji_symbols,
    hammer = hammer_symbols,
    hanging_man = hanging_man_symbols,
    inv_hammer = inv_hammer_symbols,
    shooting_star = shooting_star_symbols,
    morn_star = morn_star_symbols,
    eve_star = eve_star_symbols
)),
    paste0("indicators_",Sys.Date(),".json")
)

# https://bookdown.org/kochiuyu/Technical-Analysis-with-R/quantmod.html
# https://rdrr.io/rforge/candlesticks/
# https://github.com/joshuaulrich/TTR/blob/master/R/pivots.R