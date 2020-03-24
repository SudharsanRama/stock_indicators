library(lubridate)
library(candlesticks)
library(xts)
library(rjson)

get_tail_except <- function(df, count ,skip){
    return(head(tail(df,n=count),n=-1*skip))
}

get_results <- function(df) {
    indicators <- c()
    
    # DOJI
    close <- xts(df$CLOSE, order.by = df$TIMESTAMP)
    dfxts <- merge.xts(close, df$OPEN, df$HIGH, df$LOW)

    doji <- CSPDoji(dfxts)

    if(tail(coredata(doji),n=1)[1]){
        if(tail(coredata(doji),n=2)[1,1]){
            if((max(tail(df,n=2)$HIGH) >= max(get_tail_except(df,12,2)$HIGH)) || (min(tail(df,n=2)$LOW) <= min(get_tail_except(df,12,2)$LOW))){
                indicators <- c(indicators, 'doji')
            }
        }
        if((tail(df,n=1)$HIGH >= max(get_tail_except(df,11,1)$HIGH)) || (tail(df,n=1)$LOW <= min(get_tail_except(df,11,1)$LOW))){
            indicators <- c(indicators, 'doji')
        }
    }

    # HAMMER
    if(length(row.names(df)) > 10){
        hammer <- CSPHammer(dfxts)
        trend <- TrendDetectionChannel(dfxts, n=10)

        if(tail(coredata(hammer),n=1)[1]){
            if((tail(coredata(trend),n=1)[4] == -1) && (tail(df,n=1)$CLOSE > get_tail_except(df,2,1)$CLOSE)){
                indicators <- c(indicators, 'hammer-bullish')
            }
            if((tail(coredata(trend),n=1)[4] == 1) && (tail(df,n=1)$CLOSE < get_tail_except(df,2,1)$CLOSE)){
                indicators <- c(indicators, 'hanging-man-bearish')
            }
        }
    }
    

    # INVERTED-HAMMER
    if(length(row.names(df)) > 10){
        inv_hammer <- CSPInvertedHammer(dfxts)
        trend <- TrendDetectionChannel(dfxts, n=10)

        if(tail(coredata(inv_hammer),n=1)[1]){
            if((tail(coredata(trend),n=1)[4] == -1) && (tail(df,n=1)$CLOSE > get_tail_except(df,2,1)$CLOSE)){
                indicators <- c(indicators, 'inv-hammer-bullish')
            }
            if((tail(coredata(trend),n=1)[4] == 1) && (tail(df,n=1)$CLOSE < get_tail_except(df,2,1)$CLOSE)){
                indicators <- c(indicators, 'shooting-star-bearish')
            }
        }
    }


    # STAR
    if(length(row.names(df)) > 23){
        star <- CSPStar(dfxts)

        if((tail(coredata(star),n=1)[1,1]) && (min(tail(df,n=3)$LOW) <= min(get_tail_except(df,13,3)$LOW))){
            indicators <- c(indicators, 'morn-star-bullish')
        }
        if((tail(coredata(star),n=1)[1,2]) && (max(tail(df,n=3)$HIGH) >= max(get_tail_except(df,13,3)$HIGH))){
            indicators <- c(indicators, 'eve-star-bearish')
        }
    }

    return (indicators)
}