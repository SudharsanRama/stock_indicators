# https://www.nse-india.com/content/historical/EQUITIES/2019/JUL/cm01JUL2019bhav.csv.zip

# temp <- tempfile()
# download.file("https://www1.nseindia.com/content/historical/EQUITIES/2019/JUL/cm01JUL2019bhav.csv.zip",temp)
# stockData <- read.table(unz(temp, "cm01JUL2019bhav.csv"))

read_table <- function(temp, file_name, date_str) {
    report = read.table(unz(temp, file_name),header=TRUE,sep=",")
    report$TIMESTAMP = as.Date(date_str,format("%d-%b-%Y"))
    return (report)
}

crawl_stocks <- function(days_before) {
    if(days_before < 0){
        return(NULL)
    }
    dates <- format(seq.Date(from = as.Date(Sys.Date()-days_before),to=as.Date(Sys.Date()),by="day"),"%d-%b-%Y")
    dates <- strsplit(dates,"-")
    dates <- matrix(unlist(dates), ncol=3, byrow=TRUE)
    stockData <- NULL
    for (idx in 1:nrow(dates)){
        temp <- tempfile()
        file_name <- paste0("cm",dates[idx,1],toupper(dates[idx,2]),dates[idx,3],"bhav.csv")
        tryCatch({
            download.file(paste0("https://www1.nseindia.com/content/historical/EQUITIES/",dates[idx,3],"/",toupper(dates[idx,2]),"/",file_name,".zip"),temp)
            if (is.null(stockData)){
                stockData <- read_table(temp, file_name, paste0(dates[idx,],collapse="-"))
            } else {
                stockData <- rbind(stockData, read_table(temp, file_name, paste0(dates[idx,],collapse="-")))
            }
        }, error = function(e){
            print(paste('skipped', paste0(dates[idx,],collapse="-")))
        })
        unlink(temp)
    }
    stockData <- stockData[which(stockData$SERIES == "EQ"), ]
    stockData <- stockData[, !(names(stockData) %in% c("SERIES","X"))]
    return (stockData)
}

# args <- commandArgs(trailingOnly = TRUE)
library(RMariaDB)
stockDb <- dbConnect(RMariaDB::MariaDB(), user='stockuser', password='stockuser@123', dbname='stockdb', host='localhost')

last_run <- dbFetch(dbSendQuery(stockDb, "select max(timestamp) from stock_history"))
days_before <- Sys.Date() - last_run$`max(timestamp)` - 1
trg_table <- "stock_history"

data <- crawl_stocks(as.numeric(days_before))

if(is.null(data)){
    quit()
}

dbWriteTable(stockDb, trg_table, data, append=TRUE)
dbDisconnect(stockDb)

print("Done...")



