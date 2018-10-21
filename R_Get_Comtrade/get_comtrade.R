library(rjson)
library(stringr)
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
reporters <- fromJSON(file = string)
reporters <- as.data.frame(t(sapply(reporters$results, rbind)))

get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         , maxrec=50000
                         , type="C"
                         , freq="A"
                         , px="HS"
                         , ps="now"
                         , r
                         , p
                         , rg="all"
                         , cc="TOTAL"
                         , fmt="json") {
  string <- paste(
    url
    , "max=", maxrec, "&" # maximum no. of records returned
    , "type=", type, "&" # type of trade (c=commodities)
    , "freq=", freq, "&" # frequency
    , "px=", px, "&" # classification
    , "ps=", ps, "&" # time period
    , "r=", r, "&" # reporting area
    , "p=", p, "&" # partner country
    , "rg=", rg, "&" # trade flow
    , "cc=", cc, "&" # classification code
    , "fmt=", fmt # Format
    , sep = ""
  )

  if (fmt == "csv") {
    raw.data <- read.csv(string, header = TRUE)
    return(list(validation = NULL, data = raw.data))
  } else {
    if (fmt == "json") {
      raw.data <- fromJSON(file = string)
      data <- raw.data$dataset
      validation <- unlist(raw.data$validation, recursive = TRUE)
      ndata <- NULL
      if (length(data) > 0) {
        var.names <- names(data[[1]])
        data <- as.data.frame(t(sapply(data, rbind)))
        ndata <- NULL
        for (i in 1:ncol(data)) {
          data[sapply(data[, i], is.null), i] <- NA
          ndata <- cbind(ndata, unlist(data[, i]))
        }
        ndata <- as.data.frame(ndata)
        colnames(ndata) <- var.names
      }
      return(list(validation = validation, data = ndata))
    }
  }
}

# https://unstats.un.org/unsd/tradekb/Knowledgebase/50377/Comtrade-Country-Code-and-Name

reporter <- "32" # 792 TR 818 EG
partner <- "643"
years <- "2014,2015,2016,2017"
product <- "080510"
per_from <- "2015"
per_to <- "2016"

period_m <- character()
substr(per_from, 1, 4)
substr(per_to, 1, 4)
years_string <- as.character(as.integer(substr(per_from, 1, 4)):as.integer(substr(per_to, 1, 4)))
for (i in 1:length(years_string)) {
  period_m <- paste0(period_m, ",", paste0(years_string[i], str_pad(string = seq(1:12), 2, pad = "0"), collapse = ","))
}
period_m <- substr(period_m, 2, 10000)


trade_annual <- get.Comtrade(r = reporter, p = partner, cc = product, ps = years, fmt = "csv")
trade_annual <- trade_annual[[2]]
trade_annual <- trade_annual[trade_annual$Trade.Flow == "Export", c("Period", "Trade.Flow", "Reporter", "Partner", "Trade.Value..US..", "Netweight..kg.")]
colnames(trade_annual) <- c("PERIOD", "FLOW", "REPORTER", "PARTNER", "VAL", "NET")
trade_annual$ITS <- trade_annual$VAL / trade_annual$NET
trade_annual <- trade_annual[order(trade_annual$PERIOD), ]

trade_mnthly <- get.Comtrade(r = reporter, p = partner, cc = product, freq = "M", ps = "201612,201701,201702,201703,201704,201705,201706,201707,201708", fmt = "csv")
trade_mnthly <- trade_mnthly[[2]]
trade_mnthly <- trade_mnthly[trade_mnthly$Trade.Flow == "Exports" | trade_mnthly$Trade.Flow == "Exports", c("Period", "Trade.Flow", "Reporter", "Partner", "Trade.Value..US..", "Netweight..kg.")]
colnames(trade_mnthly) <- c("PERIOD", "FLOW", "REPORTER", "PARTNER", "VAL", "NET")
trade_mnthly$ITS <- trade_mnthly$VAL / trade_mnthly$NET
trade_mnthly <- trade_mnthly[order(trade_mnthly$PERIOD), ]


write.table(x = trade_mnthly, file = "monthly.csv", sep = ";", na = "", row.names = F, dec = ",")
write.table(x = trade_annual, file = "annual.csv", sep = ";", na = "", row.names = F, dec = ",")
