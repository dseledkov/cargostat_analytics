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

reporter <- "818" # 792 TR 818 EG
partner <- "All"
reporter <- "All" # 792 TR 818 EG
partner <- "0"
years <- "2014,2015,2016,2017"
years <- "2017"
product <- "080510"
per_from <- "2017"
per_to <- "2017"

period_m <- character()
substr(per_from, 1, 4)
substr(per_to, 1, 4)
years_string <- as.character(as.integer(substr(per_from, 1, 4)):as.integer(substr(per_to, 1, 4)))
for (i in 1:length(years_string)) {
  period_m <- paste0(period_m, ",", paste0(years_string[i], str_pad(string = seq(1:12), 2, pad = "0"), collapse = ","))
}
trade_annual <- get.Comtrade(r = reporter, p = partner, cc = product, ps = years, fmt = "csv")
trade_annual <- trade_annual[[2]]
trade_annual <- trade_annual[trade_annual$Trade.Flow == "Import", c("Reporter.ISO", "Netweight..kg.")]
colnames(trade_annual) <- c("REPORTER", "VALUE")
trade_annual$VALUE <- round((trade_annual$VALUE / 1000000),0)

boxplot(trade_annual$VALUE)
trade_annual$VALUE <- ifelse(trade_annual$VALUE<75,75,trade_annual$VALUE)
mapped_data <- joinCountryData2Map(dF = trade_annual, 
                                   joinCode = "ISO3", 
                                   nameJoinColumn = "REPORTER",
                                   )
mapCountryData(mapped_data, colourPalette = "heat", 
               nameColumnToPlot = "VALUE", 
               catMethod = "pretty"
               #mapTitle = F
               )

