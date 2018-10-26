###### googleCollector v1.0 Coded by James Swinburn ######

###### Please do not redistribute without authors permission ####

googleCollector <- function(q = "SPY",
                            exch = "NASDAQ",
                            tf = "1d",
                            ## Granularity
                            tp = "15y",
                            ## Length of data request
                            addTickerCol = TRUE,
                            addExchCol = TRUE)
{
  #  List of time zones
  # https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
  # List of exchange symbols
  # https://www.google.co.uk/googlefinance/disclaimer/
  # Load required packages
  require(stringr)
  require(data.table)
  require(RCurl)
  require(pbapply)
  require(lubridate)
  ##### Extract required timeframe interval from tf arg
  
  tfp <- str_to_lower(str_extract(tf, "[[:alpha:]]"))
  
  tfn <- as.numeric(str_extract(tf, "([0-9])"))
  
  
  
  if (tfp == "m") {
    tfp <- 60
  }
  
  if (tfp == "h") {
    tfp <- 3600
  }
  
  if (tfp == "d") {
    tfp <- 86400
  }
  
  
  
  # set correct interval in seconds
  
  i <- tfp * tfn
  
  
  
  #### Extract required time period from tp arg
  
  if ("d" %in% str_to_lower(str_extract(tp, "[[:alpha:]]")) == TRUE)
    
  {
    tp <- str_to_lower(tp)
  }
  
  
  
  if ("y" %in% str_to_lower(str_extract(tp, "[[:alpha:]]")) == TRUE)
    
  {
    tp <- str_to_upper(tp)
  }
  
  
  
  
  
  # create AJAX request for this API
  
  url <-
    paste0(
      "https://www.google.com/finance/getprices?x=",
      str_to_upper(exch),
      "&i=",
      i,
      "&p=",
      tp,
      "&f=d,t,o,h,l,c,v&df=cpct&q=",
      str_to_upper(q)
    )
  
  
  
  if (url.exists(url) == FALSE)
    
  {
    stop("Check internet or site")
  }
  
  
  
  tryCatch(
    expr = {
      # Start of error checking
      
      
      
      #### For data request greater than 1 day 86400 seconds
      
      if (i >= 86400)
        
      {
        dt <- readLines(url)
        
        dt <- dt[-c(1:7)]
        
        dt <- dt[!grepl("TIMEZONE_OFFSET", dt)]
        
        
        
        dt <-
          read.table(
            text = dt,
            sep = ",",
            header = FALSE,
            stringsAsFactors = FALSE
          )
        
        
        
        setnames(dt,
                 1:6,
                 c('Date', 'Close', 'High', 'Low', 'Open', 'Volume'))
        
        
        
        dt <- data.table(dt)
        
        
        
        dt <- dt[, newDate := {
          t1 <-
            as.POSIXct(Date[1],
                       format = "a%s",
                       origin = "1970-01-01",
                       tz = 'America/New_York')
          
          .(t1 + c(0, as.numeric(Date[-1]) * i))
          
        }
        
        , by = .(cumsum(grepl("^a", Date)))]
        
        
        
        dt$Date <- as.Date(dt$newDate, format = "%Y-%m-%d")
        
        dt <- dt[, c(1, 5, 3, 4, 2, 6)]
      }
      
      
      
      
      
      ##### For data request below 1 day 86400 seconds
      
      if (i < 86400)
        
        # Fread data into R and set names
        
        
      {
        dt <- fread(url,
                    skip = 7,
                    header = FALSE,
                    verbose = FALSE)
        
        setnames(dt,
                 1:6,
                 c('Datetime', 'Close', 'High', 'Low', 'Open', 'Volume'))
        
        
        # Convert google date format into readable format
        
        dt[, Time := {
          t1 <-
            as.POSIXct(Datetime[1],
                       format = "a%s",
                       origin = "1970-01-01",
                       tz = 'America/New_York')
          
          .(t1 + c(0, as.numeric(Datetime[-1]) * i))
          
        }
        
        , by = .(cumsum(grepl("^a", Datetime)))]
        
        
        
        dt$Datetime <- dt$Time
        
        dt <- dt[, c(1, 5, 3, 4, 2, 6)]
      }
      
      #### Add identifiers for symbol and exchange
      
      if (addTickerCol == TRUE)
        
      {
        dt$Ticker <- rep(str_to_lower(q), times = nrow(dt))
      }
      
      
      
      if (addExchCol == TRUE)
        
      {
        dt$Exch <- rep(str_to_lower(exch), times = nrow(dt))
      }
      
      
      
      return(dt)
      
      
      
    },
    error = function(x)
      return(NULL)
  )# End of error checking
  
} #### End of function
googleCollector(q = "SPY",
                exch = "NASDAQ",
                tf = "1d",
                tp = "15y")
