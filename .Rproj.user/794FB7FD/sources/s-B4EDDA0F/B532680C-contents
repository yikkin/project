"as.hour" <-
  function(x, mindt, maxdt, half.hour = FALSE){
    if(half.hour==TRUE){
      unit <- 3600/2
    } else {
      unit <- 3600
    }
    ct <- as.POSIXct(x)
    lt <- as.POSIXlt(ct)
    mindt <- as.POSIXct(mindt)
    maxdt <- as.POSIXct(maxdt)
    ct.sec <- as.numeric(ct)
    ct.hour <- ct.sec%/%unit
    ct.hour12 <- as.numeric(format(ct, format = "%I"))
    ct.hour24 <- as.numeric(format(ct, format = "%H"))
    ct.wkday <- format(ct, format = "%a")
    ct.weekday <- format(ct, format = "%A")
    cct <- seq(mindt, maxdt, 1)
    cct.sec <- as.numeric(cct)
    cct.hour <- cct.sec%/%unit
    cct.hour.tab <- as.numeric(names(table(cct.sec%/%unit)))
    cct.tab <- cct[!duplicated(cct.hour)]
    cct.hour24 <- as.numeric(format(cct.tab, format = "%H"))
    cct.hour12 <- as.numeric(format(cct.tab, format = "%I"))
    cct.ampm <- cct.ampm2 <- format(cct.tab, format = "%p")
    cct.ampm2[cct.ampm=="AM" & !is.na(cct.ampm)] <- "am"
    cct.ampm2[cct.ampm=="PM" & !is.na(cct.ampm)] <- "pm"
    cct.weekday <- format(cct.tab, format = "%A")
    cct.wkday <- format(cct.tab, format = "%a")
    cct.month <- format(cct.tab, format = "%B")
    cct.mon <- format(cct.tab, format = "%b")  
    cct.year <- format(cct.tab, format = "%Y")  
    names(cct.tab) <- cct.hour.tab
    ct.hour.factor <- factor(ct.hour, levels = cct.hour.tab)
    ct.stratum <- cct.tab[as.character(ct.hour)]
    ct.stratum.factor <- factor(unname(ct.stratum),
                                levels = unname(cct.tab))
    clt <- as.POSIXlt(cct.tab)
    list(#ct = ct,
        
   
         #cstratum = unname(cct.hour.tab),
         cstratum2 = unname(cct.tab)
        
       
    )
  }



to_half_hour <- function(df){
  df2 <- df
  df2$dt <- df2$dt + 0.5*60*60
  df_full <- bind_rows(df, df2) %>%
    as.data.frame() %>% 
    plyr::arrange(dt)
  df_full
}


df_extract_datetime <- function(df , start_date , end_date){
  df2 <- as.data.frame(with(df, df[(df$dt >= start_date & df$dt <= end_date),]))
  df2
}

