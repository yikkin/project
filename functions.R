
pre_to_df_old <- function(filepath){
  df <- read.table(filepath,
                   header=TRUE,
                   skip=5,
                   sep="\t",
                   fileEncoding="CP1252",
                   fill = TRUE,
                   row.names = NULL)
  df <- df[, 1:4]
  colnames(df) <- c("date", "hour", "pren", "prep")
  df <- na.omit(df)
  df$date <- as.POSIXct(paste(df$date, substr(df$hour,1,5)), format="%d/%m/%Y %Hh%M", tz="Europe/Paris")
  df <- df %>% dplyr::select(-hour)
  df
}


filepath <- "data/pre/MMA_HECAR_2016.xls"

pre_to_df <- function(filepath){
  df <- read.table(filepath,
                   header=TRUE,
                   skip=5,
                   sep="\t",
                   fileEncoding="CP1252",
                   fill = TRUE,
                   row.names = NULL)
  df <- df[, c(1,4,5,6)]
  colnames(df) <- c("dt", "tendancy", "prep", "pren")
  df <- na.omit(df)
  df$dt <- as.POSIXct(df$dt, format="%d/%m/%Y %H:%M", tz="Europe/Paris")
  df <- df %>% as_tbl_time(index=dt) 
  df
}

filepath <- "data/prod/PrevisionProduction_2016.xls"

prod_to_df <- function(filepath){
  df <- read.table(filepath,
                   header=TRUE,
                   skip=0,
                   sep="\t",
                   fileEncoding="CP1252",
                   fill = TRUE,
                   row.names = NULL)
  colnames(df) <- c("dt", "hour", "prod_prog", "prod_int")
  df <- na.omit(df)
  df$dt <- paste(df$dt, df$hour)
  df$dt <- as.POSIXct(df$dt, format="%d/%m/%Y %H:%M", tz="Europe/Paris")
  df <- df %>% dplyr::select(-hour) %>% as_tbl_time(index=dt) 
  df
}


filepath <- "data/conso/conso_mix_RTE_2016.xls"

conso_to_df <- function(filepath){
  df <- read.table(filepath,
                   header=FALSE,
                   skip=0,
                   sep="\t",
                   fileEncoding="CP1252",
                   fill = TRUE,
                   row.names = NULL)
  dates <- df[,1]
  to_rem <- startsWith(as.character(df[,1]), "H") | startsWith(as.character(df[,1]), "J") | startsWith(as.character(df[,1]), "R")
  dates[!startsWith(as.character(dates), "Journée du ")] <- ""
  dates <- gsub("Journée du ", "", dates)
  dates <- na.locf(dates)
  df[,1] <- paste(dates, df[,1])
  df <- df[, c(1,2,3)]
  colnames(df) <- c("dt", "conso_jm1", "conso_j")
  df <- na.omit(df)
  df <- df[!to_rem,]
  df$dt <- as.POSIXct(df$dt, format="%d/%m/%Y %H:%M", tz="Europe/Paris")
  df <- df %>% as_tbl_time(index=dt) %>% as.data.frame() 
  df
}

filepath <- "data/marges/MMA_HMARG_2016.xls"

margin_to_df <- function(filepath){
  df <- read.table(filepath,
                   header=TRUE,
                   skip=3,
                   sep="\t",
                   fileEncoding="CP1252",
                   fill = TRUE,
                   row.names = NULL)
  to_rem <- startsWith(as.character(df[,1]), "R") | startsWith(as.character(df[,1]), "*")
  df <- df[!to_rem,c(1,4:9)]
  colnames(df) <- c("date", "morning_peak", "morning_safety_mar",  "morning_available_mar", "evening_peak", "evening_safety_mar",  "evening_available_mar")
  for (i in 1:ncol(df)){
    df[startsWith(as.character(df[,i]), "*"), i] <- NA
  }
  df$date <- as.POSIXct(df$date, format="%d/%m/%Y", tz="Europe/Paris")
  df$morning_safety_mar <- as.character(df$morning_safety_mar)
  df$morning_available_mar <- as.character(df$morning_available_mar)
  df$evening_safety_mar <- as.character(df$evening_safety_mar)
  df$evening_available_mar <- as.character(df$evening_available_mar)
  df
}





#--------------------


# Spot processing ####
#a remplacer par celle de slack
format_spot <- function(spot){
  spot %>%
    gather(key = "hour", value = "price", H0:H23) %>%
    mutate(dt = as.POSIXct(paste(date, hour), format = "%Y-%m-%d H%H")) %>%   # tz = “Europe/Paris”
    select(dt, price, - date, - hour) %>%
    arrange(dt) %>%
    filter(!is.na(dt))
}



plot_spot_ratio <- function(spot){
  spot_xts <- tk_xts(spot)
  rm_spot <- rollmean(spot_xts, 672) %>% tk_xts()
  nm_spot <- spot_xts / rm_spot
  nm_spot <- data.frame(dt=index(nm_spot), coredata(nm_spot)) %>% 
    as_tbl_time(index = dt)
  nm_spot$year <- year(nm_spot$dt)
  nm_spot$quarter <- paste0("Q", as.character((month(nm_spot$dt) - 1 ) %/% 3 + 1))
  nm_spot$wd <- ifelse(weekdays(nm_spot$dt, abbreviate = T) %in% c("Sam", "Dim"), "week-end", "week")
  nm_spot$hour <- hour(nm_spot$dt)
  
  agg_spot <- nm_spot %>%
    group_by(year, quarter, wd, hour) %>%
    summarise(ratio=mean(price, na.rm = FALSE))
  
  mean_spot <- agg_spot %>%
    group_by(year, quarter, wd) %>%
    summarise(mean_ratio=mean(ratio, na.rm = FALSE))
  
  mm_spot <- merge(agg_spot, mean_spot)
  mm_spot$shape <- mm_spot$ratio / mm_spot$mean_ratio
  mm_spot <- mm_spot %>% dplyr::select(-mean_ratio)
  
  p <- ggplot(mm_spot, aes(y=shape, x=hour, colour=year)) + geom_line()
  p <- p + facet_grid(quarter ~ wd) #year + month + 
  p <- p + theme(plot.margin = margin(1,1,1,1, "cm"))
  p <- ggplotly(p)
  p
}

spot_file_to_df <- function(country_code, spot_filepath){
  spot <- read_excel(spot_filepath, sheet=country_code) %>%
    format_spot()
}

# ECO2mix processing ####

eco2mix_file_to_df <- function(file) {
  df_orig <- read_excel(file)
  df <- df_orig %>%
    dplyr::select(Date, Heures, Consommation, Eolien , Solaire) %>%
    na.omit()
  dates <- df %>%
    pull(Date) %>% 
    strftime(format="%Y-%m-%d")
  times <- df %>%
    pull(Heures) %>% 
    strftime(format="%H:%M:%S")
  dt <- as.POSIXct(paste(dates, times, sep=" "))
  df <- df %>%
    add_column(dt) %>% 
    dplyr::select(dt, Consommation, Eolien , Solaire)
  colnames(df) <- c("date", "cons", "wind", "solar")
  df
}