require(MASS)
require(readxl)
require(tibble)
require(dplyr)
require(tidyr)
require(stringr)
require(tibbletime)
require(plotly)
require(dygraphs)
require(timetk)
require(tidyquant)
require(tools)
require(psych)
require(marima)
require(scales)
require(TTR)
require(ggpubr)
require(gridExtra)
require(data.table)

source("functions.R")
source("utils.R")



pre_filepath_lst <- list.files(path="data/pre", pattern="*.xls", full.names=T, recursive=FALSE)
pre_df <- lapply(pre_filepath_lst, pre_to_df) %>% 
  bind_rows()

spot_filepath <- file.path("data", "spot_da", "spot_prices.xlsx")
country_lst <- list(fr="FR")
spot_df_lst <- lapply(country_lst, spot_file_to_df, spot_filepath=spot_filepath)
spot_plot_lst <- lapply(spot_df_lst, plot_spot_ratio)
spot_df <- spot_df_lst$fr %>% as_tbl_time(index=dt) %>% 
  na.omit() %>% 
  prefix_columns("spot.") %>%
  arrange(dt)



prod_filepath_lst <- list.files(path="data/prod", pattern="*.xls", full.names=T, recursive=FALSE)
prod_df <- lapply(prod_filepath_lst, prod_to_df) %>% 
  bind_rows()

#conso
conso_filepath_lst <- list.files(path="data/conso", pattern="*.xls", full.names=T, recursive = FALSE)
conso_df <- lapply(conso_filepath_lst, conso_to_df) %>% 
  bind_rows()



margin_filepath_lst <- list.files(path="data/marges", pattern="*.xls", full.names=T, recursive=FALSE)
margin_df <- lapply(margin_filepath_lst, margin_to_df) %>% 
  bind_rows()

margin_df$morning_safety_mar <- as.numeric(margin_df$morning_safety_mar)
margin_df$morning_available_mar <- as.numeric(margin_df$morning_available_mar)
margin_df$evening_safety_mar <- as.numeric(margin_df$evening_safety_mar)
margin_df$evening_available_mar <- as.numeric(margin_df$evening_available_mar)



imbalance_filepath <- file.path("data", "pre", "MMA_HECAR_2017.xls")
imb_nrows <- nrow(fread(imbalance_filepath, select = 1L))# - 8
imb_df <- read.table(imbalance_filepath, header=T, sep="\t", skip=5, encoding="latin1", nrows=imb_nrows)
imb_df <- imb_df[,c(1,4:6)]
names(imb_df) <- c("date", "trend", "prep", "pren")
imb_df$date <- as.POSIXct(imb_df$date, format="%d/%m/%Y %H:%M")
imb_df <- imb_df %>% as_tbl_time(index=date)