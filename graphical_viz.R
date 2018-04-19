#series representation
dyg_lst <- list()
full_xts <- xts(full_df2 %>% dplyr::select(-dt), order.by = full_df2$dt)
for (i in 2:ncol(full_xts)){
  dyg_lst[[i]] <- full_xts[,i] %>% dygraph(main = names(full_xts[,i]), group = "ensync") %>%
    dyRangeSelector()
}


