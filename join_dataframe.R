#merging dataframe on datetime (2016 to 2017)
spot_df2 = to_half_hour(spot_df)
spot_df3 = c()
spot_df3 = df_extract_datetime(spot_df2 , "2016-01-01" , "2017-12-31")

#dataframe transforamtion

pre_df2 = as.data.frame(pre_df)
conso_df2 = as.data.frame(conso_df)
prod_df2 = as.data.frame(prod_df)

attr(pre_df2$dt , "tzone") <- "Europe/Paris"
attr(conso_df2$dt , "tzone") <- "Europe/Paris"
attr(prod_df2$dt , "tzone") <- "Europe/Paris"

#regularisation size (term of datetime)
pre_df3 = df_extract_datetime(pre_df2 , "2016-01-01" , "2017-12-31")
prod_df3 = df_extract_datetime(prod_df2 , "2016-01-01" , "2017-12-31")
conso_df3 = df_extract_datetime(conso_df2 , "2016-01-01" , "2017-12-31")


#dataframes jointure
full_df <- pre_df3 %>%
  dplyr::left_join(spot_df3) %>%
  dplyr::left_join(conso_df3) %>%
  dplyr::left_join(prod_df3)

#timezone setting
attr(full_df$dt , "tzone") <- "Europe/Paris"

#filled dataframe
full_df2 = full_df[complete.cases(full_df), ]

#splitting dataframe 
#training dataframe
full_df_training = df_extract_datetime(full_df2 , "2016-01-01" , "2017-06-3O")
#testing dataframe
full_df_testing = df_extract_datetime( full_df2 , "2017-07-01" , "2017-09-3O")
#validation dataframe
full_df_validation = df_extract_datetime(full_df2 , "2017-10-01" , "2017-12-3O")


