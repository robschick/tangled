

dsub <- pregnant %>% 
  # dplyr::slice_head(n = 250) %>%
  select(EGNo, Year, Pregnant) %>% 
  arrange(EGNo, Year)
egno <- unique(dsub$EGNo)
id <- 1013 # testing
# loop over animals
df_all <- numeric(0)
for(id in egno){
  
  my_dsub <- dsub %>% 
    filter(EGNo == id) 
  
  # Expand to fill all years
  df_exp <- data.frame(EGNo = unique(my_dsub$EGNo),
                       Year = seq(my_dsub$Year[1], 
                                       my_dsub$Year[length(my_dsub$Year)],
                                       by = 1))
  
  df <- left_join(df_exp, my_dsub, by = c('Year', 'EGNo') ) 
  df$Pregnant <- df$Pregnant %>%  replace_na(0)
  df <- df %>% 
    mutate(preg_group = cumsum(Pregnant))
  
  df <- df %>% 
    filter(preg_group > 0) %>% 
    group_by(preg_group) %>% 
    mutate(elapsed = Year - first(Year)) %>% 
    ungroup(preg_group) %>% 
    select(- preg_group)
  
  df <- left_join(my_dsub, df, by = c('Year', 'EGNo', 'Pregnant') ) 
  
  df_all <- rbind(df_all, df)
}
write_csv(df_all, '../data/years_since_pregnancy.csv')

amy_dat <- read_csv('../data/years_since_pregnancy-ark.csv')
