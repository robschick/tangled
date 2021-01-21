

dsub <- pregnant %>% 
  # dplyr::slice_head(n = 250) %>%
  select(EGNo, Year, Pregnant) %>% 
  arrange(EGNo, Year)
egno <- unique(dsub$EGNo)
# id <- 1013 # testing
# loop over animals
df_all <- numeric(0)
for(id in egno){
  
  my_dsub <- pregnant %>% 
    filter(EGNo == id) 
  
  # Expand to fill all years
  df_exp <- data.frame(EGNo = unique(my_dsub$EGNo),
                       Year = seq(my_dsub$Year[1], 
                                       my_dsub$Year[length(my_dsub$Year)],
                                       by = 1))
  
  df <- left_join(df_exp, my_dsub, by = c('Year', 'EGNo') ) 
  df$severity <- df$severity %>%  replace_na(0)
    
    df <- df %>% 
    mutate(sev_num = case_when(severity == '0' ~ 0,
                               severity == 'unentangled' ~ 0,
              severity == 'minor' ~ 1,
              severity == 'moderate' ~ 2,
              severity == 'severe' ~ 3)) %>% 
      select(-severity)
  
  # df$sev_num <- as.numeric(df$sev_num)
  
  df <- df %>% 
    mutate(ent_group = cumsum(sev_num))
  
  df <- df %>% 
    filter(ent_group > 0) %>% 
    group_by(ent_group) %>% 
    mutate(ent_elapsed = Year - first(Year)) %>% 
    mutate(last_severity = first(sev_num)) %>% 
    ungroup(ent_group) %>%
    select(EGNo, Year, ent_elapsed, last_severity)
  
  # df_1 <- left_join(my_dsub, df, by = c('Year', 'EGNo') ) # keep rows in my_dsub
  # df_2 <- left_join(df, my_dsub, by = c('Year', 'EGNo') ) # keep rows in df

  df <- inner_join(my_dsub, df, by = c('Year', 'EGNo') ) # keep rows in my_dsub
  
  df_all <- rbind(df_all, df)
}
write_csv(df_all, '../data/years_since_entanglement.csv')

# amy_dat <- read_csv('../data/years_since_pregnancy-ark.csv')
