library(RColorBrewer)
mypal <- brewer.pal(4, 'Greys') # want unimpacted to be mypal[1]

# fix to this: which(rowSums(hrep1980, na.rm = TRUE) > 0)
##
# length(unique(c(names(which(rowSums(hrep1980, na.rm = TRUE) > 0)  ), names(which(rowSums(hnonrep1980, na.rm = TRUE) > 0)))))
# [1] 226
# length(unique(c(names(which(rowSums(hrep1990, na.rm = TRUE) > 0)  ), names(which(rowSums(hnonrep1990, na.rm = TRUE) > 0)))))
# [1] 210
# length(unique(c(names(which(rowSums(hrep2000, na.rm = TRUE) > 0)  ), names(which(rowSums(hnonrep2000, na.rm = TRUE) > 0)))))
# [1] 257
##

mylabs <- data.frame(category = c('1980_unimp', '1980_min','1980_mod','1980_sev',
                                 '1990_unimp', '1990_min','1990_mod','1990_sev',
                                 '2000_unimp', '2000_min','2000_mod','2000_sev'), 
                    num = as.vector(table(dfLong$severity, dfLong$decade)))

mylabs[mylabs$category == '1980_unimp', 'num'] <- 226
mylabs[mylabs$category == '1990_unimp', 'num'] <- 210
mylabs[mylabs$category == '2000_unimp', 'num'] <- 257

dfLong$severity <- factor(dfLong$severity, levels = c("Unimpacted", "minor", "moderate", "severe"), ordered = TRUE)

ggplot(subset(dfLong, !is.na(decade)),
       aes(x = factor(decade), y = hAnom)) +
  geom_boxplot(aes(fill = factor(severity)) , outlier.shape=NA, notch = FALSE) +
  scale_fill_manual(values = c(mypal[1], mypal[2], mypal[3], mypal[4]))+
  scale_y_continuous(breaks = seq(10, 90, by = 10), limits = c(20, 92))+
  theme_bw(base_size = 12)+
  labs(x = 'Decade', y = 'Estimated Health', fill = 'Entanglement\nCategory')+
  annotate('text', x = c(0.725, .9, 1.1, 1.275, 
                         1 + .725, 1 + .9, 1 + 1.1, 1 + 1.275,
                         2 + .725, 2 + .9, 2 + 1.1, 2 + 1.275), y = 92, 
           label = mylabs$num, cex = 3) 
ggsave(filename = 'decadal_boxplotHealth.pdf', path = '/Users/rob/Documents/research/manuscripts/KnowltonEtAl_Entanglement/images', device = 'pdf', width = 9, height = 6, units = 'in')
ggsave(filename = 'decadal_boxplotHealth.png', path = '/Users/rob/Documents/research/manuscripts/KnowltonEtAl_Entanglement/images', device = 'png', dpi = 300, width = 9, height = 6, units = 'in')


# Calculate median values
df_calc <- dfLong %>% 
  tidyr::drop_na(decade) %>% 
  group_by(decade, severity) %>% 
  summarise(median = median(hAnom, na.rm = TRUE))

