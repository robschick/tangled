p <- ggplot(dfb, aes(x = gearName, y = pct70, fill = status))+
  geom_bar(stat = 'identity')+
  # geom_text(aes(y = 0, label = dfb$nmon70), vjust = -0.3)+
  theme_bw(base_size = 18)+
  labs(y = '% of Months w Health < 70', x = 'Severity Category')+
  scale_x_discrete(limits = c('Unimpacted', 'Minor - no gear', 'Minor - gear', 'Moderate - no gear',
                              'Moderate - gear', 'Severe - no gear', 'Severe - gear'),
                   labels = c('Unimpacted', 'Minor\nNo Gear', 'Minor\nGear',
                              'Moderate\nNo Gear', 'Moderate\nGear',
                              'Severe\nNo Gear', 'Severe\nGear'))+
  scale_y_continuous(breaks = c(0, 25, 50, 75), labels = c('0%', '25%', '50%', '75%'))+
  labs(fill = 'Reproductive\nStatus')+
  scale_fill_brewer(type = 'qual', palette = 'Dark2', direction = -1)+
  theme(legend.position = c(0.125, 0.875))
p