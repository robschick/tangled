#' Plots of the Kaplan-Meier survival curves.
#'

# 
# pdf(file = '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/survivalCurveBySeverity.pdf', width = 9, height = 9*.61)
# ggplot(kmdfAll, aes(interval, psurv, group = sev, colour = sev)) + 
#   geom_step(lwd = 1.5) + 
#   ylim(0, 1) + 
#   geom_segment(data = csubAll, aes(x = deathMonth0 / 12, y = psurv, xend = deathMonth0 / 12, yend = psurv + 0.015)) + 
#   labs(y = 'Survivorship', x = 'Years Following End of Entanglement')+
#   theme_bw(base_size = 18)+
#   theme(panel.grid.major = element_line(size = 1.25), panel.grid.minor = element_line(size = 1))+
#   # scale_x_continuous(breaks = seq(0, 30, 5), minor_breaks = seq(0, 30, 1), expand = c(0, 0))+
#   scale_y_continuous(expand = c(0, 0.05))+
#   scale_colour_brewer(palette = 'Set2', name = 'Entanglement\nSeverity',
#                       labels = c('Minor', 'Moderate', 'Severe'))+
#   theme(legend.position = c(.15, .2))+
#   coord_cartesian(xlim = c(0, 7.5))
# # facet_grid(. ~ gender)
# dev.off()



name <- '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/survivalCurveBySeverityGenderM.pdf'
if(g == 'F') {name <- '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/survivalCurveBySeverityGenderF.pdf'}
pdf(file = name, width = 9, height = 9)
ggplot(kmdfAll, aes(interval, psurv, group = sev, colour = sev)) + 
  geom_step() + 
  ylim(0, 1) + 
  geom_segment(data = csubAll, aes(x = censMonth0 / 12, y = psurv, xend = censMonth0 / 12, yend = psurv + 0.015)) + 
  labs(y = 'Survivorship', x = 'Years Following End of Entanglement')+
  theme_bw(base_size = 18)+
  theme(panel.grid.major = element_line(size = 1.25), panel.grid.minor = element_line(size = 1))+
  # scale_x_continuous(breaks = seq(0, 30, 5), minor_breaks = seq(0, 30, 1), expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0.05))+
  scale_colour_brewer(palette = 'Dark2', name = 'Entanglement\nSeverity',
                      labels = c('Minor', 'Moderate', 'Severe'))+
  theme(legend.position = c(.1, .15))+
  coord_cartesian(xlim = c(0, 17.5))+
  facet_grid(. ~ gender)
dev.off()

oddsAll <- c(oddsF, oddsM)
oddAll <- data.frame(oddRatio = oddsAll, interval = 1:15, gender = rep(c('F', 'M'), each = 15))

name <- '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/survivalOddsRatio.pdf'
pdf(file = name, width = 9, height = 9*.61)
ggplot(oddAll, aes(x = interval, y = oddRatio, colour = gender))+
  labs(y = 'Severe / Moderate Survivorship', x = 'Years Following End of Entanglement')+
  geom_line(lwd = 1.5)+
  theme_bw(base_size = 18)+
  scale_x_continuous(breaks = seq(0, 15, 5), minor_breaks = seq(0, 15, 1), expand = c(0, 0))+
  theme(legend.position = c(0.9, 0.85))+
  scale_colour_brewer(palette = 'Set2', name = 'Sex',
                      labels = c('Female', 'Male'))
dev.off()