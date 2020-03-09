#' Return the slope health plot as a \code{gridExtra} grob
#' 
#' @param dfout A 579 by 11 data frame containing all the health and health anomaly information at three points
#' @param dfn A 6 by 2 data frame containing the number of entanglement events used in the plotting. This is used in the annotation.
#' @param dfAsuml An 12 by 5 data frame that has the information need to make the bar plot of change during each of the two windows: entanglement and recovery
#' @param aval A transparency value for plotting the lines; closer to 1 = more black.
#' 
#' @return A \code{gridExtra} element that plots the two plots
#' @export
#' @examples 
#' \dontrun{
#' plotSlopeHealth(dfout, dfn, dfMed, dfAsuml, aval)
#' }
plotSlopeHealth <- function(dfout, dfn, dfMed, dfAsuml, aval = 0.5){

  p <- ggplot(data = dfout)+
    geom_segment(aes(y = startAnom, yend = endAnom, x = 0, xend = 1), colour = alpha('black', aval)) +
    geom_segment(aes(y = endAnom, yend = recAnom, x = 1, xend = 2), colour = alpha('black', aval))+
    geom_path(data = dfMed, aes(y = value, x = x), lwd = 1.5) +
    annotate('text', x = 0, y = -75, size = 5, hjust = 0,
           label = c(paste('n = ', dfn[which(dfn$gearInj == 6), 'n'], sep = ''),   # minor NO gear
                     paste('n = ', dfn[which(dfn$gearInj == 5), 'n'], sep = ''),   # moderate NO gear
                     paste('n = ', dfn[which(dfn$gearInj == 3), 'n'], sep = ''),   # severe NO gear
                     paste('n = ', dfn[which(dfn$gearInj == 4), 'n'], sep = ''),   # minor gear
                     paste('n = ', dfn[which(dfn$gearInj == 2), 'n'], sep = ''),   # moderate gear
                     paste('n = ', dfn[which(dfn$gearInj == 1), 'n'], sep = '')))+ # severe gear
    scale_x_continuous(breaks = c(0, 1, 2),
                     labels = c( "P", "S or G", 'L'))+
    facet_grid(gearLab ~ sevLab)+
    theme_bw()+
    theme(panel.grid.minor.x = element_blank())+
    labs(x = '', y = 'Health Deviation')

  # p2 <- ggplot(dfAsuml, aes(x = fullLab, y = value, fill = variable)) +
  #   geom_bar(stat = 'identity', position = 'dodge')+
  #   coord_flip()+
  #   scale_x_discrete(limits = rev(c("Minor No Gear", "Minor Gear", "Moderate No Gear", 
  #                                 "Moderate Gear", "Severe No Gear",  "Severe Gear")))+
  #   labs(x = '', y = 'Deviation From Population Health', fill = c('Time Period'))+
  #   scale_fill_brewer(type = 'qual', palette = 'Dark2', direction = -1, 
  #                   labels = c('Entanglement\nTimeframe', "'Recovery'\nTimeframe"))+
  #   theme_bw() +
  #   theme(legend.text = element_text(size = 6))
  # 
  # grid.arrange(p, p2, ncol=1, heights = c(0.7,  0.3))
  p
}