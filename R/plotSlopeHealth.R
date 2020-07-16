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
plotSlopeHealth <- function(dfout, dfn, dfMed, dfAsuml, aval = 0.1){
  
  dfn$sevLab <- c('Severe', 'Moderate', 'Severe', 'Minor', 'Moderate', 'Minor')
  dfn$gearLab <- c('Gear', 'Gear', 'No Gear', 'Gear', 'No Gear', 'No Gear')
  dfn$label <- c('n = 33', 'n = 27', 'n = 13',
                 'n = 21', 'n = 123', 'n = 597')
  
  p <- ggplot(data = dfout)+
    geom_segment(aes(y = startHealth, yend = endHealth, x = 0, xend = 1), colour = alpha('black', aval)) +
    geom_segment(aes(y = endHealth, yend = recHealth, x = 1, xend = 2), colour = alpha('black', aval))+
    geom_path(data = dfMed, aes(y = value, x = x), lwd = 1.5) +
    facet_grid(gearLab ~ sevLab)+
    geom_text(aes(x = 0, y = 5, label = label, hjust = 0), data= dfn)+
    scale_x_continuous(breaks = c(0, 1, 2),
                       labels = c( "P", "S or G", 'L'))+
    theme_bw()+
    theme(panel.grid.minor.x = element_blank())+
    labs(x = '', y = 'Estimated Health')

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