#' Make barplot of number of months below health threshold
#' 
#' Goal is to simply make a bar plot of the number of months
#' animals are below certain health threshold values as a functino
#' of entanglement class and severity
#' 
#' @return A ggplot2 object for plotting
#' 
#' @examples 
#' plotHealthThreshold(dfb, bsize)
plotHealthThreshold <- function(dfb, bsize){
  p <- ggplot(dfb, aes(x = gearName, y = pctthold, fill = status))+
    geom_bar(stat = 'identity')+
    theme_bw(base_size = bsize)+
    labs(y = paste('% of Months w Health < ', thold, sep = ''), x = 'Severity Category')+
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
}
