#' Make barplot of number of months below health threshold
#' 
#' Goal is to simply make a box plot of the number of months
#' animals are below certain health threshold values as a function
#' of entanglement class and severity
#' 
#' @return A \code{ggplot2} object for plotting
#' @export
#' @examples 
#' \dontrun{
#' plotHealthThreshold(dfb, bsize)
#' }
plotHealthThreshold <- function(dfb, bsize){
  p <- ggplot(dfb, aes(x = gearName, y = pctthold, fill = status))+
    geom_bar(stat = 'identity')+
    theme_bw(base_size = bsize)+
    labs(y = paste('% of Months w Health < ', thold, sep = ''), x = 'Entanglement Category')+
    scale_x_discrete(limits = c('Unimpacted', 'Minor - no gear', 'Minor - gear', 'Moderate - no gear',
                                'Moderate - gear', 'Severe - no gear', 'Severe - gear'),
                     labels = c('Unimpacted', 'Minor\nNo Gear', 'Minor\nGear',
                                'Moderate\nNo Gear', 'Moderate\nGear',
                                'Severe\nNo Gear', 'Severe\nGear'))+
    scale_y_continuous(breaks = c(0, 25, 50, 75), labels = c('0%', '25%', '50%', '75%'))+
    labs(fill = 'Reproductive\nFemales')+
    scale_fill_brewer(type = 'qual', palette = 'Paired', direction = -1)+
    theme(legend.position = c(0.125, 0.875))
  p  
}
