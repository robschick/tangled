#' Plot boxplots of health during entanglement and compare to unimpacted
#' 
#' Goal of this is to make the figure needed to depict median health 
#' during the entanglement windows as a function of reproductive status
#' and entanglement severity/class.
#' 
#' @param \code{bsize} a scalar indicating how big to make the labels
#' @param \code{dfLong} a data frame contating health information
#'     for both the reproductively active females and the 
#'     \emph{non}-reproductively active females. These are the data for 
#'     plotting
#' @param \code{cval} a string controlling the size of the labels above the 
#'     box plots. Default is 4.
#' @return A \code{ggplot2} object for plotting
#' @export     
#' @examples 
#' \dontrun{
#' plotBoxplotHealth(dfLong, bsize)
#' }
plotBoxplotHealth <- function(dfLong, bsize, cval = 4){
  # set up for labeling the box plots
  namevec = c('Unimpacted'=0, 'Minor No Gear'=6, 'Minor Gear'=4, 'Moderate No Gear'=5,
              'Moderate Gear'=2, 'Severe No Gear'=3, 'Severe Gear'=1)
  
  nvals <- as.data.frame(table(dfLong$status, dfLong$gearInj))
  
  nvals$label <- namevec[nvals$Var2]
  
  plabel <- c(nvals$Freq[nvals$Var1 == 'NonRepFem' & nvals$Var2 == 6], 
              nvals$Freq[nvals$Var1 == 'RepFem' & nvals$Var2 == 6], 
              nvals$Freq[nvals$Var1 == 'NonRepFem' & nvals$Var2 == 4], 
              nvals$Freq[nvals$Var1 == 'RepFem' & nvals$Var2 == 4], 
              nvals$Freq[nvals$Var1 == 'NonRepFem' & nvals$Var2 == 5], 
              nvals$Freq[nvals$Var1 == 'RepFem' & nvals$Var2 == 5], 
              nvals$Freq[nvals$Var1 == 'NonRepFem' & nvals$Var2 == 2], 
              nvals$Freq[nvals$Var1 == 'RepFem' & nvals$Var2 == 2], 
              nvals$Freq[nvals$Var1 == 'NonRepFem' & nvals$Var2 == 3], 
              nvals$Freq[nvals$Var1 == 'RepFem' & nvals$Var2 == 3], 
              nvals$Freq[nvals$Var1 == 'NonRepFem' & nvals$Var2 == 1], 
              nvals$Freq[nvals$Var1 == 'RepFem' & nvals$Var2 == 1])
  
  plabel <- c(621, 163, plabel) # this adds in the 'all demographic' and then the 'reproductive'
  
  p <- ggplot(dfLong, aes(x = factor(gearInj, levels = c(0, 6, 4, 5, 2, 3, 1)), y = hAnom)) +
    geom_hline(aes(yintercept = 0), colour = 'grey50')+
    geom_boxplot(aes(fill = factor(status), group = paste(factor(gearInj), status)), outlier.shape=NA, notch = FALSE)+
    annotate('text', x = c(0 + 0.82, 0 + 1.18, 1 + 0.82, 1 + 1.18, 1 + 1.82, 1 + 2.18, 1 + 2.82, 1 + 3.18,
                           1 + 3.82, 1 + 4.18, 1 + 4.82, 1 + 5.18, 1 + 5.82, 1 + 6.18), y = 25, 
             label = plabel, cex = cval)+
    labs(y = 'Deviation From Population Health', x = 'Entanglement Impact Category', fill = 'Reproductive Status')+
    scale_x_discrete(labels = c('Unimpacted', 'Minor\nNo Gear', 'Minor\nGear', 
                                'Moderate\nNo Gear','Moderate\nGear', 
                                'Severe\nNo Gear', 'Severe\nGear'))+
    scale_fill_grey(start = 1, end = 0.65,labels = c('All Other\nDemographic\nCategories\n', 'Reproductive\nFemales\n'))+
    theme_bw(base_size = bsize)
  p
  
}
