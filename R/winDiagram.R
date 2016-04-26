#' Create a diagram showing how we extract health anomaly information during entanglement
#' #' 
#' Goal of this function is to return a ggplot2 plot object to show the extraction 
#' mechanics of intersecting the entanglement windows along with the health
#' information - in this case the health anomaly
#' 
#' @return A ggplot2 object for plotting the extraction diagram
#' 
#' @example winDiagram(anom, tangleOut, myName, egno = 1167)
winDiagram <- function(anom, tangleOut, myName, egno = 1167){
  
  yrvec  <- 1970:2016
  nyr    <- length(yrvec)
  monYr  <- cbind( rep(c(1:12), nyr), rep(yrvec, each = 12) )
  dateTime <- as.Date(paste(1, monYr[, 1], monYr[, 2], sep = '/'), format = '%d/%m/%Y')
  
  stopifnot(all.equal(length(myName), dim(anom)[2]))
  
  times <- tangleOut[tangleOut$EGNo == egno, c('swindmonyr', 'ewindmonyr', 'rec12monyr')]
  sidx <- match(times$swindmonyr, myName)
  eidx <- match(times$ewindmonyr, myName)
  ridx <- match(times$rec12monyr, myName)
  tidx <- cbind(sidx, eidx, ridx)
  
  df <- data.frame(dateTime = dateTime, 
                   time = 1:ncol(anom), 
                   health = anom[which(rownames(anom) == 1167), ])
  df$pos <- df$health > 0
  df <- df[!is.na(df$health), ]
  
  dfsub <- subset(df, time %in% tidx[4,])
  
  p <- ggplot(df, aes(x = dateTime, y = health, fill = pos)) +
    geom_bar(stat = 'identity', position = 'identity', width = 40)+
    # Event 1
    annotate('text', x = dateTime[325], y = 19, label = 'Example 1: entanglement\nwith scars only', size = 6) +
    annotate('segment', x = dateTime[325], xend = dateTime[tidx[2, 2]], y = 10.25, yend = 14.5) +
    annotate('segment', x = dateTime[tidx[2, 2]], xend = dateTime[337], y = 14.5, yend = 8) +
    annotate('point', x = dateTime[325], y = 10.25, size = 3) +
    annotate('text', x = dateTime[318], y = 12, label = 'Date seen prior\nwithout scars ("start")') +
    annotate('point', x = dateTime[tidx[2, 2]], y = 14.5, size = 3) +
    annotate('text', x = dateTime[tidx[2, 2] + 7], y = 16, label = 'First date seen\nwith scars ("end")') +
    annotate('point', x = dateTime[337], y = 8, size = 3) +
    annotate('text', x = dateTime[341], y = 8, label = 'After 12\nmonths') +
    annotate('rect', xmin = dateTime[tidx[2, 1]], xmax = dateTime[tidx[2, 2]], ymin = 0, ymax = 16, alpha = 0.4, fill = 'grey20')+
    annotate('text', x = dateTime[tidx[2, 1]] + 45, y = -6, 
             label = 'Entanglement health window\n3 months prior to first\n date detected with scars ')+
    annotate('segment', x = dateTime[tidx[2, 1]] + 45, xend = dateTime[tidx[2, 1]] + 45, 
             y = -3.75, yend = -0.25, arrow = arrow())+
    # Event 2
    annotate('text', x = df$dateTime[244], y = -15, label = 'Example 2: entanglement\nwith gear attached', size = 6) +
    annotate('rect', xmin = df$dateTime[217], xmax = df$dateTime[228], 
             ymin = 0, ymax = -22, alpha = 0.4, fill = 'grey20')+
    annotate('segment', x = df$dateTime[204], xend = df$dateTime[228], y = -7, yend = -10) +
    annotate('segment', x = df$dateTime[228], xend = df$dateTime[240], y = -10, yend = 2) +
    annotate('point', x = df$dateTime[204], y = -7, size = 3)+
    annotate('text', x = df$dateTime[206], y = -9, size = 3, label = 'Date seen prior\nwithout gear ("start")')+
    annotate('point', x = df$dateTime[220], y = -20, size = 3)+
    annotate('text', x = df$dateTime[226], y = -20, size = 3, label = 'First date seen \nwith gear')+
    annotate('point', x = df$dateTime[228], y = -10, size = 3)+
    annotate('text', x = df$dateTime[234], y = -10, size = 3, label = 'Last date seen \nwith gear')+
    annotate('point', x = df$dateTime[240], y = 2, size = 3)+
    annotate('text', x = df$dateTime[244], y = 3.5, size = 3, label = 'After 12 months')+
    annotate('text', x = df$dateTime[221] + 45, y = 6, 
             label = 'Entanglement health window - 3 months prior to\n first date detected with gear to 3 months after\nlast date sighted with attached gear')+
    annotate('segment', x = df$dateTime[221] + 45, xend = df$dateTime[221] + 45, 
             y = 3.75, yend = 0.25, arrow = arrow())+
    labs(x = 'Time', y = 'Health Anomaly')+
    scale_x_date(limits = c(as.Date("1996-01-01"), as.Date("2004-01-01")))+
    lims(y = c(-22, 20))+
    theme(legend.position = 'none')   
  p

}

