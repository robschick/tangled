plotOne <- function(egno,sights,calves,batch, present){
 
  if(present == FALSE){
    base_size = 10
    point_size = 1} else {
    base_size = 16
    point_size = 4
    }
    
	# library(ggplot2, RColorBrewer, gridExtra, stringr)
	dsub <- sights[which(sights$SightingEGNo == egno), ]
	bsub <- batch[which(batch$EGNo == egno), ]
	tsub  <- dsub[dsub$scar != 'FALSE',]
	
	if(nrow(tsub)>0){	events <- unique(tsub$EntglEventNo)
	trect <- data.frame(event=events, start=ymd('1000/01/01'), end=ymd('1000/01/01'), severity=NA, gear=NA, comb=NA, scar = NA)
	trectLong <- data.frame(event = rep(events, each = 2), date=ymd('1000/01/01'), severity=NA, gear=NA, comb=NA, scar = NA)
  meandur <- as.difftime(54496774, units = 'secs') # Calculated from data - See EntangledStates.R
                    
  for(i in 1:length(events)){
    if(is.na(as.Date(tsub[tsub$EntglEventNo==events[i],'EntglStartDate'][1]))){
      trect[trect$event==events[i][1],'start'] <- as.Date(tsub[tsub$EntglEventNo==events[i],'EntglEndDate'][1]) - meandur
    } else {
      trect[trect$event==events[i][1],'start'] <- as.Date(tsub[tsub$EntglEventNo==events[i],'EntglStartDate'][1])  
    }   
		trect[trect$event==events[i][1],'end'] <- as.Date(tsub[tsub$EntglEventNo==events[i],'EntglEndDate'][1])
		trect[trect$event==events[i][1],'severity'] <- tsub[tsub$EntglEventNo==events[i],'severity'][1]
		trect[trect$event==events[i][1],'gear'] <- tsub[tsub$EntglEventNo==events[i],'gear'][1]
		trect[trect$event==events[i][1],'scar'] <- tsub[tsub$EntglEventNo==events[i],'scar'][1]
}
  
  if(exists('trect')){trect$comb <- paste(str_sub(trect$severity, 1, 3), trect$gear, sep = "")}
  
trectLong <- numeric(0)  
  for(i in 1:length(events)){
    event.i <- data.frame(event = rep(trect[i, 'event'], each = 2), 
                date = c(as.Date(trect[i, 'start']), 
                         as.Date(trect[i, 'end'])), 
                severity = rep(trect[i, 'severity'], each = 2),
                gear = rep(trect[i, 'gear'], each = 2),
                comb = rep(trect[i, 'comb'], each = 2),
                scar = rep(trect[i, 'scar'], each = 2))
    trectLong <- rbind(trectLong, event.i)
    }
	}

    cyr <- as.numeric(calves[calves$EGNo==egno,'Calving.Year'])
if(length(cyr)>0){
    # cyrf <- cyr +1
    cyrp <- cyr -1
    call <- sort(c(cyr,cyrp))#,cyrf))
    crect <- data.frame(year=call,interval=NA,start=ymd('1000/01/01'),end=ymd('1000/01/01'))
    crect[crect$year %in% cyr,'interval'] <- 'Calving'
    crect[crect$year %in% cyrp,'interval'] <- 'Gestation'
    # crect[crect$year %in% cyrf,'interval'] <- 'Recovery'    
    crect[,'start'] <- as.Date(paste(crect[,'year'],1,1,sep="/"))
    crect[,'end'] <- as.Date(paste(crect[,'year'],12,31,sep="/"))}
            
	gender <- 'M'
	if('F' %in% dsub$GenderCode){gender <- 'F'} 
	
		ind <-   qplot(x=as.Date(Date),0,data=dsub,geom='line') +
			geom_line(colour=grey(.75))+
			scale_x_date(limits=as.Date(c("1970-01-01","2012-01-01")))+
		 	ylab("")+xlab("")+
		    scale_y_discrete(breaks = NULL) +
		    theme_set(theme_bw())+
		 	  labs(colour="The Data Range")+
		    labs(size="The Data Range") + 
        theme(axis.text.x = element_text(size = base_size, vjust=1),
        axis.text.y = element_text(size = base_size, hjust=1),
		    axis.title.x = element_text(size = base_size),
		    axis.title.y = element_text(size = base_size, angle=90),
		    legend.text = element_text(size = base_size),
		    legend.title = element_text(size = base_size))

		    		 	
	if(gender == 'F' & length(cyr > 0)){		ind.ts <-   ind +
		 	geom_point(data = bsub, aes(as.Date(Date), 0, size = bsub$PhotoCount))+
		    geom_rect(aes(NULL, NULL, xmin = as.Date(start), xmax = as.Date(end), fill = interval), ymin = -Inf, ymax = Inf, data = crect, alpha = 0.2)+
		    scale_fill_manual(values = c("#E41A1C", "#377EB8"), 
		    breaks =  c("Gestation", 'Calving'),
		    labels =  c("Gestation", 'Calving'))+
		 	labs(title = paste("EGNo:",egno," (",gender,')',", # of Sightings per Batch",sep=""))} else {	ind.ts <-   ind +
		 	geom_point(data = bsub, aes(as.Date(Date), 0, size = bsub$PhotoCount))+
		 	labs(title = paste("EGNo:", egno, " (",gender,')', ", # of Sightings per Batch", sep = ""))}

	if(nrow(tsub)>0){		ind.en <-   ind +
		 	geom_point(data = bsub, aes(as.Date(Date), 0))+
		    geom_rect(aes(NULL, NULL, xmin = as.Date(start), xmax = as.Date(end), fill = comb), ymin = -Inf, ymax = Inf, data = trect, alpha = 0.5)+
		    scale_fill_manual('The Data Range', values = c('min0' =  "#A6CEE3", 'min1' = "#1F78B4", 'mod0' =  "#B2DF8A", 'mod1' = "#33A02C", 'sev0' = "#FB9A99", 'sev1' = "#E31A1C"))+
		 	labs(title = "Entanglement Events")}
		 	
	ind.fat <-   ind +
			geom_point(pch = 1, colour = 'grey50', size = point_size)+
		 	geom_point(data = subset(dsub,BodyFatCode == 1 | BodyFatCode == 2 | BodyFatCode == 3), aes(colour=factor(BodyFatCode)),pch = 16, size = point_size)+
  	 	   scale_colour_manual(values= c("#1F78B4",  "#33A02C",  "#E31A1C"), labels = c("Not Thin", "Thin", 'Very Thin'))+
		 	# scale_colour_brewer(palette = 'Dark2', breaks = c(1, 2, 3), labels = c("Not Thin", "Thin", 'Very Thin'))+
		 	labs(title = "Body Fat Levels")+
		 	labs(colour = 'Body Fat Levels')
 					  
	ind.bugs <-   ind +
			geom_point(pch = 1, colour = 'grey50', size = point_size)+
		 	geom_point(data = subset(dsub, CyamidCode != NA), aes(colour = factor(CyamidCode)), pch = 1, size = point_size)+
		 	scale_colour_brewer(palette = 'Dark2',	breaks = c(1, 2, 'X'), labels = c("Few", "Many", 'Not seen'))+
		 	labs(title = "Cyamids on Blow Holes")

	ind.skin <-   ind +
			geom_point(pch = 1, colour = 'grey50', size = point_size)+
		 	geom_point(data = subset(dsub, SkinCode != NA), aes(colour = factor(SkinCode)), pch = 1,size = point_size)+
		 	scale_colour_brewer(palette = 'Dark2', breaks = c(1, 2, 'X'), labels = c("Healthy", "Poor", 'Not seen'))+
		 	labs(title = "Overall Skin Condition")
		 	
	ind.rr <-   ind +
			geom_point(pch = 1, colour = 'grey50', size = point_size)+
		 	geom_point(data = subset(dsub, RightRakeMarkCode != NA), aes(colour = factor(RightRakeMarkCode)), pch = 1, size = point_size)+
		 	scale_colour_brewer(palette = 'Dark2',	breaks = c(1, 2, 3, 'X'), labels = c("Few", "Moderate", 'Severe', 'Not Seen'))+
		 	labs(title = "Rakemarks, Right Side")
		 	
	ind.lr <-   ind +
			geom_point(pch = 1, colour = 'grey50', size = point_size)+
		 	geom_point(data = subset(dsub, LeftRakeMarkCode != NA), aes(colour = factor(LeftRakeMarkCode)), pch = 1, size = point_size)+
		 	scale_colour_brewer(palette = 'Dark2', breaks = c(1, 2, 3, 'X'), labels = c("Few", "Moderate", 'Severe', 'Not Seen'))+
		 	labs(title = "Rakemarks, Left Side")
		
 	
	if(exists('trect')){	return(list(ind.rr = ind.rr, ind.lr = ind.lr, ind.ts = ind.ts, ind.en = ind.en, ind.fat = ind.fat, dsub = dsub, trectLong = trectLong, tsub = tsub, bsub = bsub))}else{return(list(ind.rr = ind.rr, ind.lr = ind.lr, ind.ts = ind.ts,  ind.fat = ind.fat, dsub = dsub, bsub = bsub))}
	# if(exists('trect')){grid.arrange(ind.ts, ind.en, ind.fat, nrow = 3)}else{grid.arrange(ind.ts, ind.fat, nrow = 2)}
	# if(nrow(trect)>0){grid.arrange(ind.ts, ind.en, ind.fat, ind.bugs, ind.skin, ind.rr, ind.lr,nrow=7)}else{grid.arrange(ind.ts, ind.fat, ind.bugs, ind.skin, ind.rr, ind.lr,nrow=6)}
	}