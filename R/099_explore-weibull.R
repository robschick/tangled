# explore weibull vs exponential
pregnant <- read_csv(here::here('data-raw', '2021-02-25_years_since_pregnancy_1980-2013.csv'))%>% 
  select(!starts_with('X')) %>% 
  filter(Year >=1980 & Year <= 2013) %>% 
  drop_na(Pregnant) %>% 
  drop_na(elapsed) %>% 
  filter(elapsed >= 1)

pregnant$Pregnant[pregnant$Pregnant == 'F'] <- '1'
pregnant$Pregnant <- as.numeric(pregnant$Pregnant)

# assemble a dataset like firedata
# contains the unique intervals and the number of unique intervals
pregdata <- pregnant %>% 
  group_by(elapsed) %>% 
  tally()
colnames(pregdata) <- c('years', 'npreg')
allyears <- seq(0, max(pregdata$years), by = 1)
npregyr <- allyears * 0
for(i in 1:length(pregdata$years)){
  npregyr[allyears[pregdata$years[i] == allyears]] <- pregdata$npreg[i]
}
plot(allyears, npregyr, type = 's')

wlik <- function(param){
  lp <- param[1]
  cp <- param[2]
  lik <- log(cp * lp^cp*pregnant$elapsed^(cp - 1)) - (lp * pregnant$elapsed)^cp
  return(-sum(lik))
}

param0 <- c(0.1, 1)
out <- optim(param0, wlik, lower = c(0.01, 0.01), upper = c(10, 10), method = "L-BFGS-B")

# plot weibull density
lw <- out$par[1]
cw <- out$par[2]
time <- seq(0, 40, length = 100)
hazard <- cw*lw^cw*time^(cw-1)
fweib <- hazard*exp(-(lw*time)^cw)
plot(allyears, npregyr / sum(npregyr), type = 's')
lines(time, fweib)
lines(time, hazard, lty = 2)


# LRT
lmle <- 1 / mean(pregnant$elapsed)
like <- -sum(log(lmle) - lmle*pregnant$elapsed)

likw <- out$value
dev <- 2 * (like - likw)
pr <- 1 - pchisq(dev, 1)
