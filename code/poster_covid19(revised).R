library(bsts)
library(CausalImpact)
setwd("C:/Users/pc/Desktop/raw_data(poster)/making_new_data/final_data(covid_poster)")
#Data
dat <- read.csv("poster_df.csv", head = T)
str(dat)
##dat$mask <- log(dat$mask) # If log?

#model

post.period <- c(36, 50)
post.period.response <- dat$mask[post.period[1] : post.period[2]] 
dat$mask[post.period[1] : post.period[2]] <- NA

ss1 <- AddLocalLinearTrend(list(), dat$mask)
ss1 #default prior
#ss1[[1]][[2]]$prior.guess<-10 #Prior setting
#ss1[[1]][[3]]$prior.guess<-10 #Prior setting



fit_bsts <- bsts(dat$mask ~ 
                   dat$cum_confirmed+dat$cum_death+dat$confirmed+
                   dat$world_cum_confirmed+dat$world_cum_death+dat$world_confirmed+
                   dat$social_distance+dat$hand_sanitizer+dat$corona_virus+dat$anxiety_index 
                 , state.specification = ss1, data = dat, niter = 5000) #model bsts fitting
impact <- CausalImpact(bsts.model = fit_bsts,
                       post.period.response = post.period.response, alpha = 0.05) 



#plot
plot(fit_bsts)
plot(fit_bsts,"coef")
plot(impact)
summary(fit_bsts)

#summary
summary(impact)
summary(impact, "report")
impact$summary
impact$series
