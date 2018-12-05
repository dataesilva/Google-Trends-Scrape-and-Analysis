#Required library
library(gtrendsR)
library(plyr)
library(dplyr)
library(readr)

#Read in data file for over time values
gtc <- read_csv("H:/Dropbox/WSU/Research/Google Trends/Google Trend Data_CLEAN.csv")
daily <- select(gtc,-c(Day, sum))

#Create the timestamps to use in gtrends function that does not accept single-day searches, so has to be two-day ranges
startdate <- seq(as.Date("2016/2/1"), as.Date("2016/11/7"), "days")
enddate <- seq(as.Date("2016/2/2"), as.Date("2016/11/8"), "days")
#Matching the form used in gtrends function, this gives a vector of two-day ranges that can be called sequentially
times <- paste(startdate, enddate, sep=" ")

#Starting keywork values to call in the gtrends function
keys <- c("clinton", "trump", "kaine", "pence", "vote", "presidential", "campaign", "election")

#Initial null variables for the loop
dat.new <- c()
dat <- vector("list", 2)
names(dat) <- c("values", "regions")

#Begin of the loop
#Scrapes regions by two-day ranges, it's fairly slow as it is collecting more data than we are using and follows Google limits
#Clinton
for (j in 1:length(times)){
  dat.new <- gtrends(keyword = c(keys[1]), geo = "US", time = times[j], gprop = "web", low_search_volume = TRUE)
  dat$values[[j]] <- unlist(dat.new$interest_by_region$hits[dat.new$interest_by_region$keyword==keys[1]]*.01*daily[[1]][j])
  dat$regions[[j]] <- unlist(dat.new$interest_by_region$location[dat.new$interest_by_region$keyword==keys[1]])
  }
dat.clinton <- cbind("clinton", ldply(dat$regions, matrix), ldply(dat$values, matrix), rep(times, each=51))
#Trump
for (j in 1:length(times)){
  dat.new <- gtrends(keyword = "trump", geo = "US", time = times[j], gprop = "web", low_search_volume = TRUE)
  dat$values[[j]] <- unlist(dat.new$interest_by_region$hits[dat.new$interest_by_region$keyword=="trump"]*.01*daily[[2]][j])
  dat$regions[[j]] <- unlist(dat.new$interest_by_region$location[dat.new$interest_by_region$keyword=="trump"])
}
dat.trump <- cbind("trump", ldply(dat$regions, matrix), ldply(dat$values, matrix), rep(times, each=51))
#Kaine
for (j in 1:length(times)){
  dat.new <- gtrends(keyword = "kaine", geo = "US", time = times[j], gprop = "web", low_search_volume = TRUE)
  dat$values[[j]] <- unlist(dat.new$interest_by_region$hits[dat.new$interest_by_region$keyword=="kaine"]*.01*daily[[3]][j])
  dat$regions[[j]] <- unlist(dat.new$interest_by_region$location[dat.new$interest_by_region$keyword=="kaine"])
}
dat.kaine <- cbind("kaine", ldply(dat$regions, matrix), ldply(dat$values, matrix), rep(times, each=51))
#Pence
for (j in 1:length(times)){
  dat.new <- gtrends(keyword = "pence", geo = "US", time = times[j], gprop = "web", low_search_volume = TRUE)
  dat$values[[j]] <- unlist(dat.new$interest_by_region$hits[dat.new$interest_by_region$keyword=="pence"]*.01*daily[[4]][j])
  dat$regions[[j]] <- unlist(dat.new$interest_by_region$location[dat.new$interest_by_region$keyword=="pence"])
}
dat.pence <- cbind("pence", ldply(dat$regions, matrix), ldply(dat$values, matrix), rep(times, each=51))
#Vote
for (j in 1:length(times)){
  dat.new <- gtrends(keyword = "vote", geo = "US", time = times[j], gprop = "web", low_search_volume = TRUE)
  dat$values[[j]] <- unlist(dat.new$interest_by_region$hits[dat.new$interest_by_region$keyword=="vote"]*.01*daily[[5]][j])
  dat$regions[[j]] <- unlist(dat.new$interest_by_region$location[dat.new$interest_by_region$keyword=="vote"])
}
dat.vote <- cbind("vote", ldply(dat$regions, matrix), ldply(dat$values, matrix), rep(times, each=51))
#Presidential
for (j in 1:length(times)){
  dat.new <- gtrends(keyword = "presidential", geo = "US", time = times[j], gprop = "web", low_search_volume = TRUE)
  dat$values[[j]] <- unlist(dat.new$interest_by_region$hits[dat.new$interest_by_region$keyword=="presidential"]*.01*daily[[6]][j])
  dat$regions[[j]] <- unlist(dat.new$interest_by_region$location[dat.new$interest_by_region$keyword=="presidential"])
}
dat.presidential <- cbind("presidential", ldply(dat$regions, matrix), ldply(dat$values, matrix), rep(times, each=51))
#Campaign
for (j in 1:length(times)){
  dat.new <- gtrends(keyword = "campaign", geo = "US", time = times[j], gprop = "web", low_search_volume = TRUE)
  dat$values[[j]] <- unlist(dat.new$interest_by_region$hits[dat.new$interest_by_region$keyword=="campaign"]*.01*daily[[7]][j])
  dat$regions[[j]] <- unlist(dat.new$interest_by_region$location[dat.new$interest_by_region$keyword=="campaign"])
}
dat.campaign <- cbind("campaign", ldply(dat$regions, matrix), ldply(dat$values, matrix), rep(times, each=51))
#Election
for (j in 1:length(times)){
  dat.new <- gtrends(keyword = "election", geo = "US", time = times[j], gprop = "web", low_search_volume = TRUE)
  dat$values[[j]] <- unlist(dat.new$interest_by_region$hits[dat.new$interest_by_region$keyword=="election"]*.01*daily[[8]][j])
  dat$regions[[j]] <- unlist(dat.new$interest_by_region$location[dat.new$interest_by_region$keyword=="election"])
}
dat.election <- cbind("election", ldply(dat$regions, matrix), ldply(dat$values, matrix), rep(times, each=51))

#With the data, combine fies
names(c(dat.election, dat.campaign, dat.presidential, dat.vote, dat.pence, dat.kaine, dat.clinton,
        dat.trump)) <- c("keyword", "region", "dates", "value")
dat.tot <- rbind(dat.election, dat.campaign, dat.presidential, dat.vote, dat.pence, dat.kaine, dat.clinton, dat.trump)
#Few steps ommitted here but will copy in later
fin.tot <- aggregate(fin$value, by=list(fin$region, fin$days), FUN = sum)
names(fin.tot) <- c("region", "days", "value")
View(fin.tot)

gtc <- fin.tot
gtc$sum <- gtc$value
#make spline variables

gtc$DTNOM <- ifelse(gtc$days > 95, gtc$days-95,0)
gtc$HRCNOM <- ifelse(gtc$days > 127, gtc$days-127,0)
gtc$startRNC <- ifelse(gtc$days > 168,gtc$days-168,0)
gtc$endDNC <- ifelse(gtc$days > 179,gtc$days-179,0)
gtc$DEB1 <- ifelse(gtc$days > 238,gtc$days-238,0)
gtc$DEB2 <- ifelse(gtc$days > 250,gtc$days-250,0)
gtc$DEB3 <- ifelse(gtc$days > 261,gtc$days-261,0)

attach(gtc)
#model all trend outcome variables
M <- lm(sum ~ days + DTNOM + HRCNOM + startRNC + endDNC + DEB1 + DEB2 + DEB3, data = gtc)
Mlog <- lm(log(sum) ~ days + DTNOM + HRCNOM + startRNC + endDNC + DEB1 + DEB2 + DEB3, data = gtc)
Mglm <- glm(sum ~ days + DTNOM + HRCNOM + startRNC + endDNC + DEB1 + DEB2 + DEB3, data = gtc, family = poisson)
level.log <- lmer(log(sum) ~ days + DTNOM + HRCNOM + startRNC + endDNC + DEB1 + DEB2 + DEB3 + (1|region), data = gtc)
#Show results of the models
summary(M)
summary(Mlog)
summary(Mglm)
summary(level.log)
display(level.log)
AIC(M, Mlog)
#get model predicted values, columns correspond to outcome variable list order
predicted <- predict(M)
predictedlog <- predict(Mlog)
predictedglm <- predict.glm(Mglm)
#plot of raw values
plot(days,sum)
#plot of predicted values
gtc2 <- gtc
gtc <- na.omit(gtc)
attach(gtc)
library(ggplot)
plot(days, predictedlog)
plot.sum <- data.frame(x=gtc$days, y=log(sum))
plot.sum$fit <- predict(Mlog)
g1.sum <- ggplot(plot.sum, aes(x = x, y = y, group=region)) + geom_line(aes(y=fit, linetype=region), size=0.8) + 
  geom_hline(yintercept = 0, linetype="dashed") + theme_bw()
g2.sum <- g1.sum + geom_text(aes(x=95, label = "Trump Clinches Nomination", y = 1.5, angle = 90))+
  geom_text(aes(x=127, label = "Clinton Clinches Nomination", y = 1.5, angle = 90))+
  geom_text(aes(x=168, label =  "Start RNC\n ", y = 1.5, angle = 90)) + 
  geom_text(aes(x=179, label = "End DNC\n ", y = 1.5, angle = 90))+
  geom_text(aes(x=238, label = "Debate 1\n ", y = 1.5, angle = 90)) + 
  geom_text(aes(x=250, label = "Debate 2\n ", y = 1.5, angle = 90)) + 
  geom_text(aes(x=261, label = "Debate 3\n ", y = 1.5, angle = 90))
g3.sum <- g2.sum + labs(y = "Log-transformed Google Search Count", x = "Date")
g4.sum <- g3.sum + scale_x_continuous(breaks = c(0,95,127,168,250,282),labels=c("02/01/16", "05/26/16", "06/06/16", "7/21/16","10/09/16","11/08/16"))
print(g4.sum)


#El FIn