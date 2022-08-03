library(gtrendsR)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(showtext)

font_add_google("EB Garamond")
windows()
showtext_auto()

#Mapping function to scale hit values across calls
mymap <- function(n, start1, stop1, start2, stop2){
  newval <- ((n-start1)/(stop1-start1))*(stop2-start2)+start2
  return(newval)
}

#Terms list must be generally ordered from highest interest in time range to lowest
terms_elite = c("January 6",
          "Jan 6",
          "McCarthy",
          "Cheney",
          "Bannon",
          "Steve Bannon",
          "Mark Meadows",
          "Hannity",
          "Alex Jones",
          "Oath Keepers",
          "Fuentes",
          "Sean Hannity",
          "Patel",
          "Contempt",
          "Proud Boys",
          "January 6th",
          "Capitol Police",
          "Election fraud",
          "Enrique Tarrio",
          "Tanya Chtkan",
          "Misinformation",
          "Liz Cheney",
          "Hashida",
          "Fanone",
          "Bannon indictment",
          "Roger Stone",
          "Michael Fanone",
          "Jim Jordan",
          "Subpoena",
          "Ivanka Trump",
          "Jan 6 Hearing",
          "Scott Perry",
          "Trump Lawsuit",
          "Executive privilege",
          "Capitol Officers",
          "Nick Fuentes",
          "January 6 Rally",
          "Contempt of congress",
          "Jeffrey Clark",
          "Jan 6 Rally",
          "Senate judiciary committee",
          "John Wood",
          "Fail to Address",
          "Officer Hashida",
          "Committee hearing",
          "Jan 6 Investigation",
          "DeFreytag",
          "Ali Alexander",
          "Phil Waldron",
          "Meadows texts",
          "Metropolitan Police",
          "Metropolitan Police Department",
          "Bennie Thompson",
          "Trump Records",
          "Ed Martin",
          "Trump allies",
          "Brian Jack",
          "Bryan Lewis",
          "Kimberly Fletcher",
          "Denver Riggleman",
          "Joseph Maher",
          "Chairman Thompson",
          "Timothy Heaphy",
          "Patrick Rasey",
          "Executive Brand Records")

terms_misinfo <- c("January 6",
                   "Epps",
                   "QAnon",
                   "Election audit",
                   "coup",
                   "Ray Epps",
                   "Babbitt",
                   "Antifa",
                   "Cyber ninjas",
                   "ballots",
                   "Jacob Chansley",
                   "Charlie kirk",
                   "False flag",
                   "Election recount",
                   "Patriot Purge",
                   "Ashli Babbitt",
                   "Pence",
                   "Voter Fraud",
                   "Election Fraud",
                   "Babbit",
                   "Ashli Babbit",
                   "Trump Won",
                   "Georgia votes",
                   "Save America Rally",
                   "Attack trump",
                   "Fake Biden",
                   "Trump dead",
                   "False flag operation",
                   "Paid actor",
                   "Arizona ballot",
                   "Arizona ballots",
                   "Deep State",
                   "Nemos",
                   "Antifa attacks",
                   "Trump attacked",
                   "Pelosi Jan 6",
                   "Election stolen",
                   "Corrupt election",
                   "E-11",
                   "Pro V and V",
                   "Is trump dead",
                   "Crisis actor",
                   "American Greatness",
                   "Rosanne boyland",
                   "Antifa BLM",
                   "Daniel Richman",
                   "Video capitol police",
                   "Pelosi responsible",
                   "Arizona republic",
                   "Pelosi national guard",
                   "Arizona Ballots",
                   "Trump landslide",
                   "Stolen election",
                   "Antifa capitol",
                   "FBI photos",
                   "Antifa riot",
                   "Antifa at capitol",
                   "2020 election stolen",
                   "Fake text messages",
                   "Kevin greeson",
                   "Illegal Election",
                   "Dustin Nemos",
                   "Texas Election Audit")

times = c("2021-07-01 2022-01-23")

#pull interest data from gtrends across list of terms
batch_gtrends <- function(t, terms) {
  bat <- list()
  k = 0
  for (t in times){
    for (i in seq(1, length(terms), by = 4)){
      k = k + 1
      if(i < length(terms)-4){
        bat[[k]] <- data.frame(gtrends(
          keyword = c(terms[i:(i+4)]), geo = "US",
          time = t, gprop = "web", low_search_volume = TRUE)$interest_over_time)
      }
      else{
        bat[[k]] <- data.frame(gtrends(
          keyword = c(terms[i:length(terms)]), geo = "US",
          time = t, gprop = "web", low_search_volume = TRUE)$interest_over_time)
      }
    }
  }
  return(bat)
}

bat_elite <- batch_gtrends(times, terms_elite)
bat_misinfo <- batch_gtrends(times, terms_misinfo)

raw_bat_elite <- bat_elite
raw_bat_misinfo <- bat_misinfo

#gtrends data includes "<1" values instead of 0s, so need to transform these to a value lower than 1. 0.05 is chosen as an arbitrarily low value
mutate_lessthan1 <- function(bat) {
  new_bat <- list()
  for (i in seq(1, length(bat)-1)) {
    new_bat[[i]] <- bat[[i]]
    new_bat[[i]][2] <- rapply(new_bat[[i]][2], function(x) ifelse(x=="<1",0.05,x), how = 'replace')
  }
  return(new_bat)
}

bat_elite_clean <- mutate_lessthan1(bat_elite)
bat_misinfo_clean <- mutate_lessthan1(bat_misinfo)

#rescale_gtrends <- function(bat, terms) {
#  crossover_index <- seq(5, length(terms)-4, by = 4)
#  crossover_terms <- terms[c(crossover_index)]
#  for (i in seq(1, length(bat))) {
#    print(max(as.numeric(bat[[i]][2])))
#    
#  }
#  
#}

#rescale_gtrends(bat_elite_clean, terms_elite)

summary(as.factor(bat_elite_clean[[1]]$keyword))
summary(bat_elite_clean[[1]]$hits[bat_elite_clean[[1]]$keyword == "Bannon"])
bat_elite_clean[[1]]$hits <- as.numeric(bat_elite_clean[[1]]$hits)
summary(bat_elite_clean[[1]]$hits[bat_elite_clean[[1]]$keyword == "Bannon"])

summary(as.factor(bat_elite_clean[[2]]$keyword))
summary(bat_elite_clean[[2]]$hits[bat_elite_clean[[2]]$keyword == "Bannon"])
bat_elite_clean[[2]]$hits <- as.numeric(bat_elite_clean[[2]]$hits)
summary(bat_elite_clean[[2]]$hits[bat_elite_clean[[2]]$keyword == "Bannon"])
bat_elite_clean[[2]]$hits <- mymap(bat_elite_clean[[2]]$hits, 0, 100, 0, 67)
summary(bat_elite_clean[[2]]$hits[bat_elite_clean[[2]]$keyword == "Bannon"])

summary(as.factor(bat_elite_clean[[3]]$keyword))
summary(bat_elite_clean[[2]]$hits[bat_elite_clean[[2]]$keyword == "Alex Jones"])
summary(bat_elite_clean[[3]]$hits[bat_elite_clean[[3]]$keyword == "Alex Jones"])
bat_elite_clean[[3]]$hits <- mymap(bat_elite_clean[[3]]$hits, 0, 100, 0, 43.550)

summary(as.factor(bat_elite_clean[[4]]$keyword))
summary(bat_elite_clean[[3]]$hits[bat_elite_clean[[3]]$keyword == "Patel"])
summary(bat_elite_clean[[4]]$hits[bat_elite_clean[[4]]$keyword == "Patel"])
bat_elite_clean[[4]]$hits <- mymap(bat_elite_clean[[4]]$hits, 0, 71, 0, 34.404)

summary(as.factor(bat_elite_clean[[5]]$keyword))
summary(bat_elite_clean[[4]]$hits[bat_elite_clean[[4]]$keyword == "Capitol Police"])
summary(bat_elite_clean[[5]]$hits[bat_elite_clean[[5]]$keyword == "Capitol Police"])
bat_elite_clean[[5]]$hits <- mymap(bat_elite_clean[[5]]$hits, 0, 100, 0, 22.2899)

summary(as.factor(bat_elite_clean[[6]]$keyword))
summary(bat_elite_clean[[5]]$hits[bat_elite_clean[[5]]$keyword == "Misinformation"])
summary(bat_elite_clean[[6]]$hits[bat_elite_clean[[6]]$keyword == "Misinformation"])
bat_elite_clean[[6]]$hits <- mymap(bat_elite_clean[[6]]$hits, 0, 26, 0, 7.5786)

summary(as.factor(bat_elite_clean[[7]]$keyword))
summary(bat_elite_clean[[6]]$hits[bat_elite_clean[[6]]$keyword == "Bannon indictment"])
summary(bat_elite_clean[[7]]$hits[bat_elite_clean[[7]]$keyword == "Bannon indictment"])
bat_elite_clean[[7]]$hits <- mymap(bat_elite_clean[[7]]$hits, 0, 20, 0, 3.78930)

summary(as.factor(bat_elite_clean[[8]]$keyword))
summary(bat_elite_clean[[7]]$hits[bat_elite_clean[[7]]$keyword == "Subpoena"])
summary(bat_elite_clean[[8]]$hits[bat_elite_clean[[8]]$keyword == "Subpoena"])
bat_elite_clean[[8]]$hits <- mymap(bat_elite_clean[[8]]$hits, 0, 80, 0, 13.263)

summary(as.factor(bat_elite_clean[[9]]$keyword))
summary(bat_elite_clean[[8]]$hits[bat_elite_clean[[8]]$keyword == "Trump Lawsuit"])
summary(bat_elite_clean[[9]]$hits[bat_elite_clean[[9]]$keyword == "Trump Lawsuit"])
bat_elite_clean[[9]]$hits <- mymap(bat_elite_clean[[9]]$hits, 0, 100, 0, 9.6157)

summary(as.factor(bat_elite_clean[[10]]$keyword))
summary(bat_elite_clean[[9]]$hits[bat_elite_clean[[9]]$keyword == "January 6 Rally"])
summary(bat_elite_clean[[10]]$hits[bat_elite_clean[[10]]$keyword == "January 6 Rally"])
bat_elite_clean[[10]]$hits <- mymap(bat_elite_clean[[10]]$hits, 0, 32, 0, 2.2116)

summary(as.factor(bat_elite_clean[[11]]$keyword))
summary(bat_elite_clean[[10]]$hits[bat_elite_clean[[10]]$keyword == "Senate judiciary committee"])
summary(bat_elite_clean[[11]]$hits[bat_elite_clean[[11]]$keyword == "Senate judiciary committee"])
bat_elite_clean[[11]]$hits <- mymap(bat_elite_clean[[11]]$hits, 0, 55, 0, 3.5247)

summary(as.factor(bat_elite_clean[[12]]$keyword))
summary(bat_elite_clean[[11]]$hits[bat_elite_clean[[11]]$keyword == "Committee hearing"])
summary(bat_elite_clean[[12]]$hits[bat_elite_clean[[12]]$keyword == "Committee hearing"])
bat_elite_clean[[12]]$hits <- mymap(bat_elite_clean[[12]]$hits, 0, 84, 0, 5.3832)

summary(as.factor(bat_elite_clean[[13]]$keyword))
summary(bat_elite_clean[[12]]$hits[bat_elite_clean[[12]]$keyword == "Phil Waldron"])
summary(bat_elite_clean[[13]]$hits[bat_elite_clean[[13]]$keyword == "Phil Waldron"])
bat_elite_clean[[13]]$hits <- mymap(bat_elite_clean[[13]]$hits, 0, 67, 0, 2.56343)

summary(as.factor(bat_elite_clean[[14]]$keyword))
summary(bat_elite_clean[[13]]$hits[bat_elite_clean[[13]]$keyword == "Bennie Thompson"])
summary(bat_elite_clean[[14]]$hits[bat_elite_clean[[14]]$keyword == "Bennie Thompson"])
bat_elite_clean[[14]]$hits <- mymap(bat_elite_clean[[14]]$hits, 0, 100, 0, 3.8260)

summary(as.factor(bat_elite_clean[[15]]$keyword))
summary(bat_elite_clean[[14]]$hits[bat_elite_clean[[14]]$keyword == "Brian Jack"])
summary(bat_elite_clean[[15]]$hits[bat_elite_clean[[15]]$keyword == "Brian Jack"])
bat_elite_clean[[15]]$hits <- mymap(bat_elite_clean[[15]]$hits, 0, 100, 0, 1.76)

summary(as.factor(bat_elite_clean[[16]]$keyword))
summary(bat_elite_clean[[15]]$hits[bat_elite_clean[[15]]$keyword == "Joseph Maher"])
summary(bat_elite_clean[[16]]$hits[bat_elite_clean[[16]]$keyword == "Joseph Maher"])
bat_elite_clean[[16]]$hits <- mymap(bat_elite_clean[[16]]$hits, 0, 58, 0, 0.42240)

final_elite <- rbind(filter(bat_elite_clean[[1]], keyword != "Bannon"),
               filter(bat_elite_clean[[2]], keyword != "Alex Jones"),
               filter(bat_elite_clean[[3]], keyword != "Patel"),
               filter(bat_elite_clean[[4]], keyword != "Capitol Police"),
               filter(bat_elite_clean[[5]], keyword != "Misinformation"),
               filter(bat_elite_clean[[6]], keyword != "Bannon indictment"),
               filter(bat_elite_clean[[7]], keyword != "Subpoena"),
               filter(bat_elite_clean[[8]], keyword != "Trump Lawsuit"),
               filter(bat_elite_clean[[9]], keyword != "January 6 Rally"),
               filter(bat_elite_clean[[10]], keyword != "Senate judiciary committee"),
               filter(bat_elite_clean[[11]], keyword != "Committee hearing"),
               filter(bat_elite_clean[[12]], keyword != "Phil Waldron"),
               filter(bat_elite_clean[[13]], keyword != "Bennie Thompson"),
               filter(bat_elite_clean[[14]], keyword != "Brian Jack"),
               filter(bat_elite_clean[[15]], keyword != "Joseph Maher"),
               bat_elite_clean[[16]])

elite_summed <- aggregate(final_elite$hits, by=list(Category=final_elite$date), FUN=sum)
elite_summed$Category[elite_summed$x > 200]
ggplot(elite_summed, aes(x = Category, y = x)) + geom_line() + theme_bw(base_size = 20, base_family = "EB Garamond") + theme(legend.position="none") +
  ggtitle("Search frequency for political elite and media agenda terms by day: July 2021 - January 2022") +
  labs(x = "Date", y = "Aggregated Daily Google Search Term Frequency") +
  annotate("text", label = "Jul 27 - 28:\nOfficer Fanone testifies", x = as.POSIXct("2021-07-27 00:00:00"), y = 300, colour = "black", size = 6, family = "EB Garamond") +
  annotate("text", label = "Oct 20 - 21:\nCmte. opening stmts\nBannon cited", x = as.POSIXct("2021-10-20 00:00:00"), y = 245, colour = "black", size = 6, family = "EB Garamond") +
  annotate("text", label = "Nov 15 - 16:\nMeadows failure to comply\nBannon indictment", x = as.POSIXct("2021-11-13 00:00:00"), y = 290, colour = "black", size = 6, family = "EB Garamond") +
  annotate("text", label = "Dec 14 - 15:\nMeadows cited", x = as.POSIXct("2021-12-14 00:00:00"), y = 280, colour = "black", size = 6, family = "EB Garamond") +
  annotate("text", label = "Jan 5 - 7:\nAnniversery events", x = as.POSIXct("2022-01-05 00:00:00"), y = 480, colour = "black", size = 6, family = "EB Garamond")

################################## Misinformation Terms
summary(as.factor(bat_misinfo_clean[[1]]$keyword))
summary(bat_misinfo_clean[[1]]$hits[bat_misinfo_clean[[1]]$keyword == "coup"])
bat_misinfo_clean[[1]]$hits <- as.numeric(bat_misinfo_clean[[1]]$hits)
summary(bat_misinfo_clean[[1]]$hits[bat_misinfo_clean[[1]]$keyword == "coup"])

summary(as.factor(bat_misinfo_clean[[2]]$keyword))
summary(bat_misinfo_clean[[2]]$hits[bat_misinfo_clean[[2]]$keyword == "coup"])
summary(bat_misinfo_clean[[2]]$hits[bat_misinfo_clean[[2]]$keyword == "coup"])
bat_misinfo_clean[[2]]$hits <- mymap(bat_misinfo_clean[[2]]$hits, 0, 86, 0, 23)
summary(bat_misinfo_clean[[2]]$hits[bat_misinfo_clean[[2]]$keyword == "coup"])

summary(as.factor(bat_misinfo_clean[[3]]$keyword))
summary(bat_misinfo_clean[[2]]$hits[bat_misinfo_clean[[2]]$keyword == "Cyber ninjas"])
summary(bat_misinfo_clean[[3]]$hits[bat_misinfo_clean[[3]]$keyword == "Cyber ninjas"])
bat_misinfo_clean[[3]]$hits <- mymap(bat_misinfo_clean[[3]]$hits, 0, 74, 0, 13.9070)

summary(as.factor(bat_misinfo_clean[[4]]$keyword))
summary(bat_misinfo_clean[[3]]$hits[bat_misinfo_clean[[3]]$keyword == "False flag"])
summary(bat_misinfo_clean[[4]]$hits[bat_misinfo_clean[[4]]$keyword == "False flag"])
bat_misinfo_clean[[4]]$hits <- mymap(bat_misinfo_clean[[4]]$hits, 0, 49, 0, 7.7052)

summary(as.factor(bat_misinfo_clean[[5]]$keyword))
summary(bat_misinfo_clean[[4]]$hits[bat_misinfo_clean[[4]]$keyword == "Pence"])
summary(bat_misinfo_clean[[5]]$hits[bat_misinfo_clean[[5]]$keyword == "Pence"])
bat_misinfo_clean[[5]]$hits <- mymap(bat_misinfo_clean[[5]]$hits, 0, 100, 0, 14.310)

summary(as.factor(bat_misinfo_clean[[6]]$keyword))
summary(bat_misinfo_clean[[5]]$hits[bat_misinfo_clean[[5]]$keyword == "Ashli Babbit"])
summary(bat_misinfo_clean[[6]]$hits[bat_misinfo_clean[[6]]$keyword == "Ashli Babbit"])
bat_misinfo_clean[[6]]$hits <- mymap(bat_misinfo_clean[[6]]$hits, 0, 78, 0, 3.4344)

summary(as.factor(bat_misinfo_clean[[7]]$keyword))
summary(bat_misinfo_clean[[6]]$hits[bat_misinfo_clean[[6]]$keyword == "Attack trump"])
summary(bat_misinfo_clean[[7]]$hits[bat_misinfo_clean[[7]]$keyword == "Attack trump"])
bat_misinfo_clean[[7]]$hits <- mymap(bat_misinfo_clean[[7]]$hits, 0, 81, 0, 3.6986)

summary(as.factor(bat_misinfo_clean[[8]]$keyword))
summary(bat_misinfo_clean[[7]]$hits[bat_misinfo_clean[[7]]$keyword == "Paid actor"])
summary(bat_misinfo_clean[[8]]$hits[bat_misinfo_clean[[8]]$keyword == "Paid actor"])
bat_misinfo_clean[[8]]$hits <- mymap(bat_misinfo_clean[[8]]$hits, 0, 100, 0, 4.3379)

summary(as.factor(bat_misinfo_clean[[9]]$keyword))
summary(bat_misinfo_clean[[8]]$hits[bat_misinfo_clean[[8]]$keyword == "Nemos"])
summary(bat_misinfo_clean[[9]]$hits[bat_misinfo_clean[[9]]$keyword == "Nemos"])
bat_misinfo_clean[[9]]$hits <- mymap(bat_misinfo_clean[[9]]$hits, 0, 100, 0, 3.5571)

summary(as.factor(bat_misinfo_clean[[10]]$keyword))
summary(bat_misinfo_clean[[9]]$hits[bat_misinfo_clean[[9]]$keyword == "Election stolen"])
summary(bat_misinfo_clean[[10]]$hits[bat_misinfo_clean[[10]]$keyword == "Election stolen"])
bat_misinfo_clean[[10]]$hits <- mymap(bat_misinfo_clean[[10]]$hits, 0, 100, 0, 2.7745)

summary(as.factor(bat_misinfo_clean[[11]]$keyword))
summary(bat_misinfo_clean[[10]]$hits[bat_misinfo_clean[[10]]$keyword == "Is trump dead"])
summary(bat_misinfo_clean[[11]]$hits[bat_misinfo_clean[[11]]$keyword == "Is trump dead"])
bat_misinfo_clean[[11]]$hits <- mymap(bat_misinfo_clean[[11]]$hits, 0, 69, 0, 1.2485)

summary(as.factor(bat_misinfo_clean[[12]]$keyword))
summary(bat_misinfo_clean[[11]]$hits[bat_misinfo_clean[[11]]$keyword == "Antifa BLM"])
summary(bat_misinfo_clean[[12]]$hits[bat_misinfo_clean[[12]]$keyword == "Antifa BLM"])
bat_misinfo_clean[[12]]$hits <- mymap(bat_misinfo_clean[[12]]$hits, 0, 58, 0, 1.1761)

summary(as.factor(bat_misinfo_clean[[13]]$keyword))
summary(bat_misinfo_clean[[12]]$hits[bat_misinfo_clean[[12]]$keyword == "Arizona republic"])
summary(bat_misinfo_clean[[13]]$hits[bat_misinfo_clean[[13]]$keyword == "Arizona republic"])
bat_misinfo_clean[[13]]$hits <- mymap(bat_misinfo_clean[[13]]$hits, 0, 73, 0, 2.0278)

summary(as.factor(bat_misinfo_clean[[14]]$keyword))
summary(bat_misinfo_clean[[13]]$hits[bat_misinfo_clean[[13]]$keyword == "Stolen election"])
summary(bat_misinfo_clean[[14]]$hits[bat_misinfo_clean[[14]]$keyword == "Stolen election"])
bat_misinfo_clean[[14]]$hits <- mymap(bat_misinfo_clean[[14]]$hits, 0, 100, 0, 2.7778)

summary(as.factor(bat_misinfo_clean[[15]]$keyword))
summary(bat_misinfo_clean[[14]]$hits[bat_misinfo_clean[[14]]$keyword == "Antifa at capitol"])
summary(bat_misinfo_clean[[15]]$hits[bat_misinfo_clean[[15]]$keyword == "Antifa at capitol"])
bat_misinfo_clean[[15]]$hits <- mymap(bat_misinfo_clean[[15]]$hits, 0, 38, 0, 0.44445)

final_misinfo <- rbind(filter(bat_misinfo_clean[[1]], keyword != "coup"),
                     filter(bat_misinfo_clean[[2]], keyword != "Cyber ninjas"),
                     filter(bat_misinfo_clean[[3]], keyword != "False flag"),
                     filter(bat_misinfo_clean[[4]], keyword != "Pence"),
                     filter(bat_misinfo_clean[[5]], keyword != "Ashli Babbit"),
                     filter(bat_misinfo_clean[[6]], keyword != "Attack trump"),
                     filter(bat_misinfo_clean[[7]], keyword != "Paid actor"),
                     filter(bat_misinfo_clean[[8]], keyword != "Nemos"),
                     filter(bat_misinfo_clean[[9]], keyword != "Election stolen"),
                     filter(bat_misinfo_clean[[10]], keyword != "Is trump dead"),
                     filter(bat_misinfo_clean[[11]], keyword != "Antifa BLM"),
                     filter(bat_misinfo_clean[[12]], keyword != "Arizona republic"),
                     filter(bat_misinfo_clean[[13]], keyword != "Stolen election"),
                     filter(bat_misinfo_clean[[14]], keyword != "Antifa at capitol"),
                     filter(bat_misinfo_clean[[15]]))

final_misinfo <- rbind(filter(final_misinfo, keyword != "January 6"))
misinfo_summed$Category[misinfo_summed$x > 75]                       
misinfo_summed <- aggregate(final_misinfo$hits, by=list(Category=final_misinfo$date), FUN=sum)
ggplot(misinfo_summed, aes(x = Category, y = x)) + geom_line() + theme_bw(base_size = 20, base_family = "EB Garamond") + theme(legend.position="none") +
  ggtitle("Search frequency for partisan and alternative media agenda terms by day: July 2021 - January 2022") +
  labs(x = "Date", y = "Aggregated Daily Google Search Term Frequency") + 
  annotate("text", label = "Jul 15 - 16:\nTrump claims\nvoter fraud", x = as.POSIXct("2021-07-15 00:00:00"), y = 105, colour = "black", size = 6, family = "EB Garamond") +
  annotate("text", label = "Jul 29:\nTrump claims\nelection fraud", x = as.POSIXct("2021-07-29 00:00:00"), y = 90, colour = "black", size = 6, family = "EB Garamond") +
  annotate("text", label = "Aug 12:\nTrump claims\nrigged election", x = as.POSIXct("2021-08-12 00:00:00"), y = 105, colour = "black", size = 6, family = "EB Garamond") +
  annotate("text", label = "Sept 24:\nMaricopa County, AZ\nelection audit", x = as.POSIXct("2021-09-24 00:00:00"), y = 110, colour = "black", size = 6, family = "EB Garamond") +
  annotate("text", label = "Nov 2 - 3:\n2021 Election day", x = as.POSIXct("2021-11-02 00:00:00"), y = 100, colour = "black", size = 6, family = "EB Garamond") +
  annotate("text", label = "Jan 5 - 7:\nAnniversary events", x = as.POSIXct("2022-01-05 00:00:00"), y = 141, colour = "black", size = 6, family = "EB Garamond")

