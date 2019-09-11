### GIVEN
# install.packages("pinnacle.data")
# install.packages("odds.converter")

library(pinnacle.data)
library(odds.converter)
library(dplyr)

data("MLB2016")
?MLB2016

# Clean pitching columns
MLB2016 <- MLB2016 %>% 
  mutate(AwayStartingPitcher = tolower(ifelse(grepl(" ", AwayStartingPitcher), 
                                              AwayStartingPitcher,
                                              paste(substring(AwayStartingPitcher, 1, 1),
                                                    substring(AwayStartingPitcher, 2)))),
         HomeStartingPitcher = tolower(ifelse(grepl(" ", HomeStartingPicher), 
                                              HomeStartingPicher, 
                                              paste(substring(HomeStartingPicher, 1, 1), 
                                                    substring(HomeStartingPicher, 2)))), 
         AwayStartingPitcher = gsub("\\.", "", AwayStartingPitcher), 
         HomeStartingPitcher = gsub("\\.", "", HomeStartingPitcher)) %>%
  select(-HomeStartingPicher)
### /GIVEN

library(ggplot2)
library(lubridate)

## Question 1: Which team(s) played in the most double headers?

doubleHeaders <- MLB2016[c("AwayTeam", "HomeTeam", "DoubleHeaderGame")][MLB2016$DoubleHeaderGame>1,]
doubleNames <- c(doubleHeaders$HomeTeam, doubleHeaders$AwayTeam)
doubleFreq <- table(doubleNames)
mostDoubles <- names(doubleFreq[doubleFreq > 1])

print(tibble(`Most Double Headers`=mostDoubles))

## Question 2: What was the final win percentage of the Chicago Cubs at the end of 2016?
## Hint: Make sure to account for any potential ties caused by bad weather!

cubs <- subset(MLB2016, MLB2016$HomeTeam=="Chicago Cubs" | MLB2016$AwayTeam=="Chicago Cubs")
cubsHome <- subset(cubs, cubs$HomeTeam=="Chicago Cubs")
cubsAway <- subset(cubs, cubs$AwayTeam=="Chicago Cubs")

ties <- sum(cubs$FinalScoreHome == cubs$FinalScoreAway)
homeWins <- sum(cubsHome$FinalScoreHome > cubsHome$FinalScoreAway)
awayWins <- sum(cubsAway$FinalScoreHome < cubsAway$FinalScoreAway)
winPct <- (homeWins + awayWins) / (nrow(cubs) - ties)

sprintf("Chicago Cubs 2016 Win Pct: %.3f", round(winPct,3))

## Question 2a: Plot the winning percentage of the Chicago Cubs as a function of time in 2016.

cubsHome['win'] <- cubsHome$FinalScoreHome > cubsHome$FinalScoreAway
cubsAway['win'] <- cubsAway$FinalScoreHome < cubsAway$FinalScoreAway

cubsWins <- bind_rows(cubsHome, cubsAway)
cubsWins <- arrange(cubsWins, EventDateTimeET)
cubsWins <- subset(cubsWins, cubsWins$FinalScoreHome != cubsWins$FinalScoreAway)  # remove ties
cubsWins['totalWins'] <- cumsum(cubsWins$win)
cubsWins['totalLosses'] <- cumsum(!cubsWins$win)
cubsWins['totalGames'] <- 1:nrow(cubsWins)
cubsWins['winPct'] <- (cubsWins$totalWins)/(cubsWins$totalGames)
p <-
  ggplot(cubsWins, aes(EventDateTimeET, winPct)) + 
  geom_line() + 
  geom_hline(yintercept = winPct, linetype='dashed') + 
  annotate("text", x=ymd_hms('2016-05-01 00:00:00'), y=0.65, label=paste("Final Win %", round(winPct,2))) +
  xlab('2016') +
  ylab('Win Percentage') +
  ggtitle('Chicago Cubs 2016 Win Percentage') +
  scale_x_datetime(date_breaks='month', date_labels = "%B") +
  theme_bw()
print(p)

## Question: Which game had the most odds changes?

