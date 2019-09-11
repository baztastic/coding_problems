### R Assignment for Crescendo Technology
### Candidate: Barry Murphy
### Date: 2019-09-11

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

#### Question 1: Which team(s) played in the most double headers?

doubleHeaders <- MLB2016 %>% 
  select(AwayTeam, HomeTeam, DoubleHeaderGame) %>% 
  filter(DoubleHeaderGame > 1)

## Alternative with base R:
# doubleHeaders <- MLB2016[c("AwayTeam", "HomeTeam", "DoubleHeaderGame")][MLB2016$DoubleHeaderGame>1,]

doubleNames <- c(doubleHeaders$HomeTeam, doubleHeaders$AwayTeam)
mostDoubles <- tibble(doubleNames) %>% 
  count(doubleNames) %>% 
  arrange(desc(n)) %>% 
  filter(n > 1)

## Alternative with base R:
# doubleFreq <- table(doubleNames)
# mostDoubles <- names(doubleFreq[doubleFreq > 1])
# cat("Most Double Headers:\n\n", paste(mostDoubles, collapse="\n"))

print(tibble(`Most Double Headers`=mostDoubles$doubleNames))

# # A tibble: 7 x 1
# `Most Double Headers`
# <chr>                
# 1 Baltimore Orioles    
# 2 Chicago Cubs         
# 3 Minnesota Twins      
# 4 New York Mets        
# 5 Pittsburgh Pirates   
# 6 San Diego Padres     
# 7 St. Louis Cardinals  


#### Question 2: What was the final win percentage of the Chicago Cubs at the end of 2016?
## Hint: Make sure to account for any potential ties caused by bad weather!

cubs <- subset(MLB2016, MLB2016$HomeTeam=="Chicago Cubs" | MLB2016$AwayTeam=="Chicago Cubs")
cubsHome <- subset(cubs, cubs$HomeTeam=="Chicago Cubs")
cubsAway <- subset(cubs, cubs$AwayTeam=="Chicago Cubs")

ties <- sum(cubs$FinalScoreHome == cubs$FinalScoreAway)
homeWins <- sum(cubsHome$FinalScoreHome > cubsHome$FinalScoreAway)
awayWins <- sum(cubsAway$FinalScoreHome < cubsAway$FinalScoreAway)
winPct <- (homeWins + awayWins) / (nrow(cubs) - ties)

sprintf("Chicago Cubs 2016 Win Pct: %.3f", round(winPct,3))

# [1] "Chicago Cubs 2016 Win Pct: 0.640"

#### Question 2a: Plot the winning percentage of the Chicago Cubs as a function of time in 2016.

cubsHome['win'] <- cubsHome$FinalScoreHome > cubsHome$FinalScoreAway
cubsAway['win'] <- cubsAway$FinalScoreHome < cubsAway$FinalScoreAway

cubsWins <- bind_rows(cubsHome, cubsAway)
cubsWins <- arrange(cubsWins, EventDateTimeET)
cubsWins <- subset(cubsWins, cubsWins$FinalScoreHome != cubsWins$FinalScoreAway)  # remove ties

cubsWins['totalWins'] <- cumsum(cubsWins$win)
# cubsWins['totalLosses'] <- cumsum(!cubsWins$win)
cubsWins['totalGames'] <- 1:nrow(cubsWins)
cubsWins['winPct'] <- (cubsWins$totalWins)/(cubsWins$totalGames)

p1 <-
  ggplot(cubsWins, aes(EventDateTimeET, winPct)) + 
  geom_line() + 
  geom_hline(yintercept = winPct, linetype='dashed') + 
  annotate("text", x=ymd_hms('2016-05-01 00:00:00'), y=0.65, label=paste("Final Win %", round(winPct,2))) +
  xlab('2016') +
  ylab('Win Percentage') +
  ggtitle('Chicago Cubs 2016 Win Percentage') +
  scale_x_datetime(date_breaks='month', date_labels = "%B") +
  theme_bw()
print(p1)

###### Question 3: Which game had the most odds changes?
## Hint: The Lines column contains a data frame where each row represents a change in odds.

oddsChanges <- unlist(lapply(MLB2016$Lines, nrow))
oddsGame <- MLB2016[which.max(oddsChanges),]

print(paste("Game ID:", oddsGame$GameID, 
            oddsGame$AwayTeam, "away to", oddsGame$HomeTeam, 
            "on", format(oddsGame$EventDateTimeET, "%A %B %dth"), 
            "had the most odds changes at", max(oddsChanges)
            ))

# [1] "Game ID: DET201607170 Kansas City Royals away to Detroit Tigers on Sunday July 17th had the most odds changes at 308"

#### Question 3a: Visualize the probability of the home team winning the above game over time, by using 
## odds.converter to convert the MoneyUs2 column to probability.
## Hint: The US odds of the home team winning is contained in the column MoneyUS2 of the Lines field. 
## This can be converted to a probability with: odds.converter::odds.us2prob(MoneyUs2).

oddsProb <- oddsGame$Lines[[1]]
oddsProb['prob'] <- odds.us2prob(oddsProb$MoneyUS2)

attach(oddsGame)
p2 <- ggplot(oddsProb, aes(EnteredDateTimeET, prob)) + 
  geom_line() + 
  # geom_hline(yintercept = mean(oddsProb$prob), linetype='dashed') +
  # annotate("text", x=ymd_hms('2016-05-01 00:00:00'), y=0.65, label=paste("Final Win %", round(winPct,2))) +
  xlab(element_blank()) +
  ylab(paste0('Home team (', oddsGame$HomeTeam, ') win probability')) +
  ggtitle(paste("Game ID:", oddsGame$GameID, 
                format(oddsGame$EventDateTimeET, "%A %B %dth %Y"))) +
  scale_x_datetime(date_breaks='3 hour', minor_breaks = 'hour', date_labels = "%b %d %H:%M:%S") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1))
print(p2)

#### Question 4: Which starting pitcher was the largest favorite on average to win, for any pitcher with >= 10 starts?
## Hint By ‘largest favorite’, we want to see which pitcher had the highest mean ‘closing’ probability to win. 
## For each game, the ‘closing’ probabilities are contained in the last row in the Lines field. 
## The MoneyUS1 and MoneyUS2 columns contain the US moneyline odds of the AwayTeam (MoneyUS1) and the 
## HomeTeam (MoneyUS2) winning, respectively. It will help to convert these into probabilities using odds.converter.

probsMLB <- MLB2016

closingProb <- function(Lines, side="Home") {
  if(side == "Home") {
    probs <- odds.us2prob(Lines$MoneyUS2)
  } else if(side == "Away") {
    probs <- odds.us2prob(Lines$MoneyUS1)
  }
  else {
    print("Error, choose side = 'Home' or 'Away'")
    return(NA)
  }
  return(probs[length(probs)])
}

probsMLB['HomeClosingProb'] <- unlist(lapply(probsMLB$Lines, closingProb, side="Home"))
probsMLB['AwayClosingProb'] <- unlist(lapply(probsMLB$Lines, closingProb, side="Away"))

probsMLB <- probsMLB %>%
  select(GameID, AwayStartingPitcher, HomeStartingPitcher, AwayClosingProb, HomeClosingProb)

probsMLBHome <-
  probsMLB %>%
  rename(Prob = HomeClosingProb, Pitcher = HomeStartingPitcher) %>%
  select(GameID, Pitcher, Prob)

probsMLBAway <-
  probsMLB %>%
  rename(Prob = AwayClosingProb, Pitcher = AwayStartingPitcher) %>%
  select(GameID, Pitcher, Prob)

probsMLB <- bind_rows(probsMLBHome, probsMLBAway)

by_pitcher <- 
  probsMLB %>%
  group_by(Pitcher) %>%
  summarize(MeanProb = mean(Prob), GamesStarted = n()) %>%
  filter(GamesStarted > 10) %>%
  arrange(desc(MeanProb))

# head(by_pitcher)

print(paste("Largest favourite starting pitcher in 2016 was", toupper(by_pitcher[1,'Pitcher']), "with an average win probability of",  round(by_pitcher[1,'MeanProb'],3), "in",  by_pitcher[1,'GamesStarted'], "games."))

# [1] "Largest favourite starting pitcher in 2016 was C KERSHAW with an average win probability of 0.679 in 25 games."

