# Interview Query Data Science Question #1 | Weekly Aggregation
# Good morning. Here's your python question for today.
# 
# This question was asked by: Postmates
# Given a list of timestamps in sequential order, return a list of lists grouped by week (7 days) using the first timestamp as the starting point.
# 
# Example:
# 
# ts = [
#     '2019-01-01', 
#     '2019-01-02',
#     '2019-01-08', 
#     '2019-02-01', 
#     '2019-02-05',
# ]
# 
# output = [
#     ['2019-01-01', '2019-01-02'], 
#     ['2019-01-08'], 
#     ['2019-02-01', '2019-02-05'],
# ]

ts <- c('2019-01-01',
    '2019-01-02',
    '2019-01-08',
    '2019-02-01',
    '2019-02-05')

library(lubridate)

days_length <- as.numeric(days(difftime(ts[length(ts)], ts[1])))/(60*60*24)
weeks_length <- ceiling(days_length/7)
start_date <- ymd(ts[1])
output <- list()
for (i in 1:weeks_length) {
  week <- c()
  end_date <- start_date + i*days(7)
  for (t in ts) {
    if(t < end_date && t >= start_date )
      {week <- c(week, t)}
  }
  start_date <- end_date
  output[[i]] <- week
}
print(output)

