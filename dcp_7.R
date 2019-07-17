# Daily Coding Problem: Problem #7 [Medium]
# 
# This problem was asked by Facebook.
# 
# Given the mapping a = 1, b = 2, ... z = 26, and an encoded message, count the number of ways it can be decoded.
# 
# For example, the message '111' would give 3, since it could be decoded as 'aaa', 'ka', and 'ak'.
# 
# You can assume that the messages are decodable. For example, '001' is not allowed.
# 
message <- "12123"
m <- strsplit(message, "")[[1]]
n <- as.numeric(m)
combopos <- c()
combos <- c()
for (i in 1:(length(n)-1)) {
  combo <- n[i]*10 + n[i + 1]
  combos <- c(combos, combo)
  combopos <- as.character(c(combopos, i * 11 +  1))
}
combos
combopos
ns <- c(2, 3, 4, 5, 6)
cs <- c(2, 3, 5, 8, 13)

for (i in seq(length(m)-1)) {
  a <- m[i]
  b <- m[i+1]
  ab <- as.numeric(paste0(a, b))
  if (ab > 26) {
    ab <- NULL
  }
  print(paste(a, ab, b))
}
