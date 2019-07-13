# Daily Coding Problem: Problem #1 [Easy]
# 
# This problem was recently asked by Google.
# 
# Given a list of numbers and a number k, return whether any two numbers from the list add up to k.
# 
# For example, given [10, 15, 3, 7] and k of 17, return true since 10 + 7 is 17.
# 
# Bonus: Can you do this in one pass?

a <- c(10, 15, 3, 7)
k <- 17


# test <- F
# for (i in a) {
#   if(test == T) {
#     print(test)
#     break
#     }
#   for (j in a) {
#     if (i == j) {
#       next
#     }
#     if (i + j == k) {
#       test <- TRUE
#       break
#     }
#   }
# }

# a <- round(randu[1]*100)$x
# k <- round(runif(1)*100)

k %in% colSums(combn(a, 2))
