# Data Interview Questions: Problem #2
# 
# Question 2 - Calculating a moving average using Python
# You are given a list of numbers J and a single number p.
# Write a function to return the minimum and maximum averages of the sequences of p numbers in J. 
# Example:
#   
#   J = [4, 4, 4, 9, 10, 11, 12]
# 
# p = 3
# 
# The sequences will be:
#   
#   (4,4,4)
# 
# (4,4,9)
# 
# (4,9,10)
# 
# (9,10,11)
# 
# (10,11,12)
# 
# 
# Here the minimum average will be 4 and the maximum average will be 11, which corresponds 
# to the first and last sequences. Solution will be provided in Python for premium users.
# 
minmaxaverages <- function(J=c(4, 4, 4, 9, 10, 11, 12), p=3){
  M <- c()
  for (i in seq(length(J)-p+1)) {
    K <- J[i:(i+p-1)]
    M <- c(M, mean(K))
  }
  return(list(min=min(M), max=max(M)))
}

J <- c(4, 4, 4, 9, 10, 11, 12)
p <- 3

mma <- minmaxaverages(J, p)
print(mma)
