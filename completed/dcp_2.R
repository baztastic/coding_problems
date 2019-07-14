# Daily Coding Problem: Problem #2 [Hard]
# 
# Good morning! Here's your coding interview problem for today.
# 
# This problem was asked by Uber.
# 
# Given an array of integers, return a new array such that each element at index i of the new array 
# is the product of all the numbers in the original array except the one at i.
# 
# For example, if our input was [1, 2, 3, 4, 5], the expected output would be [120, 60, 40, 30, 24].
# If our input was [3, 2, 1], the expected output would be [2, 3, 6].
# 
# Follow-up: what if you can't use division?
# 
# 

input <- c(1,2,3,4,5)
# input <- c(3, 2, 1)
output <- c()

# with division
for (i in input) {
output <- c(output, prod(input)/i)
}
print(output)

# without division
for (i in input) {
  o <- 1
  for (j in input) {
    if (j == i) {
      next()
    }
    o <- o * j
  }

  output <- c(output, o)
}
print(output)
