# Daily Coding Problem: Problem #4 [Hard]
# This problem was asked by Stripe.
# 
# Given an array of integers, find the first missing positive integer in linear time 
# and constant space. In other words, find the lowest positive integer that does not 
# exist in the array. The array can contain duplicates and negative numbers as well.
# 
# For example, the input [3, 4, -1, 1] should give 2. The input [1, 2, 0] should give 3.
# 
# You can modify the input array in-place.
# 
input <- c(3, 4, -1, 1)
input <- c(1,2,3,4,5,6,7,10)
# input <- c(1, 2, 0)

input <- input[input>0]
if(length(input) == 0) {input <- 0}
input <- c(0, sort(input), max(input)+2)
answer <- input[diff(input) > 1][1] + 1
print(answer)
