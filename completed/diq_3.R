# Data Interview Questions: Problem #3
# 
# Question 3 - The carshare dilemma
# Suppose we have selected a group of people to take a survey. 35% of the group like Uber, 
# 20% like both Lyft and Uber, and 25% like neither Lyft nor Uber. Given this information, 
# what percentage of the sample likes Lyft?
# 
# Hint: You can use basic probability theory to solve this problem.

u <- 0.35
lu <- 0.2
n <- 0.25
l <- 1 - (u + lu + n)

ans <- l + lu
print(paste0(ans*100, "% like Lyft"))
