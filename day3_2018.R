# Advent of code 2018 day 3
# https://adventofcode.com/2018/day/3

library(tidyverse)

# Part 1
# Overlapping rectangles on an array at least 1000x1000

# Read data file
day3_data <- readLines("input/day3_input.txt")
# Make the divider constant - remove spaces and use , throughout
day3_data <- gsub(" |#","",day3_data)
day3_data <- gsub("#|@|:|x", ",", day3_data)
day3_colnames <- c("id", "x", "y", "width", "height")

d3t <- as.tibble(day3_data) %>%
  separate(col = value, into = day3_colnames, sep = ",") %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(area = width * height) %>%
  mutate(x1 = x + 1) %>%
  mutate(x2 = x + width) %>%
  mutate(y1 = y + 1) %>%
  mutate(y2 = y + height)

# "The whole piece of fabric they're working on is a very large square - at least 1000 inches on each side."

# Work out the maximum dimensions of the fabric that we need to deal with
my_fabric <- matrix(0, nrow = max(d3t$y2) + 1, ncol = max(d3t$x2) + 1)
# don't really want to be passing this in and out of a function as R makes a copy each time...

# Use R's matrix range notation to fill in the fabric
for (i in seq_along(d3t$id)){
  my_fabric[d3t$x1[i]:d3t$x2[i], d3t$y1[i]:d3t$y2[i]] <- my_fabric[d3t$x1[i]:d3t$x2[i], d3t$y1[i]:d3t$y2[i]] + 1
}

# Generate frequency table
my_fabric_freq <- as.tibble(table(my_fabric))

# How many squares are occupied 2 or more times?
part1 <- my_fabric_freq %>%
  filter(my_fabric >= 2)
part1_ans <- sum(part1$n)

# Part 2
# Now we need to find the ID of the only claim that doesn't overlap with the others
# Pull out the sum of that section of the matrix and compare it with the area of the claim

for (i in seq_along(d3t$id)){
  d3t$claim_sum[i] <- sum(my_fabric[d3t$x1[i]:d3t$x2[i], d3t$y1[i]:d3t$y2[i]])
  # Couldn't work out how to do this in mutate as the indices got lost somehow
}

part2 <- d3t %>%
  filter(area == claim_sum)

part2_ans <- part2$id






