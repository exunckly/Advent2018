library(tidyverse)

# Set up letter locations data
d6 <- read_csv("input/day6_input.txt", col_names = c("x", "y"))
d6$loc_name <- LETTERS[1:length(d6$x)]

# Set up all locations data
# NB: in this case I didn't need to expand further, but may have needed to if the letters were clustered and I needed
# more locations further away than the perimter to get everything < 10000
my_locs <- as.tibble(expand.grid(min(d6$x):max(d6$x),min(d6$y):max(d6$y)))
colnames(my_locs) <- c("x", "y")
my_locs$nearest <- NA
my_locs$total_dist <- NA

# Set up max safe range for part 2
max_safe <- 10000

# Loop through all locations, finding nearest letter and sum of distances to letters
# I need to learn how to do this without a loop at some point (pmap).
# This would probably also be faster with a vector instead of a table
for (i in seq_along(my_locs$x)){
  
  # Vector holding the distance from the current location to each letter
  temp <- abs(d6$x - my_locs$x[i]) + abs(d6$y - my_locs$y[i])
  
  # Nearest neighbour (Manhatten distance) for part 1
  IDs <- which (temp == min(temp))
  if (length(IDs) == 1){
    my_locs$nearest[i] <- d6$loc_name[IDs]
  } else {
    my_locs$nearest[i] <- "NA"
  }
  
  # Sum of distances for part 2
  my_locs$total_dist[i] <- sum(temp)
  if (i %% 1000 == 0){print(i)} # Benchmark this using vectors some time in the future
}

# Part 1: Check whether an area is infinite
# Is a particular location on the perimeter?
my_locs$perimeter <- if_else (my_locs$x == min(my_locs$x) | my_locs$x == max(my_locs$x) | my_locs$y == min(my_locs$y) | my_locs$y == max(my_locs$y), TRUE, FALSE)

# Fill in letter locations table with whether the letter area touches the perimeter
d6$infinite <- NA
for(i in seq_along(d6$loc_name)){
  d6$infinite[i] <- any(filter(my_locs, nearest == d6$loc_name[i])$perimeter)
}

# Add infinite/not infinite status to all locations table
my_locs <- left_join(my_locs, select(d6, loc_name, infinite), by = c("nearest" = "loc_name")) %>%
  unique()

# Calculate the largest area for finite regions
part1 <- largest_area <- my_locs %>%
  filter (infinite == FALSE, !(is.na(nearest))) %>%
  group_by(nearest) %>%
  count() %>%
  arrange(desc(n)) %>%
  pull(nearest) %>%
  first

part1

# Part 2 - already calculated total distance ot all letters in the loop up there
part2 <- my_locs %>%
  count(flag = total_dist < max_safe) %>%
  filter(flag == TRUE) %>%
  pull(n) %>%
  first

part2
