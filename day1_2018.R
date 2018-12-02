# Advent of code 2018 day 1
# https://adventofcode.com/2018/day/1

# Part 1

# Read data file
part1_data <- readLines("input/day1_part1_input.txt")
part1_values <- as.numeric(part1_data)

# Sum the values in the vector to get the answer
part1_ans <- sum(part1_values)


# Part 2 - now we need to add in sequence and find out the first repeated value
# We have to process the list multiple times if we don't find a repeated value

# Set up some variables
break_condition <- 0
len_input <- length(part1_values)

# We can compute the set of values once, then add the offset we found in part 1
# i.e. iterate one list at a time rather than one element at a time
first_set <- vector(mode = "numeric", length = len_input)
first_set[1] <- part1_values[1]
for(i in 2:len_input){
  first_set[i] <- first_set[i-1] + part1_values[i]
}
my_offset <- sum(part1_values)

# The next iteration starts at the value of my_offset instead of 0 (493 in my input)

j <- 2 # j iterates through entire lists
while (break_condition == 0){
  # set up variable with the size it needs to be each time so that we're not making an object larger
  intermediate_vals <- vector(mode = "numeric", length = len_input*j)
  
  # Fill it in one set of vakues at a time
  for (k in 1:j){
    my_start <- 1 + len_input*(k-1)
    my_end <- len_input*k
    intermediate_vals[my_start:my_end] <- first_set + my_offset*(k-1)
  }
  if (sum(duplicated(intermediate_vals)) > 0){ # check for duplicates
    break_condition <- 1
  }
  j <- j + 1
}

# Pull out all duplicate values
all_duplicates <- intermediate_vals[duplicated(intermediate_vals)]
# Answer is the first duplicated vakue
all_duplicates[1]
