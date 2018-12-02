# Advent of code 2018 day 2
# https://adventofcode.com/2018/day/2

# Super-messy version - to be tidied!

library (tidyverse)

# Function to generate the checksum for part 1
my_checksum <- function (x){
  # assumes that X will have columns named V1 V2 V3 etc.
  no_2s <- 0
  no_3s <- 0
  for (i in seq_along(x)){
    my_col <- paste0("V",i)
    my_col_summary <- day2_data_t %>%
      group_by_(my_col) %>%
      count()
    
    if (2 %in% my_col_summary$n){
      no_2s <- no_2s + 1
    }
    
    if (3 %in% my_col_summary$n){
      no_3s <- no_3s + 1
    }
  }
  return (no_2s * no_3s)
}

# Part 1

# Read data file and convert to tibble that uses factors
day2_data <- readLines("input/day2_input.txt")
day2_data_split <- strsplit(day2_data, split = "")

day2_data_t <- as.tibble(matrix(unlist(day2_data_split), ncol = length(day2_data))) %>%
  mutate_at(vars(starts_with("V")),funs(as.factor))

part1 <- my_checksum(day2_data_t)

# Part 2
# Now we need to compare all possible combinations of pairs of strings for differences
# This is pretty messy for now

my_comb <- (combn(day2_data, 2))
my_comb_t <- as.tibble(t(my_comb)) # t is base r for transpose
my_comb_t$split1 <- strsplit(my_comb_t$V1, split = "")
my_comb_t$split2 <- strsplit(my_comb_t$V2, split = "")

for (i in seq_along(my_comb_t$V1)){
  myval <- vector(mode = "logical", length = length(my_comb_t$split1[i][[1]]))
  for (j in seq_along(my_comb_t$split1[i][[1]])){
    myval[j] <- my_comb_t$split1[i][[1]][j] == my_comb_t$split2[i][[1]][j]
  }
  my_comb_t$diff[i] <- length(my_comb_t$split1[i][[1]]) - sum(myval)
  my_comb_t$test[i] <- list(myval)
}

my_row <- my_comb_t %>%
  filter(diff == 1)

part2 <- paste(my_row$split1[1][[1]][my_row$test[[1]][1]], collapse = "")
part2


