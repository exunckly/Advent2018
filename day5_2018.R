# Advent of code 2018 day 5
# https://adventofcode.com/2018/day/5

# Create main search pattern
pattern <- paste(c(paste0(LETTERS,letters), paste0(letters,LETTERS)), collapse="|")

# Create pairs of patterns for part 2
my_pairs <- paste(LETTERS,letters, sep = "|")

# Load input
d5 <- readLines("input/day5_input.txt")

# Function to remove text matching a pattern from a string
# As new letters will 'bump' together as old ones are removed,
# use gsub until the string doesn't get any shorter
remove_units <- function(x, pattern){
  exit_loop <- 0
  while (exit_loop == 0){
    if(grepl(pattern, x)){
      x <- gsub (pattern, "", x)
    } else {
      exit_loop <- 1
    }
  }
  return (x)
}

# Part 1
part1 <- nchar(remove_units(d5, pattern))

# Part 2
atoz_results <- vector(mode = "numeric", length = length(my_pairs))
for(i in seq_along(my_pairs)){
  atoz_results[i] <- nchar(remove_units(gsub(my_pairs[i], "", d5), pattern)) # To avoid storing silly long strings
  print(letters[i])
}

part2 <- min(atoz_results)

