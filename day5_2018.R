# Advent of code 2018 day 5
# https://adventofcode.com/2018/day/5

# Create search pattern
pattern <- paste(c(paste0(LETTERS,letters), paste0(letters,LETTERS)), collapse="|")

# Load input
d5 <- readLines("input/day5_input.txt")

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

part1 <- nchar(remove_units(d5, pattern))
