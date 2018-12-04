# Advent of code 2018 day 4
# https://adventofcode.com/2018/day/4

library(tidyverse)
library(lubridate)

# Need to:
#   parse the input file
#   sort by time so that the correct wake/sleep goes with the correct guard
#   mutate so that each row has a guard number, sleep start and sleep end

# Use Jenny Bryan's untangle2 function
# http://rpubs.com/jennybc/untangle-tidyeval

untangle2 <- function(df, regex, orig, new) {
  orig <- enquo(orig)
  new <- sym(quo_name(enquo(new)))
  
  df %>%
    mutate(
      !!new := if_else(grepl(regex, !! orig), !! orig, NA_character_)
    ) %>%
    fill(!! new) %>%
    filter(!grepl(regex, !! orig))
}

d4 <- readLines("input/day4_input.txt")

# All of the falling asleep etc is between 00:00 and 00:59 so extract minute and work from that

d4t <- as.tibble(d4) %>%
  separate(col = value, into = c("my_time", "jumble"), sep = "]") %>%
  mutate(jumble = trimws(jumble, which = "both")) %>%                    # Trim whitespace
  mutate(my_datetime = ymd_hm(gsub("\\[", "", my_time))) %>%             # Extract datetime
  arrange(my_datetime) %>%                                               # Sort before extracting guard data
  untangle2("Guard", jumble, my_guard) %>%                               # Extract raw guard data 'headers' to separate column
  mutate(guard_no = parse_number(my_guard)) %>%                          # Extract guard number from guard data
  mutate(my_minute = as.numeric(format(my_datetime, "%M"))) %>%          # Extract minute
  mutate(jumble = gsub(" ","_",jumble)) %>%                              # Prepare nice column names
  spread (jumble, my_minute) %>%                                         # Spread, fill and filter to line up times
  fill (falls_asleep, .direction = "down") %>%
  filter (!(is.na(wakes_up) )) %>%
  mutate (asleep_for = wakes_up - falls_asleep) %>%                      # Calculate time asleep
  select(guard_no:asleep_for)                                            # Just the columns we need


# Part 1
# Identify the guard who is asleep for the longest overall?
sleepiest_g <- d4t %>%
  group_by (guard_no) %>%
  summarise (total = sum(asleep_for)) %>%
  filter(total == max(total))

# Which minute is the guard asleep for the most?
# This is a similar puzzle to the patches yesterday...

# Careful with indexing and the fact that 'awake' means awake

# Funtion that returns a 60-length vector with the length of time a guard was asleep during each minute across the various days
# Note that minute 0 is held in location 1 and so on, so subtract 1 from the indices this function returns to get the actual minute
sleeps_across_hour <- function(x, my_guard){
  my_hour <- rep(0, 60) # index 1 is minute 0 etc.
  x <- x %>%
    filter(guard_no == my_guard)
  
  for(i in seq_along(x$guard_no)){
    sleepy_time <- (x$falls_asleep[i]+1):(x$wakes_up[i]) # so the indices work - subtract 1 later on
    my_hour[sleepy_time] <- my_hour[sleepy_time] + 1
  }
  return (my_hour)
}

my_hour <- sleeps_across_hour(d4t, sleepiest_g$guard_no[1])
part1_best_times <- which(my_hour %in% max(my_hour)) - 1 # minus 1 to adjust for R indicing from 1

part1_ans <- sleepiest_g$guard_no[1]* part1_best_times # guard ID * minute

# Part 2 - Of all guards, which guard is most frequently asleep on the same minute?
# Didn't bother calculating earlier as didn't know we'd need it :-)
# Make a new array of guards, populate with sleeps_across_hour and add stuff up
guards <- unique(d4t$guard_no)
gp2_sleeps <- matrix(nrow = length(guards), ncol = 60)

for (i in seq_along(guards)){
  gp2_sleeps[i,] <- sleeps_across_hour (d4t, guards[i])
}

my_loc <- which(gp2_sleeps == max(gp2_sleeps), arr.ind = TRUE) # Need to use == rather than %in% here

part2_ans <- guards[my_loc[1]] * (my_loc[2] - 1) # guard ID * minute
