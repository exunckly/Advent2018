# Advent of code 2018 day 7
# https://adventofcode.com/2018/day/7

# This looks like a job for igraph
library(igraph)

# Extract steps into pairs
d7 <- read_delim("input/day7_input.txt", delim = " ", col_names = FALSE) %>%
  select(X2, X8) %>%
  rename("parent" = "X2", "child" = "X8")

# Load into igraph
mygraph <- (graph_from_edgelist(as.matrix(d7), directed = TRUE))

# find first parents - my data has multiple parents!
parent_degree <- degree(mygraph, mode = "in")
my_parent <- sort(names(which(parent_degree == 0))) # my data has multiple parents!

# find last children
#child_degree <- degree(mygraph, mode = "out")
#my_child <- names(which(child_degree == 0))

# Set up vector of steps and placeholder for possible vertices to look at next
my_steps <- vector(mode = "character", length = gorder(mygraph))
possible_vertices <- my_parent
available_vertices <- my_parent # Initialise 

prereq_complete <- function(mygraph, my_steps, my_vertex){
  # Get parent vertices of the vertex we are testing
  b <- incident(mygraph, my_vertex, mode=c("in"))
  # Check if they are in my_steps (i.e. we have already dealt with the parents)
  parent_vertices <- unique(ends(mygraph, b, names = TRUE)[,1])
  return (all(parent_vertices %in% my_steps))
}

# Loop through vertices filling in steps

my_steps[1] <- my_parent[1]
i <- 2
while(i <= length(my_steps)){
  # generate list of available vertices
  available_vertices <- sort(possible_vertices[!(possible_vertices %in% my_steps)]) # Omit vertices we already have in the steps vector
 
  # Find more vertices
 # if(is_empty(available_vertices)){
    # Move onto the next vertex
    a <- incident_edges(mygraph, my_steps[1:(i-1)], mode=c("out")) # Get child vertices of current vertex
    possible_vertices <- sort(unique(c(available_vertices, ends(mygraph, unlist(a), names = TRUE)[,2]))) # add these vertices to the ones that are currently candidates
    available_vertices <- possible_vertices[!(possible_vertices %in% my_steps)]
#  }
   # Check if we already have the parents
  my_test <- i
  for (j in seq_along(available_vertices)){
    if (prereq_complete(mygraph, my_steps, available_vertices[j]) && i == my_test){
      my_steps[i] <- available_vertices[j]
      i <- i + 1
      print(i)
    }
  }
}
part1 <- paste(my_steps, collapse = "")
part1
