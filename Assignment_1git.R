
# Skeleton file 2 for Assignment 1 in BAN400. 
# -------------------------------------------

library(tidyverse)    # Contains most of what we need.

# Read the entire data file into memory using the readLines()-function.
raw_file <- readLines(con = "suites_dw_Table1.txt")

# Identify the line number L of the separator line between the column names and
# the rest of the data table.
substr(x = raw_file,start = 1,stop = 2)

# The next step is then to find out *which* line starts with "--", and pick out
# the first one.
L <- # Starts at row 14
  (substr(x = raw_file, start = 1, stop = 2) == "--") %>%
  which() %>%
  min()

# Save the variable descriptions (i.e. the information in lines 1:(L-2)) in a
# text-file for future reference using the cat()-function.
cat(raw_file[1:(L-2)], sep = "\n", file = "variable_description.txt")



# Extract the variable names (i.e. line (L-1)), store the names in a vector.
variable_names <- 
  str_split(string = raw_file[L-1], pattern = "\\|") %>% # | has a special meaning
  # in R, by ussing \\ it will be interpreted as a normal chr.
  unlist() %>% # To get out the vector
  str_trim() # To get rid of all the empty spaces

# Read the data. 
comma_separated_values <- # Take the data from row 15, and replace all | w/
  # ',' and remove all empty spaces.
  raw_file[15:length(raw_file)] %>% 
  gsub("\\|", ",", .) %>% 
  gsub(" ", "", .) # Use gsub to search and replace.

# We then just add the variable names (separated with commas) on top, and
comma_separated_values_with_names <- 
  c(paste(variable_names, collapse = ","),
    comma_separated_values)    

# cat()-the whole ting to a .csv-file in the same way as we did with the
# variable descriptions above.
cat(comma_separated_values_with_names, sep = "\n", file = "updated_file.txt")

# Read the file back in as a normal csv-file.
galaxies <- read_csv("updated_file.txt")

# You should now have a nice, clean data frame with galaxies and their
# characteristics in memory. As of March 2022 it should contain 796
# observations.

# Problem 3

# Create a plot - histogram to visualize the different size of the galaxy
# By using a_26 on the x-axes, we get to see the count for the different sizes
# The higher the number, the smaller the galaxy.

library(ggplot2)

galaxies %>%
  ggplot(aes(x = a_26)) +
  geom_histogram()

# The value represents the linear diameter, and when this value increases,
# the size of the galaxy gets smaller. This is caused by that it's a longer
# distance to the center.

# Conclussion is that the count for galaxies drops as the values increases.