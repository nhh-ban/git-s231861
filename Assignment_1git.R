
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


# Problem 4 ----
# Question 4.1 - read the data in to R

library(tidyverse) # loading req. functions

raw_file_2 <- readLines(con = "UCNG_Table4.txt") # Load to memory

substr(x = raw_file_2,start = 1,stop = 2)

L_2 <- # Starts at row 2
  (substr(x = raw_file_2, start = 1, stop = 2) == "--") %>%
  which() %>%
  min()

# Need to remove row 2:
comma_separated_values_2 <- # Remove row 2, and replace all | w/
  # ',' and remove all empty spaces.
  raw_file_2[-2] %>% # Here I remove row 2, as it consists of "--"
  gsub("\\|", ",", .) %>% # Replace |
  gsub(" ", "", .) # Use gsub to search and replace.

# cat()-the whole ting to a .csv-file in the same way as we did with the
# variable descriptions above.
cat(comma_separated_values_2, sep = "\n", file = "updated_file_2.txt")

# Create a data frame
Edwin_Hubble <- read.csv("updated_file_2.txt")

# Combine the two df so we can create a plot w/ cz against the D:
combined_df <- merge(galaxies, Edwin_Hubble, by = "name") # Use merge() and then
# by names to indicate that the df are using the same names.

combined_df %>%
  ggplot(aes(x = D, y = cz)) +
  geom_point() +
  labs(x = "Distance (D)", y = "Velocity (cz)") +
  theme_classic() +
  geom_smooth()

# Solution:
# The plot shows that galaxies w/ higher speed is further away from us
# The velocity tells how fast the galaxies are mowing away from us. 



# Question 4.2
# Compare our df w/ public values
# Hubble's law for expansion v = HD
# We want to find the Hubble's constant value

# By taking the mean of every value
Hubbles_expansion <- mean(combined_df$cz/combined_df$D)
# The public says 70 (km/s)Mpc, which in our case gives a diff of 15 units. 
