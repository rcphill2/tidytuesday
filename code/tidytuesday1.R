#Tidytuesday 1 RCode

#Inventory of registered pets in Seattle
#Data Source 
# https://github.com/rfordatascience/tidytuesday/data/2019/2019-03-26


#load tidyverse
library(tidyverse)

#extract species
species <- pets[,4]

#extract zip codes
zcodes <- pets[,7]
zcodes[species = "dog"]


#Look at pet distribution by zip code
ggplot(data = pets) +
  geom_bar(mapping = aes(x = zip_code, fill = species)) +
  theme_bw() +
  labs(x = "Zip Code", y = "Count", color = "Species", title = "Number of Pets per Zip Code based on Species")

# I want to get rid of the zip codes with less than 1000 total pets
# This would allow more clarity of the zip codes and what pets were in which zip code
# I tried a couple of things but am not sure how to do this yet

# save the plot

ggsave("Animal Count vs. Zip Code.pdf", width = 6, height = 9, units = "in")
