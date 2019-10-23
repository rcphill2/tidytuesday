######## Header----
#Tidy Tuesdsay For BAE590 HW 8
# Working in ggplot to make two different "pretty" plots 
# This data is from the 2019-06-25 TidyTuesday dataset. It represents worldwide reported UFO sightings. 





#Load tidyverse
library(tidyverse)
library(lubridate)
#Data wrangling ----
#read in data
sightings <- read_csv("data/2019/2019-06-25/ufo_sightings.csv")

#filter out only U.S. sightings
us_sightings <- filter(sightings, country == "us")

#Create region vectors

west <- c("wa", "or", "ca", "nv", "id", "mt", "wy", "co", "az", "nm", "ut", "ak", "hi")
midwest <- c("nd", "sd", "ne", "ks", "mo", "ia", "mn", "wi", "mi", "il", "in", "oh")
south <- c("tx", "ok", "ar", "la", "al", "ms", "ga", "fl", "sc", "nc", "tn", "ky", "va", "wv" ,"md", "de")
northeast <- c("pa", "nj", "ny", "ri", "ct", "ma", "vt", "nh", "me")

#Region Factor

regionv <- c("West", "Midwest", "South", "Northeast")
regions <- factor(regionv, levels = regionv)

#Create U.S. region column
us_sightings$region <- NA

#get rid of old column that I don't need anymore
names(us_sightings)[12] <- "drop"
us_sightings <- us_sightings %>%
  subset(select = -c(drop))

#Setting region to appropriate value based on US State
us_sightings %>%
  mutate(region = if_else(state %in% west, "West", 
                          if_else(state %in% midwest, "Midwest", 
                                  if_else(state %in% south, "South", "Northeast"))))



# Don't use this following way it is really clunky

# us_sightings %>%
#          if (us_sightings$state %in% west){
#            region = "West"
#          } else if (us_sightings$state %in% midwest){
#            region = "Midwest"
#          } else if (us_sightings$state %in% south){
#            region = "South"
#          } else {
#            region = "Northeast"
#          }
  

# states <- us_sightings %>%
#   group_by(state) %>%
#   summarise(num = count(state))


# Total number of sightings to be used later
total_sightings <- 65114

#Count number of occurences
num_regions <- count(us_sightings, vars = region)
#Assign number of occurences per region to a variable
# num_midwest <- num_regions[1,2]
# num_northeast <- num_regions[2,2]
# num_south <- num_regions[3,2]
# num_west <- num_regions[4,2]
#Rather than this, just mutate and add prop in the table

num_regions <- num_regions %>%
  mutate(prop = n/total_sightings) %>%
  mutate(pct = round(prop*100))

#Create barplot
# bp <- num_regions %>%
#   ggplot(mapping = aes(x = vars, y = prop, fill = vars))+
#   geom_bar(width = 1, stat = "identity")
# # Now convert to polar coordinates for pie chart
# pied <- bp + coord_polar("y", start = 0)
################### Plot 1 Creation ----


# pie(num_regions$prop, labels = num_regions$vars, col=topo.colors(4))
# legend

#Credit code skeleton to DisplayR Blog "How to Make a Pie Chart in R" by Tim Ali
# Create a basic bar
pie = ggplot(num_regions, aes(x="", y=prop, fill=vars)) + geom_bar(stat="identity", width=1)
lbls <- paste(num_regions$pct ,"%", sep = " ")
# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start=0) +
  geom_text(aes(label = lbls), position = position_stack(vjust = 0.5))

# Add color scale (hex colors from ColorBrewer2, Printer and Colorblind friendly at 4 levels qualitative)
pie = pie + scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c")) 

# Remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Proportion of UFO Sightings by U.S. Region", caption = "Data from TidyTuesday UFO Sightings 2019-06-25")

# Tidy up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))
#Display the piechart
pie


#Plot 2 Creation ----
#Retrieve only NC sightings
nc_sightings <- us_sightings %>%
  filter(state =="nc")
#Create a time order column and sort in order of when sighting happened
nc_sightings <- nc_sightings %>%
  mutate(order = mdy_hm(date_time)) %>%
  dplyr::arrange(order)

#Add a cumulative sighting column which is essentially just row ID

nc_sightings$cumul <- seq.int(nrow(nc_sightings))

#Need to delete a row, this is just so I remember
#nc_sightings <- nc_sightings[-c(14)]

#Create NC sightings plot
nc_plot <- nc_sightings %>%
  ggplot(mapping = aes(x = order, y = cumul)) +
  geom_line(color = "black", size = 2)+
  labs(x = "Date", y = "Cumulative UFO Sightings", caption = "Data from TidyTuesday UFO Sightings 2019-06-25")+
  ggtitle("Cumulative UFO Sightings in NC")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()
nc_plot












