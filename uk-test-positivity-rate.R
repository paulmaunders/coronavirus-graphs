# Coronavirus England hospitalisations forecast with GGPlot graphs
# Author: Paul Maunders 
# Twitter: @paulmaunders

rm(list=ls()); # Clear environment
library(jsonlite)
library(ggplot2)
require(scales) 
require(mgcv)

help("mgcv-package")

# Import test data from coronavirus.data.gov.uk

govapi <- fromJSON("https://api.coronavirus.data.gov.uk/v1/data?filters=areaName=United%2520Kingdom;areaType=overview&structure=%7B%22date%22:%22date%22,%22plannedPCRCapacityByPublishDate%22:%22plannedPCRCapacityByPublishDate%22,%22plannedAntibodyCapacityByPublishDate%22:%22plannedAntibodyCapacityByPublishDate%22,%22newPCRTestsByPublishDate%22:%22newPCRTestsByPublishDate%22,%22cumPCRTestsByPublishDate%22:%22cumPCRTestsByPublishDate%22,%22newAntibodyTestsByPublishDate%22:%22newAntibodyTestsByPublishDate%22,%22cumAntibodyTestsByPublishDate%22:%22cumAntibodyTestsByPublishDate%22%7D&format=json")
# print (govapi)

tests <- rev(govapi$data)
 # most recent date

govapi2 <- fromJSON("https://api.coronavirus.data.gov.uk/v1/data?filters=areaName=United%2520Kingdom;areaType=overview&structure=%7B%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22,%22cumCasesByPublishDate%22:%22cumCasesByPublishDate%22%7D&format=json")
cases <- rev(govapi2$data)

# Format date column as a date type
tests$date <- as.Date(tests$date , format = "%Y-%m-%d") 
cases$date <- as.Date(cases$date , format = "%Y-%m-%d") 

# Find latest test date to label graph later
update_date <- tests$date[1]

# Merge data sets by date key
data = merge(tests, cases, by="date");
#length(data) <- 200

# Create new column for positive test rate
data$PostitiveRate <- data$newCasesByPublishDate / data$newPCRTestsByPublishDate


# Add some extra dates to see if we can get the trend line to extrapolate
#data <- rbind(data, c("2020-10-10",NA,NA,NA,NA,NA,NA,NA,NA,0.0312))
#data$date <- as.Date(data$date , format = "%Y-%m-%d") 

# Find which day we first exceed 100k tests
hundredk_tests_day = data[ which(data$newPCRTestsByPublishDate >= 100000),]$date[1]

# Plot graph using ggplot

p <- ggplot(data, aes(x=date,y=PostitiveRate)) + 
  
  # Set default font family and colour  
  theme(text = element_text(family = "Courier", color = "grey20")) +
  
  # Remove scientific notation from y axis
  scale_y_continuous(labels = percent) + 
  
  # Set a theme
  theme_light(base_size = 14) +
  
  # Add points
  geom_point() +
  
  # Add a best fit line
  # https://stats.idre.ucla.edu/r/faq/how-can-i-explore-different-smooths-in-ggplot2/
  # fullrange parameter extends the line
  stat_smooth(method = "gam", formula = y ~ s(x), size = 1, fullrange=F) +
  
  # Add vertical line showing today
  geom_vline(xintercept = hundredk_tests_day, linetype="solid", color = "blue", size=0.5) +
  annotate("text", x=hundredk_tests_day+2, y=.05, vjust="top", hjust="left", label="100,000 tests per day", colour="grey20", angle=90, size=3, family = "Courier") +

  # Add titles
  labs(title="UK Coronavirus Positive Test Rate", subtitle=format(update_date,'%A %d %B %Y'), caption="Graph by @paulmaunders using R and ggplot. Data from coronavirus.data.gov.uk")

print(p)


aspect_ratio <- 2
ggsave(plot=p, filename = paste("~/Desktop/england-tests-", update_date, ".png", sep=""), device="png", dpi=300, height = 5 , width = 5 * aspect_ratio)
