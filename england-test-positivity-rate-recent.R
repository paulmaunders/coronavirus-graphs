# Coronavirus England hospitalisations forecast with GGPlot graphs
# Author: Paul Maunders 
# Twitter: @paulmaunders

rm(list=ls()); # Clear environment
library(jsonlite)
library(ggplot2)
require(scales) 
require(mgcv)

help("mgcv-package")

#  Set how many days you wish to see
last <- 120

# Import test data from coronavirus.data.gov.uk

govapi <- fromJSON("https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=England&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newPillarOneTestsByPublishDate%22:%22newPillarOneTestsByPublishDate%22,%22newPillarTwoTestsByPublishDate%22:%22newPillarTwoTestsByPublishDate%22,%22newPillarThreeTestsByPublishDate%22:%22newPillarThreeTestsByPublishDate%22,%22newPillarFourTestsByPublishDate%22:%22newPillarFourTestsByPublishDate%22,%22newTestsByPublishDate%22:%22newTestsByPublishDate%22,%22cumPillarOneTestsByPublishDate%22:%22cumPillarOneTestsByPublishDate%22,%22cumPillarTwoTestsByPublishDate%22:%22cumPillarTwoTestsByPublishDate%22,%22cumPillarThreeTestsByPublishDate%22:%22cumPillarThreeTestsByPublishDate%22,%22cumPillarFourTestsByPublishDate%22:%22cumPillarFourTestsByPublishDate%22,%22cumTestsByPublishDate%22:%22cumTestsByPublishDate%22%7D&format=json")
# print (govapi)

tests <- rev(govapi$data)
 # most recent date

govapi2 <- fromJSON("https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=England&structure=%7B%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22%7D&format=json")
cases <- rev(govapi2$data)

# Format date column as a date type
tests$date <- as.Date(tests$date , format = "%Y-%m-%d") 
cases$date <- as.Date(cases$date , format = "%Y-%m-%d") 

# Find latest test date to label graph later
update_date <- tests$date[1]

# Merge data sets by date key
data = merge(tests, cases, by="date");
#length(data) <- 200

# Count how many daily records are in the data set
rows <- length(data$date)

data <- tail(data, last)

# Create new column for positive test rate
data$PostitiveRate <- data$newCasesByPublishDate / data$newTestsByPublishDate


# Add some extra dates to see if we can get the trend line to extrapolate
#data <- rbind(data, c("2020-10-10",NA,NA,NA,NA,NA,NA,NA,NA,0.0312))
#data$date <- as.Date(data$date , format = "%Y-%m-%d") 

# Lockdown 2 day
lockdown_2_day = as.Date("2020-11-05")

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
  geom_vline(xintercept = lockdown_2_day, linetype="solid", color = "blue", size=0.5) +
  annotate("text", x=lockdown_2_day+1, y=.05, vjust="top", hjust="left", label="England Lockdown", colour="blue", angle=90, size=3, family = "Courier") +

  # Add titles
  labs(title="England Coronavirus Positive Test Rate", subtitle=format(update_date,'%A %d %B %Y'), caption="Graph by @paulmaunders using R and ggplot. Data from coronavirus.data.gov.uk")

print(p)


aspect_ratio <- 2
ggsave(plot=p, filename = paste("~/Desktop/england-tests-", update_date, ".png", sep=""), device="png", dpi=300, height = 5 , width = 5 * aspect_ratio)
