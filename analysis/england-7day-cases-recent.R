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
last <- 60

govapi2 <- fromJSON("https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=England&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesBySpecimenDateRollingSum%22:%22newCasesBySpecimenDateRollingSum%22,%22newCasesBySpecimenDateRollingRate%22:%22newCasesBySpecimenDateRollingRate%22%7D&format=json")
cases <- rev(govapi2$data)

cases$date <- as.Date(cases$date , format = "%Y-%m-%d") 

# Find latest test date to label graph later
update_date <- cases$date[1]

# Trim dataset to last x cases
cases <- head(cases, last)

# Lockdown 2 day
lockdown_2_day = as.Date("2020-11-05")

# Plot graph using ggplot

p <- ggplot(cases, aes(x=date,y=newCasesBySpecimenDateRollingSum)) + 
  
  # Set default font family and colour  
  theme(text = element_text(family = "Courier", color = "grey20")) +
  
  # Remove scientific notation from y axis
  scale_y_continuous() + 
  
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
  annotate("text", x=lockdown_2_day+2, y=.05, vjust="top", hjust="left", label="England Lockdown", colour="grey20", angle=90, size=3, family = "Courier") +

  # Add titles
  labs(title="England 7-day cases by specimen date", subtitle=format(update_date,'%A %d %B %Y'), caption="Graph by @paulmaunders using R and ggplot. Data from coronavirus.data.gov.uk")

print(p)


aspect_ratio <- 2
ggsave(plot=p, filename = paste("~/Desktop/england-7day-", update_date, ".png", sep=""), device="png", dpi=300, height = 5 , width = 5 * aspect_ratio)
