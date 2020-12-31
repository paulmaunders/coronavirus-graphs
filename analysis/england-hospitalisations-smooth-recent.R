# Incidence plots of hospitalisations - Recent 60 days

# https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/

rm(list=ls()) # Clear environment

library('jsonlite')
library('ggplot2')

#  Set how many days you wish to see
last <- 60

# Import data from coronavirus API
govapi <- fromJSON("https://api.coronavirus.data.gov.uk/v1/data?filters=areaName=England&structure=%7B%22date%22:%22date%22,%22newAdmissions%22:%22newAdmissions%22,%22cumAdmissions%22:%22cumAdmissions%22%7D&format=json")

# Reverse data set so it is in chronological order
data <- govapi$data

# Ensure date field is treated as a date
data$date <- as.Date(data$date , format = "%Y-%m-%d") 

# Trim to most recent last days
data <- head(data, last)

# Find date updated
update_date <- data$date[1]

# Lockdown 2 day
lockdown_2_day = as.Date("2020-11-05")

p <- ggplot(data, aes(x=date,y=newAdmissions)) + 
  
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
  labs(title="England Coronavirus Hospital Admissions", subtitle=format(update_date,'%A %d %B %Y'), caption="Graph by @paulmaunders using R and ggplot. Data from coronavirus.data.gov.uk")

# Print the graph in the plot view
print (p)

# Save the graph as a file on your Desktop in 15x15cm square format - 300 dpi for printing
ggsave(plot=p, filename = paste("~/Desktop/england-hospitalisations-smooth-recent-", update_date, ".png", sep=""), device="png", dpi=300, height = 15 , width = 15, units = "cm")
