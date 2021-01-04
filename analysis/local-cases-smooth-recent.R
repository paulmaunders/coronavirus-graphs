# Incidence plots of recent cases

# https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/

rm(list=ls()); # Clear environment

library('jsonlite')
library('ggplot2')

#  Set how many days you wish to see
last <- 90

areas <- list("Hart", "Rushmoor")

for (area in areas) {
  
  # Import data from coronavirus API
  # Hart
  govapi <- fromJSON(paste("https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=ltla;areaName=",area,"&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesBySpecimenDate%22:%22newCasesBySpecimenDate%22,%22cumCasesBySpecimenDate%22:%22cumCasesBySpecimenDate%22%7D&format=json", sep=""))
  
  # Rushmoor
  # govapi <- fromJSON("https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=ltla;areaName=Rushmoor&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesBySpecimenDate%22:%22newCasesBySpecimenDate%22,%22cumCasesBySpecimenDate%22:%22cumCasesBySpecimenDate%22%7D&format=json")
  
  # Reverse data set so it is in chronological order
  data <- govapi$data
  
  # Ensure date field is treated as a date
  data$date <- as.Date(data$date , format = "%Y-%m-%d") 
  
  # Remove incomplete data for 4 most recent entries
  data <- tail(data, -4)
  
  # Trim to required number of days
  data <- head(data, last)
  
  # Find date updated
  update_date <- data$date[1]
  
  p <- ggplot(data, aes(x=date,y=newCasesBySpecimenDate)) + 
    
    # Set default font family and colour  
    theme(text = element_text(family = "Courier", color = "grey20")) +
    
    # Remove scientific notation from y axis
    scale_y_continuous() + 
    
    # Set a theme
    theme_light(base_size = 14) +
    
    # Add background rectangle showing lockdown 2
    #geom_vline(xintercept = lockdown_2_day, linetype="solid", color = "blue", size=0.5) +
    
    geom_rect(xmin=as.Date("2020-11-05"),xmax=as.Date("2020-12-02"),ymin=-Inf,ymax=Inf, alpha=0.002, fill="orange",show.legend = "Lockdown") +
    annotate("text", x=as.Date("2020-11-05")+1, y=max(data$newCasesBySpecimenDate), vjust="top", hjust="right", label="England Lockdown", colour="blue", angle=90, size=3, family = "Courier") +
    
    # Add vertical line showing start of term
    geom_vline(xintercept = as.Date("2020-11-02"), linetype="solid", color = "blue", size=0.5) +
    annotate("text", x=as.Date("2020-11-02")-2, y=max(data$newCasesBySpecimenDate), vjust="top", hjust="right", label="School starts after half term", colour="black", angle=90, size=3, family = "Courier") +
    
    # Add vertical line showing schools break up for Christmas
    geom_vline(xintercept = as.Date("2020-12-18"), linetype="solid", color = "blue", size=0.5) +
    annotate("text", x=as.Date("2020-12-18")-2, y=max(data$newCasesBySpecimenDate), vjust="top", hjust="right", label="Christmas holidays start", colour="black", angle=90, size=3, family = "Courier") +
    
    # Add points
    geom_point() +
    
    # Add a best fit line
    # https://stats.idre.ucla.edu/r/faq/how-can-i-explore-different-smooths-in-ggplot2/
    # fullrange parameter extends the line
    stat_smooth(method = "gam", formula = y ~ s(x), size = 1, fullrange=F) +
    
    # Add titles
    labs(title=paste(area, " Coronavirus Cases", sep=""), subtitle=format(Sys.Date(),'%A %d %B %Y'), caption="Graph by @paulmaunders using R and ggplot. Data from coronavirus.data.gov.uk")
  
  
  # Print the graph in the plot view
  print (p)
  
  # Save the graph as a file on your Desktop in 15x15cm square format - 300 dpi for printing
  ggsave(plot=p, filename = paste("~/Desktop/local-",area,"-cases-smooth-recent-", update_date, ".png", sep=""), device="png", dpi=300, height = 15 , width = 15, units = "cm")
  
}



