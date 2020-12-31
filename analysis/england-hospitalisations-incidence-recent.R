# Incidence plots of hospitalisations - Recent 60 days

# https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/

rm(list=ls()) # Clear environment

library('jsonlite')
library('incidence')
library('ggplot2')

#  Set how many days you wish to see
last <- 60

# Import data from coronavirus API
govapi <- fromJSON("https://api.coronavirus.data.gov.uk/v1/data?filters=areaName=England&structure=%7B%22date%22:%22date%22,%22newAdmissions%22:%22newAdmissions%22,%22cumAdmissions%22:%22cumAdmissions%22%7D&format=json")

#Import case data into an incidence object
i <- as.incidence(govapi$data$newAdmissions, dates = as.Date(govapi$data$date), interval=1)

# Count how many daily records are in the data set
rows <- length(i$counts)

# Create an exponential fit model for the 'last' however many days 
fit.second <- fit(i[(rows-last):(rows)])

# Create label text to go on the graph to show daily growth rates / doubling times etc from the model
label_text <- paste("Last ", last ," days model:", "\n", "Daily growth rate: ", round(fit.second$info$r*100, digits = 1), "%\n", 
                    "Doubling time: ", round(fit.second$info$doubling, digits = 1), " days",
                    sep="")

# Plot the graph
p <- plot(i[(rows-last):(rows)], fit=fit.second, color="blue", border = "white") +
  
  # Add labels
  labs(title=paste("England Coronavirus Hospital Admissions"), 
       subtitle=format(i[rows]$dates,'%A %d %B %Y'),
       caption="Graph by @paulmaunders - Data from coronavirus.data.gov.uk") +
  
  # Set theme font size
  theme_light(base_size = 14) +
  
  # Add top, right, bottom, left padding
  theme(plot.margin=unit(c(0.5,1.0,0.5,0.5),"cm")) +
  
  # Add an information box with the label text set previously
  geom_label(
    label=label_text, 
    x=i$dates[(rows-last)],
    y=i$counts[(rows)]*.9,
    hjust="left",
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    size=5,
    color = "black",
    fill="#ffffff"
  )

# Show the output of the model on the console
fit.second

# Print the graph in the plot view
print (p)

# Save the graph as a file on your Desktop in 15x15cm square format - 300 dpi for printing
ggsave(plot=p, filename = paste("~/Desktop/england-hospitalisations-recent-", i$dates[rows], ".png", sep=""), device="png", dpi=300, height = 15 , width = 15, units = "cm")
