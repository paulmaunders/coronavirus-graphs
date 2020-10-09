# Coronavirus UK cases forecast with GGPlot graphs
# Author: Paul Maunders 
# Twitter: @paulmaunders

rm(list=ls()); # Clear environment
library(jsonlite)
library(ggplot2)

forecast_cases <- 50000
previous_days <- 50
future_days <- 30
total_days <- previous_days + future_days

# Set up X axis to the total days you wish you forecast
days <- -(previous_days-1):future_days

# Import case numbers from coronavirus.data.gov.uk

govapi <- fromJSON("https://api.coronavirus.data.gov.uk/v1/data?filters=areaName=United%2520Kingdom;areaType=overview&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22,%22cumCasesByPublishDate%22:%22cumCasesByPublishDate%22%7D&format=json")
cases <- rev(head(govapi$data$newCasesByPublishDate, previous_days))
update_date <- as.Date(govapi$data$date[1], "%Y-%m-%d")

#cases <- c(2659,2919,3539,3497,3330,2621,3105,3991,3395,4322,4422,3899,4368,4926,6178,6634,6874,6042,5693,4044,7143,7108,6914,6968,12872,22961,12594,14542,14162,17540)

# Pad out cases with empty values so it is the same length as our x axis
length(cases) <- total_days

# Create an exponential fit model
fit <- lm(log(cases) ~ days) # find the natural logarithm power relationship between days and cases
predicted_cases <- exp(predict(fit, data.frame(days))) # predict daily cases using fit model

# Forecast the day when cases will reach forecast_cases
forecast_case_day = min(which(predicted_cases > forecast_cases)) - previous_days  # look up the first day in the model which exceeds forecast_cases
#print (forecast_case_day);

# Combine vectors to data frame for ggplot to use

df <- data.frame(days,cases, predicted_cases)
names(df) <- c("days","cases", "predicted_cases")
#print(df)

# Plot graph using ggplot

p <- ggplot(df, aes(days, cases)) + 
  
  # Set default font family and colour  
  theme(text = element_text(family = "Courier", color = "grey20")) +
  
  # Add points
  geom_point() +
  
  # Add exponential fit line
  geom_line(data = df, aes(days, predicted_cases, color = "Exponential Fit"), size = 1, linetype = 2) +
  
  # Add vertical line showing today
  geom_vline(xintercept = 0, linetype="solid", color = "blue", size=0.5) +
  annotate("text", x=1, y=0, vjust="top", hjust="left", label="Today", colour="grey20", angle=90, size=4, family = "Courier") +
  
  # Add vertical line showing when forecast cases may occur according to model
  geom_vline(xintercept = forecast_case_day, linetype="solid", color="grey", size=0.5) + 
  annotate("text", x=forecast_case_day+1, y=forecast_cases/2, vjust="top", label=format(update_date + forecast_case_day,'%A %d %B %Y'), colour="grey20", angle=90, size=4, family = "Courier") +
  
  # Add horizontal line for forecast cases
  geom_hline(yintercept = forecast_cases, linetype="solid", color="grey", size=0.5) + 
  annotate("text", x=-previous_days, hjust="left", label=paste("Exponential growth trend:",format(forecast_cases, big.mark = ",", scientific = F), "cases in",forecast_case_day, "days"), y=forecast_cases*1.05, colour="grey20",size=4, family = "Courier") + 
  
  # Mark where cases exceed forecast on exponential curve
  geom_point(aes(x=forecast_case_day, y=predicted_cases[forecast_case_day + previous_days]), color="red") +  # mark this point on the exponential growth curve
  
  # Add titles
  labs(title=paste("UK Coronavirus Cases - Last",previous_days,"days"), subtitle=format(update_date,'%A %d %B %Y'), caption="Graph by @paulmaunders")

print(p)