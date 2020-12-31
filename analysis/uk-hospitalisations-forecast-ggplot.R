# Coronavirus England hospitalisations forecast with GGPlot graphs
# Author: Paul Maunders 
# Twitter: @paulmaunders

rm(list=ls()); # Clear environment
library(jsonlite)
library(ggplot2)

forecast_patients <- 17000
previous_days <- 40
future_days <- 30
total_days <- previous_days + future_days

# Set up X axis to the total days you wish you forecast
days <- -(previous_days-1):future_days

# Import case numbers from coronavirus.data.gov.uk

govapi <- fromJSON("https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=England&structure=%7B%22date%22:%22date%22,%22hospitalCases%22:%22hospitalCases%22%7D&format=json")
# print (govapi)

patients <- rev(head(govapi$data$hospitalCases, previous_days))
update_date <- as.Date(govapi$data$date[1], "%Y-%m-%d")

#patients <- c(2659,2919,3539,3497,3330,2621,3105,3991,3395,4322,4422,3899,4368,4926,6178,6634,6874,6042,5693,4044,7143,7108,6914,6968,12872,22961,12594,14542,14162,17540)

# Pad out patients with empty values so it is the same length as our x axis
length(patients) <- total_days

# Create an exponential fit model
fit <- lm(log(patients) ~ days) # find the natural logarithm power relationship between days and patients
predicted_patients <- exp(predict(fit, data.frame(days))) # predict daily patients using fit model

# Forecast the day when patients will reach forecast_patients
forecast_patient_day = min(which(predicted_patients > forecast_patients)) - previous_days  # look up the first day in the model which exceeds forecast_patients
#print (forecast_patient_day);

# Combine vectors to data frame for ggplot to use

df <- data.frame(days,patients, predicted_patients)
names(df) <- c("days","patients", "predicted_patients")
#print(df)

# Build formula from model for display purposes
exponential_fit = paste("y = e ^ (", format(round(fit$coefficients[1], 4), nsmall = 2) , " + ", format(round(fit$coefficients[2], 4), nsmall = 2), " * x)", sep="")

# Plot graph using ggplot

p <- ggplot(df, aes(days, patients)) + 
  
  # Set default font family and colour  
  theme(text = element_text(family = "Courier", color = "grey20")) +
  
  theme_light(base_size = 14) +
  
  # Add points
  geom_point() +
  #paste("Exponential Fit", formula)
  # Add exponential fit line
  geom_line(data = df, aes(days, predicted_patients, color = exponential_fit), size = 1, linetype = 2) +
  
  # Add vertical line showing today
  geom_vline(xintercept = 0, linetype="solid", color = "blue", size=0.5) +
  annotate("text", x=1, y=0, vjust="top", hjust="left", label="Today", colour="grey20", angle=90, size=3, family = "Courier") +
  
  # Add vertical line showing when forecast patients may occur according to model
  geom_vline(xintercept = forecast_patient_day, linetype="dotted", color="grey", size=1) + 
  annotate("text", x=forecast_patient_day+1, y=forecast_patients/2, vjust="top", label=format(update_date + forecast_patient_day,'%A %d %B %Y'), colour="grey20", angle=90, size=3, family = "Courier") +
  
  # Add horizontal line for forecast patients
  geom_hline(yintercept = forecast_patients, linetype="dotted", color="grey", size=1) + 
  annotate("text", x=-previous_days, hjust="left", label=paste("Exponential growth trend:",format(forecast_patients, big.mark = ",", scientific = F), "patients in",forecast_patient_day, "days"), y=forecast_patients*1.05, colour="grey20",size=3, family = "Courier") + 
  
  # Mark where patients exceed forecast on exponential curve
  geom_point(aes(x=forecast_patient_day, y=predicted_patients[forecast_patient_day + previous_days]), color="red") +  # mark this point on the exponential growth curve
  
  # Add titles
  labs(title="Daily Coronavirus Patients in Hospital in England", subtitle=format(update_date,'%A %d %B %Y'), caption="Graph by @paulmaunders - Data from coronavirus.data.gov.uk")

print(p)


aspect_ratio <- 2
ggsave(plot=p, filename = paste("~/Desktop/england-hospitalisations-", update_date, ".png", sep=""), device="png", dpi=300, height = 5 , width = 5 * aspect_ratio)
