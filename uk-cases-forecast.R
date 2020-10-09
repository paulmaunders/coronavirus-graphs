# Coronavirus UK cases forecast
# Author: Paul Maunders 
# Twitter: @paulmaunders

rm(list=ls()); # Clear environment
library(jsonlite)

forecast_cases <- 200000
previous_days <- 60
total_days <- previous_days * 2

# Set up X axis to the total days you wish you forecast
days <- 1:total_days

# Import case numbers from coronavirus.data.gov.uk

govapi <- fromJSON("https://api.coronavirus.data.gov.uk/v1/data?filters=areaName=United%2520Kingdom;areaType=overview&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22,%22cumCasesByPublishDate%22:%22cumCasesByPublishDate%22%7D&format=json")
cases <- rev(head(govapi$data$newCasesByPublishDate, previous_days))
#cases <- c(2659,2919,3539,3497,3330,2621,3105,3991,3395,4322,4422,3899,4368,4926,6178,6634,6874,6042,5693,4044,7143,7108,6914,6968,12872,22961,12594,14542,14162,17540)

# Pad out cases with empty values so it is the same length as our x axis
length(cases) <- total_days


# Plot graph
# pch = plotting character - 16 creates a filled circle
# cex = character expansion factor - 1.2 means 20% bigger
plot(days, cases, pch = 16, cex=1.2, ylim=c(0,forecast_cases*1.2))

# Add titles to the graph
mtext(side=3, line=2, at=-0.1, adj=0, cex=1.3, paste("UK Coronavirus Cases - Last",previous_days,"days"))
mtext(side=3, line=1, at=-0.1, adj=0, cex=1.0, format(Sys.Date(),'%A %d %B %Y') )
mtext(side=3, line=0, at=-0.1, adj=0, cex=0.8, "Graph by @paulmaunders - Source: coronavirus.data.gov.uk")

# Create an exponential fit model
fit <- lm(log(cases) ~ days) # find the natural logarithm power relationship between days and cases
predicted_cases <- exp(predict(fit, data.frame(days))) # predict daily cases using fit model
lines(days, predicted_cases, col="red", lwd=2, lty=2) # plot prediction on graph

# Straight lines v - vertical, h - horizontal
abline(v=previous_days, col="blue") # vertical line showing today
abline(h=forecast_cases, col="grey") # horizontal line showing forecast cases

# Forecast the day when cases will reach forecast_cases
forecast_case_day = min(which(predicted_cases > forecast_cases)) # look up the first day in the model which exceeds forecast_cases
abline(v=forecast_case_day, col="grey") # draw a vertical line to mark this day
points(forecast_case_day, predicted_cases[forecast_case_day], col="red") # mark this point on the exponential growth curve
days_to_forecast_cases <- forecast_case_day - previous_days # subtract the historic case days to see how many days from today to forecast_case_day
text(0, forecast_cases * 1.05, pos=4, paste("Exponential growth trend:",format(forecast_cases, big.mark = ",", scientific = F), "cases in",forecast_case_day - previous_days, "days"))
text(forecast_case_day, 0, pos=4, srt=90, format(Sys.Date() + days_to_forecast_cases,'%A %d %B %Y'))

# print(predicted_cases)


# You can recreate formula from model using co-coefficients, e.g.
#summary(fit)
#Formula to predict cases is along the lines of... model coefficients can be found using summary(fit)
#y = e ^ (7.68335 + x) 
#print (exp(7.68335+0.05895*30))  
#print (exp(7.68335+0.05895*50)) 
#print (exp(predict(fit, data.frame(days))))