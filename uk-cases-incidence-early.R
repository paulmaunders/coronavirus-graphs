# Incidence plots

# https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/

rm(list=ls()); # Clear environment

library('jsonlite')
library('incidence')
library('ggplot2')

govapi <- fromJSON("https://api.coronavirus.data.gov.uk/v1/data?filters=areaName=United%2520Kingdom;areaType=overview&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22,%22cumCasesByPublishDate%22:%22cumCasesByPublishDate%22%7D&format=json")

# Original code to convert daily case count to a large list of repeating dates, 1 per case 
#df <- data.frame(date_of_onset = as.Date(govapi$data$date), cases = govapi$data$newCasesByPublishDate)
#df <- as.data.frame(lapply(df, rep, df$cases))
#dat <- df$date
#i <- incidence(dat)
#print(plot(i))

# Found a simpler way to import case data into incidence!

i.7 <- as.incidence(govapi$data$newCasesByPublishDate, dates = as.Date(govapi$data$date), interval=7)

# First 20 weeks
fit.both <- fit(i.7[1:20], split=as.Date("2020-04-01"))

before_label_text <- paste("Daily growth rate: ", round(fit.both$before$info$r*100, digits = 2), "%\n", 
                    "Doubling time: ", round(fit.both$before$info$doubling, digits = 2), " days",
                    sep="")

after_label_text <- paste("Daily decline rate: ", round(fit.both$after$info$r*100, digits = 2), "%\n", 
                           "Halving time: ", round(fit.both$after$info$halving, digits = 2), " days",
                           sep="")

p <- plot(i.7[1:20], fit=fit.both, color="blue", border = "white") + 
  labs(title="UK Coronavirus Cases - February to June 2020") +
  theme_light(base_size = 14) +

  geom_label(
    label=before_label_text, 
    x=as.Date("2020-02-01"),
    y=60000,
    hjust="left",
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    size=5,
    color = "black",
    fill="#ffffff"
  ) +

  geom_label(
    label=after_label_text, 
    x=as.Date("2020-05-01"),
    y=60000,
    hjust="left",
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    size=5,
    color = "black",
    fill="#ffffff"
  )

print (p)
#mtext(side=3, line=2, at=-0.1, adj=0, cex=1.3, "UK Coronavirus Cases")
fit.both