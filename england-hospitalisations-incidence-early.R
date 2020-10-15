# Incidence plots of hospitalisations - March to August 2020

# https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/

gc()
rm(list=ls()) # Clear environment

library('jsonlite')
library('incidence')
library('ggplot2')

govapi <- fromJSON("https://api.coronavirus.data.gov.uk/v1/data?filters=areaName=England&structure=%7B%22date%22:%22date%22,%22newAdmissions%22:%22newAdmissions%22,%22cumAdmissions%22:%22cumAdmissions%22%7D&format=json")

# Original code to convert daily case count to a large list of repeating dates, 1 per case 
#df <- data.frame(date_of_onset = as.Date(govapi$data$date), cases = govapi$data$newCasesByPublishDate)
#df <- as.data.frame(lapply(df, rep, df$cases))
#dat <- df$date
#i <- incidence(dat)
#print(plot(i))

# Found a simpler way to import case data into incidence!

i.1 <- as.incidence(govapi$data$newAdmissions, dates = as.Date(govapi$data$date), interval=1)

# First 150 days
fit.both <- fit(i.1[1:150], split=find_peak(i.1))

before_label_text <- paste("Increasing phase - daily growth rate: ", round(fit.both$before$info$r*100, digits = 2), 
                           "% - doubling time: ", round(fit.both$before$info$doubling, digits = 2), " days",
                           sep="")

after_label_text <- paste("Decreasing phase - daily decline rate: ", round(fit.both$after$info$r*100, digits = 2), 
                          "% - halving time: ", round(fit.both$after$info$halving, digits = 2), " days",
                          sep="")

p <- plot(i.1[1:150], fit=fit.both, color="blue", border = "white") + 
  labs(title="NHS England Daily Coronavirus Hospital Admissions - March to August 2020", subtitle = paste (before_label_text, after_label_text, sep="\n"),
  caption="Graph by @paulmaunders - Generated using R incidence package with data from coronavirus.data.gov.uk") +
  theme_light(base_size = 14) +
  
  # Add rectangle showing national lockdown
  annotate(geom="rect", xmin=as.Date("2020-03-23"), xmax=as.Date("2020-05-10"), ymin=0, ymax=Inf, alpha=0.2, fill = "pink", size=0) 


print (p)
#mtext(side=3, line=2, at=-0.1, adj=0, cex=1.3, "UK Coronavirus Cases")
fit.both
