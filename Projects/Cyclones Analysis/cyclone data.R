#Libraries

library(dplyr)
library(plotrix)
library(ggplot2)
library(tidyverse)
library(scales)
library(ggplot2)

#Reading the file

d = read.csv('C:/Users/Sony/Downloads/cyclones.csv')

#Operations performed on dataset
print(head(d))

print(tail(d))

print(view(d))

print(sum(is.na(d)))

print(summary(d))

print(str(d))

print(class(d))

print(typeof(d))

print(colnames(d))
colnames(d)[colnames(d) == 'Lowest.Pressure..mbar.'] <- 'LowestPressure'

print(ncol(d))

print(nrow(d))

print(min(d$LowestPressure))

print(max(d$LowestPressure))

print(d[order(d$LowestPressure),])

print(d[order(d$LowestPressure, decreasing = TRUE),]) 


#List Data for Pressure greater than equal to 920 and less than equal to 940
Pressure_greater_than_920<-d$LowestPressure>=920 & d$LowestPressure<=940
which_over_pressure_920 <- which(Pressure_greater_than_920)
print(d[which_over_pressure_920,])

#List Data for Pressure greater than equal to 940 and less than equal to 960
Pressure_greater_than_940<-d$LowestPressure>=940 & d$LowestPressure<=960
which_over_pressure_940 <- which(Pressure_greater_than_940)
print(d[which_over_pressure_940,])


#List Data for Year equal to 2019
year_2019<-d$Year==2019
which_year_2019 <- which(year_2019)
print(d[which_year_2019,])


#List Data for Year equal to 2017
year_2017 = d$Year==2017
which_year_2017 <- which(year_2017)
print(d[which_year_2017,])

#Select Name and Filter data with LowestPressure >= 970
d %>% filter(LowestPressure >= 970) %>% select(Name)

#Select Year and Filter data with LowestPressure > 980
d %>% filter(LowestPressure >= 980) %>% select(Year)



#Graphical Representation

plot(d$Year,d$LowestPressure,type='l',col='blue',lwd=2,xlab='Year',ylab='Pressure',main='Line Graph for Year vs Pressure')


plot(d$Year,d$LowestPressure,main='Scatter Plot for Year vs Pressure',xlab='Year',ylab='Pressure',col='red')


barplot(d$LowestPressure,names.arg = d$Year,col='green',xlab = 'Year',density=50,ylab = 'Pressure',main='Bar Plot for Year vs Pressure')


hist(d$Year,d$LowestPressure,breaks=15,col = 'yellow',density = 15,xlab = 'Year',ylab = 'Pressure',main = 'Histogram for Year vs Pressure')


pie(d$LowestPressure,main = "Pie Chart for Cyclone Pressure",col = rainbow(length(d$LowestPressure)))


#ggplots(BoxPlot,ViolinPlot,LinePlot)

ggplot(d, aes(x=Year, y=LowestPressure,group=1)) + geom_boxplot(fill="slateblue", alpha=0.2) + 
xlab("Year")+ylab("Lowest Pressure")

ggplot(data = d, mapping = aes(x=Year, y=LowestPressure, group=1)) + geom_boxplot(alpha=0) + 
  geom_jitter(alpha = 0.3, color = "tomato") + xlab("Year") + ylab("Lowest Pressure")

ggplot(d, aes(x=Year, y=LowestPressure,group=1)) + geom_violin(fill="yellow", alpha=0.2) + 
  xlab("Year")+ylab("Lowest Pressure")

ggplot(data = d, aes(x = Year, y = LowestPressure, group = 1,genus)) +
  geom_line()


ggplot(data = d, aes(x = Year, y = LowestPressure)) + geom_point(color = "blue") + xlab("Year")+ylab("Lowest Pressure")


#Conclusion - The Analysis of the Cyclones (1970-2019) depicts that after the year 2000, the cyclone pressure increased drastically. 
#Since it is a natural calamity, we can just minimize the losses. The only way to do that is by detecting prior itself about the cyclone and its density. 
#Also, we can make proper barriers so that the density of the cyclone decreases to some level. 
#Government will assign Natural Disaster Management Authority(NDMA) people to look into the disaster properly and accordingly evacuate the people to camps where people can remain safe. 
#Government will also give relief funds to the ones who have lost their homes and belongings
