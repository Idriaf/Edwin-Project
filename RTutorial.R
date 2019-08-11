
library(readxl)

SportsData = read_xlsx("NewTest.xlsx")
View(SportsData)
library(plotly)

library(shiny)
library(ggplot2)
library(plotly)
library(ggrepel)
library(knit2html)
library(gridExtra)
library(TTR)
library(dplyr)
library(plyr)
library(tidyverse)
library(reshape2)
save(SportsData, file="SportsData.RData")

#----------------------------------------------------------------------------------------
#Opening and running models in larger files
largeFile = read.csv("311graffiti.csv")
View(largeFile)

ipoData = read.csv("ipos.csv")
View(ipoData)

#----------------------------------------------------------------------------------------
#large file manipulation 

ipoData$newColumn = (ipoData$price_usd + ipoData$money_raised_usd)
View(ipoData)
ipoData$newColumnMult = (ipoData$price_usd * ipoData$money_raised_usd)
View(ipoData)
IpoModel = lm(ipoData$price_usd ~ ipoData$price)

summary(IpoModel)
#----------------------------------------------------------------------------------------

data = SportsData
View(SportsData)

View(ipoData)


#----------------------------------------------------------------------------------------

#Basic Linear Models
View(largeFile)
lm(largeFile$OnTime_Status ~ largeFile$X)
#----------------------------------------------------------------------------------------

#Variables
P2 = data$`P2+`
P2To17 = data$`P2-17`
P18To34 = data$`P18-34`
P35To49 = data$`P35-49`


View(data)

#----------------------------------------------------------------------------------------

#Date data

data$Date <- as.Date(data$Date, "%Y/%m/%d")
plot(P2 ~ Date, data, xaxt = "n", type = "l")
axis(1, data$Date, format(data$Date, "%b %d"), cex.axis = .7)

data$Date <- as.Date( data$Date, '%m/%d/%Y')
require(ggplot2)
ggplot( data, aes( Date, P2 )) + geom_line()

#----------------------------------------------------------------------------------------
modelYoungerTarget = lm(data$`P2+`~data$`P18-34`)
summary(modelYoungerTarget)


modelYoungerTarget = lm(data$`P2+`~data$`P18-34`)




modelOlderTarget = lm(data$`P2+`~data$`P35-49`)

summary(modelOlderTarget)


#estimating upper and lower bounds for confidence intervals


# estimate interval (based on 95% level)  the average P2+ viewership with an average viewership of 500,000.

#older salesdemo            #when viewership is on the low end
predict(modelOlderTarget, data.frame(P2 = 500000), interval = "confidence")

predict(modelOlderTarget, data.frame(P2 = 500000), interval = "prediction" )


ggplot(data, aes(x=P2, y=P18To34)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_text_repel(label = data$...1)
geom_smooth(method=lm, color='#2C3E50') 

#----------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------
#Visualizations 
#----------------------------------------------------------------------------------------

P2= SportsData$`P2+`
P18To34 = SportsData$`P18-34`
team = SportsData$...1

p1<- ggplot(SportsData, aes(P2, P18To34,color = team)) + geom_point()
p1

ggplotly(p1)

View(SportsData)

#----------------------------------------------------------------------------------------
ggplot(SportsData )+
  geom_point(mapping = aes(x = P2, y = P18To34))+
  facet_wrap(~ SportsData$...1, nrow = 5)

#----------------------------------------------------------------------------------------

#is there a relationship between P2+ and 18-24
Date = data$Date

ggplot(SportsData)+
  geom_smooth(mapping = aes (x =P2, y = P18To34))

P35To49 = SportsData$`P35-49`

ggplot(SportsData)+
  geom_smooth(mapping = aes (x =P2, y = P35To49))


plotYounger = ggplot(SportsData)+
  geom_smooth(mapping = aes (x =P2, y = P18To34))




plotOlder = ggplot(SportsData)+
  geom_smooth(mapping = aes (x =P2, y = P35To49))


grid.arrange(plotYounger, plotOlder, ncol=2)

plotDateAnd_P2 = ggplot(data) +
  geom_point(mapping = aes (x =Date, y = P2, z=team) )+
  geom_smooth(mapping = aes(x =Date, y = P2)) 


ggplotly(plotDateAnd_P2)


#----------------------------------------------------------------------------------------------------
#applying a filter

filter(data, P2 >= 500000)


filter(data, team == 'Utah Jazz vs. Sacramento Kings')

new_Filter= filter(data, team == 'Utah Jazz vs. Sacramento Kings' | P2 >= 899819) #or

write.csv(new_Filter, file="Filter.csv",
          row.names=FALSE)


read.csv("Filter.csv")

FilterData = read.csv("Filter.csv")
#---------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------

P35To49 = data$`P35-49`


gg <- ggplot(data, aes(x=P2, y=P18To34)) + 
  geom_point(aes(col=team, size=P2)) + 
  geom_smooth(method="loess", se=F) + 
  labs(subtitle="P2 Vs P18To34", 
       y="P18To34", 
       x="P2", 
       title="Scatterplot", 
       caption = "Source: ESPN")

plot(gg)

gg <- ggplot(data, aes(x=P2, y=P35To49)) + 
  geom_point(aes(col=team, size=P2)) + 
  geom_smooth(method="loess", se=F) + 
  labs(subtitle="P2 Vs P35To49", 
       y="P18To34", 
       x="P2", 
       title="Scatterplot", 
       caption = "Source: ESPN")
plot(gg)

ggplotly(gg)
# 
# #---------------------------------------------------------------------------------------------------
# View(data)
# options(scipen = 999)
# library(ggplot2)
# library(ggalt)
# demoSelect <- data[data$`P2+` > 857793 & 
#                             data$`P2+` <= 889819 & 
#                             data$`P18-34` > 11271 & 
#                             data$`P18-34` < 382828, ]
# 
# # Plot
# ggplot(data, aes(x=P2, y=P18To34)) + 
#   geom_point(aes(col=team, size=P2)) +   # draw points
#   geom_smooth(method="loess", se=F) + 
#   xlim(c(0, 1372674)) + 
#   ylim(c(0, 384314)) +   # draw smoothing line
#   geom_encircle(aes(x=P2, y=P18To34), 
#                 data=data, 
#                 color="red", 
#                 size=2, 
#                 expand=0.8) +   # encircle
#   labs(subtitle="Area Vs Population", 
#        y="Population", 
#        x="Area", 
#        title="Scatterplot + Encircle", 
#        caption="Source: midwest")

#---------------------------------------------------------------------------------------------------


#MARKDOWN



#----------------------------------------------------------------------------------------

