library(dsEssex)
library(dplyr)
library(corrplot)
#install.packages("RColorBrewer")
library(RColorBrewer)
library(patchwork)
library(gridExtra)
fd<-read.csv("HousePrices.csv", header = TRUE)


#install.packages("explore")
library(explore)
#med %>% explore_all()


#### houses data
fd<-fd[,-1]
str(fd)
summary(fd)
attach(fd)
#install.packages("explore")
library(explore)
fd %>% explore_all()

price_hist<-hist(price, breaks=25, xlab = "Sales price ",#create a histogram of sales price
     main = "Histogram of Sales prices of houses",
     xlim = range(0:195000), ylim = range(0:65))
price_hist
lot_hist<-hist(lotsize, breaks = 25, xlab = "Lot size of house in sq feet",
     main = "Histogram of Lot size of the property in square feet",
     xlim = range(c(0:20000)), ylim= range(c(0:100)))
lot_hist

fd$price<-log(fd$price)
fd$lotsize<-log(fd$lotsize)
#linear regression

fd$driveway<-as.numeric(as.factor(fd$driveway))
fd$recreation<-as.numeric(as.factor(fd$recreation))
fd$fullbase<-as.numeric(as.factor(fd$fullbase))
fd$gasheat<-as.numeric(as.factor(fd$gasheat))
fd$aircon<-as.numeric(as.factor(fd$aircon))
fd$prefer<-as.numeric(as.factor(fd$prefer))

lm.comp<-lm(price~., data=fd)

summary(lm.comp)
plot(lm.comp)
#correlation

corre<-cor(fd)
corrplot(corre, type="upper", order="hclust",
         col=brewer.pal(n=7, name="Spectral"))

#reduced model
red.comp<-lm(price~.-recreation-bedrooms, data=fd)
summary(red.comp)
plot(red.comp)
