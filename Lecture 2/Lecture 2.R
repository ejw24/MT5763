ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

libs <- c("ggplot2", 
          "gridExtra",
          "tidyverse",
          "git2r",
          "car",
          "minpack.lm")

ipak(libs)
setwd("~/UNI/2019:2020/MT5763/MY5763/Lecture 2")
loanData <- read.csv(file = "~/UNI/2019:2020/MT5763/MY5763/Lecture 2/hmeq.csv", header = TRUE, sep = ",")
# note you can find the file line for the file you want to read in by clicking import data set and then copying and pasting the address in
str(loanData)

# 4 Some Rudimentary Plots ------------------------------------------------


library(ggplot2)
#this is a plot from the basic plot package, it is crap
plot(loanData$VALUE, loanData$MORTDUE)

#reassign the BAD data as factor rather than numeric
loanData$BAD <- factor(loanData$BAD)
#write out a ggplot plot
myplot <- ggplot(data = loanData) + geom_point(aes(x = loanData$VALUE, y = loanData$MORTDUE, col = loanData$BAD))
myplot

#now change the colouring of the plots
myplot <- ggplot(data = loanData) + geom_point(aes(x = loanData$VALUE, y = loanData$MORTDUE, col = loanData$DEBTINC))
myplot
#in this one the colour of the data points is dependent on the value in the DEBTINC column rather than the value in the BAD column


# 5 A simple Model --------------------------------------------------------

#OLS regression
myModel <- lm(loanData$MORTDUE ~ loanData$VALUE, data = loanData)
str(myModel)
names(myModel)
summary(myModel)
# could use this information to produce a trendline in excel

#find the intercept and slope of the trendline
coef(myModel)
#then produce a line using this information
myLine <- coef(myModel)
# now add this line onto the plot you have already produced
myplot <- myplot + geom_abline(intercept = myLine[1], slope = myLine[2], col = 'purple', size = 2)
myplot

#now either by using the export button or code save the plot


# 6 Exercise --------------------------------------------------------------

globalTemp <- read.csv(file = "~/UNI/2019:2020/MT5763/MY5763/Lecture 2/globalTemperature.csv", header = FALSE, sep = ",", col.names = c("Year", "TempDiff"))
#create vector of the new values
# insert variable constant which here is the average temperature from which the absolute temperatures are calculated
constant <- 5
#the new vector
absolute <- globalTemp$TempDiff + constant
#add the new vector on as a column in globalTemp
globalTemp["AbsoluteTemp"] <- absolute
# new vector for fahrenheit
fahrenheit <- (globalTemp$AbsoluteTemp * 1.8) + 32
#add the new vector into the data frame
globalTemp["FahrenheitAbs"] <- fahrenheit
MeanCelsius <- mean(globalTemp$AbsoluteTemp)
MeanFahrenheit <- mean(globalTemp$FahrenheitAbs)
SDCelsius <- sd(globalTemp$AbsoluteTemp)
SDFahrenheit <- sd(globalTemp$FahrenheitAbs)

#plot fahrenheit against time
myplot2 <- ggplot(data = globalTemp, aes(Year, FahrenheitAbs)) + geom_point()
myplot2
#now add a trendline using a simple regression
mymodel2 <- lm(globalTemp$Year ~ globalTemp$FahrenheitAbs)
myLine2 <- coef(mymodel2)
#myplot2 <- myplot2 + geom_abline(intercept = myLine2[1], slope = myLine2[2], col = 'purple', size = 2)
myplot3 <- myplot2 + geom_smooth(method = "lm", formula = y ~ x, data = globalTemp)
myplot3
myplot4 <- myplot2 + geom_smooth(method = "lm", formula = y ~ x, data = globalTemp, se = FALSE)
myplot4
write.csv(globalTemp, file = "globalTempNew.csv")