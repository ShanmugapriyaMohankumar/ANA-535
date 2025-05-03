#
#Laboratory 3 Forecasting with regression including smoothing techniques
#such as moving averages and exponential smoothing.
#
#I've left out some plots. This is because the fits are so
#overdriven that we cannot see much in these plots. 
#
#There are also a number of plots that you don't necessarily need but are 
#good to consider in interpreting the results of the analysis. 
#
#Load all the necessary libraries
#
sink("Lab3_output.txt", append = FALSE, split = TRUE)

library(xlsx)
library(fpp3)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tsibble)
library(tsibbledata)
library(fable)
library(feasts)
library(forecast)
library(lubridate)
library(zoo)

#
#Set the working directory and read in the data - if you are not continuing with
#the data already loaded!
#

setwd("C:/Users/priya/OneDrive/Documents/personal/Mcdaniel/4sem-Mcdaniel/course8-Forecasting")

Amtrak <- read.csv("Amtrak1991-2024.csv")
colnames(Amtrak) <- c("Month", "Ridership", "PassengerMiles", "RidersReported")
View(Amtrak)
str(Amtrak)
Amtrak

#
#Take care of the date format and data type and produce the results from Lab#1
#

Amtrak$Month <- mdy(Amtrak$Month)
str(Amtrak)

par(mfrow = c(1,1))
ggplot(Amtrak, aes(x=Month, y=PassengerMiles)) +
  geom_line(color = "blue") +
  stat_smooth(aes(y = PassengerMiles), method = "lm", formula = y ~ x + I(x^2))

#
#I'm not sure what syntax this is having a problem with.  If you run these lines
#alone it works.  If you run the entire script all at one time this following lines
#do not produce a plot. Don't worry too much about this.
#

ggplot(Amtrak, aes(x=Month, y=PassengerMiles)) +
  geom_line(color = "blue") +
  stat_smooth(aes(y = PassengerMiles), method = "lm", formula = y ~ x + I(x^2) + I(x^3))

#
#It is always good practice to tie your work back to previous work either you or
#someone else has completed.  This is part of "reproducible research". So, let's
#also take a couple minutes to consider ridership too.  
#
#Or, we can start using more complicated commands that allow us to illustrate
#more complicated situations.
#

Amtrak.Ridership.ts <- ts(Amtrak$Ridership, start = c(1991, 1), end = c(2024, 6), frequency = 12)
str(Amtrak.Ridership.ts)
plot(Amtrak.Ridership.ts, xlab = "Time", ylab = "Ridership", bty = "l")

#
#Or, we can start using more complicated commands that allow us to illustrate
#more complicated situations.
#

Amtrak.Ridership2.lm <- tslm(Amtrak.Ridership.ts ~ trend + I(trend^2))
plot(Amtrak.Ridership.ts, xlab = "Time", ylab = "Ridership", bty = "l")
lines(Amtrak.Ridership2.lm$fitted, lwd = 2)

Amtrak.Ridership3.lm <- tslm(Amtrak.Ridership.ts ~ trend + I(trend^2) + I(trend^3))
plot(Amtrak.Ridership.ts, xlab = "Time", ylab = "Ridership", bty = "l")
lines(Amtrak.Ridership3.lm$fitted, lwd = 2)

#
#If these plots look the same as those you got in the first laboratory you are
#good to go to Laboratory #3! 
#

#
#************************************* Step 1 *******************
#
#Always begin with some exploratory data analysis including verifying assumptions.
#Let's consider a histogram of the dependent variable "Passenger Miles".  What
#does it look like? Is it normal in distribution? How would you transform it
#to make it more normal. 
#

h <- hist(Amtrak$PassengerMiles, breaks = 40, density = 80,
          main = "Histogram of Amtrak Passenger Miles")
xfit <- seq(min(Amtrak$PassengerMiles), max(Amtrak$PassengerMiles), length = 100)
yfit <- dnorm(xfit, mean = mean(Amtrak$PassengerMiles), sd = sd(Amtrak$PassengerMiles))
yfit <- yfit * diff(h$mids[1:2]) * length(Amtrak$PassengerMiles)
lines(xfit, yfit, lwd = 2)

#
#We can see that the distribution is left-skewed.  If you don't remember how to 
#handle this from ANA 500/510 you can review that at the website, 
#https://www.statisticshowto.com/probability-and-statistics/skewed-distribution/
# and https://statacumen.com/teach/S4R/PDS_book/skewed-left-distributions.html 
#And, you can use the "square," the "cube root," or the "log" transform to try 
#to make the distribution more normal.  Let's start with the square.
#

y = (Amtrak$PassengerMiles)^2

h <- hist(y, breaks = 40, density = 80,
          main = "Histogram of Amtrak Passenger Miles")
xfit <- seq(min(y), max(y), length = 100)
yfit <- dnorm(xfit, mean = mean(y), sd = sd(y))
yfit <- yfit * diff(h$mids[1:2]) * length(y)
lines(xfit, yfit, lwd = 2)

#
#That kind-of, sort-of looks better. It is probably as good as you can get with
#real-world data.  It looks a bit like a bimodal distribution now 
#but we'll try it and see if we get some better results with regard to the model(s)
#we build.  (Note what happens if you change the formula for "y" to take a log
#transorm of Amtrak$PassengerMiles! You will want to report in Laboratory #3
#whether or not taking the log makes a difference, or at least a difference that
#makes things sufficiently better durin gthe analysis to overcome the requirement
#to transform back to real-world values when we look at the results.  If you do
#not understand this please ask your insructor!)
#
#
#So what happens now if we generate a time plot and add a trend line for a
#linear, a quadratic and a cubic fit? 
#

par(mfrow = c(1,1))
ggplot(Amtrak, aes(x=Month, y=(PassengerMiles)^2)) +
  geom_line(color = "blue") +
  stat_smooth(aes(y = PassengerMiles^2), method = "lm", 
              formula = y ~ x + I(x^1))
par(mfrow = c(1,1))
ggplot(Amtrak, aes(x=Month, y=(PassengerMiles)^2)) +
  geom_line(color = "blue") +
  stat_smooth(aes(y = PassengerMiles^2), method = "lm", 
              formula = y ~ x + I(x^2)) 

par(mfrow = c(1,1))
ggplot(Amtrak, aes(x=Month, y=(PassengerMiles)^2)) +
  geom_line(color = "blue") +
  stat_smooth(aes(y = PassengerMiles^2), method = "lm", 
              formula = y ~ x + I(x^2) +I(x^3))

#
#Consider the linear, a quadratic, and a cubic fit.  As I expected neither the 
#linear or the quadratic fits the data very well.  But here are the plots 
#for you to consider.  
#

#
#********************************Step 2 ***********************
#
#
#What about iid residuals?  First we have to build the model to get residuals
#so let's take a first look at this.  
#
#First let's copy over what we did in Laboratory #1.  Here we generate the "fitted
#values" and we zoomed into take a closer look at a few years. Note that here we
#are using the tslm() command from the forecast package.  
#

Amtrak.ts <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2024, 6), frequency = 12)
str(Amtrak.ts)
plot(Amtrak.ts, xlab = "Time", ylab = "Passenger Miles", bty = "l")

library(forecast)
Amtrak.lm <- tslm(Amtrak.ts ~ trend + I(trend^2))
Amtrak.lm

par(mfrow = c(2,1))
plot(Amtrak.ts, xlab = "Time", ylab = "Passenger Miles", bty = "l", ylim = c(0,1000000000))
lines(Amtrak.lm$fitted, lwd = 2, col = "red")
Amtrak.ts.zoom <- window(Amtrak.ts, start = c(1997, 1), end = c(2000, 12))
plot(Amtrak.ts.zoom, xlab = "Time", ylab = "Passenger Miles", bty = "l")

#
#This reproduces what we had in Lab #1. We see the same seasonality in the zoomed
#in plot that we did before. 
#
#What we want is a plot of fitted versus actuals as seen in Figure 7.6 in 
#Section 7.2 of the textbook. Let's use the TSLM() command
#to do this now.  To use this method we'll first generate a tsibble for the
#Amtrak data.  
#

#
#Here (again) is a plot of the actual values but now using the syntax associated
#with a tsibble. 
#

Amtrak.tsb <- as_tsibble(Amtrak.ts)
Amtrak.tsb
autoplot(Amtrak.tsb) +
  labs(y = "Passenger Miles", x = "Time")

#
#***************************************** Step 3 ***********************
#
#Since we have the data in a tsibble now we can do more with less, so to speak.
#That is, we can embed commands within commands to have 1 line per model, linear
#quadratic, and cubic. 
#
#We'll generate the fitted values using the TSLM() command. Notice that now the 
#TSLM() command is all upper case letters because we are using the fable package.
#Because the "signal" (that is, we can think of the Amtrak data as a signal that 
#is acquired monthly) is relatively complex let's try a quadratic and a cubed 
#fit to see what we can get.  Once the models are built then we will plot them.  
#

fit_tslm <- Amtrak.tsb |>
  model(trend_fit = TSLM(value ~ trend()),
        fit_tslm_season = TSLM(value ~ trend() + season()),
        fit_tslm2 = TSLM(value ~ trend() + season() + I(trend()^2)),
        fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm)

augment(fit_tslm) |>
  autoplot(.resid) +
  labs(x = "Time", y = "", title = "Residuals")

fit_tslm %>%
  accuracy() %>%
  arrange(MAPE)

#
#It looks like the cubic fit is the best in terms of accuracy based on MAPE and.  
#just looking at the variance of the residuals around zero. So let's plot that 
#with the actual values. We'll focus mostly on the cubic model from here on but
#there is some back and forth so you get plots and build intuition about the
#different models. 
#
#Based on what you see, what do you think an appropriate period for a forecast
#would be?  
#

fit_tslm <- Amtrak.tsb |>
  model(fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))

augment(fit_tslm) |>
  ggplot(aes(x = index)) +
  geom_line(aes(y = value, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Amtrak Passenger Miles per Month"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

#
#This does not catch all the drops, e.g. the fitted values do not catch the slight 
#drop around 2008 due to the financial meltdown or the large drop due to the 
#shutdowns during Covid. But it does catch longer-term increases and drops in
#passenger miles.  Does this change what you believe the appropriate period for
#a forecast should be? 
#
#Build a number of models for different periods of time in order to generate an
#overall piecewise model of the data. Here is the first period, from 1991 through
#the end of 1997 (that is why the data run to 01/01/1998). 
#
#It looks like the passenger miles per month is decreasing for this time period.
#

Amtrak.ts.91.97 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(1998, 1), frequency = 12)
Amtrak.tsb.91.97 <- as_tsibble(Amtrak.ts.91.97)

fit_tslm_91_97 <- Amtrak.tsb.91.97 |>
  model(trend_fit = TSLM(value ~ trend()),
        fit_tslm_season = TSLM(value ~ trend() + season()),
        fit_tslm2 = TSLM(value ~ trend() + season() + I(trend()^2)),
        fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm)

augment(fit_tslm_91_97) |>
  ggplot(aes(x = index)) +
  geom_line(aes(y = value, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Amtrak Passenger Miles per Month"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

#
#Let's look at the residuals, i.e. Actuals versus Fitted. It looks like the
#3rd order fit is best.  
#

augment(fit_tslm_91_97) |>
  autoplot(.resid) +
  labs(x = "Time", y = "", title = "Residuals")

#
#Consider the numeric values associated with measures of accuracy for this period.
#

fit_tslm_91_97 %>%
  accuracy() %>%
  arrange(MAPE)

#
#Now plot it.
#

fit_tslm_91_97 <- Amtrak.tsb.91.97 |>
  model(fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm)

augment(fit_tslm_91_97) |>
  ggplot(aes(x = index)) +
  geom_line(aes(y = value, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Amtrak Passenger Miles per Month"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

#
#The cubic fit doesn't quite capture everything but it isn't too bad either.
#
#Here are the commands for the second period 1991 through and including 2004. 
#Here I've commented out commands that consider the linear, quadratic and cubic
#models together and left only commands for plotting the cubic fit.  
#

Amtrak.ts.91.04 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2005, 1), frequency = 12)
Amtrak.tsb.91.04 <- as_tsibble(Amtrak.ts.91.04)

fit_tslm_91_04 <- Amtrak.tsb.91.04 |>
  model(trend_fit = TSLM(value ~ trend()),
        fit_tslm_season = TSLM(value ~ trend() + season()),
        fit_tslm2 = TSLM(value ~ trend() + season() + I(trend()^2)),
        fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm)

augment(fit_tslm_91_04) |>
  autoplot(.resid) +
  labs(x = "Time", y = "", title = "Residuals")

fit_tslm_91_04 %>%
  accuracy() %>%
  arrange(MAPE)

fit_tslm_91_04 <- Amtrak.tsb.91.04 |>
  model(fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm)

augment(fit_tslm_91_04) |>
  ggplot(aes(x = index)) +
  geom_line(aes(y = value, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Amtrak Passenger Miles per Month"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

#
#It looks like there is more variance in the Residuals for this period 1991 - 2004
#than there was for the first period 1991-1997. The MAPE goes from 3.56 to 4.15 
#so numerically the results match our intuition looking at the plot. If you 
#want to see all the measures of accuracy for all models just remove the 
#"hashtags" that comment out the lines below.  
#

#
#Here are the commands for a third period 1991 through and including 2016.
#

Amtrak.ts.91.17 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)
Amtrak.tsb.91.17 <- as_tsibble(Amtrak.ts.91.17)

fit_tslm_91_17 <- Amtrak.tsb.91.17 |>
  model(trend_fit = TSLM(value ~ trend()),
        fit_tslm_season = TSLM(value ~ trend() + season()),
        fit_tslm2 = TSLM(value ~ trend() + season() + I(trend()^2)),
        fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm)

augment(fit_tslm_91_17) |>
  autoplot(.resid) +
  labs(x = "Time", y = "", title = "Residuals")

fit_tslm_91_17 %>%
  accuracy() %>%
  arrange(MAPE)

fit_tslm_91_17 <- Amtrak.tsb.91.17 |>
  model(fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm)

augment(fit_tslm_91_17) |>
  ggplot(aes(x = index)) +
  geom_line(aes(y = value, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Amtrak Passenger Miles per Month"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

#
#Here are the commands for the fourth period 1991 through and including 2020.
#

Amtrak.ts.91.20 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2021, 1), frequency = 12)
Amtrak.tsb.91.20 <- as_tsibble(Amtrak.ts.91.20)

fit_tslm_91_20 <- Amtrak.tsb.91.20 |>
  model(trend_fit = TSLM(value ~ trend()),
        fit_tslm_season = TSLM(value ~ trend() + season()),
        fit_tslm2 = TSLM(value ~ trend() + season() + I(trend()^2)),
        fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm)

augment(fit_tslm_91_20) |>
  autoplot(.resid) +
  labs(x = "Time", y = "", title = "Residuals")

fit_tslm_91_20 %>%
  accuracy() %>%
  arrange(MAPE)

fit_tslm_91_20 <- Amtrak.tsb.91.20 |>
  model(fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm)

augment(fit_tslm_91_20) |>
  ggplot(aes(x = index)) +
  geom_line(aes(y = value, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Amtrak Passenger Miles per Month"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

#
#As anticipated, the accuracy is further decreased as shown by the measures of 
#error such as MAPE = 14.0.  
#
#*************************************** Step 4 *************************
#
#Generate a time plot of the period of interest 1991 - 2016 and add
#the appropriate trend line.
#

Amtrak.ts.91.17 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)
Amtrak.tsb.91.17 <- as_tsibble(Amtrak.ts.91.17)

par(mfrow = c(1,1))
ggplot(Amtrak, aes(x=Month, y=PassengerMiles)) +
  geom_line(color = "blue") +
  stat_smooth(aes(y = PassengerMiles), method = "lm", formula = y ~ x + I(x^2) + I(x^3))

#
#******************************** Step 5 *****************************
#
#Use gg_tsresiduals to consider additional diagnostics for the model
#from 1991 through and including 2016.  
#

fit_tslm_91_17 <- Amtrak.tsb.91.17 |>
  model(fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm)

fit_tslm_91_17 |> gg_tsresiduals()

#
#This presents some interesting results about the residuals.  In addition to a
#graphical presentation of the distribution of the residuals it also includes
#a plot of the ACF.  The residuals look ok for real-world data.  The ACF decreases
#rapidly as desired.  However, there is just a hint that there may still be some
#annual periodicity (lag = 12) in the ACF.  If this were more pronounced I'd
#recommend doing another difference.  In this case I think it would be ok to
#leave that alone and proceed.
#

Amtrak.tsb.91.17 |>
  ACF() |> autoplot() +
  labs(subtitle = "Amtrak Passenger Miles")

#
#First we'll remove the seasonality in the data. As usual with R, there are a lot
#of different ways to do this.  We'll use a couple of them so you get familiar
#with different methods. We only want to use the years 1991 through and including 2016.
#This is sort of a convoluted way to do that, i.e. we'll stop the "window" for the
#period at 01/01/2017 or through the end of 2016.  
#

Amtrak.ts.91.16 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)
Amtrak.comp.91.16 <- decompose(Amtrak.ts.91.16)
autoplot(Amtrak.comp.91.16)
str(Amtrak.comp.91.16)

#
#Now detrend and/or deseason the data by differencing as required
#

Amtrak.ts.91.16_desea <- Amtrak.comp.91.16$x - Amtrak.comp.91.16$seasonal
Amtrak.ts.91.16_detren <- Amtrak.ts.91.16_desea - Amtrak.comp.91.16$trend
Amtrak.ts.91.16_de <- decompose(Amtrak.ts.91.16_detren)
autoplot(Amtrak.ts.91.16_de)

#
#This is the plot of the decomposed differenced data, i.e. the plot of the data
#after the trend and seasonality have been subtracted. So, let's look at the ACF
#again. 
#

Amtrak.tsb.91.16_de <- as_tsibble(Amtrak.ts.91.16_detren)

Amtrak.tsb.91.16_de |>
  ACF() |> autoplot() +
  labs(subtitle = "Amtrak Passenger Miles after differencing")

Amtrak.tsb.91.16_de |>
  PACF() |> autoplot() +
  labs(subtitle = "Amtrak Passenger Miles after differencing")

#
#It looks like there is still some seasonality in the data so we'll take a 
#second differencing to remove that.
#

Amtrak.ts.91.16_desea2 <- Amtrak.ts.91.16_de$x - Amtrak.ts.91.16_de$seasonal
autoplot(Amtrak.ts.91.16_desea2)

Amtrak.tsb.91.16_desea2 <- as_tsibble(Amtrak.ts.91.16_desea2)
Amtrak.tsb.91.16_desea2 |>
  ACF() |> autoplot() +
  labs(subtitle = "Amtrak Passenger Miles (2nd seasonal differencing")

#
#Taking a second difference to remove residual seasonality does not appear
#to have made any difference.  Let's go back to the start and look at what
#happens if we had taken the natural log of the data to begin with. We won't
#do all the steps again.  We only want to see the effect of taking a log of
#the original data. We'll still use Passenger Miles as the variable of interest.
#

lnAmtrakPassengerMiles <- log(Amtrak$PassengerMiles)
lnAmtrakMonth <- Amtrak$Month
lnAmtrak <- data.frame(lnAmtrakMonth, lnAmtrakPassengerMiles)

lnAmtrak.ts <- ts(lnAmtrak$lnAmtrakPassengerMiles, start = c(1991, 1), 
                  end = c(2024, 6), frequency = 12)
plot(lnAmtrak.ts, xlab = "Time", ylab = "Passenger Miles", bty = "l")

#
#Generate a time series for the natural log of Passenger Miles and consider its
#decomposition. 
#

lnAmtrak.comp <- decompose(lnAmtrak.ts)
autoplot(lnAmtrak.comp)

#
#Do a first differencing to remove the seasonality in the data, decompose that
#data and plot it. 
#

lnAmtrak_desea <- lnAmtrak.comp$x - lnAmtrak.comp$seasonal
lnAmtrak_desea.comp <- decompose(lnAmtrak_desea)
autoplot(lnAmtrak_desea.comp) +
  labs(subtitle = "lnAmtrak Passenger Miles (1st seasonal differencing")

#
#Do a second differencing on the seasonal component to remove any remaining
#seasonality and plot.
#

lnAmtrak_desea2 <- lnAmtrak_desea.comp$x - lnAmtrak_desea.comp$seasonal
lnAmtrak_desea2.comp <- decompose(lnAmtrak_desea2)
autoplot(lnAmtrak_desea2.comp)

#
#Compute the ACF of the twice differences Amtrak Passenger Miles Data and plot.
#

lnAmtrak_desea2.comp <- as_tsibble(Amtrak.ts.91.16_desea2)
lnAmtrak_desea2.comp |>
  ACF() |> autoplot() +
  labs(subtitle = "Amtrak Passenger Miles (2nd seasonal differencing")

#
#Ok, after initially taking the natural log of the variable Passenger Mile the ACF
#still looks the same as it did without taking the natural log.  There are still 
#some things going on but all are +/-0.3 or less than 0.3. That is, we have 
#demonstrated that initially taking the natural log of this variable, 
#Amtrak$PassengerMiles does not make any difference in the results of the data 
#analysis. Let's just go back to where we were and not worry about taking the 
#natural log of the variable of interest in this case.  
#


#
#**************************************** Step 6 *********************
#

#
#Generate a 3-month centered moving average model for the Amtrak data (all years).
#Do we need to deseasonalize the data?  Not necessarily but a moving average will
#suppress seasonality in data.  From one point of view this is a good thing becase
#we've seen that differencing twice was not sufficient to remove all the 
#seasonality from the Amtrak data.  
#

#For the moving averages start with the deseasonalized data.  Don't forget to go back 
#to the regular data after that!!!  The ARIMA and related models take trend and
#seasonality into consideration.  You only need to detrend and deseasonalize data
#for some types of smoothing such as moving averages! 
#

#
#How do you choose the amount of time to generate the moving average over? It
#isn't that different from beginning to consider the time period for validation.
#Shumeli suggests that you use the period associated with the seasonality in the
#data.  In this case a strong periodicity is annually or w, the size of the window
#over which we'll take the average, would equal 12, i.e. we'll take averages across
#12-month periods.  That is probably not a good choice to start with because that 
#would smooth out all the other features in the data too.  Let's start with a 
#3-month moving average.  
#
#Note that I've gone back to a period from 1991 to 2016 removing any effects from
#the Covid lock-downs.  
#

Amtrak_3MA <- Amtrak.tsb.91.16_de

#
#Now work on the moving average smoothing beginning with the centered moving
#average
#

Amtrak_3MA.19.16 <- Amtrak_3MA |>
  mutate(
    '3-MA' = slider::slide_dbl(value, mean,
                               .before = 1, .after = 1, .complete = TRUE)
  )

Amtrak_3MA.19.16 |>
  autoplot(Amtrak_3MA$value) +
  geom_line(aes(y = `3-MA`), colour = "#D55E00") +
  labs(y = "Passenger Miles",
       title = "Amtrak Passenger Miles by Month and Centered Moving Average")

#
#Let's zoom into a few years to see exactly how the 3-month moving average
#is handling the fit.
#

str(Amtrak.ts.91.16_detren)

Amtrak_MAZoom_2001_03 <- window(Amtrak.ts.91.16_detren, start = c(2001,1), 
                               end = c(2004,1), frequency = 12)
str(Amtrak_MAZoom_2001_03)
Amtrak.tsb_2001_03 <- as_tsibble(Amtrak_MAZoom_2001_03)

Amtrak_3MA_2001_03 <- Amtrak.tsb_2001_03 |>
  mutate(
    '3-MA' = slider::slide_dbl(value, mean,
                               .before = 1, .after = 1, .complete = TRUE)
  )

Amtrak_3MA_2001_03 |>
  autoplot(Amtrak.tsb_2001_03$value) +
  geom_line(aes(y = Amtrak_3MA_2001_03$`3-MA`), colour = "#D55E00") +
  labs(y = "Passenger Miles",
       title = "Amtrak Passenger Miles by Month and Centered Moving Average")

#
#Ok, you can see how the moving average "smoothes" out the peaks and troughs in
#the data. Using the centered moving average, the fit follows the values in the data
#as they increase and decrease. 
#

#
#Let's look at those same zoomed in years to see exactly how the 3-month trailing 
#moving average is handling the fit.
#

Amtrak.ts_2001_03 <- ts(Amtrak$PassengerMiles, start = c(2001, 1), end = c(2004, 1), freq = 12)
Amtrak.tsb_2001_03 <- as_tsibble(Amtrak.ts_2001_03)

Amtrak_3MATR_2001_03 <- Amtrak.tsb_2001_03 |>
  mutate(
    '3-MA' = slider::slide_dbl(value, mean,
                               .before = 2, .after = 0, .complete = TRUE)
  )

Amtrak_3MATR_2001_03 |>
  autoplot(Amtrak.tsb_2001_03$value) +
  geom_line(aes(y = Amtrak_3MATR_2001_03$`3-MA`), colour = "#D55E00") +
  labs(y = "Passenger Miles",
       title = "Amtrak Passenger Miles by Month and Trailing Moving Average")

#
#Now you can see that the up/down movement of values in the fit trails the actual values.
#

#
#Let's look at a specific example that incorporates moving average smoothing and a 
#prescribed (3 year) validation period.  So I don't mix up anything I'll go back to
#the original Amtrak Ridership values but divide by 1000. to get them scaled a little.
#I'll zoom into the years between 1991 and 2004 (one of the periods we considered before).
#

Amtrak <- read.csv("Amtrak1991-2024.csv")
colnames(Amtrak) <- c("Month", "Ridership", "PassengerMiles", "RidersReported")
View(Amtrak)
str(Amtrak)
Amtrak

#
#Take care of the date format and data type and produce the results from Lab#1
#

Amtrak$Month <- mdy(Amtrak$Month)
str(Amtrak)

#
#Start the code to plot the time series and moving average.
#

Amtrak$Ridership <- Amtrak$Ridership/1000.
head(Amtrak)
ridership.ts <- ts(Amtrak$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

#
#This is an example from Shumeli's book, Practicval Time Series Forecasting with R. 
#

nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))
ma.trailing <- rollmean(train.ts, k = 12, align = "right")
last.ma <- tail(ma.trailing, 1)
ma.trailing.pred <- ts(rep(last.ma, nValid), start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid), freq = 12)

# Figure 5-3
plot(train.ts, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ma.trailing, lwd = 2) 
lines(ma.trailing.pred, lwd = 2, col = "blue", lty = 2) 
lines(valid.ts)
lines(c(2004.25 - 3, 2004.25 - 3), c(0, 3500)) 
lines(c(2004.25, 2004.25), c(0, 3500))
text(1996.25, 2500, "Training")
text(2002.75, 2500, "Validation")
text(2005.25, 2500, "Future")
arrows(2004 - 3, 2450, 1991.25, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 2450, 2004, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 2450, 2006, 2450, code = 3, length = 0.1, lwd = 1, angle = 30)

#
#This is an interesting plot.  You can see that using a window of 12 months (or
#annually) really smooths out a lot of the fluctuations in the Amtrak data. This
#might or might not be a good thing.  If you want to keep the information about 
#periodicities smaller than annual then it is bad.  You will have "smoothed" out
#or lost all that information.  If you only are interested in a general idea of
#whether or not the ridership is increasing or decreasing then it is probably ok.
#

#
#Other things to keep in mind is that these simple methods, like moving averages,
#don't work with data that includes a trend or seasonality.  You need to remove
#that from the data prior to applying a moving average.  If you use a trailing 
#moving average with w = 1 you will get the same result as a naive forecast.  
#

#
#Now consider the seasonal naive method of "simple" forecasting
#

AmtrakSNaive <- window(Amtrak.ts, start=c(1991, 1), end = c(2024, 6))
autoplot(AmtrakSNaive) +
  autolayer(snaive(AmtrakSNaive), h = 36,
            series = "Seasonal naive", PI=FALSE) +
  ggtitle("Amtrak Passenger Miles by Month and Seasonal Naive Forecast") +
  xlab("Time") + ylab("Passenger Miles") +
  guides(colour = guide_legend(title = "Forecast"))

#
#A naive forecast just uses the last period values as the forecast for future
#periods.  It will never change.  Obviously this is "usually" a bad way
#of forecasting.  But, where values almost never change and keeping things
#as simple as possible then the Naive forecast can be pretty good.  
#

#
#********************************** Step 7 **************************
#

#
#Let's look more at Exponential Smoothing. First, we'll build a model using the
#ets() command.  Remember that usually these acronyms have the same or similar
#meanings.  ets is error-trend-seasonality.  stl is seasonality-trend-level. However,
#even though the acronyms are similar the commands/functions do very different 
#things.  For example, the ets() command can be used to build a model but not to
#make a forecast (predictions).  
#
#Remember that you only need to go from 1991 through and including 2016 for this
#step and then add your prediction.  
#

Amtrak1991_2019 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2019, 1), frequency = 12)

fit_ets <- ets(Amtrak1991_2019)
fit_ets
autoplot(fit_ets)

fit_ets_fcast <- forecast(fit_ets, h=60) %>%
  autoplot() +
  ylab("Amtrak Passenger Miles by Month")

#
#This looks a bit different.  As indicated by the automatic labeling of the plot,
#the components of the ets model (by the plot title) are error = multiplicative, 
#trend = null, and seasonality = additive.
#
#
#Just to see what is there, build a model (using the same exponential smoothing) 
#for the years 2021 through and including the months available for 2024. 
#

Amtrak2024 <- ts(Amtrak$PassengerMiles, start = c(2020, 1), end = c(2024, 6), frequency = 12)

fit_ets2 <- ets(Amtrak2024)
ret <- forecast(fit_ets2)

fit_ets2
autoplot(fit_ets2)

fit_ets2_fcast <- forecast(fit_ets2, h=60) %>%
  autoplot() +
  ylab("Amtrak Passenger Miles by Month")

#
#Now the automatic labeling indicates that the error is multiplicative, the trend
#is null and the seasonality is multiplicative. 
#
#It looks like there is a slightly decreasing trend but the change in values are very 
#small.  There is definitely still the seasonality as we saw before the Covid
#pandemic.  
#

#
#This is all nice but we want to save the values of a forecast.  So, we'll modify
#what was outlined in fpp2 to store the output from the forecast above and then plot
#as the following: (Note that the plots look like naive forecasts as they should.)
#

fit_ets_fcast <- forecast(fit_ets, h = 72)
autoplot(fit_ets$fitted) + 
  autolayer(fit_ets_fcast$mean, series = "Forecast") +
  ylab("Passenger Miles")

fit_ets2_fcast <- forecast(fit_ets2, h = 72)
autoplot(fit_ets2$fitted) + 
  autolayer(fit_ets2_fcast$mean, series = "Forecast") +
  ylab("Passenger Miles")

#
#Well the naive forecast seems a little off.  There is no real variation in the 
#level even though the years prior look like there should be.  The biggest part of the 
#problem is that there are multiple seasonalities and we've been trying to apply a
#simple moving average.  That is, we have not yet applied
#the Holt-Winter's method to build a model which is required when seasonality is
#present!
#

#
#*********************************** Step 8 ******************************
#

#
#Let's look at the Holt-Winter's models.  We'll forecast or predict the years
#following 2019.  
#

Amtrak1991_2019 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2019, 1), frequency = 12)

fit1_hw <- hw(Amtrak1991_2019, seasonal = "additive", h = 96)
fit2_hw <- hw(Amtrak1991_2019, seasonal = "multiplicative", h = 96)
fit1_hw
fit2_hw
autoplot(Amtrak1991_2019) +
  autolayer(fit1_hw, series = "HW additive forecasts", PI = FALSE) +
  autolayer(fit2_hw, series = "HW multiplicative forecasts", PI = FALSE) +
  xlab("Time") +
  ylab("Passenger Miles") +
  ggtitle("Amtrak Passenger Miles by Month") +
  guides(colour = guide_legend(title="Forecast"))

accuracy(fit1_hw)
accuracy(fit2_hw)

#
#Now you can see the slight decrease we expected from the slightly decreasing level
#in the decomposition from the ets() model. You can also see that the multiplicative
#forecast is trending lower than the additive forecast as time goes on.  

#
#Now build a model (using the same Holt-Winter's exponential smoothing) for the 
#years 2021 through and including the months available for 2024. 
#

Amtrak2024 <- ts(Amtrak$PassengerMiles, start = c(2020, 1), end = c(2024, 6), frequency = 12)


fit3_hw <- hw(Amtrak2024, seasonal = "additive", h=36)
fit4_hw <- hw(Amtrak2024, seasonal = "multiplicative", h=36)
fit3_hw
fit4_hw

autoplot(Amtrak2024) +
  autolayer(fit3_hw, series = "HW additive forecasts", PI = FALSE) +
  autolayer(fit4_hw, series = "HW multiplicative forecasts", PI = FALSE) +
  xlab("Time") +
  ylab("Passenger Miles") +
  ggtitle("Amtrak Passenger Miles by Month") +
  guides(colour = guide_legend(title="Forecast"))

accuracy(fit3_hw)
accuracy(fit4_hw)

#
#That actually looks pretty good!  Now let's plot the first predictions with these
#actuals and predictions to see what it all looks like. We'll plot it all and then
#restrict the x-axis to 2020 through 2024.  
#

autoplot(Amtrak1991_2019) +
  autolayer(fit1_hw, series = "HW Fit 1", PI = FALSE) +
  autolayer(fit2_hw, series = "HW Fit 2", PI = FALSE) +
  autolayer(fit3_hw, series = "HW Fit 3", PI = FALSE) +
  autolayer(fit4_hw, series = "HW Fit 4", PI = FALSE) +
  xlab("Time") +
  ylab("Passenger Miles") +
  ggtitle("Amtrak Passenger Miles by Month") +
  coord_cartesian(xlim = c(2024, 2027)) +
  guides(colour = guide_legend(title="Forecast"))

#
#This is interesting!  It looks like Fit 1 and Fit 2 which were the HW fit for all the
#data from 1991 through 2024 are somewhat higher than Fit 3 and Fit 4 which only 
#used the data from 2020 to 2024 to make the forecast.  It also looks like Fit 3 and
#Fit 4 lag or are somewhat behind the forecasts using all the data.  I would make 
#the conclusion that the years prior to 2020 do effect the forecast! 
#
#Now we could compute the difference between what the forecast was before the onset
#of Covid with what the actual was to determine if there was a difference.  We will
#leave that for a future exercise and move on. 
#
#Below are the Holt-Winter's forecasts for the years between 1991 and 2016, 
#or before the onset of the Covid pandemic. This includes the HW_linear trend and
#the HW_seasonal additive and multiplicative. 
#

Amtrak1991_2016 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)

fit_hw <- hw(Amtrak1991_2016)
fit_hwA <- hw(Amtrak1991_2016, seasonal = "additive")
fit_hwM <- hw(Amtrak1991_2016, seasonal = "multiplicative")

summary(fit_hw)
summary(fit_hwA)
summary(fit_hwM)

#
#If you haven't caught it yet, not designating a "method" defaults to the HW-
#additive method.
#

autoplot(Amtrak1991_2016) +
  autolayer(fit_hwA, series = "HW additive forecasts", PI=FALSE) +
  autolayer(fit_hwM, series = "HW multiplicative forecasts", PI=FALSE) +
  xlab("Time") +
  ylab("Passenger Miles") +
  ggtitle("Amtrak Passenger Miles by Month") +
  guides(colour = guide_legend(title = "Forecast"))


autoplot(Amtrak1991_2016) +
  autolayer(fit_hwA, series = "HW additive forecasts", PI=FALSE) +
#  autolayer(fit_hwM, series = "HW multiplicative forecasts", PI=FALSE) +
  xlab("Time") +
  ylab("Passenger Miles") +
  ggtitle("Amtrak Passenger Miles by Month") +
  guides(colour = guide_legend(title = "Forecast"))

autoplot(Amtrak1991_2016) +
#  autolayer(fit_hwA, series = "HW additive forecasts", PI=FALSE) +
  autolayer(fit_hwM, series = "HW multiplicative forecasts", PI=FALSE) +
  xlab("Time") +
  ylab("Passenger Miles") +
  ggtitle("Amtrak Passenger Miles by Month") +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(fit_hwA)
accuracy(fit_hwM)

#
#To be thorough I am including use of the ets() method from the Forecast
#package.  In order to get a prediction we need to add a couple more lines
#of code.  See the code below.  This is an example from Shumeli's book,
#Practicval Time Series Forecasting with R. 
#

diff.twice.ts <- diff(diff(ridership.ts, lag = 12), lag = 1)
nValid <- 36
nTrain <- length(diff.twice.ts) - nValid
train.ts <- window(diff.twice.ts, start = c(1992, 2), end = c(1992, nTrain + 1))
valid.ts <- window(diff.twice.ts, start = c(1992, nTrain + 2), end = c(1992, nTrain + 1 + nValid))

ses <- ets(train.ts, model = "ANN", alpha = 0.2)
ses.pred <- forecast(ses, h = nValid, level = 0)

plot(ses.pred, ylim = c(-250, 300),  ylab = "Ridership (Twice-Differenced)", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", 
     flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ses.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)
lines(c(2004.25 - 3, 2004.25 - 3), c(-250, 350)) 
lines(c(2004.25, 2004.25), c(-250, 350))
text(1996.25, 275, "Training")
text(2002.75, 275, "Validation")
text(2005.25, 275, "Future")
arrows(2004 - 3, 245, 1991.5, 245, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 245, 2004, 245, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 245, 2006, 245, code = 3, length = 0.1, lwd = 1, angle = 30)

ses.opt <- ets(train.ts, model = "ANN")
ses.opt.pred <- forecast(ses.opt, h = nValid, level = 0)
ses.opt
accuracy(ses.pred, valid.ts)
accuracy(ses.opt.pred, valid.ts)
sink()
