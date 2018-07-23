# Relationship between wins and runs for the San Francisco Giants (1958-2017)
myteams = subset(Teams, teamID =="SFN")[, c("teamID","yearID","lgID","G","W","L","R","RA")]
tail(myteams)
myteams$RD = with(myteams, R-RA)
myteams$Wpct = with(myteams, W/G)
plot(myteams$RD,myteams$Wpct,xlab = "Run Differential",ylab = "Winning Percentage", main = "Run Differential vs Winning Percentage for 1958-2017 SF Giants")
with(myteams,identify(myteams$RD,myteams$Wpct,teamID,n=2))

# Linear Regression to predict Giant's winning percentage
linfit = lm(Wpct ~ RD, data = myteams)
linfit
abline(a = coef(linfit)[1], b = coef(linfit)[2], lwd=2)
# Winning % = 0.5025632 + (0.0006293 x RD), as of 7/23/18: Giants Winning % should be 0.487 vs 0.505. (-24 RD)
myteams$linWpct = predict(linfit)
myteams$linResiduals = residuals(linfit)
# Residuals can be interpreted as the error of the linear model in predicting winning %
plot(myteams$RD, myteams$linResiduals, xlab="run differential", ylab="residual")
abline(h=0,lty=3)
mean(myteams$linResiduals)
# Root Mean Square Error
linRMSE = sqrt(mean(myteams$linResiduals^2))
linRMSE
nrow(subset(myteams, abs(linResiduals) < linRMSE)) / nrow(myteams)
nrow(subset(myteams, abs(linResiduals) < 2*linRMSE)) /  nrow(myteams)
# Approx 96.67% of residuals are between -2 * RMSE & 2 * RMSE

# The Pythagorean Formula for Winning Percentage : WPCT = (R^2)/ (R^2 + RA^2), as of 7/23/18: Giants Winning % should be 0.472 vs 0.505. (412 R & 489 RA)

myteams$pytWpct = with(myteams, R^2 / (R^2 + RA^2))
plot(myteams$pytWpct, myteams$Wpct, xlab="Predicted WPCT", ylab="Actual WPCT")
myteams$pytResiduals= myteams$Wpct - myteams$pytWpct
# Root Mean Square Error
sqrt(mean(myteams$pytResiduals ^2))

# Proper Exponent in the Pythagorean Formula = 1.81
myteams$logWratio = log(myteams$W / myteams$L)
myteams$logRratio = log(myteams$R / myteams$RA)
pytFit = lm(logWratio ~ 0 + logRratio, data = myteams)
pytFit

# The Adjusted Pythagorean Formula for Winning PErcentage : WPCT = (R^1.81)/ (R^1.81+ RA^1.81), as of 7/23/18: Giants Winning % should 0.474 vs 0.505 
myteams$pytWpct = with(myteams, R^1.81 / (R^1.81 + RA^1.81))
plot(myteams$pytWpct, myteams$Wpct, xlab="Predicted WPCT", ylab="Actual WPCT")
myteams$pytResiduals= myteams$Wpct - myteams$pytWpct
# Root Mean Square Error
sqrt(mean(myteams$pytResiduals ^2))

# Incremental Runs per Win = ( (R^2 + RA^2)^2 / (2 * G * R * RA^2) )
# As of 7/23/18: Giants Incremtal Runs per Win = 8.18 runs, less than the average 10 runs due to Low Scoring Environment 

# Relationship Between Winning Percentage and Run Differential Across Decades
myteams1961 = subset(Teams, yearID == 1961 : 1970)[, c("teamID","yearID","lgID","G","W","L","R","RA")]
myteams1961$RD = with(myteams1961, R-RA)
myteams1961$Wpct = with(myteams1961, W/G)
plot(myteams1961$RD,myteams1961$Wpct,xlab = "Run Differential",ylab = "Winning Percentage", main = "1961-1970")
linfit = lm(Wpct ~ RD, data = myteams1961)
linfit
abline(a = coef(linfit)[1], b = coef(linfit)[2], lwd=2)
# WIN PCT for 1961-1970: 0.5082590 + 0.0006604 * RD

myteams1971 = subset(Teams, yearID == 1971 : 1980)[, c("teamID","yearID","lgID","G","W","L","R","RA")]
myteams1971$RD = with(myteams1971, R-RA)
myteams1971$Wpct = with(myteams1971, W/G)
plot(myteams1971$RD,myteams1971$Wpct,xlab = "Run Differential",ylab = "Winning Percentage", main = "1971-1980")
linfit = lm(Wpct ~ RD, data = myteams1971)
linfit
abline(a = coef(linfit)[1], b = coef(linfit)[2], lwd=2)
# WIN PCT for 1971-1980: 0.5044578 + 0.0006637 * RD

myteams1981 = subset(Teams, yearID == 1981 : 1990)[, c("teamID","yearID","lgID","G","W","L","R","RA")]
myteams1981$RD = with(myteams1981, R-RA)
myteams1981$Wpct = with(myteams1981, W/G)
plot(myteams1981$RD,myteams1981$Wpct,xlab = "Run Differential",ylab = "Winning Percentage", main = "1981-1990")
linfit = lm(Wpct ~ RD, data = myteams1981)
linfit
abline(a = coef(linfit)[1], b = coef(linfit)[2], lwd=2)
# WIN PCT for 1981-1990: 0.50207 + 0.00071 * RD

myteams1991 = subset(Teams, yearID == 1991 : 2000)[, c("teamID","yearID","lgID","G","W","L","R","RA")]
myteams1991$RD = with(myteams1991, R-RA)
myteams1991$Wpct = with(myteams1991, W/G)
plot(myteams1991$RD,myteams1991$Wpct,xlab = "Run Differential",ylab = "Winning Percentage", main = "1991-2000")
linfit = lm(Wpct ~ RD, data = myteams1991)
linfit
abline(a = coef(linfit)[1], b = coef(linfit)[2], lwd=2)
# WIN PCT for 1991-2000:0.4909939 + 0.0006309 * RD

myteams2001 = subset(Teams, yearID == 2001 : 2010)[, c("teamID","yearID","lgID","G","W","L","R","RA")]
myteams2001$RD = with(myteams2001, R-RA)
myteams2001$Wpct = with(myteams2001, W/G)
plot(myteams2001$RD,myteams2001$Wpct,xlab = "Run Differential",ylab = "Winning Percentage", main = "2001-2010")
linfit = lm(Wpct ~ RD, data = myteams2001)
linfit
abline(a = coef(linfit)[1], b = coef(linfit)[2], lwd=2)
# WIN PCT for 2001-2010:  0.5027548 + 0.0006756 * RD

myteams2011 = subset(Teams, yearID == 2011 : 2017)[, c("teamID","yearID","lgID","G","W","L","R","RA")]
myteams2011$RD = with(myteams2011, R-RA)
myteams2011$Wpct = with(myteams2011, W/G)
plot(myteams2011$RD,myteams2011$Wpct,xlab = "Run Differential",ylab = "Winning Percentage", main = "2011-2017")
linfit = lm(Wpct ~ RD, data = myteams2011)
linfit
abline(a = coef(linfit)[1], b = coef(linfit)[2], lwd=2)
# WIN PCT for 2011-2017: 0.494639 + 0.000646 * RD

# Pythagorean Residuals for Rich & Poor Teams in the 19th century
myteams19thcentury = subset(Teams, yearID < 1900)[, c("teamID","yearID","lgID","G","W","L","R","RA")]
myteams19thcentury$pytWpct = with(myteams19thcentury, R^1.81 / (R^1.81 + RA^1.81))
myteams19thcentury$Wpct = with(myteams19thcentury, W/G)
plot(myteams19thcentury$pytWpct, myteams19thcentury$Wpct, xlab="Predicted WPCT", ylab="Actual WPCT")
myteams19thcentury$pytResiduals= myteams19thcentury$Wpct - myteams19thcentury$pytWpct
# Root Mean Square Error
sqrt(mean(myteams19thcentury$pytResiduals ^2))
with(myteams19thcentury,identify(myteams19thcentury$pytWpct,myteams19thcentury$Wpct,teamID,n=4))



