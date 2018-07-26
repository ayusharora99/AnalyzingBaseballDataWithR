# Run Expectancy Matrix for the 2011 Season

data2011 <- read.csv("~/Desktop/Analyzing Baseball Data with R/baseball_R-master/data/all2011.csv", header=FALSE)
names(data2011) = fields[ , "Header"]

data2011$RUNS = with(data2011, AWAY_SCORE_CT + HOME_SCORE_CT)
data2011$HALF.INNING = with(data2011, paste(GAME_ID, INN_CT, BAT_HOME_ID))
data2011$RUNS.SCORED = with(data2011, (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))
RUNS.SCORED.INNING <- aggregate(data2011$RUNS.SCORED, list(HALF.INNING = data2011$HALF.INNING), sum)
RUNS.SCORED.START = aggregate(data2011$RUNS, list(HALF.INNING=data2011$HALF.INNING), "[",1)

MAX = data.frame(HALF.INNING = RUNS.SCORED.START$HALF.INNING)
MAX$x = RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
data2011 = merge(data2011, MAX)
N = ncol(data2011)
names(data2011)[N] = "MAX.RUNS"

data2011$RUNS.ROI = with(data2011, MAX.RUNS - RUNS)

RUNNER1 = ifelse(as.character(data2011[ , "BASE1_RUN_ID"]) == "", 0,1)
RUNNER2 = ifelse(as.character(data2011[ , "BASE2_RUN_ID"]) == "", 0,1)
RUNNER3 = ifelse(as.character(data2011[ , "BASE3_RUN_ID"]) == "", 0,1)

get.state = function(runner1,runner2,runner3,outs){
  runners = paste(runner1, runner2, runner3, sep = "")
  paste(runners,outs)
}

data2011$STATE = get.state(RUNNER1, RUNNER2, RUNNER3, data2011$OUTS_CT)

NRUNNER1 = with(data2011, as.numeric(RUN1_DEST_ID == 1 | BAT_DEST_ID == 1))
NRUNNER2 = with(data2011, as.numeric(RUN1_DEST_ID == 2 | RUN2_DEST_ID == 2 | BAT_DEST_ID == 2))
NRUNNER3 = with(data2011, as.numeric(RUN1_DEST_ID == 3 | RUN2_DEST_ID ==3 | RUN3_DEST_ID == 3 | BAT_DEST_ID == 3))
NOUTS = with(data2011, OUTS_CT + EVENT_OUTS_CT)
data2011$NEW.STATE = get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)

data2011 = subset(data2011, (STATE != NEW.STATE) | (RUNS.SCORED > 0))

library(plyr)
data.outs = ddply(data2011, .(HALF.INNING), summarize, Outs.Inning = sum(EVENT_OUTS_CT))
data2011 = merge(data2011, data.outs)
data2011C = subset(data2011, Outs.Inning ==3)

RUNS = with(data2011C, aggregate(RUNS.ROI, list(STATE), mean))
RUNS$Outs = substr(RUNS$Group, 5, 5)
RUNS = RUNS[order(RUNS$Outs), ]
RUNS.out = matrix(round(RUNS$x,2),8,3)
dimnames(RUNS.out)[[2]] = c("0 outs", "1 out", "2 outs")
dimnames(RUNS.out)[[1]] = c("000","001","010","011","100","101","110","111")

# Measuring Succes of a Batting Play
RUNS.POTENTIAL = matrix(c(RUNS$x, rep(0, 8)), 32, 1)
dimnames(RUNS.POTENTIAL)[[1]] = c(RUNS$Group, "000 3","001 3","010 3","011 3","100 3","101 3","110 3","111 3") 
data2011$RUNS.STATE = RUNS.POTENTIAL[data2011$STATE, ]                        
data2011$RUNS.NEW.STATE = RUNS.POTENTIAL[data2011$NEW.STATE,]
data2011$RUNS.VALUE = data2011$RUNS.NEW.STATE - data2011$RUNS.STATE + data2011$RUNS.SCORED

# Albert Pujol's Run Value
albert.id = subset(roster2011, First.Name == "Albert" & Last.Name == "Pujols")$Player.ID
albert.id = as.character(albert.id)
albert = subset(data2011, BAT_ID == albert.id)
albert = subset(albert,BAT_EVENT_FL == TRUE)
# Albert Pujols' First 2 at bats of the 2011 season
albert[1:2, c("STATE","NEW.STATE","RUNS.VALUE")]
# Base Scenarios of Albert's 2011 season
albert$RUNNERS = substr(albert$STATE, 1, 3)
table(albert$RUNNERS)
# Albert Pujols' Performance with Baserunner Oppurtunites
with(albert,stripchart(RUNS.VALUE ~ RUNNERS, vertical = TRUE, jitter = 0.2, xlab = "RUNNERS", method = "jitter", pch = 1, cex = 0.8, main = "2011 Albert Pujols' Performance with Baserunner Oppurtunites", ylim = c(-2,5)))
abline(h=0)
A.runs = aggregate(albert$RUNS.VALUE, list(albert$RUNNERS), sum)
names(A.runs)[2] = "RUNS"
A.PA = aggregate(albert$RUNS.VALUE, list(albert$RUNNERS), length)
names(A.PA)[2] = "PA"
A = merge(A.PA, A.runs)
A
sum(A$RUNS)

# Batting Effectiveness of Players 
data2011b = subset(data2011, BAT_EVENT_FL == TRUE)
runs.sums = aggregate(data2011b$RUNS.VALUE, list(data2011b$BAT_ID), sum)
names(runs.sums)= c("Batter","Runs")
runs.pa = aggregate(data2011b$RUNS.VALUE, list(data2011b$BAT_ID), length)
names(runs.pa) = c("Batter", "PA")
runs.start = aggregate(data2011b$RUNS.STATE, list(data2011b$BAT_ID), sum)
names(runs.start) = c("Batter","Runs.Start")
runs = merge(runs.sums,runs.pa)
runs = merge(runs, runs.start)
runs400 = subset(runs, PA >= 400)
head(runs400)

# Relationship between batters' oppurtunities and their success in converting these oppurtunities to runs
with(runs400,plot(Runs.Start, Runs))
with(runs400, lines(lowess(Runs.Start, Runs)))
abline(h=0)

# Most Productive Batters
runs400.top = subset(runs400, Runs >= 40)
runs400.top = merge(runs400.top, roster2011, by.x = "Batter", by.y = "Player.ID")
with(runs400.top, text(Runs.Start, Runs, Last.Name, pos = 1))

# Positioning in the Batting Lineup
get.batting.pos = function(batter){
  TB = table(subset(data2011, BAT_ID == batter)$BAT_LINEUP_ID)
  names(TB)[TB == max(TB)[1]]
}

position = sapply(as.character(runs400$Batter), get.batting.pos)
with(runs400, plot(Runs.Start, Runs, type = "n"))
with(runs400, lines(lowess(Runs.Start, Runs)))
abline(h=0)
with(runs400, text(Runs.Start, Runs, position))

# Alber Pujols vs the Rest
AP = subset(runs400, Batter == albert.id)
points(AP$Runs.Start, AP$Runs, pch=19, cex=3)

# Run Value of a home run
d.homerun = subset(data2011, EVENT_CD = 23)
table(d.homerun$STATE)
round(prop.table(table(d.homerun$STATE)),3) # over half of the homeruns are hit with no runners on base
library(MASS)
truehist(d.homerun$RUNS.VALUE)
subset(d.homerun, RUNS.VALUE==max(RUNS.VALUE))[1, c("STATE", "NEW.STATE", "RUNS.VALUE")]
# mean = 1.392393

# Run Value of a single 
d.single = subset(data2011, EVENT_CD == 20)
library(MASS)
truehist(d.single$RUNS.VALUE)
table(d.single$STATE)
subset(d.single, d.single$RUNS.VALUE == max(d.single$RUNS.VALUE))[ , c("STATE","NEW.STATE","RUNS.VALUE")] #largest run value
subset(d.single, d.single$RUNS.VALUE == min(d.single$RUNS.VALUE))[ , c("STATE","NEW.STATE","RUNS.VALUE")] #smallest run value
mean.single = mean(d.single$RUNS.VALUE)
mean.single
abline(v = mean.single, lwd=3)
text(.5, 5, "Mean Runs Value", pos=4)

# Value of Base Stealing
stealing <- subset(data2011, EVENT_CD==6 | EVENT_CD==4)
table(stealing$EVENT_CD)
table(stealing$STATE)
truehist(stealing$RUNS.VALUE)
stealing.1001 = subset(stealing, STATE == "100 1")
table(stealing.1001$EVENT_CD)
with(stealing.1001, table(NEW.STATE))
mean(stealing.1001$RUNS.VALUE)

# Run Value of a Double
d.double = subset(data2011, EVENT_CD == 21)
library(MASS)
truehist(d.double$RUNS.VALUE)
table(d.double$STATE)
subset(d.double, d.double$RUNS.VALUE == max(d.double$RUNS.VALUE))[ , c("STATE","NEW.STATE","RUNS.VALUE")] #largest run value
subset(d.double, d.double$RUNS.VALUE == min(d.double$RUNS.VALUE))[ , c("STATE","NEW.STATE","RUNS.VALUE")] #smallest run value
mean.double = mean(d.double$RUNS.VALUE)
mean.double
d.double
abline(v = mean.double, lwd=3)
text(.5, 5, "Mean Runs Value", pos=4)

# Run Value of a Triple
d.triple = subset(data2011, EVENT_CD == 22)
library(MASS)
truehist(d.triple$RUNS.VALUE)
table(d.triple$STATE)
subset(d.triple, d.triple$RUNS.VALUE == max(d.triple$RUNS.VALUE))[ , c("STATE","NEW.STATE","RUNS.VALUE")] #largest run value
subset(d.triple, d.triple$RUNS.VALUE == min(d.triple$RUNS.VALUE))[ , c("STATE","NEW.STATE","RUNS.VALUE")] #smallest run value
mean.triple = mean(d.triple$RUNS.VALUE)
mean.triple
d.triple
abline(v = mean.triple, lwd=3)
text(.5, 5, "Mean Runs Value", pos=4)

# Proper weightage of base hits: Single: 0.4424277, Double: 0.735907, Triple: 1.064471, Homerun: 1.392393

# Value of Different Ways of Reaching First Base

situation.100 = subset(data2011, STATE == "100 0" | STATE == "100 1"  | STATE == "100 2")
s.single = subset(situation.100, EVENT_CD == 20)
library(MASS)
truehist(s.single$RUNS.VALUE)
table(s.single$STATE)
subset(s.single, s.single$RUNS.VALUE == max(s.single$RUNS.VALUE))[ , c("STATE","NEW.STATE","RUNS.VALUE")] #largest run value
subset(s.single, s.single$RUNS.VALUE == min(s.single$RUNS.VALUE))[ , c("STATE","NEW.STATE","RUNS.VALUE")] #smallest run value
mean.single = mean(s.single$RUNS.VALUE)
mean.single # 0.439317
abline(v = mean.single, lwd=3)

s.hbp = subset(situation.100, EVENT_CD == 16)
library(MASS)
truehist(s.hbp$RUNS.VALUE)
table(s.hbp$STATE)
subset(s.hbp, s.hbp$RUNS.VALUE == max(s.hbp$RUNS.VALUE))[ , c("STATE","NEW.STATE","RUNS.VALUE")] #largest run value
subset(s.hbp, s.hbp$RUNS.VALUE == min(s.hbp$RUNS.VALUE))[ , c("STATE","NEW.STATE","RUNS.VALUE")] #smallest run value
mean.hbp = mean(s.hbp$RUNS.VALUE)
mean.hbp # 0.3825492
abline(v = mean.hbp, lwd=3)

s.bb = subset(situation.100, EVENT_CD == 14)
library(MASS)
truehist(s.bb$RUNS.VALUE)
table(s.bb$STATE)
subset(s.bb, s.bb$RUNS.VALUE == max(s.bb$RUNS.VALUE))[ , c("STATE","NEW.STATE","RUNS.VALUE")] #largest run value
subset(s.bb, s.bb$RUNS.VALUE == min(s.bb$RUNS.VALUE))[ , c("STATE","NEW.STATE","RUNS.VALUE")] #smallest run value
mean.bb = mean(s.bb$RUNS.VALUE)
mean.bb # 0.364487
abline(v = mean.bb, lwd=3)

# Single > HPB > BB

# Comparing Two Players with Similar OBPS
rickie.id = subset(roster2011, First.Name == "Rickie" & Last.Name == "Weeks")$Player.ID
rickie.id = as.character(rickie.id)
rickie = subset(data2011, BAT_ID == rickie.id)
rickie = subset(rickie,BAT_EVENT_FL == TRUE)
# Base Scenarios of Rickie's 2011 season
rickie$RUNNERS = substr(rickie$STATE, 1, 3)
table(rickie$RUNNERS)
# Rickie Weeks' Performance with Baserunner Oppurtunites
with(rickie,stripchart(RUNS.VALUE ~ RUNNERS, vertical = TRUE, jitter = 0.2, xlab = "RUNNERS", method = "jitter", pch = 1, cex = 0.8, main = "2011 Rickie Weeks' Performance with Baserunner Oppurtunites", ylim = c(-2,5)))
abline(h=0)
A.runs = aggregate(rickie$RUNS.VALUE, list(rickie$RUNNERS), sum)
names(A.runs)[2] = "RUNS"
A.PA = aggregate(rickie$RUNS.VALUE, list(rickie$RUNNERS), length)
names(A.PA)[2] = "PA"
A = merge(A.PA, A.runs)
A
sum(A$RUNS)

michael.id= subset(roster2011, First.Name == "Michael" & Last.Name == "Bourne")$Player.ID
michael.id = as.character("bourm001")
michael = subset(data2011, BAT_ID == michael.id)
michael = subset(michael,BAT_EVENT_FL == TRUE)
# Base Scenarios of Michael's 2011 season
michael$RUNNERS = substr(michael$STATE, 1, 3)
table(michael$RUNNERS)
# Michael Bourne's Performance with Baserunner Oppurtunites
with(michael,stripchart(RUNS.VALUE ~ RUNNERS, vertical = TRUE, jitter = 0.2, xlab = "RUNNERS", method = "jitter", pch = 1, cex = 0.8, main = "2011 Michael Bourne' Performance with Baserunner Oppurtunites", ylim = c(-2,5)))
abline(h=0)
A.runs = aggregate(michael$RUNS.VALUE, list(michael$RUNNERS), sum)
names(A.runs)[2] = "RUNS"
A.PA = aggregate(michael$RUNS.VALUE, list(michael$RUNNERS), length)
names(A.PA)[2] = "PA"
A = merge(A.PA, A.runs)
A
sum(A$RUNS)

# Michael Bourne had more at bats with atleast 1 runner on base, but Rickie's run value with the bases empty was far greater than that of Bourne's, which shows rickie had a higher SLG. 


