# Run Expectancy Matrix for the 2017 Season

data2017 <- read.csv("~/Desktop/Analyzing Baseball Data with R/all2017.csv", header=FALSE)
names(data2017) = fields[ , "Header"]
data2017SF = subset(data2017, substr(data2017$GAME_ID,1,3) == "SFN")

data2017SF$RUNS = with(data2017SF, AWAY_SCORE_CT + HOME_SCORE_CT)
data2017SF$HALF.INNING = with(data2017SF, paste(GAME_ID, INN_CT, BAT_HOME_ID))
data2017SF$RUNS.SCORED = with(data2017SF, (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))
RUNS.SCORED.INNING <- aggregate(data2017SF$RUNS.SCORED, list(HALF.INNING = data2017SF$HALF.INNING), sum)
RUNS.SCORED.START = aggregate(data2017SF$RUNS, list(HALF.INNING=data2017SF$HALF.INNING), "[",1)

MAX = data.frame(HALF.INNING = RUNS.SCORED.START$HALF.INNING)
MAX$x = RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
data2017SF = merge(data2017SF, MAX)
N = ncol(data2017SF)
names(data2017SF)[N] = "MAX.RUNS"

data2017SF$RUNS.ROI = with(data2017SF, MAX.RUNS - RUNS)

RUNNER1 = ifelse(as.character(data2017SF[ , "BASE1_RUN_ID"]) == "", 0,1)
RUNNER2 = ifelse(as.character(data2017SF[ , "BASE2_RUN_ID"]) == "", 0,1)
RUNNER3 = ifelse(as.character(data2017SF[ , "BASE3_RUN_ID"]) == "", 0,1)

get.state = function(runner1,runner2,runner3,outs){
  runners = paste(runner1, runner2, runner3, sep = "")
  paste(runners,outs)
}

data2017SF$STATE = get.state(RUNNER1, RUNNER2, RUNNER3, data2017SF$OUTS_CT)

NRUNNER1 = with(data2017SF, as.numeric(RUN1_DEST_ID == 1 | BAT_DEST_ID == 1))
NRUNNER2 = with(data2017SF, as.numeric(RUN1_DEST_ID == 2 | RUN2_DEST_ID == 2 | BAT_DEST_ID == 2))
NRUNNER3 = with(data2017SF, as.numeric(RUN1_DEST_ID == 3 | RUN2_DEST_ID ==3 | RUN3_DEST_ID == 3 | BAT_DEST_ID == 3))
NOUTS = with(data2017SF, OUTS_CT + EVENT_OUTS_CT)
data2017SF$NEW.STATE = get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)

data2017SF = subset(data2017SF, (STATE != NEW.STATE) | (RUNS.SCORED > 0))

library(plyr)
data.outs = ddply(data2017SF, .(HALF.INNING), summarize, Outs.Inning = sum(EVENT_OUTS_CT))
data2017SF = merge(data2017SF, data.outs)
data2017SFC = subset(data2017SF, Outs.Inning ==3)

RUNS = with(data2017SFC, aggregate(RUNS.ROI, list(STATE), mean))
RUNS$Outs = substr(RUNS$Group, 5, 5)
RUNS = RUNS[order(RUNS$Outs), ]
RUNS.out = matrix(round(RUNS$x,2),8,3)
dimnames(RUNS.out)[[2]] = c("0 outs", "1 out", "2 outs")
dimnames(RUNS.out)[[1]] = c("000","001","010","011","100","101","110","111")
RUNS.out
# Measuring Succes of a Batting Play
RUNS.POTENTIAL = matrix(c(RUNS$x, rep(0, 8)), 32, 1)
dimnames(RUNS.POTENTIAL)[[1]] = c(RUNS$Group, "000 3","001 3","010 3","011 3","100 3","101 3","110 3","111 3") 
data2017SF$RUNS.STATE = RUNS.POTENTIAL[data2017SF$STATE, ]                        
data2017SF$RUNS.NEW.STATE = RUNS.POTENTIAL[data2017SF$NEW.STATE,]
data2017SF$RUNS.VALUE = data2017SF$RUNS.NEW.STATE - data2017SF$RUNS.STATE + data2017SF$RUNS.SCORED
RUNS.POTENTIAL
