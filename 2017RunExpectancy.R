all2017 <- read.csv("~/Desktop/Analyzing Baseball Data with R/all2017.csv", header=FALSE)

# Run Expectancy Matrix for the 2017 Season

all2017 <- read.csv("~/Desktop/Analyzing Baseball Data with R/baseball_R-master/data/all2011.csv", header=FALSE)
names(all2017) = fields[ , "Header"]

all2017$RUNS = with(all2017, AWAY_SCORE_CT + HOME_SCORE_CT)
all2017$HALF.INNING = with(all2017, paste(GAME_ID, INN_CT, BAT_HOME_ID))
all2017$RUNS.SCORED = with(all2017, (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))
RUNS.SCORED.INNING <- aggregate(all2017$RUNS.SCORED, list(HALF.INNING = all2017$HALF.INNING), sum)
RUNS.SCORED.START = aggregate(all2017$RUNS, list(HALF.INNING=all2017$HALF.INNING), "[",1)

MAX = data.frame(HALF.INNING = RUNS.SCORED.START$HALF.INNING)
MAX$x = RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
all2017 = merge(all2017, MAX)
N = ncol(all2017)
names(all2017)[N] = "MAX.RUNS"

all2017$RUNS.ROI = with(all2017, MAX.RUNS - RUNS)

RUNNER1 = ifelse(as.character(all2017[ , "BASE1_RUN_ID"]) == "", 0,1)
RUNNER2 = ifelse(as.character(all2017[ , "BASE2_RUN_ID"]) == "", 0,1)
RUNNER3 = ifelse(as.character(all2017[ , "BASE3_RUN_ID"]) == "", 0,1)

get.state = function(runner1,runner2,runner3,outs){
  runners = paste(runner1, runner2, runner3, sep = "")
  paste(runners,outs)
}

all2017$STATE = get.state(RUNNER1, RUNNER2, RUNNER3, all2017$OUTS_CT)

NRUNNER1 = with(all2017, as.numeric(RUN1_DEST_ID == 1 | BAT_DEST_ID == 1))
NRUNNER2 = with(all2017, as.numeric(RUN1_DEST_ID == 2 | RUN2_DEST_ID == 2 | BAT_DEST_ID == 2))
NRUNNER3 = with(all2017, as.numeric(RUN1_DEST_ID == 3 | RUN2_DEST_ID ==3 | RUN3_DEST_ID == 3 | BAT_DEST_ID == 3))
NOUTS = with(all2017, OUTS_CT + EVENT_OUTS_CT)
all2017$NEW.STATE = get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)

all2017 = subset(all2017, (STATE != NEW.STATE) | (RUNS.SCORED > 0))

library(plyr)
data.outs = ddply(all2017, .(HALF.INNING), summarize, Outs.Inning = sum(EVENT_OUTS_CT))
all2017 = merge(all2017, data.outs)
all2017C = subset(all2017, Outs.Inning ==3)

RUNS = with(all2017C, aggregate(RUNS.ROI, list(STATE), mean))
RUNS$Outs = substr(RUNS$Group, 5, 5)
RUNS = RUNS[order(RUNS$Outs), ]
RUNS.out = matrix(round(RUNS$x,2),8,3)
dimnames(RUNS.out)[[2]] = c("0 outs", "1 out", "2 outs")
dimnames(RUNS.out)[[1]] = c("000","001","010","011","100","101","110","111")
