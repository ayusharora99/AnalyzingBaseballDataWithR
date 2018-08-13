# Run Expectancy Matrix for the 2017 Season

data2017 = read.csv("~/Desktop/Analyzing Baseball Data with R/all2017.csv", header=FALSE)
fields = read.csv("~/Desktop/Analyzing Baseball Data with R/baseball_R-master/_old/test/fields.csv")

names(data2017) = fields[ , "Header"]

data2017SF = subset(data2017, substr(data2017$GAME_ID,1,3) == "SFN")

data2017SF$RUNS = with(data2017SF, AWAY_SCORE_CT + HOME_SCORE_CT)
data2017SF$HALF.INNING = with(data2017SF, paste(GAME_ID, INN_CT, BAT_HOME_ID))
data2017SF$RUNS.SCORED = with(data2017SF, (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))

RUNS.SCORED.INNING = aggregate(data2017SF$RUNS.SCORED, list(HALF.INNING = data2017SF$HALF.INNING), sum)
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

data2017SFC = subset(data2017SF, BAT_EVENT_FL == TRUE)

library(car)
data2017SFC$NEW.STATE = recode(data2017SFC$NEW.STATE,
                     "c('000 3', '100 3', '010 3', '001 3',
                 '110 3', '101 3', '011 3', '111 3') = '3'")

# Computing the transition probabilites 
T.matrix = with(data2017SFC, table(STATE,NEW.STATE))
T.matrix

P.matrix = prop.table(T.matrix,1)
P.matrix

P.matrix = rbind(P.matrix,c(rep(0,24),1))
P.matrix

# Transition probabilities from no runners, no outs
P1 = round(P.matrix["000 0", ], 3)
data.frame(Prob=P1[P1>0])

# Transition probabilities from runner on 2nd, 2 outs
P2 = round(P.matrix["010 2",],3)
data.frame(Prob=P2[P2>0])

# Simulating the Markov Chain
# Runs Scored = (sum of runners and outs before the play) - (sum of runners and outs after the play) + 1

count.runners.outs = function(s)
  sum(as.numeric(strsplit(s,"")[[1]]), na.rm = TRUE)
  runners.outs = sapply(dimnames(T.matrix)[[1]], count.runners.outs)[-25]
  R = outer(runners.outs + 1, runners.outs, FUN="-")
  dimnames(R)[[1]] = dimnames(T.matrix)[[1]][-25]
  dimnames(R)[[2]] = dimnames(T.matrix)[[1]][-25]
  R = cbind(R, rep(0, 24))

# simulate Markov Chain -- replicate runs expectancy table

simulate.half.inning = function(P, R, start=1){
  s = start; path = NULL; runs = 0
  while(s < 25){
    s.new = sample(1:25, 1, prob = P[s, ])
    path = c(path, s.new)
    runs = runs + R[s, s.new]
    s = s.new
  }
  runs
}

simulate.half.inning(P.matrix,R,1)

RUNS = replicate(10000, simulate.half.inning(T.matrix, R))
table(RUNS)

# Chance of scoring 4 or more runs
sum(RUNS[RUNS >=4]/10000)

# Mean Number of runs scored per half-inning
mean(RUNS)


RUNS.j = function(j){
  mean(replicate(10000, simulate.half.inning(T.matrix, R, j)))
}

RUNS.j = function(j){
  mean(replicate(10000, simulate.half.inning(T.matrix, R, j)))
}
Runs.Expectancy = sapply(1:24, RUNS.j)
Runs.Expectancy = t(round(matrix(Runs.Expectancy, 3, 8), 2))
dimnames(Runs.Expectancy)[[2]] = c("0 outs", "1 out", "2 outs")
dimnames(Runs.Expectancy)[[1]] = c("000", "001", "010", "011", "100", "101", 
                                    "110", "111")
Runs.Expectancy

# Beyond Runs Expectancy

# State of inning after 3 plate appearancees

P.matrix.3 = P.matrix %*% P.matrix %*% P.matrix 
sorted.P = sort(round(P.matrix.3["000 0", ], 3), decreasing = TRUE)
head(data.frame(Prob = sorted.P))

Q <- P.matrix[-25, -25]
N <- solve(diag(rep(1, 24)) - Q)

N.0000 <- round(N["000 0", ], 2)
head(data.frame(N = N.0000))

# Average # of PA's in a half-inning
sum(N.0000)

# Average # of PA's in each starting state
Length = round(t(N %*% rep(1,24)),2)
data.frame(L=Length[1,1:8])

# Simulating a Baseball Season

# Making up a Schedule 
make.schedule = function(teams,k){
  n.teams = length(teams)
  Home = rep(gl(n.teams,n.teams,length=n.teams^2,labels=teams),k)
  Visitor = rep(gl(n.teams,1,length=n.teams^2,label=teams), k)
  schedule = data.frame(Home=Home,Visitor=Visitor)
  subset(schedule, Home != Visitor)
}

NL = c("ATL","CHN","CIN","HOU","LAN","NYN","PHI","PIT","SFN","SLN")
AL = c("BAL","BOS","CAL","CHA","CLE","DET","MIN","NYA","OAK","WS2")
teams = c(NL,AL)
league = c(rep(1,10),rep(2,10))
schedule = rbind(make.schedule(NL,9),make.schedule(AL,9))
schedule

# Simulating Talents and Computing Win Porbabilities 
s.talent = 0.20
talents = rnorm(20,0,s.talent)
TAL = data.frame(Team=teams,League=league,Talent=talents)
SCH = merge(schedule,TAL,by.x="Home",by.y="Team")
names(SCH)[4] = "Talent.Home"
SCH = merge(SCH,TAL,by.x="Visitor",by.y="Team")
names(SCH)[6] = "Talent.Visitor"
SCH$prob.Home = with(SCH, exp(Talent.Home) / (exp(Talent.Home) + exp(Talent.Visitor)))
head(SCH)

# Simulating the Regular Season
SCH$outcome = with(SCH,rbinom(nrow(SCH),1,prob.Home))
SCH$winner = with(SCH, ifelse(outcome,as.character(Home),as.character(Visitor)))
head(SCH[,c("Visitor","Home","prob.Home","outcome","winner")])

wins = table(SCH$winner)
WIN = data.frame(Team=names(wins),Wins=as.numeric(wins))
RESULTS = merge(TAL,WIN)
RESULTS

# Simulating The Post-Season
win.league = function(RR,league){
  wins = RR$Wins * (RR$League == league)
  MAX = max(wins)
  if(sum(wins == MAX) > 1){
    prob = exp(RR$Talent) * (wins == MAX)
    outcome = c(rmultinom(1,1,prob))
    RR$Winner.Lg = RR$Winner.Lg + outcome
  }
  if(sum(wins == MAX) == 1){
    RR$Winner.Lg = RR$Winner.Lg + as.numeric(wins == MAX)}
    RR
  }
RESULTS$Winner.Lg = 0
RESULTS$Winner.WS = 0
for(j in 1:2){
  RESULTS = win.league(RESULTS,j)}
teams = (1:20)[RESULTS$Winner.Lg ==1]
outcome = c(rmultinom(1,7,exp(RESULTS$Talent)[teams]))
winner = teams[1] * (diff(outcome)<0) + teams[2] * (diff(outcome)>0)
RESULTS$Winner.WS[winner] = 1
RESULTS

display.standings <- function(RESULTS, league){
  Standings <- subset(RESULTS, League == league)[, c("Team", "Wins")]
  Standings$Losses <- 162 - Standings$Wins
  Standings[order(Standings$Wins, decreasing=TRUE), ]
}

cbind(display.standings(RESULTS, 1), display.standings(RESULTS, 2))

with(RESULTS, as.character(Team[Winner.Lg == 1]))

with(RESULTS, as.character(Team[Winner.WS == 1]))

RESULTS
