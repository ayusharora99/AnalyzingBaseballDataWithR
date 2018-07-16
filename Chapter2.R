# Career of Warren Spahn 
W = c(8,21,15,21,21,22,14)
L = c(5,10,12,14,17,14,19)

Win.Pct = 100 * (W / (W+L) )

Year = 1946 : 1952

Age = Year - 1921

plot(Age, Win.Pct)

mean(Win.Pct)
sd(Win.Pct)
length(Win.Pct)
sum(Win.Pct)
max(Win.Pct)
sort(Win.Pct)
Win.Pct
order(Win.Pct)

summary(Win.Pct)

W[c(1,2,5)]

W[1:4]

W[-c(1,2,5)]

Win.Pct > 65
Win.Pct[2]

(W > 20) & (Win.Pct > 65)
W[2]
Win.Pct[2]

W == max(W)

Age[Win.Pct == max(Win.Pct)]
Year[Win.Pct == max(Win.Pct)]

# Analysis on 2003-2012 World Series
NL = c("FLA","STL","HOU","STL","COL","PHI","PHI","SFG","STL","SFG")
AL = c("NYY","BOS","CHW","DET","BOS","TBR","NYY","TEX","TEX","DET")

Winner = c("NL","AL","AL","NL","NL","NL","AL","NL","NL","NL")

N.games = c(6,4,4,5,4,5,6,5,7,4)

Year = c(2003:2012)

results = matrix(c(NL,AL),10,2)
results

dimnames(results)[[1]] = Year
dimnames(results)[[2]] = c("NL", "AL")
results

barplot(table(NL))
barplot(table(AL))

World.Series = list(Winner = Winner, Number.Games = N.games, Seasons = "2003 to 2012")
World.Series["N.games"]
N.games
World.Series$Number.Games

table(Winner)
barplot(table(Winner))
by(N.games,Winner,summary)

# Career Homeruns Calculator
hr.rates = function(age,hr,ab){
  rates = round(100 * hr / ab, 1)
  list(x=age, y=rates)
}

HR = c(13,23,21,27,37,52,34,42,31,40,54)
AB = c(341,549,461,543,517,533,474,519,541,527,514)
Age = 19:29

plot(hr.rates(Age,HR,AB))

getwd()

# More on Warren Spahn
spahn1[ , c("IP","SO","SO/9","ERA+")]

summary(spahn$ERA)

#FIP
spahn1$FIP = with(spahn1, (13*HR + 3*BB - 2*SO) / IP)

spahn1[ , c("ERA","FIP")]

pos = order(spahn1$FIP)
head(spahn1[pos, c("Year","Age","W","L","ERA","FIP")])

# Splits: Red Sox Career vs Brewers Career
spahn2 = subset(spahn1, Tm == "BSN" | Tm == "MLN")
spahn2$Tm = factor(spahn2$Tm,levels = c("BSN","MLN"))

by(spahn2[ , c("W-L","ERA","WHIP","FIP")], spahn2$Tm,summary)
spahn2$Tm: "BSN"
spahn2$Tm: "MLN"

# 2011 Season Analysis
batting = rbind(NLbatting,ALbatting)

NL = merge(NLbatting,NLpitching, by = "Tm")
NL

NL.150 = subset(NLbatting, HR > 150)
NL.150

library(Lahman)
?Batting

# Who hit the most homeruns in 1960's ?
Batting.60 = subset(Batting,yearID >= 1960 & yearID <= 1969)
Batting.60

compute.hr = function(pid){
  d = subset(Batting.60, playerID == pid)
  sum(d$HR)
}

players = unique(Batting.60$playerID)
S = sapply(players, compute.hr)
S

R = data.frame(Players = players, HR = S)
R = R[order(R$HR, decreasing=TRUE), ]
head(R)

# Collecting career at-bats, homeruns, and strikeouts for all players in baseball history with atleast 5000 career at-bats
library(plyr)
dataframe.AB = ddply(Batting, .(playerID), summarize, Career.AB = sum(AB,na.rm=TRUE))

Batting = merge(Batting,dataframe.AB,by="playerID")
Batting.500 = subset(Batting, Career.AB >= 5000)

ab.hr.so = function(d){
  c.AB = sum(d$AB, na.rm=TRUE)
  c.HR = sum(d$HR, na.rm=TRUE)
  c.SO = sum(d$SO, na.rm=TRUE)
  data.frame(AB = c.AB, HR = c.HR, SO = c.SO)
}

aaron = subset(Batting.500, playerID == "aaronha01")
ab.hr.so(aaron)
d.5000 = ddply(Batting.500, .(playerID), ab.hr.so)
head(d.5000)

# Is there an association between a player's home run rate and his strikeout rate?
with(d.5000, plot(HR/AB,SO/AB))
with(d.5000, lines(lowess(HR/AB,SO/AB)))

# SB,GS, and G for 9 players inducted in the Hall of Fame
SB = c(1406,938,897,741,738,689,506,504,474)
CS = c(335,307,212,195,109,162,136,131,114)
G = c(3081,2616,3034,2826,2476,2649,2599,2683,2379)
SB.Attempt = c(SB+CS)
Success.Rate = c(SB/SB.Attempt)
SB.Game = c(SB/G)
head(SB.Game)
max(SB.Game)
plot(SB.Game,Success.Rate)

# Recording the outcomes of a batter in 10 plate appearances 
outcomes = c("Single","Out","Out","Single","Out","Double","Out","Walk","Out","Single")
table(outcomes)
outcomes = factor(outcomes, levels = c("Out","Walk","Single","Double"))
table(outcomes)
sum(outcomes == "Walk")
sum(outcomes == "Single")
sum(outcomes == "Double")

# Pitchers in the 350 Wins Club
Wins = c(373,354,364,417,355,373,361,363,511)
Losses = c(208,184,310,279,227,188,208,245,316)
Name = c("Alexander","Clemens","Galvin","Johnson","Maddux","Mathewson","Nichols","Spahn","Young")
Win.Pct = (100* Wins / (Wins + Losses))
Win.Pct
Wins.350 = data.frame(Name,Wins,Losses,Win.Pct)
?order
Wins.350[order(Wins.350$Win.Pct, decreasing=TRUE), ]

SO = c(2198,4672,1806,3509,3371,2502,1868,2583,2803)
BB = c(951,1580,745,1363,999,844,1268,1434,1217)
Name = c("Alexander","Clemens","Galvin","Johnson","Maddux","Mathewson","Nichols","Spahn","Young")
SO.BB.Ratio = c(SO/BB)
SO.BB = data.frame(Name,SO,BB,SO.BB.Ratio)
Ratio = subset(SO.BB, SO.BB.Ratio > 2.8)
SO.BB[order(SO.BB$BB,decreasing=TRUE),]

# Pitcher Strikeout / Walk Ratios
stats = function(d){
  c.SO = sum(d$SO, na.rm=TRUE)
  c.BB = sum(d$BB, na.rm=TRUE)
  c.IPouts = sum(d$IPouts, na.rm=TRUE)
  c.midYear = median(d$yearID, na.rm=TRUE)
  data.frame(SO=c.SO,BB=c.BB,IPouts=c.IPouts,midYear=c.midYear)
}

library(plyr)
career.pitching = ddply(pitching, .(playerID), stats)
head(career.pitching)
Pitching = merge(pitching,career.pitching)
career.1000 = subset(Pitching, IPouts > 1000)
plot(career.1000$midYear,(career.1000$SO/career.1000$BB))
