# Hall of Fame Batters
hofbatting$MidCareer = with(hofbatting, (From + To) / 2)
hofbatting$Era = cut(hofbatting$MidCareer, breaks = c(1800, 1900, 1919, 1941, 1960, 1976, 1993, 2050), labels = c("19th Century", "Dead Ball", "Lively Ball", "Integration", "Expansion", "Free Agency", "Long Ball"))
T.Era = table(hofbatting$Era)
T.Era
barplot(T.Era, xlab = "Era", ylab = "Frequency", main = "Era of the Batting Hall of Famers")

pie(T.Era)

dotchart(as.numeric(T.Era), labels = names(T.Era), xlab = "Frequency")

hofbatting.500 = subset(hofbatting, HR >= 500)
hofbatting.500 = hofbatting.500[order(hofbatting.500$OPS), ]
dotchart(hofbatting.500$OPS, labels = hofbatting.500$X2, xlab = "OPS")

stripchart(hofbatting$MidCareer, method = "jitter", pch = 1, xlab = "Mid Career")
hist(hofbatting$MidCareer, xlab = "Mid Career", main = "", breaks = seq(1880,2000, by=10))

# Relationship between a player's OPS & Baseball Era
with(hofbatting,plot(MidCareer, OPS))
with(hofbatting, lines(lowess(MidCareer,OPS,f=0.3)))
with(hofbatting,identify(MidCareer,OPS,X2,n=4))

# Relationship between a player's OBP & SLG
with(hofbatting, plot(OBP,SLG))
with(hofbatting, plot(OBP, SLG, xlim = c(0.25,0.50), ylim = c(0.28,0.75), pch = 19, xlab = "On-Base Percentage", ylab = "Slugging Percentage"))
curve(.7-x, add= TRUE)
text(.27,.42, "OPS = 0.7")
curve(.8-x,add= TRUE)
text(.27,.52, "OPS = 0.8")
curve(.9-x,add= TRUE)
text(.27,.62, "OPS = 0.9")
curve(1.0-x,add = TRUE)
text(.27,.72, "OPS = 1.0")
with(hofbatting, identify(OBP,SLG,X2, n=6))

# Change in Homerun Frequency by Baseabll Era
hofbatting$HR.Rate = with(hofbatting, HR/AB)
stripchart(HR.Rate ~ Era, data = hofbatting)
par(plt = c(.2,.94,.145,.883))
stripchart(HR.Rate ~ Era, data=hofbatting, method = "jitter", pch = 1, las =2, xxlab = "HR RATE")           
boxplot(HR.Rate ~ Era, data = hofbatting, las = 2, horizonatal = FALSE, xlab = "HR RATE")

# 1998 Home Run Race between Sammy Sosa & Mark McGwire
names(all1998) = fields[,"Header"]
sosa.id = as.character(subset(retrosheetIDs, FIRST == "Sammy" & LAST == "Sosa")$ID)
mac.id = as.character(subset(retrosheetIDs, FIRST == "Mark" & LAST == "McGwire")$ID)
sosa.data = subset(all1998, BAT_ID = sosa.id)
mac.data = subset(all1998, BAT_ID = mac.id)

createdata = function(d){
  d$Date = as.Date(substr(d$GAME_ID,4,11),format="%Y%m%d")
  d = d[order(d$Date),]
  d$HR = ifelse(d$EVENT_CD == 23,1,0)
  d$cumHR = cumsum(d$HR)
  d[,c("Date", "cumHR")]
}

mac.hr = createdata(mac.data)
sosa.hr = createdata(sos.data)
plot(mac.hr, type = "1", lwd=2, ylab = "Home Rruns in the Season")
lines(sosa.hr,lwd=2, col = "grey")

# Hall of Fame Pitchers
hofpitching$BF.group = with(hofpitching, cut(BF, c(0,10000,15000,200000,30000), labels = c("Less than 1000","(10000,15000)","(15000,20000)","more than 20000")))
# Frequency table of BF.group using table function
P.group = table(hofpitching$BF.group)
P.group
# Bar Graph
barplot(P.group, xlab = "Groups", ylab = "Frequency", main = "Batters Faced by Hall of Fame Pitchers")
# Pie Graph
pie(P.group)
# Histogram of WAR
hist(hofpitching$WAR, xlab = "WAR", main = "", breaks = seq(0,200,by = 10))

hofpitching$WAR.Season = with(hofpitching, WAR/Yrs)
stripchart(hofpitching$WAR.Season, method = "jitter", pch = 1, xlab = "Seasonal WAR")

hofpitching$MidYear = with(hofpitching, (From + To) / 2)
hofpitching.recent = subset(hofpitching, MidYear >= 1960)
hofpitching.recent = hofpitching.recent[order(hofpitching.recent$WAR.Season), ]
dotchart(as.numeric(hofpitching.recent$WAR.Season), labels = names(hofpitching.recent$X2), xlab = "Frequency")
with(hofpitching.recent,identify(WAR.Season,X2,n=2))

with(hofpitching,plot(MidYear, WAR.Season))
with(hofpitching,identify(WAR.Season,X2,n=2))


