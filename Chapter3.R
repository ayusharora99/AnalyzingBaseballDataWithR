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
