Edge_Calls_82018 <- read.csv("~/Downloads/Edge_Calls_82018")
dim(Edge_Calls_82018)
names(Edge_Calls_82018)

# Setting the left and right border of a stike zone
sz_right = 0.833
sz_left = -0.833

# Numbers aren't meaning full beacause dataset includes high and low pitch calls
mean(Edge_Calls_82018$plate_z)
mean(Edge_Calls_82018$plate_x)

# Creating subsets for balls & strikes
Edge_Calls_82018_Strikes = subset(Edge_Calls_82018, description == "called_strike")
Edge_Calls_82018_Balls = subset(Edge_Calls_82018, description == "ball")
dim(Edge_Calls_82018_Strikes)
dim(Edge_Calls_82018_Balls)

# Check if plate_x within sz_right & sz_left first & if plate_z within sz_bot & sz_top for Strikes
summary(Edge_Calls_82018_Strikes$plate_x < 0.833 & Edge_Calls_82018_Strikes$plate_x > -0.833 & Edge_Calls_82018_Strikes$plate_z > Edge_Calls_82018_Strikes$sz_bot & Edge_Calls_82018_Strikes$plate_z < Edge_Calls_82018_Strikes$sz_top)

# Check if plate_x is NOT within sz_right & sz_left & if plate_z is NOT within sz_bot & sz_top for Strikes
summary((Edge_Calls_82018_Balls$plate_x > 0.833 | Edge_Calls_82018_Balls$plate_x < -0.833) | (Edge_Calls_82018_Balls$plate_z < Edge_Calls_82018_Balls$sz_bot | Edge_Calls_82018_Balls$plate_z > Edge_Calls_82018_Balls$sz_top))


