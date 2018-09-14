# __DATE__ : Giants vs __OPPONENT__ : __UMPIRE__

# __NUMBER OF__ pitches were called strikes/balls

# The robot-ump called __#__ of those pitches as called strikes & __#__ as balls

# __UMPIRE__ called __#__ of those pitches as called strikes & __#__ as balls 

# Accuracy: __%

# File --> Import Dataset --> From text (base) (YES TO HEADER) & import csv file downloaded from https://baseballsavant.mlb.com/statcast_search
Umpire_Name <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Umpire_Name_8:27.18.csv", header=TRUE)

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Umpire_Name)
names(Umpire_Name)
is.na(Umpire_Name)
colSums(is.na(Umpire_Name)) 
Umpire_Name = Umpire_Name[,colSums(is.na(Umpire_Name)) == 0] 
dim(Umpire_Name)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Umpire_Name = Umpire_Name[ , !(names(Umpire_Name) %in% drops)]
dim(Umpire_Name)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Umpire_Name_train = Umpire_Name[0:(0.8 * nrow(Umpire_Name)),]
dim(Umpire_Name_train)
prop.table(table(Umpire_Name_train$type))
Umpire_Name_test = Umpire_Name[(0.8*nrow(Umpire_Name)):nrow(Umpire_Name),]
dim(Umpire_Name_test)
prop.table(table(Umpire_Name_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Umpire_Name_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Umpire_Name_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Umpire_Name_test$type))

# Subset for Borderline Calls
Umpire_Name$Borderline = ifelse(((abs(Umpire_Name$plate_x)> 0.748) & (abs(Umpire_Name$plate_x)<0.914)) 
                               & (((Umpire_Name$plate_z > Umpire_Name$sz_top-0.83) & (Umpire_Name$plate_z < Umpire_Name$sz_top+0.83))
                                  | (((Umpire_Name$plate_z)<Umpire_Name$sz_bot+0.83) & ((Umpire_Name$plate_z) > Umpire_Name$sz_bot-0.83))), 'T','F')

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Umpire_Name_Strikes = subset(Umpire_Name, Umpire_Name$type == "S")
Umpire_Name_Balls = subset(Umpire_Name, Umpire_Name$type == "B")

# Borderline
Umpire_Name_Borderline = subset(Umpire_Name, Umpire_Name$Borderline == "T")


# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Umpire_Name_Strikes$AdjustedCall = ifelse((Umpire_Name_Strikes$plate_x < 0.833 & Umpire_Name_Strikes$plate_x > -0.833) & (Umpire_Name_Strikes$plate_z > Umpire_Name_Strikes$sz_bot & Umpire_Name_Strikes$plate_z < Umpire_Name_Strikes$sz_top), 'S', 'B')
table(Umpire_Name_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Umpire_Name_Balls$AdjustedCall = ifelse((Umpire_Name_Balls$plate_x > 0.833 | Umpire_Name_Balls$plate_x < -0.833)|(Umpire_Name_Balls$plate_z < Umpire_Name_Balls$sz_bot | Umpire_Name_Balls$plate_z > Umpire_Name_Balls$sz_top),'B','S')
table(Umpire_Name_Balls$AdjustedCall)

# Borderline
Umpire_Name_Borderline$AdjustedCall = ifelse((Umpire_Name_Borderline$plate_x < 0.833 & Umpire_Name_Borderline$plate_x > -0.833) & (Umpire_Name_Borderline$plate_z > Umpire_Name_Borderline$sz_bot & Umpire_Name_Borderline$plate_z < Umpire_Name_Borderline$sz_top), 'S', 'B')

# Merge to create new dataset
Umpire_Name_AdjustedCalls = rbind(Umpire_Name_Strikes,Umpire_Name_Balls)
Umpire_Name_AdjustedCalls$OnFieldRuling = ifelse(Umpire_Name_AdjustedCalls$type == "S","S","B")

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Umpire_Name)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Umpire_Name_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Umpire_Name_AdjustedCalls$AdjustedCall))

# Borderline
Prediction_BORDERLINE<-predict(tree_model, newdata = Umpire_Name_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,Umpire_Name_Borderline$AdjustedCall))

# Correct vs InCorrect Call

# Correct Calls
Umpire_Name_AdjustedCalls$Call = ifelse( ((Umpire_Name_AdjustedCalls$type == 'B') & ( (Umpire_Name_AdjustedCalls$AdjustedCall == "B") | (Umpire_Name_AdjustedCalls$Borderline == "T") ) ), "C","I" )
Umpire_Name_AdjustedCalls$Call = ifelse( ((Umpire_Name_AdjustedCalls$type == 'S') & ((Umpire_Name_AdjustedCalls$AdjustedCall == "S") | (Umpire_Name_AdjustedCalls$Borderline == "T") ) ), "C","I")

# InCorrect Calls
Umpire_Name_AdjustedCalls$Call = ifelse( ( (Umpire_Name_AdjustedCalls$type == 'B') & ((Umpire_Name_AdjustedCalls$AdjustedCall == "S") & (Umpire_Name_AdjustedCalls$Borderline == "F") ) ), "I","C")
Umpire_Name_AdjustedCalls$Call = ifelse( ( (Umpire_Name_AdjustedCalls$type == 'S') & ((Umpire_Name_AdjustedCalls$AdjustedCall == "B") & (Umpire_Name_AdjustedCalls$Borderline == "F") ) ), "I","C")

table(Umpire_Name_AdjustedCalls$Call)
View(Umpire_Name_AdjustedCalls)
