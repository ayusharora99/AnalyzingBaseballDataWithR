# Hunter Wendelstedt's Pitch Calling for DET-CWS on 8/26/18

library(e1071)
library(caret)
library(rpart)
library(randomForest)
HunterWendelstedt <- read.csv("~/Downloads/HunterWendelstedt.csv")
dim(HunterWendelstedt)
names(HunterWendelstedt)
is.na(HunterWendelstedt)
colSums(is.na(HunterWendelstedt)) 
HunterWendelstedt = HunterWendelstedt[,colSums(is.na(HunterWendelstedt)) == 0] 
dim(HunterWendelstedt)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
HunterWendelstedt = HunterWendelstedt[ , !(names(HunterWendelstedt) %in% drops)]
HunterWendelstedt = HunterWendelstedt[keep]
dim(HunterWendelstedt)
HunterWendelstedt

# Split Testing(28) & Training(108) data
HunterWendelstedt_train = HunterWendelstedt[0:108,]
dim(HunterWendelstedt_train)
HunterWendelstedt_test = HunterWendelstedt[109:136,]
dim(HunterWendelstedt_test)

# Naive Bayes to Predict Umpire's Call 
NB_model = naiveBayes(type~., data = HunterWendelstedt_train)
names(HunterWendelstedt)
prediction <-predict(NB_model, HunterWendelstedt_test[,-17])
table(prediction, HunterWendelstedt_test[,17])
confusionMatrix(prediction,HunterWendelstedt_test[,17])
# 75% Accuracy, 85.71% Precision, 70.59% Recall w/ Keeps
# 50% Accuracy, 100% Precision,  17.64% Recall w/ Drops


# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = HunterWendelstedt_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction<-predict(tree_model, newdata = HunterWendelstedt_test, type = 'class')
table(prediction, HunterWendelstedt_test$type)
confusionMatrix(prediction,HunterWendelstedt_test$type)
# 78.57% Accuracy, 82.35% Precision, 82.35% Recall w/ Keeps
# 100% in ALL w/ Drops

# Random Forest 
names(HunterWendelstedt)
drops = c("pos1_person_id","pos2_person_id.1","pos3_person_id","pos4_person_id","pos5_person_id","pos6_person_id","pos7_person_id","pos8_person_id","pos9_person_id","at_bat_number","pitch_name","bat_score","fld_score","post_away_score","post_home_score","post_bat_score","post_fld_score","if_fielding_alignment","of_fielding_alignment")
HunterWendelstedt = HunterWendelstedt[ , !(names(HunterWendelstedt) %in% drops)]
dim(HunterWendelstedt)
HunterWendelstedt_train = HunterWendelstedt[0:108,]
dim(HunterWendelstedt_train)
HunterWendelstedt_test = HunterWendelstedt[109:136,]
dim(HunterWendelstedt_test)


bestmtry = tuneRF(HunterWendelstedt_train,HunterWendelstedt_train$type,stepFactor = 1.2, improve = 0.01,trace = T,plot =T)
HunterWendelstedt_forest = randomForest(type~.,data = HunterWendelstedt_train)
HunterWendelstedt_forest
importance(HunterWendelstedt_forest)
varImpPlot(HunterWendelstedt_forest)
predict_HunterWendelstedt = predict(HunterWendelstedt_forest, newdata = HunterWendelstedt_test, type = "class")
predict_HunterWendelstedt 
confusionMatrix(table(predict_HunterWendelstedt,HunterWendelstedt_test$type))
# 85.71% Accuracy, 88.23% Precision, 88.23% Recall w/ Keeps


# Kmeans Clustering
# https://www.youtube.com/watch?v=4R8nWDh-wA0&index=5&list=PLaozD487rOR_LgH8ObhA2-ns2ijiB-uaf 

