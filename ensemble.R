# Random Forest
colnames(tour_2021_team_shooting)[colnames(tour_2021_team_shooting)=="90s"]="S90"
colnames(tour_2021_team_shooting)=gsub(" |/|:|-|%","_",colnames(tour_2021_team_shooting))
rf <- randomForest(Rank~.,tour_2021_team_shooting[,-1],
                   importance=TRUE)
varImpPlot(rf, main = "Feature Importance")
rf_shooting <- rf
rf_shooting

colnames(tour_2021_team_passing)[colnames(tour_2021_team_passing)=="90s"]="S90"
colnames(tour_2021_team_passing)[colnames(tour_2021_team_passing)=="1_3"]="One_Third"
colnames(tour_2021_team_passing)=gsub(" |/|:|-|%","_",colnames(tour_2021_team_passing))
rf_passing <- randomForest(Rank~.,tour_2021_team_passing[,-1],
                           importance=TRUE)
varImpPlot(rf_passing, main = "Feature Importance")
rf_passing

colnames(tour_2021_team_defense)[colnames(tour_2021_team_defense)=="90s"]="S90"
colnames(tour_2021_team_defense)[colnames(tour_2021_team_defense)=="Tkl+Int"]="Tkl_Int"
colnames(tour_2021_team_defense)=gsub(" |/|:|-|%","_",colnames(tour_2021_team_defense))
rf_defense <- randomForest(Rank~.,tour_2021_team_defense[,-1],
                           importance=TRUE)
varImpPlot(rf_defense, main = "Feature Importance")
rf_defense

colSums(is.na(tour_2021_team_goalkeeping))
tour_2021_team_goalkeeping$Pos[is.na(tour_2021_team_goalkeeping$Pos)] <- "GK"
tour_2021_team_goalkeeping<-tour_2021_team_goalkeeping[-24,]
tour_2021_team_goalkeeping[is.na(tour_2021_team_goalkeeping)] <- 0
colnames(tour_2021_team_goalkeeping)=gsub(" |/|:|-|%","_",colnames(tour_2021_team_goalkeeping))
rf_goalkeeping <- randomForest(Rank~.,tour_2021_team_goalkeeping[,-1],
                               importance=TRUE)
varImpPlot(rf_goalkeeping, main = "Feature Importance")
rf_goalkeeping
