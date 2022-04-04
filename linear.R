library(dplyr)
#convert player-level to team level
num_col_shooting=setdiff(num_col_shooting,c('Nation','Born','Standard Sh/90','Standard SoT/90','Expected npxG','Expected np:G-xG','Year'))
num_col_passing=setdiff(num_col_passing,c('Nation','Born','Total Att','Total TotDist','Short Att','Medium Att','Year'))
num_col_goalkeeping=setdiff(num_col_goalkeeping,c('Nation','Born','Year','Playing Time MP','Playing Time Starts','Playing Time Min','Performance GA90','Performance CS%'))

num_col_mean=paste0("num_col_mean_",df.name)
num_col_sum=paste0("num_col_sum_",df.name)
tour_2021_sum=paste0("tour_2021_sum_",df.name)
tour_2021_mean=paste0("tour_2021_mean_",df.name)
tour_2021_team=paste0("tour_2021_team_",df.name)
df2021_i=c('tour_shooting_2021_i','tournament_passing_i','tournament_defense_i','tour_goalkeeping_2021')

for ( i in 1:4){
  assign(num_col_mean[i],c('Age','Rank',get(num_col[i])[grepl('%|/',get(num_col[i]))]))
  assign(num_col_sum[i],setdiff(get(num_col[i]),get(num_col_mean[i])))
  assign(tour_2021_sum[i],get(df2021_i[i])%>%group_by(as.factor(Nation),as.factor(Pos))%>%
           summarise_at(get(num_col_sum[i]),sum,na.rm=T))
  assign(tour_2021_mean[i],get(df2021_i[i])%>%group_by(as.factor(Nation),as.factor(Pos))%>%
           summarise_at(get(num_col_mean[i]),mean,na.rm=T))
  setnames(get(tour_2021_sum[i]),old=c("as.factor(Nation)","as.factor(Pos)"),new=c("Nation","Pos"))
  setnames(get(tour_2021_mean[i]),old=c("as.factor(Nation)","as.factor(Pos)"),new=c("Nation","Pos"))
  assign(tour_2021_team[i],get(tour_2021_sum[i])%>%full_join(
    get(tour_2021_mean[i]),by=c('Nation','Pos')))
}


#linear model
linear_model=paste0("linear_model",df.name)
'''
for ( i in 1:4){
  assign(linear_model,lm(Rank~.,data=get(tour_2021_team[i])[,-1]))
  print(paste0(linear_model[i],":"))
  print(summary(get(linear_model[i])))
}
'''  


linear_shoot=lm(Rank~.,data=get(tour_2021_team[1])[,-1])
summary(linear_shoot)
stepAIC(linear_shoot,direction = 'backward')

linear_pass=lm(Rank~.,data=get(tour_2021_team[2])[,-1])
summary(linear_pass)
stepAIC(linear_pass,direction = 'backward')

linear_defense=lm(Rank~.,data=get(tour_2021_team[3])[,-1])
summary(linear_defense)
stepAIC(linear_defense,direction = 'backward')


#lasso
x.shoot=cbind(data.matrix(get(tour_2021_team[1])[,num_col_shooting]),
              model.matrix( ~ Pos-1, get(tour_2021_team[1])) )
cv_shoot <- cv.glmnet(x.shoot[ ,!(colnames(x.shoot) == "Rank")], get(tour_2021_team[1])[["Rank"]], alpha = 1)
best_lambda <- cv_shoot$lambda.min
plot(cv_shoot) 
best_shoot<- glmnet(x.shoot[ ,!(colnames(x.shoot) == "Rank")], get(tour_2021_team[1])[["Rank"]], alpha = 1, lambda = best_lambda)
coef(best_shoot)
##passing
x.pass=cbind(data.matrix(get(tour_2021_team[2])[,num_col_passing]),
             model.matrix( ~ Pos-1, get(tour_2021_team[2])) )

cv_pass <- cv.glmnet(x.pass[ ,!(colnames(x.pass) == "Rank")], get(tour_2021_team[2])[["Rank"]], alpha = 1)
best_lambda <- cv_pass$lambda.min
plot(cv_pass) 
best_pass<- glmnet(x.pass[ ,!(colnames(x.pass) == "Rank")], get(tour_2021_team[2])[["Rank"]], alpha = 1, lambda = best_lambda)
coef(best_pass)
##defense
tour_2021_team_defense=tour_2021_team_defense[,-3]
x.defense=cbind(data.matrix(get(tour_2021_team[3])[,-c(1:2)]),
                model.matrix( ~ Pos-1, get(tour_2021_team[3])) )
cv_defense <- cv.glmnet(x.defense[ ,!(colnames(x.defense) == "Rank")], get(tour_2021_team[3])[["Rank"]], alpha = 1)
best_lambda <- cv_defense$lambda.min
plot(cv_defense) 
best_defense<- glmnet(x.defense[ ,!(colnames(x.defense) == "Rank")], get(tour_2021_team[3])[["Rank"]], alpha = 1, lambda = best_lambda)
coef(best_defense)
##goalkeeping
x.goalkeep=data.matrix(get(tour_2021_team[4])[,-c(1:2)])

cv_goalkeep <- cv.glmnet(x.goalkeep[ ,!(colnames(x.goalkeep) == "Rank")], get(tour_2021_team[4])[["Rank"]], alpha = 1)
best_lambda <- cv_goalkeep$lambda.min
plot(cv_goalkeep) 
best_goalkeep<- glmnet(x.goalkeep[ ,!(colnames(x.goalkeep) == "Rank")],  get(tour_2021_team[4])[["Rank"]], alpha = 1, lambda = best_lambda)
coef(best_goalkeep)

#ridge 
##shooting
cv_shoot_r <- cv.glmnet(x.shoot, get(tour_2021_team[1])[["Rank"]], alpha = 0)
best_lambda_r  <- cv_shoot_r $lambda.min
plot(cv_shoot_r ) 
best_shoot_r <- glmnet(x.shoot, get(tour_2021_team[1])[["Rank"]], alpha = 0, lambda = best_lambda_r )
coef(best_shoot_r)
##passing
cv_pass_r  <- cv.glmnet(x.pass, get(tour_2021_team[2])[["Rank"]], alpha = 0)
best_lambda_r  <- cv_pass_r $lambda.min
plot(cv_pass_r ) 
best_pass_r <- glmnet(x.pass, get(tour_2021_team[2])[["Rank"]], alpha = 0, lambda = best_lambda_r )
coef(best_pass_r )
##defense
cv_defense_r  <- cv.glmnet(x.defense, get(tour_2021_team[3])[["Rank"]], alpha = 0)
best_lambda_r  <- cv_defense_r $lambda.min
plot(cv_defense_r ) 
best_defense_r <- glmnet(x.defense, get(tour_2021_team[3])[["Rank"]], alpha = 0, lambda = best_lambda_r )
coef(best_defense_r )
##goalkeeping
cv_goalkeep_r  <- cv.glmnet(x.goalkeep, get(tour_2021_team[4])[["Rank"]], alpha = 0)
best_lambda_r  <- cv_goalkeep_r $lambda.min
plot(cv_goalkeep_r ) 
best_goalkeep_r <- glmnet(x.goalkeep,  get(tour_2021_team[4])[["Rank"]], alpha = 0, lambda = best_lambda_r )
coef(best_goalkeep_r )
