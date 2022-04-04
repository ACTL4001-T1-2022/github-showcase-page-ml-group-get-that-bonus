library(dplyr)
library(corrplot)
library(readxl)
library(stringr)
library(ggplot2)
library(naniar)
library(mice)
library(glmnet)
library(data.table)
library(MASS)
library(tidyr)
library(tidyverse)
library(gridExtra)
library(VIM)
library(e1071)

#import data
import_path="C:/Users/lenovo/Desktop/Control cycle/SOA challenge/"
tournament_shooting <- read_excel(paste0(import_path,"player-data.xlsx"), 
                                  sheet = "Tournament Shooting", range = "A18:AA2033", 
                                  col_types = c("numeric","text","text", "text", "text", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "text", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric"))

tournament_passing <- read_excel(paste0(import_path,"player-data.xlsx"), 
                                 sheet = "Tournament Passing", range = "A14:AF502")

tournament_defense <- read_excel(paste0(import_path,"player-data.xlsx"), 
                                 sheet = "Tournament Defense", range = "A12:AG500")

tournament_goalkeeping <- read_excel(paste0(import_path,"player-data.xlsx"), 
                                     sheet = "Tournament Goalkeeping", range = "A13:AB142")
X2021_rank <- read_excel(paste0(import_path,"player-data.xlsx"), 
                         sheet = "Tournament Results", range = "E11:F35")
colnames(X2021_rank)=c('Rank','Nation')


#check the data issues for the imported datasets
df.name=c('shooting','passing','defense','goalkeeping')
dfnames=paste0('tournament_',df.name)
for (data.name in dfnames){
  print(paste0(data.name,":"))
  print(summary(get(data.name)))
}

#filtering data for 2021 tournament
tour_goalkeeping_2021=tournament_goalkeeping%>%filter(Year=='2021')
tour_shooting_2021=tournament_shooting%>%filter(Year=='2021')

# combine the position dataset to tournament result
df2021=c('tour_shooting_2021','tournament_passing','tournament_defense','tour_goalkeeping_2021')
for ( data.name in df2021){
  assign(data.name,get(data.name)%>%full_join(X2021_rank,by='Nation'))
}

#classify the data type
col.type=paste0("col.type_",df.name)
num_col=paste0("num_col_",df.name)
char_col=paste0("char_col_",df.name)
for ( i in 1:4){
  assign(col.type[i],as_tibble(sapply(get(df2021[i]), class)))
  assign(num_col[i],colnames(get(df2021[i]))[get(col.type[i])=='numeric'])
  assign(char_col[i],colnames(get(df2021[i]))[get(col.type[i])!='numeric'])
}

#correlation plot
for ( i in 1:4){
  p=corrplot(cor(get(df2021[i])[,setdiff(get(num_col[i]),'Year')],use='pairwise.complete.obs'),tl.cex=0.5,method = "color",number.cex=0.3,addCoef.col = "black")
  print(p)
}


#missing value calculation
miss_df=paste0("miss_",df.name)
Na_df=paste0("Na_",df.name)
neg_df=paste0("neg_",df.name)
data.issue=paste0("data.issue.",df.name)
for ( i in 1:4){
  assign(miss_df[i],as.matrix(colSums(is.na(get(df2021[i])[,get(num_col[i])]))))
  #% of missing values
  assign(Na_df[i],data.frame(round(get(miss_df[i])/nrow(get(df2021[i]))*100,3)))
  #% of negative values
  assign(neg_df[i],sapply(1:length(get(num_col[i])),function(j) round(sum(get(df2021[i])[get(num_col[i])[j]]<0, na.rm=TRUE)/dim(get(df2021[i]))[1]*100,3)))
  #data.issue %summary
  assign(data.issue[i],cbind("Neg_percent"=get(neg_df[i]),"Na_percent"=get(Na_df[i])))
  #missing value plots
  p=vis_miss(get(df2021[i]))+ggtitle(paste("Fig",i,": Missing values",df2021[i]))+theme(axis.text.x = element_text(angle = 90),axis.text = element_text(size = 7))
  print(p)
  g=md.pattern(get(df2021[i]),rotate.names = T)
  print(g)
}
vis_miss(get(df2021[3]))+ggtitle(paste("Fig",i,": Missing values",df2021[3]))+theme(axis.text.x = element_text(angle = 90),axis.text = element_text(size = 7))
# Filling missing data on Pos & Nation
tournament_defense[is.na(tournament_defense$Pos),]$Pos=tournament_shooting[tournament_shooting$Player==tournament_defense[is.na(tournament_defense$Pos),]$Player,]$Pos

#aggr(tournament_shooting, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
#     labels=names(tournament_shooting), cex.axis=.2, gap=1, 
#     ylab=c("Histogram of missing data","Pattern"))


#missing data issue
#orginal column names
orig_colnames=paste0("orig_colnames_",df.name)
for ( i in 1:4){
  assign(orig_colnames[i],colnames(get(df2021[i])))
}
colnames(tour_shooting_2021)[colnames(tour_shooting_2021)=="90s"]="S90"
colnames(tour_shooting_2021)=gsub(" |/|:|-|%","_",colnames(tour_shooting_2021))

colnames(tournament_passing)[colnames(tournament_passing)=="90s"]="S90"
colnames(tournament_passing)=gsub(" |/|:|-","_",colnames(tournament_passing))
colnames(tournament_passing)[colnames(tournament_passing)=="1_3"]="one_over3"
colnames(tournament_passing)=gsub("%","percent",colnames(tournament_passing))

colnames(tournament_defense)[colnames(tournament_defense)=="90s"]="S90"
colnames(tournament_defense)=gsub(" |/|:|-","_",colnames(tournament_defense))

colnames(tournament_defense)[colnames(tournament_defense)=="Tkl+Int"]="Tkl_Int"
colnames(tournament_defense)=gsub("%","percent",colnames(tournament_defense))

#solution 1: imputate data
tour_shooting_2021_i=mice(tour_shooting_2021,meth='rf',maxit=50,seed=500)
tour_shooting_2021_i=complete(tour_shooting_2021_i,1)
summary(tour_shooting_2021_i)

tournament_passing_i=mice(tournament_passing,meth='rf',maxit=50,seed=500)
tournament_passing_i=complete(tournament_passing_i,1)

tournament_defense_i=mice(tournament_defense,meth='rf',maxit=50,seed=500)
tournament_defense_i=complete(tournament_defense_i,1)


#check imputation
colnames(tour_shooting_2021_i)=orig_colnames_shooting
colnames(tournament_passing_i)=orig_colnames_passing
colnames(tournament_defense_i)=orig_colnames_defense

unique(tour_shooting_2021_i$Pos)
tour_shooting_2021_i[tour_shooting_2021_i$Pos=='MFFW',]$Pos='FWMF'
tour_shooting_2021_i[tour_shooting_2021_i$Pos=='DFFW',]$Pos='FWDF'
tour_shooting_2021_i[tour_shooting_2021_i$Pos=='DFMF',]$Pos='MFDF'

unique(tournament_passing_i$Pos)
tournament_passing_i[tournament_passing_i$Pos=='MFFW',]$Pos='FWMF'
tournament_passing_i[tournament_passing_i$Pos=='DFFW',]$Pos='FWDF'
tournament_passing_i[tournament_passing_i$Pos=='DFMF',]$Pos='MFDF'
unique(tournament_defense_i$Pos)
tournament_defense_i[tournament_defense_i$Pos=='MFFW',]$Pos='FWMF'
tournament_defense_i[tournament_defense_i$Pos=='DFFW',]$Pos='FWDF'
tournament_defense_i[tournament_defense_i$Pos=='DFMF',]$Pos='MFDF'
unique(tour_goalkeeping_2021$Pos)
tour_goalkeeping_2021=tour_goalkeeping_2021[tour_goalkeeping_2021$Pos=='GK',]

tour_goalkeeping_2021[tour_goalkeeping_2021$Pos!='GK',]$Pos='GK'
#exploratory plots
plot.freqpoly.shooting=lapply(unique(tournament_passing_i$Pos), function(x) ggplot(data = tournament_passing_i%>%filter(grepl(x, Pos)), mapping = aes(x = 'Total Cmp' ))+ 
                                geom_freqpoly(binwidth = 0.05)+theme_minimal()+xlab(x))
do.call(grid.arrange, c(plot.freqpoly.shooting, ncol = 2, nrow = 5))

#by rank
plot.bar.shooting=lapply(unique(tournament_passing_i$Rank), function(x) ggplot(data = tournament_passing_i%>%filter(Rank==x), mapping = aes(x =`Total Cmp`))+ 
                           geom_density(fill='skyblue1')+theme_minimal()+xlab(x))
do.call(grid.arrange, c(plot.bar.shooting, ncol = 4, nrow = 6))

#boxplot in loop
for (j in setdiff(num_col_shooting,c('Born','Year','Rank'))){
  g=ggplot(data = tour_shooting_2021_i, mapping = aes(y=tour_shooting_2021_i[[j]],fill=as.factor(Rank)))+ 
    geom_boxplot()+theme_minimal()+ylab(j)+ facet_wrap(vars(Pos))+theme(legend.position="bottom",legend.justification="right")+  theme(legend.key.size = unit(0.2, 'cm'))
  print(g)
}
ggplot(data = tour_shooting_2021_i, mapping = aes(y=tour_shooting_2021_i$Gls,fill=as.factor(Rank)))+ 
  geom_boxplot()+theme_minimal()+ylab('Gls')+ylim(0,3)+ facet_wrap(vars(Pos))+theme(legend.position="bottom",legend.justification="right")+  theme(legend.key.size = unit(0.2, 'cm'))

ggplot(data = tour_shooting_2021_i, mapping = aes(y=tour_shooting_2021_i$Standard_SoT,fill=as.factor(Rank)))+ 
  geom_boxplot()+theme_minimal()+ylab("Standard SoT")+ylim(0,5)+ facet_wrap(vars(Pos))+theme(legend.position="bottom",legend.justification="right")+  theme(legend.key.size = unit(0.2, 'cm'))

ggplot(data =tournament_defense_i, mapping = aes(y=tournament_defense_i$`Vs Dribbles Att`,fill=as.factor(Rank)))+ 
  geom_boxplot()+theme_minimal()+ylab("Vs Dribbles Att")+ylim(0,5)+theme(legend.position="bottom",legend.justification="right")+  theme(legend.key.size = unit(0.2, 'cm'))

ggplot(data =tour_goalkeeping_2021, mapping = aes(y=tour_goalkeeping_2021$L,fill=as.factor(Rank)))+ 
  geom_boxplot()+theme_minimal()+ylab("Lose")+theme(legend.position="bottom",legend.justification="right")+  theme(legend.key.size = unit(0.2, 'cm'))


