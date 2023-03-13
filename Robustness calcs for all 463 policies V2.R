# Robustness calculations for 463 solutions tested in 500 member SOW ensemble
# Phase III robustness calcs
# Nathan Bonham
# 2/16/21

## Libaries

library(dplyr)
setwd('G:/My Drive/CU Boulder/CRB publications/Robustness app related/CRB robustness app EMS/CRB-Robustness-App-BOR') # directory to robustness function library.R
source('robustness function library.R')


############# import objectives
# the processing of objective .rdf files is handled in readRDF scenario folders.R

setwd("G:/My Drive/CU Boulder/Phase 3 Robustness Calculations/R/data")
obj=read.table('objectives_all463.txt')

## unit conversion for objectives. I want all LB shortage volumes in KAF and Powell WY Release in MAF
vol_index=which(colnames(obj)%in% c('LB.Shortage.Volume', 'LB.Shortage.Volume.Policy','Max.Annual.LB.Shortage'))
obj[vol_index]=obj[vol_index]/1000
obj$Powell.WY.Release=obj$Powell.WY.Release/1e6

MOEA_archive=read.table('Mead Solutions/Archive_463solns.txt') # data from MOEA optimization. From Rebecca
MOEA_data=MOEA_archive[,15:22] # subset only the objectives (remove decision levers)
# names=c('Mead.1000', 'LB.Max.Cons.Shortage.Duration', 'LB.Shortage.Frequency',
#         'LB.Shortage.Volume', 'Max.Annual.LB.Shortage', 'Powell.3490', 'Powell.WY.Release', 'Lee.Ferry.Deficit') # I double checked the names with the spreadsheet Rebecca gave me
names=c('M1000', 'LB.Dur', 'LB.Freq','LB.Avg', 'LB.Max', 'P3490', 'P.WYR', 'LF.Deficit') 

# the spreadsheet lives here: G:/My Drive/CU Boulder/Phase 3 Robustness Calculations/R/data/Mead Solutions/from Rebecca/Archive_Master_withID.xlsx
colnames(MOEA_data)=names
## unit conversion for MOEA_data
vol_index2=which(colnames(MOEA_data)%in% c('LB.Avg', 'LB.Max'))
MOEA_data[vol_index2]=MOEA_data[vol_index2]/1000
MOEA_data$P.WYR=MOEA_data$P.WYR/1e6

## define different subsets of objectives, including if they are minimized or maximized

## From the Borg Archive, LB.Shortage.Volume.Policy is NOT included. Further, from my robusntess analysis it is redundant with  LBSV, so will not include from here on
# allObj=colnames(obj)[-c(1,2)] # includes LB.Shortage.Volume and LB.Shortage.Volume.Policy
# all_best_if= c('min','min','min','min','min','min','min','min','min')

colnames(obj)=c('TraceNumber', 'policy', 'LB.Avg', 'LB.Avg.Policy', 'LF.Deficit', 'M1000', 'P3490', 'LB.Dur', 'LB.Freq', 'LB.Max', 'P.WYR')

my_order=c('LF.Deficit', 'P.WYR', 'P3490', 'M1000', 'LB.Avg', 'LB.Freq', 'LB.Max', 'LB.Dur')

obj=obj[c('TraceNumber', 'policy', my_order)]
MOEA_data=MOEA_data[my_order]

obj8_best_if=c('min','min','min','min','min','min','min','min')

## select objectives to use in the robustness metrics

obj_vec=my_order
best_if=obj8_best_if

metrics.list=list() # preallocate a list to store calculations

############################## Satisficing #########

satisficing.M1000=satisficing(data=obj, objectives='M1000', thresholds = 10)
satisficing.P3490=satisficing(objectives='P3490', thresholds = 5)
satisficing.LB.Avg=satisficing(objectives='LB.Avg', thresholds=600)
sat.dev.M1000=satisficing.deviation(objectives='M1000', thresholds=10)
sat.dev.P3490=satisficing.deviation(objectives='P3490', thresholds = 5)
sat.dev.LB.Avg=satisficing.deviation(objectives='LB.Avg', thresholds = 600)


satisficing.df=data.frame(policy=satisficing.M1000$policy, sat.M1000=satisficing.M1000$satisficing, sat.P3490=satisficing.P3490$satisficing, sat.LB.Avg=satisficing.LB.Avg$satisficing,
                          sat.dev.M1000=sat.dev.M1000$satisficing.deviation, sat.dev.P3490=sat.dev.P3490$satisficing.deviation,
                          sat.dev.LB.Avg=sat.dev.LB.Avg$satisficing.deviation)

metrics.list[['satisficing']]=satisficing.df

############################## Regret type 2 (regret from best) ###########

regret2.df=data.frame(matrix(NA, ncol=length(obj_vec)+1, nrow=nrow(MOEA_data)))
regret2.df[,1]=satisficing.df$policy

c=2
for (i in obj_vec){
  regret2.df[,c]=regret2(objectives = i, best_if = best_if[c-1])$regret2$regret2
    c=c+1  
}

colnames(regret2.df)=c('policy', obj_vec)


metrics.list[['regret.from.best']]=regret2.df

############################# Percent deviation from baseline ################

metrics.list[['percent.deviation']]=percent_deviation(data=obj, baseline = MOEA_data, objectives = colnames(MOEA_data),
                                                     max_objectives = 'none', percentile = 90)

############################# Laplace's PIR ######################

metrics.list[['mean']]=LaplacePIR(objectives = obj_vec)

############################# Hurwicz OP #######################

metrics.list[['Hurwicz.OP']]=HurwiczOP(objectives = obj_vec, best_case_weight = 0.5, best_if = rep('min', length(obj_vec)))

############################ mean-variance ########################

metrics.list[['mean.variance']]=mean_variance(objectives = obj_vec)

########################## maximin (worst case scenario) ##########

metrics.list[['maximin']]=maximin(objectives=obj_vec)


############################ add non-dominated front for app ######################
###################################################################################

names=names(metrics.list)
metrics_4app=list()

for (i in 1:length(metrics.list)){
  if(names[i]=='satisficing'){
    max_id=2:4
  } else{
    max_id=NULL
  }
  metrics_4app[[names[i]]]=add_NonDom_front(data=metrics.list[[i]],max_cols = max_id)
  
}


############### add baseline data to metrics_4app
metrics_4app[['optimization']]=data.frame(policy=1:nrow(MOEA_data), MOEA_data)

############## change "policy" to "ID" in each dataframe ########

for (i in names(metrics_4app)){
  colnames(metrics_4app[[i]])[1]="ID"
}
for (i in names(metrics.list)){
  colnames(metrics.list[[i]])[1]="ID"
}


####################### export data ########################

# includes non dominated front. Specifically for robustness app

setwd('G:/My Drive/CU Boulder/CRB publications/Robustness app related/CRB robustness app EMS/CRB-Robustness-App-BOR')
saveRDS(metrics_4app, file = 'tradeoff_dataframes.rds')

# also send data w/out non dominated front to app
saveRDS(metrics.list, file='Robustness_metrics_463solns_List.rds')


