# Prepare data frames for stacked bar plotting of policy decision variables
# Phase III robustness calculations
# Nathan Bonham
# 2/10/21

library(tidyr)
library(dplyr)

rm(list=ls())

setwd('G:/My Drive/CU Boulder/Phase 3 Robustness Calculations/R/data/Mead Solutions')
Archive='Archive_463_Condensed.txt'

Archive.df=read.table(Archive, header = T, sep = "")

lastDV=which(colnames(Archive.df)=='T6V')

dv=Archive.df[,1:lastDV]



#################### Create data frame for stacked bar plots ###################

# prepare for plotting
elevation=dv[1:6]
elevation$dead.pool=895 # need to add dead pool or tier 6 is thrown out for policies where it exists
el_max=unlist(apply(elevation, 1, max)) # save max elevation for ggplotting later

e_delta=elevation[-7]-elevation[-1] # difference between shortage tiers elevations, used for plotting
volume=dv[7:12]
volume$dead.pool=0
maxVol=apply(volume,1, max)

tier_bin=volume>0 # identify actual tiers
nTiers=apply(tier_bin, 1, sum) # sum number of tiers. You want this for filtering in your web app

volume_labs=matrix(NA, nrow=nrow(volume), ncol=ncol(volume)) # create shortage volume labels, ex V = 500 KAF
for (i in 1:ncol(volume_labs)){
  volume_labs[,i]=paste(volume[,i], ' KAF', sep='')
}

volume_labs=data.frame(volume_labs) 
colnames(volume_labs)=colnames(volume)


# add policy ID to each data frame
elevation$policy=as.character(1:nrow(dv))
e_delta$dead_pool=895
e_delta$policy=as.character(1:nrow(dv))
volume$policy=as.character(1:nrow(dv))
volume_labs$policy=as.character(1:nrow(dv))

piv_col=which(colnames(e_delta)=='dead_pool')

# wide to long format for ggplot
elevation=pivot_longer(elevation, cols=1:piv_col, names_to = 'Tier')
e_delta=pivot_longer(e_delta, cols=1:piv_col, names_to = 'Tier') 

e_delta$Tier=factor(e_delta$Tier, levels = c("T1e","T2e","T3e","T4e","T5e", "T6e","dead_pool")) # need to reorder the factor levels such that dead pool is last
e_delta=dplyr::rename(e_delta, 'delta'='value')
# stacked bar plot order depends on the level order

volume=pivot_longer(volume, cols=1:piv_col, names_to = 'Tier')
volume_labs=pivot_longer(volume_labs, cols=1:piv_col, names_to = 'Tier')
volume_labs=data.frame(volume_labs, elevation=elevation$value)


df=data.frame(e_delta, v_lab= volume_labs$value, elevation=volume_labs$elevation, volume=volume$value)
df$Tier=rep(c('1', '2', '3', '4', '5', '6', 'dead pool'), length(unique(df$policy)))


############# Create data frame for policy labels, SOM node labels, and label y position

## SOM group
setwd('G:/My Drive/CU Boulder/Phase 3 Robustness Calculations/R/data/Mead Solutions')
som_group=read.table('Mead 4x4 SOM node.txt')
som_group$nodeID=paste('Node:',som_group$node, sep='')

## policy ID
policy_lab=paste('ID:', som_group$policy)

policy_SOM_labs=data.frame(policy_lab, SOM_node=som_group$nodeID, policy_lab_y=el_max)

policy_SOM_labs$T1e=el_max
policy_SOM_labs$maxVol=maxVol
policy_SOM_labs$T1V=dv$T1V
policy_SOM_labs$nTiers=nTiers


stacked_bar_plot_data=list(long_data=df, wide_data=policy_SOM_labs)

setwd('G:/My Drive/CU Boulder/Phase 3 Robustness Calculations/R/data')

saveRDS(stacked_bar_plot_data, file='data for stacked bar plot.rds')





