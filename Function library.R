# Function library for CRB robustness web application
# Phase III robustness analysis
# Nathan Bonham
# April 2021

# load data in 'global' chunk so it can be shared by all users of the dashboard
library(knitr)
library(flexdashboard)
library(dplyr)
library(plotly) # one method for par coords.
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(stringr)
library(gdata) #startsWith
library(nsga2R)
library(DT) # for sortable data table
library(openxlsx)
library(prospectr) # for Kennard Stone sampling
library(shinyBS) # for tooltips
library(Kendall) # rank correlation

####################################### wrapper to plot_ly parallel coordinates #################################
#################################################################################################################


par_coords=function(data, max_cols=NULL, n_var, color_var, title='User selected metrics', labels=colnames(data), source=NULL, policy_ID=NULL,
                    color_scale=input$ColorScale, reverse_scale=input$ReverseTF, colorbarTitle="", maintainAxesRange=input$MaintainAxes, axes_data=data, show_ID_front=input$ShowID_Front, 
                    labelangle=input$labelangle, titlesize=input$titlesize, labelsize=input$labelsize, img_height=input$height, img_width=input$width, img_scale=input$scale,
                    img_format=input$format){
  
  # identify maximization axes
  
  T_F=rep(FALSE,ncol(data))
  
  if (is.null(max_cols)){
    
  } else {
    
    T_F[max_cols]=TRUE
    
  }
  
  # custom color palette
  mypal=matrix(nrow=4, ncol=2)
  mypal[,1]=c(0,.25,.75,1)
  mypal[,2]=c('rgb(255,0,0)','rgb(255,102,255)','rgb(153,255,255)','rgb(0,0,255)')
  

  dimensions=list()
  for (i in 1: ncol(data[,1:n_var])){
    dimensions[[i]]=list()
    
    if (maintainAxesRange==FALSE){ # use axes range of data. This means if policies are removed, axes ranges can shrink
      dimensions[[i]][['range']]=sort(range(data[[i]]), decreasing=T_F[i])
    } else { # axes ranges are kept as the range of all policies
      dimensions[[i]][['range']]=sort(range(axes_data[[i]]), decreasing=T_F[i])
    }
    
    dimensions[[i]][['label']]=labels[i]
    dimensions[[i]][['values']]=data[[i]]
    
    if (colnames(data)[i]=='ID'){
      dimensions[[i]][['constraintrange']]=policy_ID
    }
    
    if (colnames(data)[i] %in% c("ID", "front")){
      dimensions[[i]][["visible"]]=show_ID_front
    }
    
    
  }
  
  
  if (color_scale=='blue-red'){color_scale=mypal}
  
  p <-data %>% plot_ly(type = 'parcoords', tickfont=list(size=13),
                       line = list(color =data[[color_var]], colorbar=list(title=list(text=colorbarTitle, side='right'), thickness=20, x=1.00, xpad=10),
                                   colorscale = color_scale, reversescale=reverse_scale, showscale=TRUE), labelangle=labelangle, labelside="top",
                       dimensions = dimensions, source=source, labelfont=list(size=labelsize)
                       
  ) # end plot_ly()
  
  # add title and set margins
  p=p %>% layout(margin=list(l=60,r=20,b=25,t=0, pad=0), title=list(text=title, font=list(size=titlesize)))
  
  # see these sources for wayt to change the download image behavior. You can change height, width, and fyle type to obtain MUCH IMPROVED quality
  # https://plotly.com/python/configuration-options/
  # https://www.rdocumentation.org/packages/plotly/versions/4.9.3/topics/config
  # https://github.com/plotly/plotly.js/blob/master/src/plot_api/plot_config.js
  
  p=config(p, toImageButtonOptions=list(format=img_format, height=img_height, width=img_width, scale=img_scale))

  
  
  return(p)
  
  
}

############################# stacked histogram Decison Variable plotting ###############################
##########################################################################################################

# function to change ggplot legend size: https://stackoverflow.com/questions/52297978/decrease-overal-legend-size-elements-and-text
addSmallLegend <- function(myPlot=bar_plot, pointSize = 0.25, textSize = 8, spaceLegend = 0.03) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_blank(), #element_text(size = textSize)
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}

DV_plot=function(long.data=long_data, wide.data=wide_data, metric= robustness_metrics$satisficing,
                 to_plot=robustness_metrics$satisficing$policy, metric_label='satisficing', preferred_direction='max', y_axis2=T,
                 labelsize=delayFontSlider()){
  
  # filter for chosen policies
  filter.long=dplyr::filter(long.data, policy %in% to_plot)
  filter.wide=dplyr::filter(wide.data, ID %in% to_plot)
  filter.metric=dplyr::filter(metric, ID %in% to_plot)
  
  # get rank, append to data frames
  
  correction=ifelse(preferred_direction == 'min', 1, -1) # to handle metrics that should be minimized and maximizied accordingly in the ranking
  decreasing=ifelse(preferred_direction == 'min', F, T) # needed for add_lines in plot function
  
  filter.metric$rank=rank(correction*filter.metric[[metric_label]], ties.method = 'first')
  filter.long$rank=rep(filter.metric$rank, each=length(unique(filter.long$Tier)))
  filter.wide$rank=filter.metric$rank
  
  
  ############################# plotting ################################
  text_size=labelsize
  
  n_policies=nrow(filter.wide)

  filter.long$volume[filter.long$volume==0]=NA
  
  breaks=seq(0,2400, length.out=13)
  my_labs= c("[0-200)","[200-400)", "[400-600)", "[600-800)", "[800-1000)", "[1000-1200)","[1200-1400)",
             "[1400-1600)","[1600-1800)", "[1800-2000)", "[2000-2200)", "[2200-2400)")
  filter.long$vol_group=cut(filter.long$volume, breaks = breaks, include.lowest = T, right = F, labels = my_labs)

  color_func=colorRampPalette(colors=c("greenyellow","lightgoldenrod1", "goldenrod1", "darkorange1", "maroon2","firebrick3"))
  my_cols=color_func(12)
  names(my_cols)=my_labs
  
  
  bar_plot=ggplot()+
    geom_bar(data=filter.long, aes(fill=vol_group, x=rank, y=delta), position = 'stack', stat = 'identity', color='darkgrey')+
    geom_text(data=filter.long, aes(x=rank, y= elevation, label=v_lab),color='black', nudge_y = -4, size=text_size, check_overlap = T)+
    geom_text(data=filter.wide, aes(x=rank, y= policy_lab_y, label=policy_lab), nudge_y = 4, size=text_size, check_overlap = T)+
    # geom_text(data=filter.wide, aes(x=rank, y= policy_lab_y, label=SOM_node), nudge_y = 10, size=text_size, check_overlap = T)+ I have removed SOM node for NOW
    scale_fill_manual(values=my_cols, na.value="black", name="Volume [KAF]", na.translate=F, drop=F)+
    #scale_fill_brewer(palette='RdYlBu', direction = -1)+
    xlab(paste(metric_label, 'rank', sep=' '))+
    ylab('pool elevation [ft msl]')+
    theme(plot.title = element_text(size=10), plot.margin=margin(t=0, r=0, b=0, l=0, unit='pt'))+
    coord_cartesian(ylim=c(905,1110), xlim=c(1, min((n_policies+1),20)))
  
  bar_plot=addSmallLegend(bar_plot)
  # ggtitle(paste(metric_label, 'rank for selected policies', sep=' '))+
  
  int_plot=ggplotly(p=bar_plot, tooltip = c('rank','elevation', 'volume'), dynamicTicks=T, originalData=F) # convert ggplot to interactive plotly html
  # add legend title in correct location
  int_plot=int_plot %>%   layout(legend = list(
    orientation = "v", title=list(text=" Vol [KAF] "))
  )
  
  if (y_axis2==T){ # only add second y axis if y_axis2==T
    
    int_plot_2y=int_plot %>%
      add_lines(data=filter.metric, x=~sort(filter.metric$rank), y=~sort(filter.metric[[metric_label]], decreasing = decreasing), yaxis='y2',
                inherit=FALSE, showlegend=FALSE, line=list(color='purple', width=2, dash='dash')) %>%
      layout(yaxis2 = list(overlaying = "y", side = "right",
                           tickfont = list(color = 'purple', size=10), color = 'purple',
                           title = metric_label),
             legend = list(x = 1.05, y = 0.95), xaxis=list(range=c(0, min((n_policies+1),20))), yaxis=list(range=c(895,1110))
      )
    
  } else { # do not add second axis. Used on sensitivity analysis page
    
    int_plot_2y=int_plot %>%
      layout(legend = list(x = 1.05, y = 0.95), xaxis=list(range=c(0, min((n_policies+1),20)), title=""), yaxis=list(range=c(895,1110)))
    
  }

  
  int_plot_2y$x$layout$xaxis$autorange = FALSE # need to tell plotly to NOT change the axis range to fit all data
  int_plot_2y$x$layout$yaxis$autorange = FALSE
  return(int_plot_2y)
  
  
  
}

############################### calculate non Dominated fronts ###########################
##########################################################################################

calc_NonDom_front=function(data, policy_ID_column=1, max_cols=NULL){
  
  front=rep(NA, nrow(data))
  
  if(is.null(max_cols)==FALSE){ # fastNonDOminatedSorting assumes minimization. Multiply be negative 1 for metrics that should be maximized
    
    data[,max_cols]=data[,max_cols]*-1
    
  }
  
  front_list=fastNonDominatedSorting(data[-policy_ID_column])
  
  nfronts=length(front_list)
  
  for (i in 1:nfronts){
    
    policies=front_list[[i]]
    front[policies]=i
    
  }
  
  
  if(is.null(max_cols)==FALSE){ # convert back to positive where necessary
    
    data[,max_cols]=data[,max_cols]*-1
    
  }
  
  data=data.frame(front=front, data)
  
  return(front)
  
}

############################# functions to process brush and manual filters ###############
###########################################################################################

manual_filters=function(ID_key, unique_thresh_ID, dataframe, page="baseline"){
  
  # prepare log dataframe
  df.initialize=data.frame(matrix(ncol=6, nrow=1))
  colnames(df.initialize)=c("page", "metric", "filter.type", "lower", "upper", "table.selection.ID")
  manual_log$input=df.initialize
  
  function_df$r=dataframe
  
  rows=list()
  log_vec=vector()
  
  if (!isTruthy(input[[ID_key]]) ){ # if user DID NOT hand selected policies to view
    
    for (i in 1:3){ # loop through filters
      
      
      
      if (input[[paste0('Var',unique_thresh_ID,i)]] == 'None' ){
        rows[[i]]=1:nrow(function_df$r)
      } else  if (input[[paste0('Ineq',unique_thresh_ID,i)]] == '<'){
        rows[[i]]=which(function_df$r[[input[[paste0('Var',unique_thresh_ID,i)]]]] < as.numeric(input[[paste0('Thresh',unique_thresh_ID,i)]]))
        
        log_vec[1:6]=c(page, input[[paste0('Var',unique_thresh_ID,i)]], "manual", "0", input[[paste0('Thresh',unique_thresh_ID,i)]], NA)
        
      } else { # greater than
        rows[[i]]=which(function_df$r[[input[[paste0('Var',unique_thresh_ID,i)]]]] > as.numeric(input[[paste0('Thresh',unique_thresh_ID,i)]]))
        
        log_vec[1:6]=c(page, input[[paste0('Var',unique_thresh_ID,i)]], "manual", input[[paste0('Thresh',unique_thresh_ID,i)]], "+inf", NA)
        
      }
      
      # add log_vec to manual log
      if (length(log_vec)>0){
        manual_log$input=rbind(manual_log$input, log_vec)
      }
      
      
    } # end for
    
    a=intersect(rows[[1]],rows[[2]])
    b=intersect(a, rows[[3]])
    
    ID=function_df$r[['ID']][b]
    log=manual_log$input
    return(list(ID, log))
    # return(ID)
  } else { # user selected specific policies to view
    
    log_vec[1:6]=c(page, "ID", "manual", NA, NA,input[[ID_key]])
    
    if (length(log_vec)>0){
      manual_log$input=rbind(manual_log$input, log_vec)
    }
    ID=unlist(lapply(str_split(input[[ID_key]], ','), as.integer))
    log=manual_log$input
    return(list(ID,log ))
    
  } # end else
  
}

########################### functions to get brush ranges and filter data frame #############

## get brush ranges

get_brush_ranges=function(source, prep_df){
  
  d <- event_data("plotly_restyle", source = source)
  # what is the relevant dimension (i.e. variable)?
  dimension <- as.numeric(stringr::str_extract(names(d[[1]]), "[0-9]+"))
  # careful of the indexing in JS (0) versus R (1)!
  
  # code for debugging dimension number. Used this to identify need for if(is.na(dimension)) statement below
  # cat(file=stderr(), 'trying to get index', dimension, "\n") # prints dimension to R console
  
  if (is.na(dimension)){ # d is NA when user changes axis position
    # do nothing
    return(NA)
  } else {
    dimension_name <- names(prep_df)[[dimension + 1]]
    # a given dimension can have multiple selected ranges
    # these will come in as 3D arrays, but a list of vectors 
    # is nicer to work with
    info <- d[[1]][[1]]
    ranges <- if (length(dim(info)) == 3) {
      lapply(seq_len(dim(info)[2]), function(i) info[,i,])
    } else if (is.null(info)){
      # ranges[[dimension_name]]=
      NULL # I added this if statement to handle the case where user deselects an axis range. That returns NULL and throws an error if     not skipped with next
    } else {
      list(as.numeric(info))
    }
    return(list(dimension_name, ranges))  
  }
  
  
}

## get brush policy IDs

brush_log=reactiveValues()

get_brush_policyID=function(ranges, prep_df, page="baseline"){
  
  # initialize brush_log
  df.initialize=data.frame(matrix(ncol=6, nrow=1))
  colnames(df.initialize)=c("page", "metric", "filter.type", "lower", "upper", "table.selection.ID")
  brush_log$log=df.initialize
  
  keep <- TRUE
  for (i in names(ranges)) {
    if(is.null(ranges[[i]])){# I added this if statement to handle the case where user deselects an axis range. That returns NULL and throws an error if not skipped with next
      next
    }
    range_ <- ranges[[i]]
    keep_var <- FALSE
    for (j in seq_along(range_)) {
      rng <- range_[[j]]
      
      keep_var <- keep_var | dplyr::between(prep_df[[i]], min(rng), max(rng))
      
      add_vec=c(page,i, 'brush', min(rng), max(rng), NA)
      brush_log$log=rbind(brush_log$log, add_vec)
      
    }
    keep <- keep & keep_var
  }
  policyID=prep_df[['ID']][keep]
  return(list(policyID, brush_log$log))
}

####################### selectInput options #################################
#############################################################################

  get_options=function(tradeoff_df){
    a=colnames(metrics_4app_n500[[tradeoff_df]])
    b=colnames(add_to_tradeoff) # use to add to filter option drop down menu
    return(c('None',a,b))
  }

get_satisficing_options=function(tradeoff_df="satisficing", df_list=metrics_4app$l){
  a=colnames(df_list[[tradeoff_df]])
  b=colnames(add_to_tradeoff) # use to add to filter option drop down menu
  return(c('None',a,b))
}


####################### Satisficing function ###############################
###################################################################

satisficing=function(data=obj, objectives=c('LB.Shortage.Volume', 'Mead.1000', 'Powell.3490'),
                     thresholds=c(600, 10, 5), fail_if_inequality=rep('greater', length(objectives)), 
                     n_satisficing=length(objectives), policy_ID_column=2, SOW_column=1){
  
  ####### prepare data frame to store results
  
  ncol=2 # one column for satisficing plus one for policy ID
  nrow=length(unique(data[, policy_ID_column])) # one row per policy
  
  metric.df=matrix(NA, nrow=nrow, ncol=ncol)
  metric.df=data.frame(metric.df)
  colnames(metric.df)=c('ID', 'satisficing')
  metric.df[,1]=unique(data[, policy_ID_column])
  
  policy_iter=unique(data[, policy_ID_column]) # id of policies to loop through
  
  if(n_satisficing > length(objectives)){
    
    n_satisficing=length(objectives)
    print('n_satisficing must be <= number of objectives. n_satisficing has been changed to length(objectives).')
    
  }
  
  if(length(fail_if_inequality) != length(objectives)){
    
    fail_if_inequality=rep('greater', length(objectives))
    print('length(fail_if_inequality) must = length(objectives). fail_if_inequality has been changed to rep(greater, length(objectives)).')
    
  }
  
  
  for (r in 1:length(policy_iter)){ ########## loop through policies
    
    filter.policy=data[which(data[,policy_ID_column]==policy_iter[r]),]
    
    ##################### Compute satisficing #################################
    
    satisficing_calcs=matrix(ncol=length(objectives)+2, nrow=nrow(filter.policy))
    
    for (cr in 1:length(objectives)){
      
      if (fail_if_inequality[cr]=='greater'){
        
        satisficing_calcs[,cr]=ifelse(filter.policy[,which(colnames(filter.policy)==objectives[cr])] > thresholds[cr], 0,1) # a 1 means 'met criteria'
        
      } else if (fail_if_inequality[cr]=='less'){
        
        satisficing_calcs[,cr]=ifelse(filter.policy[,which(colnames(filter.policy)==objectives[cr])] < thresholds[cr], 0,1) # a 1 means 'met criteria'
        
      } else {
        
        satisficing_calcs[,cr]=NA
        
      }
      
      
    } # satisficing criteria loop 
    
    if (length(objectives)==1){ # only one objective, don't need to do rowSums operator
      satisficing_calcs[, length(objectives)+1]=satisficing_calcs[,1]
      satisficing_calcs[, length(objectives)+2]=ifelse(satisficing_calcs[, length(objectives)+1] < n_satisficing , 0, 1) # a 1 in final column means yes, satisficing
    } else { # more than one objective. Use rowSums
      satisficing_calcs[, length(objectives)+1]=rowSums(satisficing_calcs[,1:length(objectives)])
      satisficing_calcs[, length(objectives)+2]=ifelse(satisficing_calcs[, length(objectives)+1] < n_satisficing , 0, 1) # a 1 in final column means yes, satisficing
    }
    
    metric.df$satisficing[r]=sum(satisficing_calcs[,ncol(satisficing_calcs)])/nrow(satisficing_calcs)
    
    
  } # end policy loop
  
  return(metric.df)
  
} # end function


############################ Satisficing deviation function ##################################
#####################################################################################


satisficing.deviation=function(data=obj, objectives=c('LB.Shortage.Volume', 'Mead.1000', 'Powell.3490'),
                               thresholds=c(600, 10, 5), fail_if_inequality=c('greater', 'greater', 'greater'), 
                               policy_ID_column=2, SOW_column=1, SOW_agg_method='mean',
                               percentile=0.9){
  
  ####### prepare data frame to store results
  
  ncol=2 # one column for satisficing plus one for policy ID
  nrow=length(unique(data[, policy_ID_column])) # one row per policy
  
  metric.df=matrix(NA, nrow=nrow, ncol=ncol)
  metric.df=data.frame(metric.df)
  colnames(metric.df)=c('policy', 'satisficing.deviation')
  metric.df[,1]=unique(data[, policy_ID_column])
  
  policy_iter=unique(data[, policy_ID_column]) # id of policies to loop through
  
  
  for (r in 1:length(policy_iter)){ ########## loop through policies
    
    filter.policy=data[which(data[,policy_ID_column]==policy_iter[r]),]
    
    ##################### Compute satisficing deviation #################################
    
    baseline_performance=thresholds
    
    sat_dev_calcs=matrix(ncol=length(objectives)+1, nrow=nrow(filter.policy))
    
    for (cr in 1:length(objectives)){
      
      if (fail_if_inequality[cr]=='greater'){
        
        sat_dev_calcs[,cr]=(filter.policy[,which(colnames(filter.policy)==objectives[cr])] - baseline_performance[cr])/baseline_performance[cr] # if greater than fails criteria, actual - baseline 
        
      } else if (fail_if_inequality[cr]=='less'){
        
        sat_dev_calcs[,cr]=(baseline_performance[cr]- filter.policy[,which(colnames(filter.policy)==objectives[cr])] )/baseline_performance[cr] # if less than fails criteria, baseline - actual
        
      } else {
        
        sat_dev_calcs[,cr]=NA
        
      }
      
      
    } # satisficing deviation criteria loop  
    
    
    if (length(objectives)==1){ # only one objective, don't need to do rowSums operator
      sat_dev_calcs[, length(objectives)+1]=sat_dev_calcs[,1]
    } else { # more than one objective. Use rowSums
      sat_dev_calcs[, length(objectives)+1]=rowSums(sat_dev_calcs[,1:length(objectives)]) 
    }
    ######### SOW aggregation ################
    
    if (SOW_agg_method=='percentile'){
      
      metric.df$satisficing.deviation[r]=quantile(sat_dev_calcs[,ncol(sat_dev_calcs)],percentile)
      
    } else {
      agg_func=match.fun(SOW_agg_method) # transform string into function. 
      metric.df$satisficing.deviation[r]=agg_func(sat_dev_calcs[,ncol(sat_dev_calcs)])
      
    }
    
    ######## end SOW aggregation #############
    
    
  } # end policy loop
  
  return(metric.df)
  
} # end function


############################# Hurwicz optimism-pessimism rule ##################
###############################################################################

HurwiczOP=function(data=obj, objectives=c('LB.Shortage.Volume', 'Mead.1000', 'Powell.3490'),
                   best_case_weight=0.5, best_if=rep('min', length(objectives)), 
                   policy_ID_column=2, SOW_column=1){
  
  ####### prepare data frame to store results
  
  ncol=length(objectives)+1 #plus one for policy ID
  nrow=length(unique(data[, policy_ID_column])) # one row per policy
  
  metric.df=matrix(NA, nrow=nrow, ncol=ncol)
  metric.df=data.frame(metric.df)
  colnames(metric.df)=c('ID', objectives)
  metric.df[,1]=unique(data[, policy_ID_column])
  
  policy_iter=unique(data[, policy_ID_column]) # id of policies to loop through
  
  worst_case_weight=1-best_case_weight
  
  
  for (r in 1:length(policy_iter)){ ########## loop through policies
    
    filter.policy=data[which(data[,policy_ID_column]==policy_iter[r]),]
    
    ##################### Compute metric #################################
    
    
    for (cr in 1:length(objectives)){
      
      min=min(filter.policy[,which(colnames(filter.policy)==objectives[cr])])
      max=max(filter.policy[,which(colnames(filter.policy)==objectives[cr])])
      
      if(best_if[cr]=='min'){
        
        metric.df[r,(cr+1)]=(best_case_weight*min + worst_case_weight*max)/2
        
      } else if (best_if[cr]== 'max'){
        
        metric.df[r,(cr+1)]=(best_case_weight*max + worst_case_weight*min)/2
        
      } else {
        print('This function only supports max or min for the best_if values')
      }
      
      
      
    } #  objective loop  
    
    
    
  } # end policy loop
  
  return(metric.df)
  
} # end function

############################## maximin ###################################
##########################################################################

maximin=function(data=obj, objectives=c('LB.Shortage.Volume', 'Mead.1000', 'Powell.3490'),
                 percentile=rep(100, length(objectives)), 
                 policy_ID_column=2, SOW_column=1){
  
  ####### prepare data frame to store results
  
  ncol=length(objectives)+1 #plus one for policy ID
  nrow=length(unique(data[, policy_ID_column])) # one row per policy
  
  metric.df=matrix(NA, nrow=nrow, ncol=ncol)
  metric.df=data.frame(metric.df)
  colnames(metric.df)=c('ID', objectives)
  metric.df[,1]=unique(data[, policy_ID_column])
  
  policy_iter=unique(data[, policy_ID_column]) # id of policies to loop through
  
  
  for (r in 1:length(policy_iter)){ ########## loop through policies
    
    filter.policy=data[which(data[,policy_ID_column]==policy_iter[r]),]
    
    ##################### Compute maximin #################################
    
    
    calcs=matrix(ncol=length(objectives), nrow=nrow(filter.policy))
    
    for (cr in 1:length(objectives)){
      
      
      metric.df[r,(cr+1)]=quantile( filter.policy[,which(colnames(filter.policy)==objectives[cr])], percentile[cr]/100) #return the desired quantile. For case of minimization and maximin, want 100%
      
    } #  criteria loop  
    
    
    
  } # end policy loop
  
  return(metric.df)
  
} # end function


####################### functions and font definitions for sensitivity heatmaps in Sensitivity analysis page ####################
####################################################################################################################

## axis for plotly heatmap. you can call this for yaxis if you want a blank label
xaxis=function(name='difference from baseline pool elevation thresholds (%)'){
  return(list(title=name))
}

## another axis, use for Mead1000 and Powell 3490
axis=function(name){
  return(list(title=paste(name, '< [] AF')))
}

# plotly subplot titles
# originally from here: https://rpubs.com/bcd/subplot-titles

# annotations
subplotTitle=function(title=""){
  
  f <- list(
    family = "Arial",
    size = 18,
    color = "black")
  
  a <- list(
    text = title,
    font = f,
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE
  )
  return(a)
} 


################## satisficing sensitivity calcs ###############################
#################################################################################

axis4sensitivity=function(name, units){
  return(list(title=paste0(name, ' < [] ', units)))
}

satisficing_sensitivity=function(data=obj_all, n=20, policy_IDs=1:463, obj1, range1,  obj2, range2, baseline_obj1, baseline_obj1_threshold, baseline_obj2, baseline_obj2_threshold ){
  
  # evaluation points
  vec1=seq(range1[1], range1[2], length.out = n)
  vec2=seq(range2[1], range2[2], length.out = n)
  eval.points=expand.grid(vec1, vec2)
  colnames(eval.points)=c(obj1, obj2)
  
  ######## compute satisficing for every combo of thresholds
  
  satisficing_list=list()
  
  # before calculating, subset data according to policy_IDs
  data=dplyr::filter(data, policy %in% policy_IDs)
  
  for (i in 1:nrow(eval.points)){
    satisficing_list[[i]]=satisficing(data=data, objectives = c(obj1, obj2), thresholds = unlist(eval.points[i,]), fail_if_inequality = c('greater', 'greater'))
  }
  
  # compute baseline satisficing
  
  # baseline_satisficing=satisficing(objectives=c(baseline_obj1, baseline_obj2), thresholds = c(baseline_obj1_threshold, baseline_obj2_threshold), fail_if_inequality = c('greater', 'greater'))
  # baseline_satisficing$rank=rank(-1*baseline_satisficing$satisficing)
  
  sensitivity.df=data.frame(matrix(NA, nrow=nrow(eval.points), ncol=1)) # need to change ncol = 1 to ncol = 2 if I implement baseline comparisons for rank correlation
  colnames(sensitivity.df)= c('mean') #c('correlation', 'mean')
  
  for (i in 1:nrow(sensitivity.df)){
    rank=rank(-1*satisficing_list[[i]]$satisficing)
    
    # sensitivity.df$correlation[i]=Kendall(x=baseline_satisficing$rank, rank)$tau
    sensitivity.df$mean[i]=mean(satisficing_list[[i]]$satisficing)
    
  }
  
  # sensitivity.df$delta.mean=sensitivity.df$mean-mean(baseline_satisficing$satisficing)
  
  plot.df=data.frame(eval.points, sensitivity.df)
  
  
  ############### plotting #######################
  
  units=data.frame(LB.Avg="KAF", LB.Avg.Policy="KAF", LF.Deficit= "%", M1000="%",
                   P3490="%", LB.Dur="Yr", LB.Freq="%", LB.Max="KAF", P.WYR="MAF")
  
 fig=plot_ly(type="heatmap", x=plot.df[[obj1]], y=plot.df[[obj2]], z=plot.df[["mean"]] )
  fig = layout(fig, xaxis=axis4sensitivity(obj1, units[[obj1]]), yaxis=axis4sensitivity(obj2, units[[obj2]]), title=paste0(obj1, " & ", obj2, ": satisficing fraction"), margin=list(l=40,r=40,b=40,t=40, pad=0))
  
  return(fig)
  
}



