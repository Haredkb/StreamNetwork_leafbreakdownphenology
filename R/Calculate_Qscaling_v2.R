###### 
#Find avg flow for each section 
library(igraph)
library(tidyverse)
library(lubridate)
library(tidygraph)
source("R/global.R")

#Copied from Analysis_ObservedDta_noserial.Rto place in one location
intial_dates = as.POSIXct(c("08-01-2018"), format = "%m-%d-%Y")
  ####net <- readRDS("data/eg_watershed.RDS") #examples  small wateshed WS37
  net <- readRDS("data/network_intital.RDS") #full Coweeta Watershed
  
  for(i in 1:length(V(net))){
    up.all <- ego(net,order=length(V(net)),nodes=V(net)[i],mode=c("in"),mindist=0)
    V(net)$up.all[i] <- length(unlist(up.all))
  }
  
  # Re-arrange network graph so that model starts with headwaters, and moves down the network to larger river reaches:
  net2 <- igraph::graph_from_data_frame(d=igraph::as_data_frame(net,what="edges"),
                                        vertices=igraph::as_data_frame(net,what="vertices") %>%
                                          arrange(up.all))
  V(net2)$label <- V(net)$up.all
  net <- net2
  
  #add reach length in (incident edges) and nodes contributing (nodes in)
  net <- up_nodes(net) #functions_MB
  
  #set up time steps
  timesteps = 364 #days
  ts_units = "day" #hour' #days for Q as its the same for every hour within the day, but still units are per hr. 
  # Set up model run times start dates
  
  s_date = as.POSIXct(intial_dates[1], format = "%m-%d-%Y")
  #intial_dates = as.POSIXct(c("11-01-2018 00:00", "02-01-2019 00:00", "05-01-2019 00:00", "08-01-2019 00:00"), format = "%m-%d-%Y", tz = "GMT")
  #))#
  
  #Baseflow Linear Model for Watershed- baseQ_predict m3/hr - slope only
  Q_JDate_lm_m3hr <- readRDS("data/Q_JDate_lm_m3hr.RDS") #check should be Q for baseflow only, Q_all for total stream Flow
  
  #### Basin data
  V(net)$basin_id <- if_else(V(net)$basin_id == "CoweetaCreek", "ShopeFork", V(net)$basin_id)
  V(net)$basin_id <- if_else(is.na(V(net)$basin_id) == TRUE, "ShopeFork", V(net)$basin_id)
  
  #create new object to keep original intact
  network <- net
  
  ### CALCULATE BASE Q
  dates_day <- seq(from = as.Date(s_date), to = as.Date(s_date + days(timesteps)), by = "day")
  
  
  net_lstQ <- lapply(dates_day, function(t_s){
    message(t_s)
    #initialize date details
    #add date to igraph
    V(network)$date <- as.character(t_s)  #, format = "%Y-%m-%d")
    #print(V(network)$date[1])
    
    #pull timestep specific values for filtering 
    day <- yday(t_s)
    mon <- month(t_s)
    season <- quarter(t_s, fiscal_start = 1)
    
    
    #set up igraph variables
    # Inflow discharge from local catchment (m3 d-1):
    V(network)$Qlocal <- 0
    # Inflow discharge from upstream reaches (m3 d-1):
    V(network)$Qup <- 0
    # Outflow discharge from each reach (m3 d-1):
    V(network)$Qout <- NA
    
    bq_m3hrm <- Q_JDate_lm_m3hr %>%
      dplyr::filter(Jdate == day)
    
    ### CALCULATE GEOMORPHIC PARAMETERS and NETWORK 
    
    #network_pre <- get("network", env.pre())
    ##Set up parameters that do not change. 
    network <- netset(network, bq_m3hrm) 
  })#end net_lstQ
  
  names(net_lstQ) <- as.character(yday(dates_day))#name each igraph as its date
  
  saveRDS(net_lstQ, "output/data/net_lst_baseQ_v3.RDS")


#make igraph vertices into one dataframe
net_Q_df <- do.call(rbind, lapply(net_lstQ, igraph::as_data_frame, what = "vertices")) %>%
  mutate(month = month(date))

#This is done in two steps to account for the non-calendar year start dates and get better annual avg Q
avg_Q <- net_Q_df %>%
  group_by(name, month)%>%
  summarise(avgQ = mean(Qout))%>%
  group_by(name)%>%
  summarise(avgQ = mean(avgQ))%>%
  mutate(
            avg_width = 7.3 * (avgQ/3600)^0.45, #from Helton 2011 and Leopold 1953
            avg_depth = 0.25 * (avgQ/3600)^0.25 #from Catalan 2022 (Eq2) and Leopold 1953
    )

#add annual avg Q back into igraph networks 
net_Q_lst <- lapply(net_lstQ, function(x){
    tidygraph::as_tbl_graph(x) %>%
    activate(nodes) %>%
      #dplyr::select(-"avgQ", -"avg_width", -"avg_depth")%>% #remove previous runs to replace with newest AvgQ values
    left_join(., y = avg_Q, by = "name")%>%
    mutate(
      width_m   = if_else(Qout> 0, avg_width * (((Qout/3600)/(avgQ/3600)))^0.26, 0), #from Catalan 2022 eq3
      depth_m   = if_else(Qout> 0, avg_depth * (((Qout/3600)/(avgQ/3600)))^0.4, 0), #from Catalan 2022 eq3
      Qout_ls   = Qout/3.6, #m3/hr to l/s
      Bedarea_m2 = width_m * length_reach,
      CSarea_m2 = width_m * depth_m,
      avg_v_perh = if_else(Qout> 0, Qout/CSarea_m2, 0)
    )
}
)

net_lstQ <- write_rds(net_Q_lst, "output/data/net_lst_baseQ_v4.RDS") 

review_df <- do.call(rbind, lapply(net_Q_lst, igraph::as_data_frame, what = "vertices")) %>%
  dplyr::select("stream", "name", "n_lscp_name", "date", "Qout_ls") #stream will be na except for measured locations
#2023-11-15 checked againist net_lst_baseQ_v2 with all_equal - with no differences found (changed structure to make reproductiability simpler)
saveRDS(review_df, "data/final_lmer_model_network/Qout_inputdata_v4.RDS")



#####################
## Create Igraph Network for Total Flow
#####################
## Only used as Base comparison

###### 
#Find avg flow for each section 
library(igraph)
library(tidyverse)
library(lubridate)
library(tidygraph)
source("R/global.R")

#Copied from Analysis_ObservedDta_noserial.Rto place in one location
intial_dates = as.POSIXct(c("08-01-2018"), format = "%m-%d-%Y")
####net <- readRDS("data/eg_watershed.RDS") #examples  small wateshed WS37
net <- readRDS("data/network_intital.RDS") #full Coweeta Watershed

for(i in 1:length(V(net))){
  up.all <- ego(net,order=length(V(net)),nodes=V(net)[i],mode=c("in"),mindist=0)
  V(net)$up.all[i] <- length(unlist(up.all))
}

# Re-arrange network graph so that model starts with headwaters, and moves down the network to larger river reaches:
net2 <- igraph::graph_from_data_frame(d=igraph::as_data_frame(net,what="edges"),
                                      vertices=igraph::as_data_frame(net,what="vertices") %>%
                                        arrange(up.all))
V(net2)$label <- V(net)$up.all
net <- net2

#add reach length in (incident edges) and nodes contributing (nodes in)
net <- up_nodes(net) #functions_MB

#set up time steps
timesteps = 364 #days
ts_units = "day" #hour' #days for Q as its the same for every hour within the day, but still units are per hr. 
# Set up model run times start dates

s_date = as.POSIXct(intial_dates[1], format = "%m-%d-%Y")
#intial_dates = as.POSIXct(c("11-01-2018 00:00", "02-01-2019 00:00", "05-01-2019 00:00", "08-01-2019 00:00"), format = "%m-%d-%Y", tz = "GMT")
#))#

#Baseflow Linear Model for Watershed- baseQ_predict m3/hr - slope only
Q_JDate_lm_m3hr <- readRDS("data/Q_all_JDate_lm_m3hr.RDS") #check should be Q for baseflow only, Q_all for total stream Flow

#### Basin data
V(net)$basin_id <- if_else(V(net)$basin_id == "CoweetaCreek", "ShopeFork", V(net)$basin_id)
V(net)$basin_id <- if_else(is.na(V(net)$basin_id) == TRUE, "ShopeFork", V(net)$basin_id)

#create new object to keep original intact
network <- net

### CALCULATE BASE Q
dates_day <- seq(from = as.Date(s_date), to = as.Date(s_date + days(timesteps)), by = "day")


net_lstQ <- lapply(dates_day, function(t_s){
  message(t_s)
  #initialize date details
  #add date to igraph
  V(network)$date <- as.character(t_s)  #, format = "%Y-%m-%d")
  #print(V(network)$date[1])
  
  #pull timestep specific values for filtering 
  day <- yday(t_s)
  mon <- month(t_s)
  season <- quarter(t_s, fiscal_start = 1)
  
  
  #set up igraph variables
  # Inflow discharge from local catchment (m3 d-1):
  V(network)$Qlocal <- 0
  # Inflow discharge from upstream reaches (m3 d-1):
  V(network)$Qup <- 0
  # Outflow discharge from each reach (m3 d-1):
  V(network)$Qout <- NA
  
  bq_m3hrm <- Q_JDate_lm_m3hr %>%
    dplyr::filter(Jdate == day)
  
  ### CALCULATE GEOMORPHIC PARAMETERS and NETWORK 
  
  #network_pre <- get("network", env.pre())
  ##Set up parameters that do not change. 
  network <- netset(network, bq_m3hrm) 
})#end net_lstQ

names(net_lstQ) <- as.character(yday(dates_day))#name each igraph as its date

saveRDS(net_lstQ, "output/data/net_lst_FlowQ_v3.RDS")


#make igraph vertices into one dataframe
net_Q_df <- do.call(rbind, lapply(net_lstQ, igraph::as_data_frame, what = "vertices")) %>%
  mutate(month = month(date))

#This is done in two steps to account for the non-calendar year start dates and get better annual avg Q
avg_Q <- net_Q_df %>%
  group_by(name, month)%>%
  summarise(avgQ = mean(Qout))%>%
  group_by(name)%>%
  summarise(avgQ = mean(avgQ))%>%
  mutate(
    avg_width = 7.3 * (avgQ/3600)^0.45, #from Helton 2011 and Leopold 1953
    avg_depth = 0.25 * (avgQ/3600)^0.25 #from Catalan 2022 (Eq2) and Leopold 1953
  )

#add annual avg Q back into igraph networks 
net_Q_lst <- lapply(net_lstQ, function(x){
  tidygraph::as_tbl_graph(x) %>%
    activate(nodes) %>%
    #dplyr::select(-"avgQ", -"avg_width", -"avg_depth")%>% #remove previous runs to replace with newest AvgQ values
    left_join(., y = avg_Q, by = "name")%>%
    mutate(
      width_m   = if_else(Qout> 0, avg_width * (((Qout/3600)/(avgQ/3600)))^0.26, 0), #from Catalan 2022 eq3
      depth_m   = if_else(Qout> 0, avg_depth * (((Qout/3600)/(avgQ/3600)))^0.4, 0), #from Catalan 2022 eq3
      Qout_ls   = Qout/3.6, #m3/hr to l/s
      Bedarea_m2 = width_m * length_reach,
      CSarea_m2 = width_m * depth_m,
      avg_v_perh = if_else(Qout> 0, Qout/CSarea_m2, 0)
    )
}
)

net_lstQ <- write_rds(net_Q_lst, "output/data/net_lst_FlowQ_v4.RDS") 

review_df <- do.call(rbind, lapply(net_Q_lst, igraph::as_data_frame, what = "vertices")) %>%
  dplyr::select("stream", "name", "n_lscp_name", "date", "Qout_ls") #stream will be na except for measured locations
#2023-11-15 checked againist net_lst_baseQ_v2 with all_equal - with no differences found (changed structure to make reproductiability simpler)
saveRDS(review_df, "data/final_lmer_model_network/Qout_FLOW_inputdata_v4.RDS")








