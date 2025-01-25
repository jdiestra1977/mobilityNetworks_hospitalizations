library(tidyverse)
library(sf)
library(tidycensus)
library(dplyr)
library(igraph)
source("networkFunctions.R")

setwd("~/Documents/GitHub/mobilityNetworks_hospitalizations/")
#
#Getting mapping cbgs - zip code -------
# Load data for population by ZCTA in Harris County
zip_codes_TX_2019 <- get_acs(
  geography = "zcta",
  variables = "B01003_001", # Total population
  state = "TX",
  year = 2019, # Change to the desired year
  survey = "acs5"
)

US_population_2022 <- get_acs(
  geography = "zcta",
  variables = "B01003_001", # Total population
  year = 2022, # Change to the desired year
  survey = "acs5",
  geometry=T
)

US_population_2022 %>% filter(GEOID %in% zip_codes_TX_2019$GEOID)

cbg_shapes <- tigris::block_groups(state = "TX", year = 2021, cb = TRUE) # Replace "TX" with your state
zcta_shapes <- tigris::zctas(state="TX",year = 2021)

zip_codes <- c("77002", "77003", "77004") # Replace with your list of ZIP codes
zcta_filtered <- zcta_shapes %>%
  filter(ZCTA5CE20 %in% zip_codes_TX_2019$GEOID)

cbg_in_zips <- st_intersection(cbg_shapes, zcta_filtered)

cbg_zip_mapping_TX<-cbg_in_zips %>% as_tibble() %>%
  select(cbgs=GEOID,ZCTA=ZCTA5CE20) %>%
  st_drop_geometry() %>% unique()

save(cbg_zip_mapping_TX,file="cbg_zip_mapping_TX.RData")
###

## Processing visits - from cbgs to zip codes ------
load("cbg_zip_mapping_TX.RData")

cbg_zip_mapping_TX

#This file contains visits between cbgs and labels for NAICS and place
visits_sample<-read_csv("visits_cbg_to_cbg_TX_2018-08-06.csv")
fecha_week<-"visits_cbg_to_cbg_TX_2018-08-06.csv" %>% str_split(.,"TX_") %>% unlist(.) %>% 
  str_subset(.,"csv") %>% str_remove(.,".csv")
#Making the netowrk
graph_zip<-make_net_zipcode(visits_sample)
#Adding different normalizations
graph_zip<-diff_normalized_weights(graph_zip)
#I need to save each graph in a list, with the week as name

#These are for each normalization for the weights of the network

norms<-c("totalVisits","weight_inv","weight_range","weight_sum","weight_max")

#I uploaded and fix this file in TACC to create networks and get statistics

#Metric for each node and normalization
bind_rows(node_metrics(graph_zip,norms[1]) %>% mutate(Nomal=norms[1],Week=fecha_week),
          node_metrics(graph_zip,norms[2]) %>% mutate(Nomal=norms[2],Week=fecha_week),
          node_metrics(graph_zip,norms[3]) %>% mutate(Nomal=norms[3],Week=fecha_week),
          node_metrics(graph_zip,norms[4]) %>% mutate(Nomal=norms[4],Week=fecha_week),
          node_metrics(graph_zip,norms[5]) %>% mutate(Nomal=norms[5],Week=fecha_week)) %>%
  as.data.frame(row.names = 1:nrow(.))

#Metric for ach network and normalization
bind_rows(network_metrics(graph_zip,norms[1]) %>% mutate(Normal=norms[1],Week=fecha_week),
         network_metrics(graph_zip,norms[2]) %>% mutate(Normal=norms[2],Week=fecha_week),
         network_metrics(graph_zip,norms[3]) %>% mutate(Normal=norms[3],Week=fecha_week),
         network_metrics(graph_zip,norms[4]) %>% mutate(Normal=norms[4],Week=fecha_week),
         network_metrics(graph_zip,norms[5]) %>% mutate(Normal=norms[5],Week=fecha_week))

# Data from TACC

library(tidyverse)

setwd("~/Documents/GitHub/mobilityNetworks_hospitalizations/Data_from_TACC/")

load("node_stats_TX.RData")
net_stats<-read_csv("network_stats_TX.csv")
net_stats$Normal %>% unique() #"totalVisits"  "weight_inv"   "weight_range" "weight_sum"   "weight_max"  
net_stats %>%
#  mutate(propor_SCC=size_SCC/Nodes,propor_WCC=size_WCC/Nodes) %>%
  select(-contains("size"),-Nodes,-diameter) %>%
  pivot_longer(cols = -c("Normal","Week")) %>%
  filter(Normal == "weight_max") %>%
  ggplot(aes(x=Week,y=value)) +
  geom_line() + geom_point() + 
  facet_wrap(~name,scales = "free_y")

net_stats %>% group_by(Normal) %>% slice_max(degree_assortativity)
net_stats %>% group_by(Normal) %>% slice_max(clustering_global)
net_stats %>% group_by(Normal) %>% slice_max(mean_dist)
net_stats %>% group_by(Normal) %>% slice_min(density)




  
