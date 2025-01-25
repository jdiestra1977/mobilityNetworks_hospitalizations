
#I am commenting this function, since our most granular resolution are Zip codes
#Function to build a network for cbgs. Needs x="file with visits".
#This file must contain, at least, Week, home_cbg, dest_cbg, and totalVisits
# make_net_cbgs<-function(x){
#   edgelist_cbgs <- x %>%
#     select(Week, contains("cbg"), totalVisits) %>%
#     group_by(Week, home_cbg, dest_cbg) %>%
#     summarise(across(everything(), sum), .groups = "drop")
#   
#   edge_list_cbgs<-edgelist_cbgs %>% ungroup() %>% select(-Week)
#   graph_cbgs <- graph_from_data_frame(edge_list_cbgs, directed = TRUE)
#   return(graph_cbgs)  
# }


#Function to build a network for Zip codes. Needs x="file with visits".
#This file must contain, at least, Week, home_cbg, dest_cbg, and totalVisits
#It also uses the data frame "cbg_zip_mapping_TX" which must be loaded 
#load("cbg_zip_mapping_TX.RData")
make_net_zipcode<-function(x){
  edgelist_zip<-x %>% select(Week,contains("cbg"),totalVisits) %>%
    group_by(Week,home_cbg,dest_cbg) %>% 
    summarise(across(everything(), sum), .groups = "drop")%>%
    merge(cbg_zip_mapping_TX,by.x="home_cbg",by.y="cbgs") %>%
    rename_at("ZCTA",~"home_ZCTA") %>% 
    merge(cbg_zip_mapping_TX,by.x="dest_cbg",by.y="cbgs") %>%
    rename_at("ZCTA",~"dest_ZCTA") %>% as_tibble() %>%
    select(-contains("cbg")) %>% group_by(Week,home_ZCTA,dest_ZCTA) %>%
    summarise(across(everything(), sum), .groups = "drop")
  
  edge_list<-edgelist_zip %>% ungroup() %>% select(-Week)
  graph <- graph_from_data_frame(edge_list, directed = TRUE)
  return(graph)  
}

#To have different normalizations for weights to compare
diff_normalized_weights<-function(graph){
  E(graph)$weight_inv <- 1 / E(graph)$totalVisits
  # Normalize weights to the range [0, 1]
  epsilon<-1e-6
  E(graph)$weight_range <- (E(graph)$totalVisits - min(E(graph)$totalVisits)) / 
    (max(E(graph)$totalVisits) - min(E(graph)$totalVisits))
  E(graph)$weight_range[E(graph)$weight_range <= 0] <- epsilon
  # Normalize weights to sum to 1
  E(graph)$weight_sum <- E(graph)$totalVisits / sum(E(graph)$totalVisits)
  # Normalize weights by the maximum value
  E(graph)$weight_max <- E(graph)$totalVisits / max(E(graph)$totalVisits)
  return(graph)
}

#Stats for each node in the network
node_metrics<-function(graph,y){
  g=graph
  weights <- edge_attr(g, y)
  node_metrics <- data.frame(
    node = V(g)$name,
    in_degree = degree(g, mode = "in"),
    out_degree = degree(g, mode = "out"),
    in_strength = strength(g, mode = "in", weights = weights),
    out_strength = strength(g, mode = "out", weights = weights),
    bet_cent=betweenness(g, directed = TRUE, weights = weights),
    clustering_local=transitivity(g, type = "local", weights = weights)
  )
  return(node_metrics)
}

#Stats for the whole network
network_metrics<-function(graph,y){
  g=graph
  weight <- edge_attr(g, y)
  network_metric<-data.frame(
    Nodes=vcount(g),
    size_SCC=max(components(g, mode = "strong")$csize),
    size_WCC=max(components(g, mode = "weak")$csize),
    mean_dist=mean_distance(g,weights = weight),
    diameter=diameter(g, directed = TRUE, weights = weight),
    clustering_global=transitivity(g, type = "global", weights = weight),
    degree_assortativity=assortativity_degree(g, directed = TRUE),
    density=edge_density(g, loops = FALSE)
  )
  return(network_metric)
}

