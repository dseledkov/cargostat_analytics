options(stringsAsFactors = FALSE)
#-----------------------------------------#
#--------- best minimal example ----------#
#-----------------------------------------#
library(visNetwork)
edges  <- data.frame(from = c(2,2,4,1,1), to = c(1,3,2,3,5))
colnames(edges) <- c("from","to")
edges <- data.frame(from = edges$from, to = edges$to)
nodes <- data.frame(id = 1:nrow(edges))

#-----------------------------------------#
#--------- plot basic network ------------#
#-----------------------------------------#
visNetwork(nodes, edges, width = "100%")

#-----------------------------------------#
#--------- plot directed network ---------#
#-----------------------------------------#
visNetwork(nodes, edges) %>%
  visEdges(arrows = 'from', scaling = list(min = 2, max = 2)) %>%
  visLegend()

#-----------------------------------------#
#--------- save results to html ----------#
#-----------------------------------------#
network <- visNetwork(nodes, edges) %>%
  visEdges(arrows = 'from', scaling = list(min = 2, max = 2))
visSave(network, file = "network.html")
