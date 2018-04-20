#assignment 4
library("igraph")
library("networkD3")
library("plotly")
library("RColorBrewer")

setwd("/Users/julianna/Documents/HKU/Social media analytics/assignment4")
nodes = read.csv("ussenators_nodes.csv", header=T, as.is=T)
links = read.csv("ussenators_edges.csv", header=T, as.is=T)

#clean out those who are not sanators from the data
edges <- subset(links,links$followed %in% links$ego)

#building the net
names(nodes) <- c("name","party","gender","state")
names(edges) <- c("follower","name")
net <- graph.data.frame(edges, nodes, directed=T)
net 

#Calculation
degree(net,mode = c("all"))
degree(net,mode = c("in"))
degree(net,mode = c("out"))
betweenness(net, directed=T, weights=NA)
reciprocity(net)

#Using igraph
V(net)$color <- ifelse(V(net)$party == "Republican", "red", "blue")
V(net)$shape <- ifelse(V(net)$gender == "Male", "square", "circle")
p <- plot(net,edge.arrow.size=.1, vertex.size=5, edge.curved=0, vertex.color=V(net)$color, vertex.label=V(net)$name, 
          vertex.shape=V(net)$shape, vertex.label.color='black',vertex.label.cex=.7, layout=layout.fruchterman.reingold)


#d3
edgeList <- get.data.frame(net,"edges")
nodeList <- get.data.frame(net,"vertices")

edgeList$SourceID <- match(edgeList$to, nodeList$name)-1
edgeList$TargetID <- match(edgeList$from, nodeList$name)-1
my_color <- 'd3.scaleOrdinal() .domain(["Republican", "Democrat"]) .range(["red", "blue"])'

D3_network <- networkD3::forceNetwork(
  Links = edgeList, # data frame that contains info about edges
  Nodes = nodeList, # data frame that contains info about nodes
  Source = "SourceID", # ID of source node 
  Target = "TargetID", # ID of target node
  NodeID = "name", # display value from the node list (e.g., node name)
  Group = "party",  # value we want to use for node color
  charge = -500,
  height = 600, # Size of the plot (vertical)
  width = 1200,  # Size of the plot (horizontal)
  fontSize = 35, # Font size
  linkWidth = 0.8, # Link width
  linkColour = "lightgrey",
  colourScale = my_color,
  legend = TRUE,
  zoom = FALSE,
  bounded = TRUE
) 

networkD3::saveNetwork(D3_network, "D3.html", selfcontained = TRUE)

#community
wc <- walktrap.community(net)
dendPlot(wc, mode="hclust")

plot(wc, net, vertex.size=5, vertex.color=V(net)$color, edge.arrow.size=.5, edge.color = "grey", edge.curved=0,
      layout=layout.fruchterman.reingold)
V(net)$label <- NA
