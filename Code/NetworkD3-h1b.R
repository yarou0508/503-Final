# set work dirction
setwd("C:/Users/47532/Desktop/503Visualization/Final Exam")

library(igraph)
library(htmlwidgets)
library(networkD3)
library(dplyr)
library(stringr)
# Read in dataset
h1b <- read.csv('h1b_2016_computer_certified.csv')

h1b_data <- h1b%>% filter(str_detect(h1b$JOB_TITLE, c('DATA'))|
                                         str_detect(h1b$SOC_NAME, c('DATA')))


# Compute the number of certified records for each kind of job per state
C_count <- h1b_data %>% group_by(EMPLOYER_STATE, SOC_NAME) %>% summarise(number = n())
range01 <- function(x){x/30}
C_count$number <-range01(C_count$number)


# Make dataframe for NetworkD3
edgeList <- C_count
colnames(edgeList) <- c("SourceName","TargetName", "Weight")
#(head(edgeList))

# Create a graph. Use simplyfy to ensure that there are no duplicated edges or self loops
gD <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=FALSE))
# Create a node list object (actually a data frame object) that will contain information about nodes
# because networkD3 library requires IDs to start at 0
nodeList <- data.frame(ID = c(0:(igraph::vcount(gD) - 1)), nName = igraph::V(gD)$name)
#(head(nodeList ))

# Map node names from the edge list to node IDs
getNodeID <- function(x){
  which(x == igraph::V(gD)$name) - 1 # to ensure that IDs start at 0
}
# And add them to the edge list
edgeList <- plyr::ddply(edgeList, .variables = c("SourceName", "TargetName" , "Weight"), 
                        function (x) data.frame(SourceID = getNodeID(x$SourceName), 
                                                TargetID = getNodeID(x$TargetName)))

#(head(edgeList))

# Give group identifier
group <- factor(c(rep(0, 52),rep(1, 135)))
nodeList <- cbind(nodeList, group)
#(head(nodeList))

# Generate node size
nodesize <- c(rep(40, 52),rep(5, 135))
nodeList <- cbind(nodeList, nodesize)
#(head(nodeList))

# Create a networkD3
ColourScale <- 'd3.scaleOrdinal()
            .domain(["0", "1"])
.range(["#FF6900", "#694489"]);'

D3_network_h1b <- networkD3::forceNetwork(Links = edgeList, # data frame that contains info about edges
                                         Nodes = nodeList, # data frame that contains info about nodes
                                         Source = "SourceID", # ID of source node 
                                         Target = "TargetID", # ID of target node
                                         Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
                                         NodeID = "nName",
                                         Nodesize = "nodesize",
                                         colourScale = JS(ColourScale),
                                         Group = "group",  # value from the node list (data frame) that contains value we want to use for node color
                                         opacity = 0.65, # opacity
                                         zoom = TRUE) # ability to zoom when click on the node

# Plot network
D3_network_h1b 

# Save network as html file
networkD3::saveNetwork(D3_network_h1b, "NetworkD3_h1b.html", selfcontained = TRUE)
