library(provParseR)
library(igraph)

graph.env <- new.env(parent = emptyenv())
graph.env$adj.graph <- NULL

create.graph <- function(filename){
  
  # This is where our provenacne data comes from, 
  # specified by whoever calls the create graph function
  prov.parse(filename)
  
  # Collects all the labels of all the nodes that are put into the graph
  # Type is a vector of characters
  labels <- c(get.proc.nodes()$'label', 
                  get.data.nodes()$'label', 
                  get.func.nodes()$'label', 
                  get.libs()$'label')
  
  # Collects the connections between nodes that exist in the prov
  # Originally there will be 3 columns including the labels, so 
  # subset out the two columns that are required for the graph
  # Type is a matrix so that the graph can be subset by it
  edges <- as.matrix(rbind(get.proc.data(), 
                           get.data.proc(), 
                           get.func.proc())
  )[, c("activity","entity")]
  
  # Create the graph, populating each element with zeros by the length of nodes
  adj.graph <- matrix(0, nrow = length(labels), ncol = length(labels))
  
  # Make sure the resulting matrix is labelled for grabbing the right node
  rownames(adj.graph) <- colnames(adj.graph) <- labels
  
  # Sets all connections to 1 by subsetting by the edges matrix
  adj.graph[edges] <- 1
  
  assign("adj.graph", adj.graph, envir = graph.env)
}
