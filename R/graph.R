library(provParseR)
library(igraph)

graph.env <- new.env(parent = emptyenv())
graph.env$adj.graph <- NULL

create.graph <- function(filename){
  
  prov.parse(filename)
  
  node.names <- c(get.proc.nodes()$'label', 
                  get.data.nodes()$'label', 
                  get.func.nodes()$'label', 
                  get.libs()$'label')
  
  edges <- as.matrix(rbind(get.proc.data(), 
                           get.data.proc(), 
                           get.func.proc())
  )[, c("activity","entity")]
  
  adj.graph <- matrix(0, nrow = length(node.names), ncol = length(node.names))
  rownames(adj.graph) <- colnames(adj.graph) <- node.names
  adj.graph[edges] <- 1
  return(adj.graph)
}
