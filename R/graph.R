library(provParseR)
library(igraph)

graph.env <- new.env(parent = emptyenv())
graph.env$adj.graph <- NULL

## IMPORTANT ## prov.parse must be called before this function
#'@export
create.graph <- function(){

  result = tryCatch({
    get.proc.nodes()
  }, error = function(e) {
    stop("You must call prov.parse first")
  })

  # Collects all the labels of all the nodes that are put into the graph
  # Type is a vector of characters
  labels <- c(get.proc.nodes()$'label',
              get.data.nodes()$'label')

  # Collects the connections between nodes that exist in the prov
  # Originally there will be 3 columns including the labels, so
  # subset out the two columns that are required for the graph
  # Type is a matrix so that the graph can be subset by it
  edges <- as.matrix(rbind(get.proc.data()[c("entity", "activity")],
                          setNames(rev((get.data.proc()[c("activity", "entity")])),
                                   names(get.data.proc()[c("activity", "entity")])))
  )[,c("entity", "activity")]

  # Create the graph, populating each element with zeros by the length of nodes
  adj.graph <- matrix(0, nrow = length(labels), ncol = length(labels))

  # Make sure the resulting matrix is labelled for grabbing the right node
  rownames(adj.graph) <- colnames(adj.graph) <- labels

  # Sets all connections to 1 by subsetting by the edges matrix
  adj.graph[edges] <- 1

  assign("adj.graph", adj.graph, envir = graph.env)

}

# This functions returns the connections that nodes have
#'@export
get.spine <- function(node.id, forward = F){
  if(!forward){
    ig <- igraph::graph_from_adjacency_matrix(graph.env$adj.graph)
  } else {
    ig <- igraph::graph_from_adjacency_matrix(t(graph.env$adj.graph))
  }

  as.character(na.omit(names(dfs(ig, node.id, "out" , unreachable = FALSE)$order)))
}
