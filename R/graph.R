library(provParseR)
library(igraph)

prov.parse("ddg.json")

node.names <- c(get.proc.nodes()$'label', get.data.nodes()$'label', get.func.nodes()$'label', get.libs()$'label')
edges <- as.matrix(rbind(dfs$procDataEdges, dfs$dataProcEdges, dfs$funcProcEdges))
adj.graph <- matrix(0, nrow = length(node.names), ncol = length(node.names))
rownames(adj.graph) <- colnames(adj.graph) <- node.names
adj.graph[edges] <- 1




create.graph <- function(nodes, req.edges){
  node.names <- unlist(lapply(nodes, rownames), use.names = F)
  edges <- as.matrix(do.call(rbind, req.edges))
  adj.graph <- matrix(0, nrow = length(node.names), ncol = length(node.names))
  rownames(adj.graph) <- colnames(adj.graph) <- node.names
  adj.graph[edges] <- 1
  return(adj.graph)
}
