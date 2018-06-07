library(provParseR)
library(igraph)

prov.parse("testing.json")

dfs <- get.all.df()

node.names <- c(get.proc.nodes()[[1]], rownames(dfs$dataNodes), rownames(dfs$funcNodes), rownames(dfs$libs))
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
