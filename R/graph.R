#' Create provenance graph
#' 
#' create.graph uses saved provenance to create an adjacency 
#' graph that captures the dependencies between
#' data and the R statements that use or modify the data.  
#' 
#' The graph contains a node for each R statement that is executed, for each 
#' variable set, and for each file read or written, and for each URL read.  There is 
#' an edge from each R statement node to the nodes representing variables set 
#' in that statement, or files written by the statement.  There is an edge from 
#' each variable node to the statement nodes that use the variable with that value.
#' There is also an edge from each input file or URL to the statement node that performs
#' the input operation.
#' 
#' The lineage of any data value can be traced through this graph by calling \code{\link{get.lineage}}.  
#'
#' @param prov.input This is either a file name or a string containing provenance
#'   collected by rdt or rdtLite.  If this parameter is
#'   missing, and prov.parse has been called previously, that parsed provenance
#'   will be used.  The exact format of the JSON files is described in 
#'   \href{https://github.com/End-to-end-provenance/RDataTracker/blob/development/vignettes/ExtendingProvJson.pdf}{ExtendingProvJson.pdf}.
#' @param isFile A logical value indicating whether prov.input should be treated as a file name (isFile=TRUE) 
#'   or a string containing provenance (isFile=False). If prov.input is NULL, this parameter is ignored.
#' 
#' @return create.graph returns a matrix representation of the graph.  There is a row and a column for each
#'   data and procedure node in the graph.  The values in the matrix are either 1 or 0.
#'   A 1 indicates that there is an edge for the column node to the row node.  create.graph returns
#'   NULL if there is no provenance available.
#'
#' @export
#' @examples 
#' create.graph(system.file("testdata", "prov.json", package = "provGraphR"))
create.graph <- function(prov.input = NULL, isFile = T){

  if (!is.null (prov.input)) {
    prov <- provParseR::prov.parse (prov.input, isFile)
  }
  
  proc.nodes <- provParseR::get.proc.nodes(prov)
  if (is.null (proc.nodes)) {
    warning ("There is no provenance to create a graph from.")
    return (NULL)
  }

  # Collects all the ids of all the nodes that are put into the graph
  # Type is a vector of characters
  ids <- c(provParseR::get.proc.nodes(prov)$'id', provParseR::get.data.nodes(prov)$'id')

  # Collects the connections between nodes that exist in the prov
  # Originally there will be 3 columns including the ids, so
  # subset out the two columns that are required for the graph
  # Type is a matrix so that the graph can be subset by it
  proc.data.edges <- provParseR::get.proc.data(prov)[c("entity", "activity")]
  data.proc.edges <- provParseR::get.data.proc(prov)[c("activity", "entity")]
  edges <- as.matrix(rbind(proc.data.edges,
                           stats::setNames(rev(data.proc.edges), names(data.proc.edges))))
                  
  # Create the graph, populating each element with zeros by the length of nodes
  adj.graph <- Matrix::Matrix(0, nrow = length(ids), ncol = length(ids), sparse =T)

  # Make sure the resulting matrix is labelled for grabbing the right node
  rownames(adj.graph) <- colnames(adj.graph) <- ids

  # Sets all connections to 1 by subsetting by the edges matrix
  apply(edges, 1, function(edge){
    adj.graph[edge[1], edge[2]] <<- 1
  })

  return (adj.graph)
}

#' Calculate lineage of a node
#' 
#' get.lineage returns either the list of nodes that the provided node depends
#' on (backward lineage) or the list of nodes that depend on the provided node 
#' (forward lineage).
#' 
#' Most commonly, the node passed in is a data node representing either a variable,
#' a file, or a plot.  Forward lineage reports everything computed from that variable
#' or file.  Backward lineage reports everything that contributed to the variable's 
#' value, the contents of an output file or plot.
#' 
#' @param adj.graph An adjacency graph to get the lineage from.  The
#'   graph can be created by a call to create.graph.
#' 
#' @param node.id The string id for a node that the lineage is
#'   being requested for
#'
#' @param forward Logical that states whether the search is going forward
#'   through the graph from the provided node, or backwards.
#' 
#' @return get.lineage returns the forward or backward lineage of the specified node.  The lineage
#'   is represented as a vector of strings, with each string being the id of a node in the lineage.
#'   The first entry in the returned vector is the node.id passed in.  The remaining entries
#'   form a path either forward or backward through the adjacency graph.
#' 
#' @export
#' @examples 
#' adj.graph <- create.graph(system.file("testdata", "prov.json", package = "provGraphR"))
#' get.lineage (adj.graph, "d33")
#' 
#' @seealso \code{\link{create.graph}}
get.lineage <- function(adj.graph, node.id, forward = F){
  if(!forward){
    ig <- igraph::graph_from_adjacency_matrix(adj.graph)
  } else {
    ig <- igraph::graph_from_adjacency_matrix(Matrix::t(adj.graph))
  }

  as.character(stats::na.omit(names(igraph::dfs(ig, node.id, "out" , unreachable = FALSE)$order)))
}

