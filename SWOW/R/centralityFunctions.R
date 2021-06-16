
#' Network centrality
#'
#' Calculate some simple network centrality measures
#' @param X dataset
#' @param X column
#' @return The centrality of the network
#' @examples
#' temp1 <- cen
#'
#' centrality(SWen,R1.strength);
#' @export

centrality <- function(X){
 message('centrality')
}


#' Extract the largest connected component from a graph
#'
#' @param G
#' @param mode The mode (weak or strong)
#'
#' @return The strongest connected component subgraph
#' @export
#'
#' @examples
getLargestComponent <- function(G,mode){
  comp = igraph::clusters(G,mode)
  gcc = igraph::V(G)$name[comp$membership == which.max(comp$csize)]
  return(igraph::induced.subgraph(G,gcc))
}

#' Extract Isolated nodes
#'
#' Isolated nodes aka hapax legomena
#'
#' @param G
#' @param mode
#'
#' @return A list of hapax legomena nodes
#' @export
#'
#' @examples
getHapaxNodes <- function(G,mode){
  comp = igraph::clusters(G,mode)
  gcc = igraph::V(G)$name[comp$membership %in% which(comp$csize==1)]
  return(gcc)
}

#' Show graph summary statistics
#'
#' @param G
#'
#' @return description basic network statistics
#' @export
#'
#' @examples desc = graphSummary(G)
graphSummary <- function(G){
  description =  list('nodes'= igraph::gorder(G),
                     'edges' = igraph::gsize(G),
                     'directed' = igraph::is.directed(G),
                     'weighted' = igraph::is.weighted(G),
                     'bipartite' = igraph::is.bipartite(G),
                     'edge_density' = igraph::edge_density(G,loops = F),
                     'loops' = igraph::has.multiple(G)
                     )
  return(description)

}

#' Computes the normalized empirical Shannon Entropy for counts delivered in
#' the argument \code{counts.table}. Basis of the \code{log} function is
#' bits and normalization is done by division by the maximum entropy
#' \code{log(length(counts.table))} (see
#' \href{https://en.wikipedia.org/wiki/Entropy_(information_theory)#Efficiency)}{Normalized
#' Entropy}.
#'
#' Note: this corresponds to igraph diversity function when mode = all
#' This function is however much less efficient than the igraphs implementation
#'
#' @param counts.table result of invoking \code{base::table(count.vector)}
#' @param log.base The base of the logarithm. Default is the natural logarithm,
#' \code{getOption('GeneFamilies.entropy.log.base', base::exp(1))}.
#'
#' @export
#' @return A numeric value element [0,1], the normalized Shannon Entropy.
shannonEntropy <- function(v,G,mode){
  counts = igraph::E(G)[igraph::incident(G,v,mode)]$weight
  k = length(counts)
  n = sum(counts)
  return ( -sum(((counts/n) * log2(counts/n) )/log2(k) ) )
}




