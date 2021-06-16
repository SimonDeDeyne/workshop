


#' Import association data as unipartite graph
#'
#' The graph should have two columns, cue, and response
#'
#' @param X
#'
#' @return
#' @export
#'
#' @examples G = importGraph(SWen)
importGraph <- function(X){
  # Only keep responses that are also cues
  X = X %>% filter(response %in% X$cue) %>% select(from = cue,to = response, weight = R1.Strength)

  # Convert to graph
  return(as_tbl_graph(X))
}
