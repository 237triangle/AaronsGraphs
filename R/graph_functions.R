#' Edge list
#'
#' @param A unweighted adjacency matrix of graph
#'
#' @return ordered list of edges
#' @export
#'
#' @examples
#' a <- matrix(c(0,1,1,0), nrow =2)
#' edge_list_fn(a)
#'
edge_list_fn <- function(A){
  out <- list()
  n <- dim(A)[1]
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (A[i,j] == 1) {
        out[[n*i+j]] <- c(i,j) #changed so no overlap in indices
      }
    }
  }
  return(
    out[-which(sapply(out, is.null))]
  )
}




#' Triangle list
#'
#' @param A unweighted adjacency matrix
#'
#' @return ordered list of triangles
#' @export
#'
#' @examples
#' a <- matrix(c(0,1,1,1,0,1,1,1,0), nrow = 3)
#' triangle_list_fn(a)
#'
triangle_list_fn <- function(A){
  x <- igraph::triangles(
    igraph::graph_from_adjacency_matrix(A)
  )
  x <- matrix(x, nrow = 3)
  if (dim(x)[2] == 0) { # break if there are no 3-cliques
    return(list())
  }
  x <- apply(x, 2, sort)
  df <- data.frame(V1 = x[1,], V2 = x[2,], V3 = x[3,])
  df <- df |>
    dplyr::arrange(V1) |>
    dplyr::group_by(V1) |> dplyr::arrange(V2) |>
    dplyr::ungroup()
  df <- df |> dplyr::group_by(V1, V2) |>
    dplyr::arrange(V3, .by_group = T) |>
    dplyr::ungroup()
  out <- as.list(as.data.frame(t(df)))
  names(out) <- NULL
  return(out)
}
