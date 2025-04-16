#' Solve a square diagonal matrix game
#'
#' Analyzes a square diagonal matrix to determine optimal strategies and the value of a zero-sum game. Handles various cases including all-positive, all-negative, zero entries, and mixed-sign diagonals.
#'
#' @param mat A square diagonal matrix.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{p}: Optimal mixed strategy for the row player.
#'   \item \code{q}: Optimal mixed strategy for the column player.
#'   \item \code{V}: Value of the game.
#' }
#' If there are multiple zeros on the diagonal, the function simplifies the matrix and recursively re-evaluates.
#'
#' @examples
#' example_matrix <- diag(c(2, 0, 4))
#' solveDiagonalMatrix(example_matrix)
#'
#' example_matrix <- diag(c(-2, -3, 1, -4))
#' solveDiagonalMatrix(example_matrix)
#'
#' example_matrix <- diag(c(-2, -3, -1, -4))
#' solveDiagonalMatrix(example_matrix)
#'
#' example_matrix <- diag(c(-2, 0, -1, 0))
#' solveDiagonalMatrix(example_matrix)
#'
#' @export
solveDiagonalMatrix <- function(mat) {
  
  ps <- list()
  
  if (!all(mat[upper.tri(mat)] == 0 & mat[lower.tri(mat)] == 0)) {
    stop("There are non-zero values outside diagonal.")
  }
 
  diag_values <- diag(mat)
  
  if ((sum(diag_values == 0) == 1) || (any(diag_values > 0) && any(diag_values < 0))) {
    if (sum(diag_values == 0) == 1) {
      value = 0
    } else {
      value = min(diag_values)
    }
    zero_pos <- which(diag_values == value)
    count <- 1
    p <- rep(0, length(diag_values))
 
    for (i in seq_along(diag_values)) {
      if (diag_values[i] != value) {
        p[i] <- 1
        ps[[length(ps) + 1]] <- p
        count <- count + 1
        p[i] <- 0
      }
    }
    
    q <- rep(0, length(diag_values))
    q[zero_pos] <- 1
    return(list(V = 0, p = ps, q = q))
  }
  
  if (sum(diag_values == 0) >= 2) {
    message("V = 0, at least 2 zeros on the diagonal.")
    message("Removing duplicit rows and coumns.")
    new_mat <- unique(t(unique(t(mat))))
    if (identical(mat, new_mat)) return(findSaddlePoint(mat))
    print(new_mat)
    return(solveDiagonalMatrix(new_mat))
  }
  
  if (all(diag_values > 0) || all(diag_values < 0) ) {
    V <- 1 / sum(1 / diag_values)
    p <- V / diag_values
    q <- V / diag_values  
    return(list(V = V, p = p, q = q))
  }
}