#' Solve a nonsingular square matrix
#'
#' Solves a nonsingular square matrix by calculating a value \code{V}, a column strategy vector \code{q}, and a row strategy vector \code{p_t} using matrix inversion. If the determinant is zero, a temporary regularization is applied.
#'
#' @param mat A square numeric matrix.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{p}: Optimal mixed strategy for the row player.
#'   \item \code{q}: Optimal mixed strategy for the column player.
#'   \item \code{V}: Value of the game.
#' }
#' If the matrix is singular, the function adjusts it temporarily by adding 1 to all elements. If the matrix is not invertible or its rows are linearly dependent, an error is raised.
#'
#' @examples
#' example_matrix <- matrix(c(1,2,-1,2,-1,4,-1,4,-3), nrow = 3, byrow = TRUE)
#' example_matrix
#' solveNonsingularSquareMatrix(example_matrix)
#'
#' # With determinant = 0
#' example_matrix <- matrix(c(0,1,-2,1,-2,3,-2,3,-4), nrow = 3, byrow = TRUE)
#' example_matrix
#' solveNonsingularSquareMatrix(example_matrix)
#'
#' @export
solveNonsingularSquareMatrix <- function(mat) {
  
  if (!is.matrix(mat)) {
    stop("Input is not a matrix")
  }
  
  if (nrow(mat) != ncol(mat)) {
    stop("Input matrix is not square.")
  }
  
  shift_by <- 0
  # Check if determinant is not 0
  if (det(mat) == 0) {
    message("Determinant of matrix is 0. Temporarily adding 1.")
    shift_by <- 1
    mat <- mat + matrix(1, nrow = nrow(mat), ncol = ncol(mat))
  }
  
  # Check rows for linear independence
  rank_value <- qr(mat)$rank
  if (rank_value != nrow(mat)) {
    stop("Rows of matrix are linearly dependent")
  }
  
  # Check if 1^T * A^-1 * 1 != 0
  inv_mat <- solve(mat)
  ones <- matrix(rep(1, nrow(mat)), ncol = 1)
  result <- as.numeric(t(ones) %*% inv_mat %*% ones)
  if (result == 0) {
    stop("1^T * A^-1 * 1 equals 0.")
  }
  
  V <- 1 / result
  q <- as.numeric(V) * (inv_mat %*% ones)
  p <- as.numeric(V) * (t(ones) %*% inv_mat)
  
  return(list(p = p, q = q, V = V - shift_by))
}