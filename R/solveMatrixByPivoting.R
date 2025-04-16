#' Solve a matrix game using the pivoting algorithm
#'
#' Uses a full pivoting method to solve a matrix game, returning optimal strategies and game value.
#'
#' @param mat A numeric matrix representing a two-player zero-sum game.
#' @param steps Logical. If \code{TRUE}, prints each pivoting step.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{p}: Optimal mixed strategy for the row player.
#'   \item \code{q}: Optimal mixed strategy for the column player.
#'   \item \code{V}: Value of the game.
#' }
#'
#' @examples
#' # Without negative values
#' example_matrix <- matrix(c(4,1,8,2,3,1,0,4,3), nrow = 3, byrow = TRUE)
#' solveMatrixByPivoting(example_matrix, steps = TRUE)
#'
#' # With negative values
#' example_matrix <- matrix(c(2,-1,6,0,1,-1,-2,2,1), nrow = 3, byrow = TRUE)
#' modified_matrix <- modifyMatrixForPivoting(example_matrix)
#' writeMatrixForPivoting(modified_matrix)
#' solveMatrixByPivoting(example_matrix, steps = TRUE)
#'
#' @export
solveMatrixByPivoting <- function(mat, steps = FALSE) {

  shift_by <- 0
  
  if (any(mat < 0)) {
    shift_by <- min(mat) * -1
    message("Negative values present in matrix. Temporarily increasing values.")
    mat <- mat + shift_by
  }
  
  mat <- modifyMatrixForPivoting(mat)
  D <- 1
  
  while (any(mat[nrow(mat), -c(1, ncol(mat))] < 0)) {
    if (steps) {
      writeMatrixForPivoting(mat)
      cat("\n")
    }
    coords <- getPivot(mat)
    temp <- pivotingStep(mat, coords[1,1], coords[1,2], D)  
    D <- temp$P
    mat <- temp$mat
  }
  
  writeMatrixForPivoting(mat)
  
  V <- (D / mat[nrow(mat), ncol(mat)]) - shift_by
  elements_of_first_row <- mat[1, -c(1, ncol(mat))]
  elements_of_last_row <- mat[nrow(mat), -c(1, ncol(mat))]
  result_row <- rep(0, nrow(mat)-2)
  for (i in 1:length(elements_of_first_row)) {
    if (elements_of_first_row[i] > 0) {
      result_row[elements_of_first_row[i]] <- elements_of_last_row[i] / mat[nrow(mat), ncol(mat)]
    }
  }
  
  elements_of_first_column <- mat[-c(1, nrow(mat)), 1]
  elements_of_last_column <- mat[-c(1, nrow(mat)), ncol(mat)]
  result_column <- rep(0, ncol(mat)-2)
  for (i in 1:length(elements_of_first_column)) {
    if (elements_of_first_column[i] > 0) {
      result_column[elements_of_first_column[i]] <- elements_of_last_column[i] / mat[nrow(mat), ncol(mat)]
    }
  }

  return(list(p = result_row, q = result_column, V = V))
}