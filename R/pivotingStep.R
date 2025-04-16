#' Perform one pivoting step
#'
#' Executes one step of the pivoting algorithm.
#'
#' @param mat The current pivot matrix.
#' @param x The row index of the pivot element.
#' @param y The column index of the pivot element.
#' @param D A scalar used to calculate updated values.
#'
#' @return A list with the updated matrix and the pivot value used for the next step.
#'
#' @examples
#' example_matrix <- matrix(c(4, 1, 8, 2, 3, 1, 0, 4, 3), nrow = 3, byrow = TRUE)
#' modified_matrix <- modifyMatrixForPivoting(example_matrix)
#' writeMatrixForPivoting(modified_matrix)
#' pivots <- getPivot(modified_matrix)
#' pivoted_matrix <- pivotingStep(modified_matrix, x=pivots[1,1], y=pivots[1,2], D=1)$mat
#' writeMatrixForPivoting(pivoted_matrix)
#'
#' @export
pivotingStep <- function(mat, x, y, D = 1) {

  P <- mat[x, y]

  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {
      if (!(i == 1 || j == 1 || i == x || j == y)) {
        mat[i, j] <- ((mat[i, j] * P - mat[i, y] * mat[x, j]) / D)
      }
    }
  }

  # Multiply all elements in column y except first by -1
  mat[-1, y] <- -mat[-1, y]

  # Set mat[x, y] to D
  mat[x, y] <- D
  
  # Swap values of mat[1, y] and mat[x, 1] and multiply them by -1
  # i.e. swap labels of first row and column
  temp <- mat[1, y] * -1
  mat[1, y] <- mat[x, 1] * -1
  mat[x, 1] <- temp
  
  # Increase value in upper left field by 1 (counter of iterations)
  mat[1, 1] <- mat[1, 1] + 1
  
  # Return P value (i.e. future D value)
  return(list(mat = mat, P = P))
}