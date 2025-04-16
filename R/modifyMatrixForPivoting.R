#' Modify a matrix for pivoting procedure
#'
#' Prepares a matrix for pivoting by adding artificial labeling rows/columns and markers for iteration control.
#'
#' @param mat A numeric matrix to be modified.
#'
#' @return The modified matrix with added labeling and tracking elements.
#'
#' @examples
#' example_matrix <- matrix(c(4, 1, 8, 2, 3, 1, 0, 4, 3), nrow = 3, byrow = TRUE)
#' example_matrix
#' modifyMatrixForPivoting(example_matrix)
#'
#' @export
modifyMatrixForPivoting <- function(mat) {
  nrow <- nrow(mat)
  ncol <- ncol(mat)
  
  # Adding upper row with values -index_x
  mat <- rbind(-seq_len(ncol(mat)), mat)
  
  # Adding left column with values -index_y + 1
  mat <- cbind(-seq_len(nrow(mat)) + 1, mat)
  
  # Adding lower row filled with -1
  mat <- rbind(mat, rep(-1, ncol(mat)))
  
  # Adding right column filled with 1
  mat <- cbind(mat, rep(1, nrow(mat)))
  
  mat[1, 1] <- 1               		 	# Left upper corner
  mat[1, ncol(mat)] <- 0        		# Right upper corner
  mat[nrow(mat), 1] <- 0        		# Left lower corner
  mat[nrow(mat), ncol(mat)] <- 0 		# Right lower corner
  
  return(mat)
}