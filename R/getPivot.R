#' Get pivot element coordinates
#'
#' Identifies the pivot position.
#'
#' @param input_matrix A matrix prepared for pivoting.
#'
#' @return Matrix of coordinates of possible pivots.
#'
#' @examples
#' example_matrix <- matrix(c(4, 1, 8, 2, 3, 1, 0, 4, 3), nrow = 3, byrow = TRUE)
#' modified_matrix <- modifyMatrixForPivoting(example_matrix)
#' getPivot(modified_matrix)
#'
#' @export
getPivot <- function(mat) {

  # Save last row without first and last number
  last_row <- mat[nrow(mat), ]
  row_vector <- last_row[-c(1, length(last_row))]
  
  # Save last column without first and last number
  last_col <- mat[, ncol(mat)]
  col_vector <- last_col[-c(1, length(last_col))]
  
  # Creation of submatrix by omiting first and last row and column
  sub_matrix <- mat[-c(1, nrow(mat)), -c(1, ncol(mat))]
  sub_matrix2 <- matrix(NA, nrow = nrow(sub_matrix), ncol = ncol(sub_matrix))
  
  for (i in 1:nrow(sub_matrix)) {
    for (j in 1:ncol(sub_matrix)) {
      if (row_vector[j] < 0 && sub_matrix[i, j] > 0) {
        sub_matrix2[i, j] <- -(row_vector[j] * col_vector[i]) / sub_matrix[i, j]
      }
    }
  }
  
  column_mins <- apply(sub_matrix2, 2, function(col) {
    if (all(is.na(col))) {
      return(NA)
    } else {
      return(min(col, na.rm = TRUE))
    }
  })
  
  max_of_mins <- max(column_mins, na.rm = TRUE)
  sub_matrix2[,-which(column_mins == max_of_mins, arr.ind = TRUE)] <- NA
  max_coords <- which(sub_matrix2 == max_of_mins, arr.ind=TRUE) + 1 
  # increased by 1 because of added row and column for pivoting

  return(max_coords)
}