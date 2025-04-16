#' Print matrix in pivoting format
#'
#' Nicely formats and prints a matrix used in a pivoting algorithm, showing variable labels and separators.
#'
#' @param mat A matrix prepared for pivoting with labeling rows/columns.
#'
#' @examples
#' example_matrix <- matrix(c(4, 1, 8, 2, 3, 1, 0, 4, 3), nrow = 3, byrow = TRUE)
#' example_matrix
#' modified_matrix <- modifyMatrixForPivoting(example_matrix)
#' writeMatrixForPivoting(modified_matrix)
#'
#' @export
writeMatrixForPivoting <- function(mat) {
  formatNumber <- function(num, prefix_pos, prefix_neg) {
    if (num < 0) {
      return(paste0(prefix_neg, num))
    } else {
      return(paste0(prefix_pos, num))
    }
  }
  
  nrow <- nrow(mat)
  ncol <- ncol(mat)
  
  col_width <- 6
  
  for (i in seq_len(nrow)) {
    row_output <- ""
    
    if (i == 1) {
      # Processing of first row
      row_output <- sprintf(paste0("%", col_width + 1, "s"), paste0("č.", mat[i, 1]))
      for (j in 2:(ncol - 1)) {
        row_output <- paste0(row_output, sprintf(paste0("%", col_width, "s"), formatNumber(mat[i, j], "x_", "y_")))
      }
      row_output <- paste0(row_output, "|", sprintf(paste0("%", col_width, "s"), ""))
    } else if (i == nrow) {
      # Processing of last row
      row_output <- sprintf(paste0("%", col_width, "s"), "")
      for (j in 2:(ncol - 1)) {
        row_output <- paste0(row_output, sprintf(paste0("%", col_width, "s"), mat[i, j]))
      }
      row_output <- paste0(row_output, "|", sprintf(paste0("%", col_width, "s"), mat[i, ncol]))
    } else {
      # Processing of other rows
      row_output <- sprintf(paste0("%", col_width, "s"), formatNumber(mat[i, 1], "y_", "x_"))
      for (j in 2:(ncol - 1)) {
        row_output <- paste0(row_output, sprintf(paste0("%", col_width, "s"), mat[i, j]))
      }
      row_output <- paste0(row_output, "|", sprintf(paste0("%", col_width, "s"), mat[i, ncol]))
    }
    
    cat(row_output, "\n")
    
    # Horizontal line before last row
    if (i == nrow - 1) {
      cat(rep("-", ncol * col_width +2), "\n", sep = "")
    }
  }
}