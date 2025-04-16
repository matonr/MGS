#' Remove dominated strategies from a payoff matrix
#'
#' This function removes dominated rows and columns (strategies) from a matrix, using either strict or weak domination rules. It optionally checks for domination by combinations of strategies.
#'
#' @param mat A numeric payoff matrix.
#' @param strictly Logical. If \code{TRUE}, uses strict domination; if \code{FALSE}, uses weak domination.
#' @param granularity A numeric value between 0 and 1 indicating the step size for combinations of weights. Must be evenly divisible into 1.
#' @param skip_combinations Logical. If \code{TRUE}, only single strategies are checked for domination, not combinations.
#'
#' @return A reduced matrix with dominated strategies removed.
#'
#' @examples
#' example_matrix <- matrix(c(3,6,5,3,1,2,6,0,3), nrow = 3, byrow = TRUE)
#'
#' # Without combinations
#' removeDominatedStrategies(example_matrix, strictly = TRUE, granularity = 0.1, skip_combinations = TRUE)
#'
#' # With combinations and low granularity
#' removeDominatedStrategies(example_matrix, strictly = TRUE, granularity = 0.5, skip_combinations = FALSE)
#'
#' # With combinations and high granularity
#' removeDominatedStrategies(example_matrix, strictly = TRUE, granularity = 0.1, skip_combinations = FALSE)
#'
#' @export
removeDominatedStrategies <- function(mat, strictly = TRUE, granularity = 0.05, skip_combinations = FALSE) {
  
  if (granularity <= 0 || granularity > 1) {
    stop("Granularity must be a positive value less or equal to 1.")
  }
  
  if ((1 / granularity ) %% 1 != 0) {
    stop("Enter such granularity, that interval can be divided into whole parts. E.g. 0.1, 0.05, 0.01")
  }

  # Function to check if a row is dominated by other rows or their combinations
  isRowDominated <- function(row_index, mat, strictly, granularity, skip_combinations) {
    other_rows <- mat[-row_index, , drop = FALSE]  # Exclude current row
    n_other_rows <- nrow(other_rows)
    
    if (n_other_rows == 0) return(FALSE)  # If no other row exists, return FALSE
    
    if (skip_combinations) {
      # Check for domination directly by any single row
      for (i in 1:n_other_rows) {
        if (strictly) {
          if (all(other_rows[i, ] > mat[row_index, ])) {
            return(TRUE)
          }
        } else {
          if (all(other_rows[i, ] >= mat[row_index, ])) {
            return(TRUE)
          }
        }
      }
    } else {
      # Check for domination by combinations of rows
      weights <- seq(0, 1, by = granularity)
      combinations <- expand.grid(replicate(n_other_rows, weights, simplify = FALSE))
      combinations <- combinations[abs(rowSums(combinations) - 1) < 1e-8, , drop = FALSE]  # Weights sum to 1
      
      if (is.null(combinations) || nrow(combinations) == 0) return(FALSE)  # No valid combinations, return FALSE
      
      for (i in 1:nrow(combinations)) {
        combination <- as.numeric(combinations[i, ])
        combined_row <- combination %*% other_rows
        
        if (strictly) {
          if (all(combined_row > mat[row_index, ])) {
            return(TRUE)
          }
        } else {
          if (all(combined_row >= mat[row_index, ])) {
            return(TRUE)
          }
        }
      }
    }
    return(FALSE)
  }
  
  # Function to check if a column is dominated by other columns or their combinations
  isColumnDominated <- function(col_index, mat, strictly, granularity, skip_combinations) {
    other_cols <- mat[, -col_index, drop = FALSE]  # Exclude current column
    n_other_cols <- ncol(other_cols)
    
    if (n_other_cols == 0) return(FALSE)  # If no other column exists, return FALSE
    
    if (skip_combinations) {
      # Check for domination directly by any single column
      for (i in 1:n_other_cols) {
        if (strictly) {
          # Strict domination
          if (all(other_cols[, i] < mat[, col_index])) {
            return(TRUE)
          }
        } else {
          # Weak domination
          if (all(other_cols[, i] <= mat[, col_index])) {
            return(TRUE)
          }
        }
      }
    } else {
      # Check for domination by combinations of columns
      weights <- seq(0, 1, by = granularity)
      combinations <- expand.grid(replicate(n_other_cols, weights, simplify = FALSE))
      combinations <- combinations[abs(rowSums(combinations) - 1) < 1e-8, , drop = FALSE]  # Weights sum to 1
      
      if (is.null(combinations) || nrow(combinations) == 0) return(FALSE)  # No valid combinations, return FALSE
      
      for (i in 1:nrow(combinations)) {
        combination <- as.numeric(combinations[i, ])
        combined_col <- other_cols %*% combination
        
        if (strictly) {
          if (all(combined_col < mat[, col_index])) {
            return(TRUE)
          }
        } else {
          if (all(combined_col <= mat[, col_index])) {
            return(TRUE)
          }
        }
      }
    }
    return(FALSE)
  }
  
  # Remove dominated strategies until matrix stops changing
  repeat {    
    
    if (strictly == FALSE) {
      mat <- unique(mat)
      mat <- t(unique(t(mat)))
    }

    new_mat <- mat

    # Remove dominated rows
    dominated_rows <- sapply(1:nrow(new_mat), isRowDominated, mat = new_mat, strictly, granularity, skip_combinations)
    if (all(dominated_rows == TRUE)) {
      return(new_mat[1,1])
    }
    new_mat <- new_mat[!dominated_rows, , drop = FALSE]    

    # Remove dominated columns
    dominated_columns <- sapply(1:ncol(new_mat), isColumnDominated, mat = new_mat, strictly, granularity, skip_combinations)
    if (all(dominated_columns == TRUE)) {
      return(new_mat[1,1])
    }
    new_mat <- new_mat[, !dominated_columns, drop = FALSE]

    # Check if the matrix has changed
    if (identical(mat, new_mat)) {
      break
    }
    
    # Update the matrix for the next iteration
    mat <- new_mat
  }
  
  # Return the reduced matrix
  return(mat)
}