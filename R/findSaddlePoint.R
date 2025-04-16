#' Find saddle points in a matrix
#'
#' Identifies all saddle points in a numeric matrix. A saddle point is an element
#' that is the minimum in its row and the maximum in its column.
#'
#' @param mat A numeric matrix.
#'
#' @return A list of saddle point(s), each with its position (row and column) and value.
#' If no saddle points are found, a message is printed and an empty list is returned.
#'
#' @examples
#' mat <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
#' findSaddlePoint(mat)
#'
#' @export
findSaddlePoint <- function(mat) {
  saddle_points <- list()
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      element <- mat[i, j]
      if (element == min(mat[i, ]) && element == max(mat[, j])) {
        saddle_points[[length(saddle_points) + 1]] <- list(
          position = c(row = i, col = j), value = element
        )
      }
    }
  }
  if (length(saddle_points) == 0) message("No saddle points found in the matrix.")
  return(saddle_points)
}