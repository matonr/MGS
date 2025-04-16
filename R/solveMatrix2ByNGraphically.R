#' Graphical solution for a 2-by-n (or n-by-2) matrix game
#'
#' Solves a matrix game where one player's strategies are limited to two (i.e., a 2-by-n or n-by-2 matrix). The function finds intersections of the corresponding lines, identifies the saddle point graphically, plots the game structure, and returns the optimal strategies and value.
#'
#' @param mat A numeric matrix with either 2 rows or 2 columns.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{p}: Optimal mixed strategy for the row player.
#'   \item \code{q}: Optimal mixed strategy for the column player.
#'   \item \code{V}: Value of the game.
#' }
#' A plot showing the lines and their intersection points is also generated.
#'
#' @examples
#' example_matrix <- matrix(c(1, -1, 5, -2, 3, 2), nrow = 2, byrow = TRUE)
#' solveMatrix2ByNGraphically(example_matrix)
#'
#' example_matrix <- matrix(c(1, 0, 1, 0, 3, 1), nrow = 2, byrow = TRUE)
#' solveMatrix2ByNGraphically(example_matrix)
#'
#' example_matrix <- matrix(c(1, 1, 3, 0, 0, 4), nrow = 3, byrow = TRUE)
#' solveMatrix2ByNGraphically(example_matrix)
#'
#' @export
solveMatrix2ByNGraphically <- function(mat) {

  # Checks if matrix is 2xn or nx2 and makes it 2xn
  is_transposed <- ncol(mat) == 2
  if (is_transposed) mat <- t(mat)

  n <- ncol(mat)
  intersections <- data.frame(x = numeric(), y = numeric(), line1 = integer(), line2 = integer())
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      m1 <- mat[1, i]
      c1 <- mat[2, i]
      m2 <- mat[1, j]
      c2 <- mat[2, j]
            
      denom <- (m1 - c1) - (m2 - c2)
      if (denom != 0) {
        x_int <- (c2 - c1) / denom
        y_int <- m1 * x_int + c1 * (1 - x_int)
        intersections <- rbind(intersections, data.frame(x = x_int, y = y_int, line1 = i, line2 = j))
      }
    }
  }
  
  if (nrow(intersections) == 0) {
    message("No intersections.")
    return(list())
  }
  else if (!any(intersections$x >= 0 & intersections$x <= 1)){
    message("No intersections for p between 0 and 1.")
    return(list())
  }
  else{
    intersections <- intersections[intersections$x >= 0 & intersections$x <= 1, ]
    min_x <- 0
    max_x <- 1
    
    plot(NULL, xlim = c(0, 1), ylim = c(min(intersections$y) - 1, max(intersections$y) + 1), 
    xlab = "x", ylab = "y", main = "Lines and intersections")

    plotLines<- function(m, c, min_x, max_x, color = "green") {
      x_values <- seq(min_x, max_x, length.out = 100)
      y_values <- m * x_values + c * (1 - x_values)

      lines(x_values, y_values, col = color, lwd = 2)
    }
    for (i in 1:ncol(mat)) {
      plotLines(mat[1, i], mat[2, i], min_x, max_x)
    }

    # Lowest/highest intersection based on y value
    if (!is_transposed) found_intersection <- intersections[which.min(intersections$y), ]
    else found_intersection <- intersections[which.max(intersections$y), ]
    
    plotLines(mat[1, found_intersection$line1], mat[2, found_intersection$line1], min_x, max_x, "red")
    plotLines(mat[1, found_intersection$line2], mat[2, found_intersection$line2], min_x, max_x, "red")
    points(intersections$x, intersections$y, col = "blue", pch = 19, cex = 1.5)
    points(found_intersection$x, found_intersection$y, col = "red", pch = 19, cex = 1.5)

    result_m <- mat[, c(found_intersection$line1, found_intersection$line2)]

    temp_s <- findSaddlePoint(result_m)
    if (length(temp_s) != 0){
      p <- rep(0, ncol(mat))
      q <- rep(0, nrow(mat))
      if (temp_s[[1]]$position[2] == 1) p[found_intersection$line1] <- 1
      else p[found_intersection$line2] <- 1
      if (temp_s[[1]]$position[1] == 1) q[1] <- 1
      else q[2] <- 1
      V <- temp_s[[1]]$value
      if (is_transposed) return(list(p = p, q = q, V = V))
      else return(list(p = q, q = p, V = V))
    }

    temp_m <- solveNonsingularSquareMatrix(result_m)

    p <- rep(0, ncol(mat))
    p[found_intersection$line1] <- (temp_m$p)[1]
    p[found_intersection$line2] <- (temp_m$p)[2]

    q <- rep(0, nrow(mat))
    q[1] <- (temp_m$q)[1]
    q[2] <- (temp_m$q)[2]

    if (is_transposed) return(list(p = p, q = q, V = temp_m$V))
    else return(list(p = q, q = p, V = temp_m$V))
  }
}