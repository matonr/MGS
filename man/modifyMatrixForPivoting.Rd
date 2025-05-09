% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modifyMatrixForPivoting.R
\name{modifyMatrixForPivoting}
\alias{modifyMatrixForPivoting}
\title{Modify a matrix for pivoting procedure}
\usage{
modifyMatrixForPivoting(mat)
}
\arguments{
\item{mat}{A numeric matrix to be modified.}
}
\value{
The modified matrix with added labeling and tracking elements.
}
\description{
Prepares a matrix for pivoting by adding artificial labeling rows/columns and markers for iteration control.
}
\examples{
example_matrix <- matrix(c(4, 1, 8, 2, 3, 1, 0, 4, 3), nrow = 3, byrow = TRUE)
example_matrix
modifyMatrixForPivoting(example_matrix)

}
