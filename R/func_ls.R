loss_ls <- function(X, y, beta) {

  # Add a column of 1s to X to include the intercept in the model
  X <- cbind(rep(1, n), X)

  # Calculate the sum of squared residuals between observed values (y) and predicted values (X %*% beta)
  sum((y - X %*% beta)^2)

}


#' @title LS through optimization
#'
#' @description XXX
#' @param y A \code{vector} of response.
#' @param X A \code{matrix} of predictors.
#' @return A \code{list} containing the following attributes:
#' \describe{XX}
#' @author Roberto Molinari
#' @importFrom stats runif
#' @export
#' @examples
#' n <- 100
#' set.seed(123)
#' X <- cbind(rnorm(n), rpois(n, lambda = 3))
#' beta <- c(2, -3, 1) # true beta
#' design <- cbind(rep(1, n), X)
#' y <- design %*% beta + rnorm(n)
#' opt_ls(y = y, X = X)
plot_beta <- function(beta) {

  plot(beta)

}
