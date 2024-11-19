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
opt_ls <- function(y, X) {

  p <- ncol(X) + 1

  optim(rep(0, p), fn = loss_ls, X = X, y = y)$par

}
