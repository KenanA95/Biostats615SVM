cost <- function(theta, x, y, C) {
  ### N is the number of rows in the X matrix
  N <- nrow(x)
  ### Compute dist
  dist <- 1 - y * (as.matrix(x) %*% theta)
  ### replace all negative dist values with 0
  dist[dist < 0] = 0
  ### Compute the hinge loss
  hinge_loss <- C * (sum(dist) / N)
  ### Compute the cost
  cost <- 1 / 2 * crossprod(theta) + hinge_loss
  return(cost)
}
