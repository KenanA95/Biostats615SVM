gradient <- function(theta, x, y, C) {
  ### Compute the distance
  dist <- 1 - y * (as.matrix(x) %*% theta)
  ### Create a data frame with each row being theta for nrow of length X
  thetaDF <- as.data.frame(matrix(data = rep(theta, nrow(x)), byrow = TRUE, nrow = nrow(x)))
  ### Initialize dW
  dW <- as.data.frame(matrix(data = rep(theta, nrow(x)), byrow = TRUE, nrow = nrow(x)))
  ### For distance greater than 0
  dW[dist > rep(0, length(dist)), ] <- (dW + thetaDF - C * y * x)[dist > rep(0, length(dist)), ]
  ### For distance less than or equal to 0
  dW[dist <= rep(0, length(dist)), ] <- (dW + thetaDF)[dist <= rep(0, length(dist)), ]
  ### Return the mean of each column
  return((apply(dW, 2, mean)) - theta)
}
