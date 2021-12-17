computeRBFKernel = function(X, gamma=NULL) {
  ### This can be further optimzed by using only the support vectors to define the hyperplane (figure out the math behind it)
  n = nrow(X)
  # kernel_matrix : ndarray of shape (n_samples_X, n_samples_Y)
  if (is.null(gamma)) {
    gamma = 1 / ncol(X)
  }
  K = pdist(X, "euclidean")
  K = exp(K * -gamma)
  return(K)
}
computePolynomialKernel = function(X, C, degrees=2, gamma=NULL) {
  if (is.null(gamma)) {
    gamma = 1 / ncol(X)
  }
  K = (gamma * X%*%t(X) + C)^degrees
  return(K)
}
computeSigmoidKernel = function(X, C, gamma=NULL) {
  if (is.null(gamma)) {
    gamma = 1 / ncol(X)
  }
  K = tanh(gamma * crossprod(t(X)) + C)
  return(K)
}
