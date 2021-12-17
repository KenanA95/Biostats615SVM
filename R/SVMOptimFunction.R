#' Implement the SVM function using optim()
#'
#' @param x a matrix
#' @param y a matrix of the classes
#' @param cost the cost function
#' @param gradient the gradient function
#' @param C the hyperparameter C for SVM
#' @param method the method to use in the optim() function. Here, we suggest L-BFGS-B or Nelder-Mead
#' @export
#'
SVM.optim <- function(x, y, cost, gradient, C, method){
  #Optimization function
  optimOut <- optim(par = rep(0, ncol(x)), f = cost, gr = gradient, x = x, y = y, C = C, method = method)
  #Prediction Classification, As We sometimes input matrix, perform ifelse statement to generate predictions as is.
  time <- system.time(optim(par = rep(0, ncol(x)), f = cost, gr = gradient, x = x, y = y, C = C, method = method))
  theta <- optimOut$par
  preds <- ifelse(x %*% theta  > 0, 1, -1)
  return(list(optim_output = optimOut,predictions = as.vector(preds),ConfusionMatrix = table(factor(y),factor(preds)),Accuracy = sum(ifelse(preds==y,1,0))/length(preds), time = time))
}
