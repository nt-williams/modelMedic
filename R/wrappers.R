SL.caretRF <- function(Y, X, newX, family, obsWeights, ...) {
  SL.caret(Y, X, newX, family, obsWeights, method = 'rf',  tuneLength = 50,
           trControl =  caret::trainControl(method = "LGOCV", number = 1,
                                            verboseIter = TRUE), ...)
}
SL.caretXGB <- function(Y, X, newX, family, obsWeights, ...) {
  SL.caret(Y, X, newX, family, obsWeights, method = 'xgbTree', tuneLength = 300,
           trControl =  caret::trainControl(method = "LGOCV", number = 1, search = 'random',
                                            verboseIter = TRUE), ...)
}
SL.caretFDA <- function(Y, X, newX, family, obsWeights, ...) {
  SL.caret(Y, X, newX, family, obsWeights, method = 'fda', tuneLength = 100,
           trControl =  caret::trainControl(method = "LGOCV", number = 1, search = 'random',
                                            verboseIter = TRUE), ...)
}
SL.caretSVM <- function(Y, X, newX, family, obsWeights, ...) {
  SL.caret(Y, X, newX, family, obsWeights, method = 'lssvmPoly',
           tuneLength = 50,
           trControl =  caret::trainControl(method = "LGOCV", number = 1, search = 'random',
                                            verboseIter = TRUE),
           ...)
}

SL.caretGAM <- function(Y, X, newX, family, obsWeights, ...) {
  SL.caret(Y, X, newX, family, obsWeights, method = 'gamSpline',
           tuneLength = 50,
           trControl =  caret::trainControl(method = "LGOCV", number = 1, search = 'random',
                                            verboseIter = TRUE),
           ...)
}


SL.caretGLMB <- function(Y, X, newX, family, obsWeights, ...) {
  SL.caret(Y, X, newX, family, obsWeights, method = 'glmboost',
           tuneLength = 50,
           trControl =  caret::trainControl(method = "LGOCV", number = 1, search = 'random',
                                            verboseIter = TRUE),
           ...)
}

SL.caretMARS <- function(Y, X, newX, family, obsWeights, ...) {
  SL.caret(Y, X, newX, family, obsWeights, method = 'bagEarth',
           tuneLength = 50,
           trControl =  caret::trainControl(method = "LGOCV", number = 1, search = 'random',
                                            verboseIter = TRUE),
           ...)
}

SL.caretGAMB <- function(Y, X, newX, family, obsWeights, ...) {
  SL.caret(Y, X, newX, family, obsWeights, method = 'glmboost',
           tuneLength = 50,
           trControl =  caret::trainControl(method = "LGOCV", number = 1, search = 'random',
                                            verboseIter = TRUE),
           ...)
}

SL.glmnet2 <- function (Y, X, newX, family, obsWeights, id, alpha = 1, nfolds = 10,
                        nlambda = 100, useMin = TRUE, loss = "deviance", ...)
{
  .SL.require("glmnet")
  if (!is.matrix(X)) {
    X <- model.matrix(~-1 + .^2, X)
    newX <- model.matrix(~-1 + .^2, newX)
  }
  fitCV <- glmnet::cv.glmnet(x = X, y = Y, weights = obsWeights,
                             lambda = NULL, type.measure = loss, nfolds = nfolds,
                             family = family$family, alpha = alpha, nlambda = nlambda,
                             ...)
  pred <- predict(fitCV, newx = newX, type = "response", s = ifelse(useMin,
                                                                    "lambda.min", "lambda.1se"))
  fit <- list(object = fitCV, useMin = useMin)
  class(fit) <- "SL.glmnet"
  out <- list(pred = pred, fit = fit)
  return(out)
}

SL.glmnet3 <- function (Y, X, newX, family, obsWeights, id, alpha = 1, nfolds = 10,
                        nlambda = 100, useMin = TRUE, loss = "deviance", ...)
{
  .SL.require("glmnet")
  if (!is.matrix(X)) {
    X <- model.matrix(~-1 + .^3, X)
    newX <- model.matrix(~-1 + .^3, newX)
  }
  fitCV <- glmnet::cv.glmnet(x = X, y = Y, weights = obsWeights,
                             lambda = NULL, type.measure = loss, nfolds = nfolds,
                             family = family$family, alpha = alpha, nlambda = nlambda,
                             ...)
  pred <- predict(fitCV, newx = newX, type = "response", s = ifelse(useMin,
                                                                    "lambda.min", "lambda.1se"))
  fit <- list(object = fitCV, useMin = useMin)
  class(fit) <- "SL.glmnet"
  out <- list(pred = pred, fit = fit)
  return(out)
}
