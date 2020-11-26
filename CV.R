CV_pred <- function(data, model_pred, k, ...) {
  # Source: https://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation
  # data : dataset (filtrato)
  # model_pred : preparazione del dei dati + stima su training + previsione su test
  # k : numero di folds
  
  require(cvTools)
  # Ottengo k folds
  folds <- cvFolds(NROW(data), K=k)
  # Aggiungo al dataset la colonna dei valori previsti
  data$pred <- rep(0, NROW(data))
  # CV vero e proprio
  for (i in 1:k) {
    train <- data[folds$subsets[folds$which != i], ]
    test <- data[folds$subsets[folds$which == i], ]
    # Stima del modello
    newpred <- model_pred(train, test, ...)
    data[folds$subsets[folds$which == i], ]$pred <- newpred
  }
  data$pred
}

