library(randomForest)
library(caret)

# Implementation of multi-view stacking.
mvstacking <- function(D, v1cols, v2cols, k = 10){
  
  # Generate folds for internal cross validation.
  folds <- sample(1:k, size = nrow(D), replace = T)
  
  trueLabels <- NULL
  predicted.v1 <- NULL # predicted labels with view 1
  predicted.v2 <- NULL # predicted labels with view 2
  probs.v1 <- NULL # predicted probabilities with view 1
  probs.v2 <- NULL # predicted probabilities with view 2

  # Perform internal cross validation.
  for(i in 1:k){
    
    train <- D[folds != i, ]
    
    test <- D[folds == i, ]
    
    trueLabels <- c(trueLabels, as.character(test$label))
    
    # Train learner with view 1 and make predictions.
    m.v1 <- randomForest(label ~., train[,c("label",v1cols)], nt = 100)
    raw.v1 <- predict(m.v1, newdata = test[,v1cols], type = "prob")
    probs.v1 <- rbind(probs.v1, raw.v1)
    pred.v1 <- as.character(predict(m.v1, newdata = test[,v1cols], type = "class"))
    predicted.v1 <- c(predicted.v1, pred.v1)
    
    # Train learner with view 2 and make predictions.
    m.v2 <- randomForest(label ~., train[,c("label",v2cols)], nt = 100)
    raw.v2 <- predict(m.v2, newdata = test[,v2cols], type = "prob")
    probs.v2 <- rbind(probs.v2, raw.v2)
    pred.v2 <- as.character(predict(m.v2, newdata = test[,v2cols], type = "class"))
    predicted.v2 <- c(predicted.v2, pred.v2)
  }
  
  # Build first-order learners with all data.
  learnerV1 <- randomForest(label ~., D[,c("label",v1cols)], nt = 100)
  learnerV2 <- randomForest(label ~., D[,c("label",v2cols)], nt = 100)
    
  # Construct meta-features.
  metaFeatures <- data.frame(label = as.factor(trueLabels),
                             ((probs.v1 + probs.v2) / 2),
                             pred1 = predicted.v1,
                             pred2 = predicted.v2)

  #train meta-learner
  metalearner <- randomForest(label ~., metaFeatures, nt = 100)
  
  res <- structure(list(metalearner=metalearner,
                        learnerV1=learnerV1,
                        learnerV2=learnerV2,
                        v1cols = v1cols,
                        v2cols = v2cols),
                   class = "mvstacking")
  
  return(res)
}

predict.mvstacking <- function(object, newdata){
  
  # Predict probabilities with view 1.
  raw.v1 <- predict(object$learnerV1,
                    newdata = newdata[,object$v1cols],
                    type = "prob")
  
  # Predict classes with view 1.
  pred.v1 <- as.character(predict(object$learnerV1,
                                  newdata = newdata[,object$v1cols],
                                  type = "class"))
  
  # Predict probabilities with view 2.
  raw.v2 <- predict(object$learnerV2,
                    newdata = newdata[,object$v2cols],
                    type = "prob")
  
  # Predict classes with view 2.
  pred.v2 <- as.character(predict(object$learnerV2,
                                  newdata = newdata[,object$v2cols],
                                  type = "class"))

  # Check if there were missing values.
  raw.v1[is.na(raw.v1)] <- 1/ncol(raw.v1)
  raw.v2[is.na(raw.v2)] <- 1/ncol(raw.v2)
  
  # Build meta-features
  metaFeatures <- data.frame(((raw.v1 + raw.v2) / 2),
                             pred1 = pred.v1,
                             pred2 = pred.v2)

  
  # Set levels on factors to avoid errors in randomForest predict.
  levels(metaFeatures$pred1) <- object$metalearner$classes
  levels(metaFeatures$pred2) <- object$metalearner$classes
  
  predictions <- as.character(predict(object$metalearner,
                                      newdata = metaFeatures),
                              type="class")
  
  return(predictions)
}
