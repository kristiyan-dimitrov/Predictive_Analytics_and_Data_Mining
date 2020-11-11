# Debugging Homework 4


# The Problem with the below function right now, is that it takes the observed values from the same fold, not the other 4 training folds
print(paste("Number of missing values before imputation:",sum(is.na(redwine))))

MSE.training <- c() # Initialize vectors to store MSE on training set
MSE.test <- c() # Initialize vectors to store MSE on test set

for (j in seq(1,1)){ # Take each fold of data one at a time
  # print("Start of first loop")
  testFold <- redwine[unlist(folds[j]),] # Take indices from the j-th fold and retrieve the redwine data for them
  trainingFolds <- redwine[unlist(folds[-j]),] # Take the indices in all the folds except the j-th one
  # and retrieve the corresponding redwine data as training set
  
  for(i in seq(1,ncol(testFold))){ # Take one column at a time from the testFold
    # print("Start of inner for loop")
    missing = is.na(testFold[,i]) # Find all the locations where there is a missing value in that column
    # print(missing)
    n.missing = sum(missing) # Find the number of missing values
    # print(n.missing)
    observed_values = na.omit(trainingFolds[,i]) # Take all the non-NA values from the i-th column of the trainingFolds
    # print(observed_values)
    testFold[,i][missing] <- sample(observed_values, n.missing, replace = TRUE) # Replace all the missing values with random samples from the training Folds
    print(testFold[,i])
    print(testFold[,i][missing])
    # print(sum(is.na(testFold)))
    # print(testFold)
    # print("End of inner for loop")
  }
  # print("Before LMFIT")
  lmfit <- lm(QA ~ ., data = na.omit(trainingFolds)) # Fit linear regression on trainingFolds (excluding incomplete observations!)
  # print("After LMFIT")
  MSE.training <- c(MSE.training, mean(lmfit$residuals^2)) # Compute MSE for trainingFolds and store it
  
  # print("Before Prediction")
  # print(sum(is.na(testFold)))
  testFold.predicted <- predict(lmfit.test, newdata = testFold) # Make predictions on testFold
  # print("After Prediction")
  # print("Before Storing MSE test")
  # print(testFold.predicted)
  # print(testFold)
  MSE.test <- c(MSE.test, mean((testFold[,1] - testFold.predicted)^2)) # Compute MSE for testFold and store it
  # print("After Storing MSE test")
}
print(paste("Number of missing values after imputation:",sum(is.na(redwine))))
print(paste("The average MSE for the trainingFolds is:", mean(MSE.training)))
print(paste("The average MSE for the testFold is:", mean(MSE.test)))
print(MSE.training)
print(MSE.test)