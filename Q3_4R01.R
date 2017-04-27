## k(k-1)/2 perceptron

set.seed(1234);
# load datasets
testData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2C_test.csv")
testLabel <- testData[, 'y']
testData <- testData[, 1:4]
trainData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2C_train.csv ")
trainLabel <- trainData[, 'y']
trainData <- trainData[, 1:4]

target <- "C1"

oneOneClassifier <- function(trainData,trainLabel,testData,testLabel,eta,epsilon,iterMax) {

    # dermine k
    k_ <- length(levels(trainLabel));

    # container to store weights of K(K-1)/2 classifiers
    w__ <- matrix(0, nrow = k_ * (k_ - 1) / 2, ncol = ncol(trainData) + 1);

    # build K(K-1)/2 classifiers
    for (m in 1:k_*(k_ - 1) / 2) {
        w__[m,] <- binaryClassifier(trainData, trainLabel, eta, epsilon, iterMax, levels(trainLabel)[m]);
    }

    # predict test set using each classifier.
    # if the result is positive, then the prediction is that class.

    res_ <- matrix(, nrow = nrow(testData), ncol = k_ * (k_ - 1) / 2);
    for (m in 1:k_ * (k_ - 1) / 2) {

        res_[, m] <- as.matrix(cbind(1, testData)) %*% w__[m,];
    }

    preds_ <- matrix(, nrow = nrow(testData), ncol = 1);

    # 
    for (i in 1:nrow(testData)) {

        tmp_ <- which(res_[i,] > 0);

        if (length(tmp_) >= 2) {
            preds_[i] <- "confusion";
        } else {
            preds_[i] <- levels(testLabel)[tmp_];
        }

    }

    return(preds_);

}

binaryClassifier <- function(trainData,trainLabel,eta,epsilon,iterMax,target) {

    # make a binary classifier.
    # create phi
    phi_ <- as.matrix(cbind(1, trainData));

    eta_ <- eta;
    epsilon_ <- epsilon;
    iterMax_ <- iterMax;

    # rename the labels with +1 as the target class, and others as -1
    levels(trainLabel)[levels(trainLabel) == target] <- 1;
    levels(trainLabel)[levels(trainLabel) != 1] <- -1;

    # cast numeric
    trLab_ <- as.character(trainLabel)
    trLab_ <- as.numeric(trLab_);

    # weights container
    w_ <- matrix(, nrow = iterMax_, ncol = ncol(phi_));

    # initialise randome values for weights.
    w_[1,] <- runif(ncol(phi_));

    # container for errors
    err_ <- matrix(0, nrow = iterMax_, ncol = 1);

    # get random guess error
    err_[1] <- sum((phi_ %*% w_[1,]) * trLab_ < 0) / nrow(trainData) * 100;

    iter_ <- 1;
    term_ <- FALSE;

    while (!term_) {

        # shuffle data
        trainIndex <- sample(1:nrow(trainData), replace = FALSE);
        phi_ <- phi_[trainIndex,];
        trLab_ <- trLab_[trainIndex];

        for (n in 1:nrow(trainData)) {

            # termination check
            if (iter_ == iterMax_) {
                break
            };

            # search misclassified
            if ((w_[iter_, ] %*% phi_[n, ]) * trLab_[n] < 0) {

                # increment iteration
                iter_ <- iter_ + 1;

                # update weights
                w_[iter_,] <- w_[iter_ - 1,] + eta_ * phi_[n,] * trLab_[n];

                # update errors
                err_[iter_] <- sum((phi_ %*% w_[iter_,]) * trLab_ < 0) / nrow(trainData) * 100;
            }

        }

        # decay eta
        eta_ = eta_ * 0.99;

        # refresh termination
        term_ <- iter_ >= iterMax_ | abs(sum((phi_ %*% w_[iter_,]) * trLab_ < 0) / nrow(trainData) - sum((phi_ %*% w_[iter_ - 1,]) * trLab_ < 0) / nrow(trainData));
    }

    coeffs_ <- w_[iter_,];

    return(coeffs_);

}

predictions_ <- oneOneClassifier(trainData, trainLabel, testData, testLabel, 0.1, 0.001, 100)

table(predictions_)