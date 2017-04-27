# Multiclass perceptron

require(ggplot2)

set.seed(1234);
# load datasets
testData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2C_test.csv")
testLabel <- testData[, 'y']
testData <- testData[, 1:4]
trainData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2C_train.csv ")
trainLabel <- trainData[, 'y']
trainData <- trainData[, 1:4]

mCPerceptron <- function(trainData,trainLabel,eta=0.1,iterMax=100) {

    # get levels
    levels_ <- levels(trainLabel);


    # store weight vectors for each level
    wK_ <- matrix(, nrow = length(levels_), ncol = ncol(trainData));

    # instantiate weight vectors
    for (i in 1:length(levels_)) {

        wK_[i,] <- runif(ncol(wK_));

    }

    # initialise phi
    phi_ <- as.matrix(trainData);

    # iteration counter
    iter_ <- 1;

    # max iterations
    iterMax_ <- iterMax;

    # termination flag
    term_ <- FALSE;

    # learning rate
    eta_ <- eta;

    error_ <- 0;

    # initiate main loop
    while (term_) {

        # randomise data points
        trIndex_ <- sample(1:nrow(trainData), nrow(trainData));
        phi_ <- phi_[trIndex_,];
        trLab_ <- trainLabel[trIndex_];

        # numerify labels
        trLab_ <- as.numeric(trLab_);

        # visit each datapoint
        for (n in 1:nrow(trainData)) {

            # get results of weight vectors
            pred_ <- wK_ %*% phi_[i,];

            # get the class of the predition
            predClass_ <- which.max(pred_);

            # if the class predicted does not match the label update the weights
            if (predClass_ != trLab_[i]) {

                # update weights of the class that produced the largest dot product.
                wK_[predClass_,] <- wK_[predClass_,] - eta_ * phi_[i,]

                # update the weights of the true class.
                wK_[trLab_[i],] <- wK_[trLab_[i],] + eta_ * phi_[i,];

            }
        }


        # increment iterator
        iter_ <- iter_ + 1;
        term_ <- iter_ >= iterMax_;

    }

    return(wK_);

}

mCPredictions <- function(data,labels,weights) {

    # container for predictions
    predictions_ <- matrix(, nrow = nrow(data), ncol = 1);

    # get label levels
    levels_ <- levels(labels);

    err_ <- 0;

    for (n in 1:nrow(data)) {

        # predict
        pred_ <- weights %*% as.matrix(data)[n,];

        # assign class
        predictions_[n] <- levels_[which.max(pred_)];

        # if error, increment error count.
        if (levels_[which.max(pred_)] != labels[n]) {
            err_ = err_ + 1;
        }

    }
    

    # errors
    #err_ <- sum(predictions_!=labels)

    lst_ <- list("errors" = err_/nrow(data)*100, "predictions" = predictions_);

    return(lst_);

}


# container for minibatch errors
errTot_ <- matrix(,nrow=length(seq(1,nrow(trainData),5)),ncol = 2)
index_ <- 1; # indexer


for (i in seq(1, nrow(trainData), 5)) {

    wts_ <- mCPerceptron(trainData[1:i + 4,], trainLabel[1:i + 4]);
    res_ <- mCPredictions(testData, testLabel, wts_);

    errTot_[index_,1] <- i
    errTot_[index_,2] <- res_$errors;

    index_ = index_ + 1;

}

# plot
df_ <- as.data.frame(errTot_);
ggplot(data = df_, aes(x = df_$V1, y = df_$V2)) + geom_line() + ylab("Error %") + xlab("Minibatch") + ggtitle("Multiclass Perceptron Errors");