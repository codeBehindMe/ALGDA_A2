# multiclass perceptron

set.seed(1234);
# load datasets
testData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2C_test.csv")
testLabel <- testData[, 'y']
testData <- testData[, 1:4]
trainData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2C_train.csv ")
trainLabel <- trainData[, 'y']
trainData <- trainData[, 1:4]

# step 1
# develop basis function
phi_ <- as.matrix(cbind(1, trainData));

# initalisation
eta_ <- 0.01;
epsilon_ <- 0.00001;
iterMax_ <- 100;

# container for training labels with numerics.
trLab_ <- trainLabel;
# training labels as numericss.
trLab_ <- as.numeric(trLab_)

# this gives 
# 0 <= C1 < 1;
# 1 <= C2 < 2;
# 2 <= C3 < 3;

# initialise empty weight vector
w_ <- matrix(, nrow = iterMax_, ncol = length(levels(trainLabel)));

# initialise some randome values
w_[1,] <- runif(ncol(w_));


# container for errors
err_ <- matrix(0, nrow = iterMax_, ncol = 1);

###############################
# checking argmax
#res_ <- w_[1,] * phi_[1,]
#tmp_ <- which(res_ == max(res_), arr.ind = TRUE)

# when we get the colnumber,
# check to see if the label of the predicted datapoint is the same as the training label.
# if it's not the same, update the weight vector of the true class.
# and update weight vector of the class that produced argmax.
################################

# initialise iter
iter_ <- 1;

# initialise termination flag
term_ <- FALSE;

# turn off warings
options(warn = -1);

f_predict <- function(weights, data, k) {

    # this function predicts the glass given weights and data

    # container for predictions
    x_ <- numeric();

    # make a prediction in each class
    for (i in 1:k) {
        x_ <- append(x_, sum(weights[i] %*% data));
    }

    # get the weight which produced the maximum.
    # since W1 is k1 , this corresponds to the predicted class.
    return(which.max(x_));

}

f_error <- function(weights, data, labels, k) {

    # this function counts the number of missclassifications
    # static counter.
    errCount <- 0;
    for (i in 1:nrow(data)) {

        predictedClass_ <- f_predict(weights, data[i], k);
        
        if (predictedClass_ != labels[i]) {
            print(paste("predictedclass: ", predictedClass_));
            print(paste("actualclass: ", labels[i]));
            errCount = errCount + 1;
        }
        
        return(errCount / nrow(data) * 100);
    }


}

while (!term_) {

    # randomise datapoints
    trainIndex <- sample(1:nrow(trainData), nrow(trainData), replace = FALSE);

    # rearrange basis functions and training labels
    phi_ <- phi_[trainIndex,];
    trLab_ <- trLab_[trainIndex];

    

    # visit each datapoint in training set
    for (n in 1:nrow(trainData)) {
        
      
        ## get the predicted label for the data
        predictedClass_ <- f_predict(w_[iter_,], phi_[n,], length(levels(trainLabel)))

        

        # if missclassified
        if (trLab_[n] != predictedClass_) {

            

            # update the predicted class
            w_[iter_ + 1, predictedClass_] <- w_[iter_, predictedClass_] - eta_ * sum(phi_[n,]);

            # update the the correct class
            w_[iter_ + 1, trLab_[n]] <- w_[iter_, trLab_[n]] + eta_ * sum(phi_[n,]);
        }

    }

    # get the error of this iteration
    err_[iter_] <- f_error(w_[iter_,], phi_, trLab_, length(levels(trainLabel)))

    iter_ = iter_ + 1;

    if (iter_ >= iterMax_) {
        term_ <- TRUE
    };


}

