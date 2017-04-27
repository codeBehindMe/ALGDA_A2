## Functions for ridge regression using stochastic descent ##
############################################################################
##      Name: Aruna (Aaron) Tillekeratne                                    ##
##      Date: 22/07/2016                                                    ##
##      Version: 10001                                                      ##
##      Assigment: Linear models for regression and classification.         ##
##      Application: Part A - Ridge Regression                              ##
##      Dev R Version: 3.3.0 (2016-05-03) -- Supposedly Educational         ##
##      Developer Platform: x64 Windows 10 (64--bit)                        ##
##      Developer IDE: Visual Studio 2015                                   ##
############################################################################

stochasticDescent <- function(trainData, trainLabel, testData = NULL, testLabel = NULL, eta = 0.00000000001, iterMax = 10000, lambda = 0.2, epsilon = 0.1, basis = 1, seed = NULL) {

    # reproducibility
    if (!is.null(seed)) {
        set.seed(seed);
    };

    # this function uses stochastic descent to find the optimal parameter weights for regression function.

    ## parameterisation ##

    # training data length
    trainLen <- nrow(trainData);
    # learning rate
    eta_ <- eta;
    # max number if iterations
    iterMax_ <- iterMax;
    # lambda value
    lambda_ <- lambda;
    # rmse terminiation condition
    epsilon_ <- epsilon;

    ## containers ##

    # develop basis function and container
    phi_ <- as.matrix(cbind('x0' = basis, trainData));
    # container for coefficients
    w_ <- matrix(, nrow = iterMax_, ncol = ncol(phi_));
    # container for errors
    err_ <- data.frame('iteration' = 1:iterMax_);

    ## utility functions ##

    f_prediction <- function(phi, w) {
        # basis function * params.
        return(phi %*% w);
    }

    f_error <- function(phi, w, label) {
        # rmse
        return(sqrt(sum((((label - f_prediction(phi = phi, w = w)) ^ 2) / 2))));
    }


    ## initialisation ##

    # initialise iterations
    iter_ <- 1;
    # guess initial coefficients
    w_[iter_,] <- rnorm(ncol(phi_));
    # initialise termination flag
    term_ <- FALSE;

    # Begin Descent
    while (!term_) {

        # check termination criteron
        term_ <- iter_ >= iterMax_ | epsilon_ > f_error(phi = phi_, w = w_[iter_,], label = trainLabel);

        # randomise data
        trainIndex_ <- sample(1:trainLen, trainLen, replace = FALSE);

        # rearrange the basis function and training labels (do this locally).
        phi_ <- phi_[trainIndex_,];
        trLabels_ <- trainLabel[trainIndex_];

        # visit data point
        for (n in 1:trainLen) {

            # calculate error in iter, it should terminiate correctly.
            err_[iter_, 'train'] <- f_error(phi = as.matrix(cbind(1, trainData)), w = w_[iter_,], trainLabel);
            if (!is.null(testData)) {
                err_[iter_, 'test'] <- f_error(phi = as.matrix(cbind(1, testData)), w = w_[iter_,], testLabel);
            }

            # check for termination criterea.
            if (iter_ >= iterMax_) {
                term_ <- TRUE;
                break;
            }

            # make a prediction with teh current coefficients for the datapoint.
            wPhi_ <- f_prediction(phi = phi_[n,], w = w_[iter_,]);

            # for each coefficient, use the loss function to update the coefficients.
            for (c in 1:ncol(w_)) {

                w_[iter_ + 1, c] <- w_[iter_, c] + eta_ * sum((trLabels_[n] - wPhi_) * phi_[n, c] + (lambda_ * w_[iter_, c]));
                # L2
            }
            # increment iterator.
            iter_ = iter_ + 1;
        }
    }
    # return objects as list.
    return(list('coefficients' = w_[iter_ - 1,], 'errors' = err_));
}

batchDescent <- function(trainData, trainLabel, testData = NULL, testLabel = NULL, eta = 0.1, iterMax = 1000, lambda = 0.2, epsilon = 0.000000000001, basis = 1, seed = NULL) {

    # reproducibility
    if (!is.null(seed)) {
        set.seed(seed);
    };

    # this function uses batch descent to find the optimal parameter weights for regression function.

    ## parameterisation ##

    # training data length
    trainLen <- nrow(trainData);
    # learning rate
    eta_ <- eta;
    # max number if iterations
    iterMax_ <- iterMax;
    # lambda value
    lambda_ <- lambda;
    # rmse terminiation condition
    epsilon_ <- epsilon;

    ## containers ##

    # develop basis function and container
    phi_ <- as.matrix(cbind('x0' = basis, trainData));
    # container for coefficients
    w_ <- matrix(, nrow = iterMax_, ncol = ncol(phi_));
    # container for errors
    err_ <- data.frame('iteration' = 1:iterMax_);

    ## utility functions ##

    f_prediction <- function(phi, w) {
        # basis function * params.
        return(phi %*% w);
    }

    f_error <- function(phi, w, label) {
        # rmse
        return(sqrt(sum((((label - f_prediction(phi = phi, w = w)) ^ 2) / 2))));
    }

    ## initialisation ##

    # initialise iterations
    iter_ <- 1;
    # guess initial coefficients
    w_[iter_,] <- rnorm(ncol(phi_));
    # initialise termination flag
    term_ <- FALSE;

    # Begin Descent
    while (!term_) {

        # set terminiation critereon.
        term_ <- iter_ >= iterMax_ | epsilon_ > f_error(phi = phi_, w = w_[iter_,], trainLabel);

        # make prediction from the current coefficients;
        wPhi_ <- f_prediction(phi = phi_, w = w_[iter_,]);

        # initialise line search eta for decay
        etaPrime_ <- eta_;

        # begin line search
        while (etaPrime_ > epsilon_) {

            # check termination criteria
            if (iter_ >= iterMax | epsilon_ > f_error(phi = phi_, w = w_[iter_, ], trainLabel)) {
                term_ <- TRUE;
                break;
            }

            # update coeffs for each column
            for (c in 1:ncol(w_)) {
                w_[iter_ + 1, c] <- w_[iter_, c] + etaPrime_ * sum((trainLabel - wPhi_) * phi_[, c] + (lambda_ * w_[iter_, c]));
            }
            
            # get the errors for comparison
            eIter <- f_error(phi = phi_, w = w_[iter_,], trainLabel);
            eIterPlus1 <- f_error(phi = phi_, w = w_[iter_ + 1,], trainLabel);

            # break if no improvement
            if (eIter > eIterPlus1) {
                break;
            }

            # decay eta 
            etaPrime_ = etaPrime_ / 2;
        }

        # record error
        err_[iter_, 'train'] <- f_error(phi = phi_, w = w_[iter_,], trainLabel);
        if (!is.null(testData)) {
            err_[iter_, 'test'] <- f_error(phi = as.matrix(cbind(1, testData)), w = w_[iter_,], testLabel);
        }

        # record eta usage
        err_[iter_, 'eta'] <- etaPrime_;

        iter_ = iter_ + 1;

    }

    return(list('coeffs' = w_[iter_ - 1,], 'errors' = err_));
}