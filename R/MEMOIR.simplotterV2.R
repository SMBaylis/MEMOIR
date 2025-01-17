#' Generate plots and densely-sampled estimated survival curves from a fitted MEMOIR model
#' with observations from multiple locations or populations
#'
#' Estimates survival curves within each population using both non-parametric Kaplan-Meier
#' estimation and a parametric model of choice. Returns a figure showing the estimated
#' mortality curve for each iteration in simulated data, or a single estimated mortality
#' curve in real species data. For discussion of the different models implemented here, see
#' Baylis et al (xxxx) and Baylis S. M. (2016) [PhD title goes here].
#' @param x A dataframe generated using MEMOIR.fit()[[1]]
#' @param zombieMode Convenience function, re-codes CENS values by flipping all zeroes to
#'                   ones and vice-versa. Implemented because MEMOIR.sim() codes live and
#'                   dead recaptures using opposite coding to the functions in package
#'                   Survival by default. TRUE by default.
#' @param deadOnly When TRUE, excludes live records from the dataset before analysis. Can be
#'                 useful to prevent bias arising from informative censoring.
#' @param PMType Type of parametric model to use in location-specific sub-models. Must be
#'               a distribution included in survreg() from package Survival, so currently must
#'               be one of "weibull", "exponential", "gaussian", "logistic", "lognormal",
#'               or "loglogistic".
#' @param displayType Type of fit to plot. One of "kaplan" or "parametric". If "parametric",
#'                    the plot will display models based on whatever parametric distribution
#'                    was chosen in PMType.
#' @return The function returns a list with two dataframes. The first dataframe has estimated
#'         survival by age for each iteration based on non-parametric Kaplan-Meier estimates.
#'         The second dataframe has estimated survival by age for each iteration based on
#'         whatever parametric distribution was selected in PMType. Also returns a plot, as
#'         chosen in displayType.
#' @export



MEMOIR.simplotterV2 <- function(x, zombieMode = TRUE, deadOnly = FALSE, PMType = "weibull",
                              displayType = "kaplan") {

library(survival)
outs <- data.frame(iter = c(0),
                   ages = c(0), numberAlive = c(0),
                   propAlive = c(0))
KMOuts <- data.frame(iter = c(0),
                     ages = c(0), numberAlive = c(0),
                     propAlive = c(0))
data <- x
    
for (i in unique(data$SCIENA)) {
    spData <- subset(data, SCIENA == i)
    if(zombieMode == TRUE) {
        spData$CENS <- 1-spData$CENS
    }
    if(deadOnly == TRUE) {
        spData <- subset(spData, spData$CENS == 1)
    }
    surviv <- with(spData, Surv(ELAPSED_DAYS, CENS))
    fKM <- survfit(surviv ~ CENS, data = spData, conf.int=FALSE)
    plot(fKM, xlab = "Age (years)", ylab = "Proportion surviving to age x", axes = FALSE,
         col = "grey70")
    axis(side = 2)
    axis(side = 1, at = seq(365.25, max(spData$ELAPSED_DAYS), (2*365.25)),
         labels = seq(1, floor(max(spData$ELAPSED_DAYS))/365.25, 2))
    title(paste(i))
    box()

    for(j in unique(spData$repNum)) {
        activeData <- subset(spData, spData$repNum == j)
        if(nrow(activeData) >= 1) {
            MRL <- max(activeData$ELAPSED_DAYS)
            sampleAges <- data.frame(ages = c(0,
                                          seq(from = max(spData$ELAPSED_DAYS)/1000,
                                              to = max(spData$ELAPSED_DAYS),
                                              by = max(spData$ELAPSED_DAYS)/1000)),
                                 blanks = c(rep(NaN, 1001))
                                 )
            KMAges <- data.frame(ages = c(0,
                                          seq(from = max(spData$ELAPSED_DAYS)/1000,
                                              to = max(spData$ELAPSED_DAYS),
                                              by = max(spData$ELAPSED_DAYS)/1000)),
                                 blanks = c(rep(NaN, 1001))
                                 )
            activeData <- droplevels(activeData)
            nHolder <- c(NaN)
            try( {
            for (l in unique(activeData$BANDLOC)) {
                ARAL <- subset(activeData, activeData$BANDLOC == l)
                ARALHist_d <- (max(activeData$RECOVERYEAR) - min(ARAL$BANDYEAR)) * 365
                ARALSurviv <- with(ARAL, Surv(ELAPSED_DAYS, CENS))
                ###### the ARALSurviv ~ 1 case is no longer supported###################
                ARALwkm <- survfit(ARALSurviv ~ ARAL$CENS, weights = round(ARAL$Wbail))
                ########################################################################
                survest <- stepfun(ARALwkm$time, c(1, ARALwkm$surv))
                KMAges[,ncol(KMAges) + 1] <- survest(KMAges$ages)

                nHolder[ncol(KMAges) - 2] <- nrow(ARAL)

                if (ARALHist_d <= max(activeData$ELAPSED_DAYS) &&
                    nrow(ARAL) >= 2) {
                    ARALwwei <- survreg(ARALSurviv ~ CENS, weights = round(ARAL$Wbail)
                                        , dist = PMType, data = ARAL)
                    if (!is.nan(as.vector(ARALwwei[[1]][1]))) {
                        rawpreds <- data.frame(days = c(0,
                                                        predict(ARALwwei,
                                                                newdata = list(CENS = 1),
                                                                type = "quantile",
                                                          p = seq(.001, .999, by = .001))),
                                               propSurv = c(1, seq(.999, .001, -.001)))
                        sampleAges[,ncol(sampleAges)+1] <- c(rep(NaN, nrow(sampleAges)))
                        for(k in 1:nrow(sampleAges)) {
                            diffs <- abs(sampleAges$ages[k] - rawpreds$days)
                            isMin <- diffs == min(diffs)
                            sampleAges[k, ncol(sampleAges)] <- rawpreds$propSurv[isMin == TRUE]
                        }

                    }
                }
            }

            sampleAges <- sampleAges[,-2]
            KMAges <- KMAges[,-2]
            totalN <- sum(nHolder)

            if(!is.null(ncol(sampleAges))) {
            for (l in 2:ncol(sampleAges)) {
                sampleAges[,l] <- sampleAges[,l]*nHolder[l-1]
            }
            }
            for (l in 2:ncol(KMAges)) {
                KMAges[,l] <- KMAges[,l]*nHolder[l-1]
            }

            if(!is.null(ncol(sampleAges))) {
            if(ncol(sampleAges) > 2) {
                sampleAges <- transform(sampleAges,
                                    numberAlive = rowSums(sampleAges[,2:ncol(sampleAges)]),
                                    propAlive =
                                        rowSums(sampleAges[,2:ncol(sampleAges)]) / totalN)
            } else {
                sampleAges <- transform(sampleAges,
                                        numberAlive = sampleAges[,2],
                                        propAlive = sampleAges[,2] / totalN)
            }  ## transforms to pooled number and proportion alive across locations for Weibull
            }

            if (ncol(KMAges) > 2) {
                KMAges <- transform(KMAges,
                                    numberAlive = rowSums(KMAges[,2:ncol(KMAges)]),
                                    propAlive = rowSums(KMAges[,2:ncol(KMAges)]) / totalN)
            } else {
                KMAges <- transform(KMAges,
                                    numberAlive = KMAges[,2],
                                    propAlive = KMAges[,2] / totalN)
            }   ## transforms to pooled number and proportion alive across locations for KM
            
#             KMAges$propAlive <- KMAges$propAlive / max(KMAges$propAlive)

#           lines(sampleAges$propAlive ~ sampleAges$ages, type = "l",
#                  col = rgb(0,1,0,alpha = 0.9), lwd = 1)
## The fitted line for KM fits
           lines(KMAges$propAlive ~ KMAges$ages, type = "l",
                 col = rgb(1,0,0, alpha = 0.9), lwd = 1)

            ## if weightByN = TRUE:
            try( {
            outFits <- data.frame(iter = rep(activeData$repNum[1], nrow(sampleAges)),
                                  ages = sampleAges$ages, numberAlive = sampleAges$numberAlive,
                                  propAlive = sampleAges$propAlive)
            } )
            try( {
            KMoutFits <- data.frame(iter = rep(activeData$repNum[1], nrow(KMAges)),
                                  ages = KMAges$ages, numberAlive = KMAges$numberAlive,
                                  propAlive = KMAges$propAlive)
            } )
        })
        }
        KMOuts<- rbind(KMOuts, KMoutFits)
        try( {
        outs <- rbind(outs, outFits)
        })
    }    
}
KMOuts <- KMOuts[-1,]
outs <- outs[-1,]
output <- list(KMOuts, outs)
return(output)
}


