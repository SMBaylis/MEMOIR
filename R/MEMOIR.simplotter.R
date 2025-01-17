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

MEMOIR.simplotter <- function(x, zombieMode = TRUE, deadOnly = FALSE, PMType = "weibull",
                              displayType = "kaplan") {
        library(survival)

        data <- x
        if (zombieMode == TRUE) {
            data$CENS <- 1-data$CENS
        }
        if(deadOnly == TRUE) {
            data <- subset(data, data$CENS == 1)
        }
        surviv <- with(data, Surv(ELAPSED_DAYS, CENS))
        fKM <- survfit(surviv ~ CENS,data=data) ###
        plot(fKM,xlab="Age (years)",ylab="Proportion surviving to age x",axes=FALSE,
             col="white")
        axis(side=2)
        axis(side=1,at=seq(365,max(data$ELAPSED_DAYS),(2*365)),
             labels=seq(1,floor(max(data$ELAPSED_DAYS))/365,2))
        box()
        outs <- data.frame(iter = c(0),
                           ages = c(0), numberAlive = c(0),
                           propAlive = c(0))
        KMOuts <- data.frame(iter = c(0),
                             ages = c(0), numberAlive = c(0),
                             propAlive = c(0))

        for (i in unique(data$repNum)) {
            activeData <- subset(data, data$repNum == i)
            MRL <- max(activeData$ELAPSED_DAYS)
            sampleAges <- data.frame(ages = c(0,
                                              seq(from = max(data$ELAPSED_DAYS)/1000,
                                                  to = max(data$ELAPSED_DAYS),
                                                  by = max(data$ELAPSED_DAYS)/1000)),
                                     blanks = c(rep(NaN, 1001))
                                     )
            KMAges <- data.frame(ages = c(0,
                                          seq(from = max(data$ELAPSED_DAYS)/1000,
                                              to = max(data$ELAPSED_DAYS),
                                              by = max(data$ELAPSED_DAYS)/1000)),
                                 blanks = c(rep(NaN, 1001))
                                 )
            nHolder <- c(NaN)
            try( {
            for (l in unique(activeData$BANDLOC)) {
                ARAL <- subset(activeData, activeData$BANDLOC == l) ##Active Rep, Active Loc
                ARALHist_d <- (max(activeData$RECOVERYEAR) - min(ARAL$BANDYEAR)) * 365
                if (ARALHist_d >= max(activeData$ELAPSED_DAYS)) {
                    ARALSurviv <- with(ARAL, Surv(ELAPSED_DAYS, CENS))
#                    ARALwei <- survreg(ARALSurviv ~ CENS,
#                                       dist = "weibull", data = ARAL)
                    ARALwwei <- survreg(ARALSurviv ~ CENS, weights = round(ARAL$Wbail)
                                      , dist = PMType, data = ARAL)
                    ARALwkm <- survfit(ARALSurviv ~ 1, weights = round(ARAL$Wbail))
                    survest <- stepfun(ARALwkm$time, c(1, ARALwkm$surv))
                       ## A regression
                     ## with time-dependent risk and local smoothing would probably be
                     ## better in both of these
                     ## instances. I can't get it to predict (possibly Aalen doesn't? - check).
#                    ARALaareg <- aareg(ARALSurviv ~ ELAPSED_DAYS,
#                                       data = ARAL, weights = round(ARAL$Wbail))
                  #  ARAL.KM <- survfit(ARALSurviv ~ 1, data = ARAL,
                  #                     weights = round(ARAL$Wbail))
                  #  lines(ARAL.KM, col = l)            
#                    lines(predict(ARALwei, newdata = list(CENS = 1), type = "quantile",
#                                  p = seq(.01, .99, by = .01)), seq(.99, .01, by = -.01),
#                          col = "red")
                    if(is.nan(as.vector(ARALwwei[[1]])[1]) == FALSE ) {
                        rawpreds <- data.frame(days = c(0,
                                                    predict(ARALwwei, newdata = list(CENS = 1),
                            type = "quantile", p = seq(.001, .999, by = .001))),
                            propSurv = c(1, seq(0.999, 0.001, -.001)))
                        sampleAges[,ncol(sampleAges)+1] <- c(rep(NaN, nrow(sampleAges)))
                        for (j in 1:nrow(sampleAges)) {
                            diffs <- abs(sampleAges$ages[j] - rawpreds$days)
                            isMin <- diffs == min(diffs)
                            sampleAges[j, ncol(sampleAges)] <- rawpreds$propSurv[isMin == TRUE]
                        }   
                        nHolder[ncol(sampleAges) - 2] <- nrow(ARAL)

                        KMAges[,ncol(KMAges)+1] <- survest(sampleAges$ages) 
                    }          
                    }

                    }# the close for all band locations within this iter
            sampleAges <- sampleAges[,-2]
            KMAges <- KMAges[,-2]
            totalN <- sum(nHolder)
            for (l in 2:ncol(sampleAges)) {
                sampleAges[,l] <- sampleAges[,l]*nHolder[l-1]
            }  ## converts to number alive by location for weibull
            for (l in 2:ncol(KMAges)) {
                KMAges[,l] <- KMAges[,l]*nHolder[l-1]
            }  ## converts to number alive by location for KM

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

            if(ncol(KMAges) > 2) {
                KMAges <- transform(KMAges,
                                    numberAlive = rowSums(KMAges[,2:ncol(KMAges)]),
                                    propAlive = rowSums(KMAges[,2:ncol(KMAges)]) / totalN)
            } else {
                KMAges <- transform(KMAges,
                                    numberAlive = KMAges[,2],
                                    propAlive = KMAges[,2] / totalN)
            }  ## transforms to pooled number and proportion alive across locations for KM
            KMAges$propAlive <- KMAges$propAlive / max(KMAges$propAlive)

            if (displayType == "parametric") {
            lines(sampleAges$propAlive ~ sampleAges$ages, type = "l",
                  col = rgb(0,1,0,alpha = 0.3), lwd = 0.7)
            }
## The fitted line for KM fits
           if (displayType == "kaplan") {
           lines(KMAges$propAlive ~ KMAges$ages, type = "l",
                 col = rgb(1,0,0, alpha = 0.3), lwd = 0.7)
           }
            
            ## if weightByN = TRUE:
            outFits <- data.frame(iter = rep(activeData$repNum[1], nrow(sampleAges)),
                                  ages = sampleAges$ages, numberAlive = sampleAges$numberAlive,
                                  propAlive = sampleAges$propAlive)
            KMoutFits <- data.frame(iter = rep(activeData$repNum[1], nrow(KMAges)),
                                  ages = KMAges$ages, numberAlive = KMAges$numberAlive,
                                  propAlive = KMAges$propAlive)
            KMOuts <- rbind(KMOuts, KMoutFits)
            outs <- rbind(outs, outFits)            
            })  #the try close  
        }
#        segments(x0 = 0, y0 = 1, x1 = 20*365, y1 = 0, col = "black", lwd = 1.2)
        KMOuts <- KMOuts[-1,]
        outs <- outs[-1,]

        finalOuts <- list(KMOuts = KMOuts, ParametricOuts = outs)
#        write.table(outs, file = paste("line ", s, n, ".csv", sep = ""), sep = ","
#                  , row.names = FALSE)
#        write.table(KMOuts, file = paste("line ", s, n, "KM.csv", sep = ""), sep = ",",
#                    row.names = FALSE)
}

