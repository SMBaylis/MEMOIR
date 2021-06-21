#' MEMOIR.boot(): Generate bootstrap estimates of annual adult survival, immature survival,
#'                first-year survival, and senescence for a single species.
#'
#' Returns a list with values [[1]]: The estimated adult survival, immature survival,
#' first-year survival and senescence for each iteration, and [[2]]: the SE of the
#' iterated survival and senescence estimates.
#'
#' @param records A data-frame of records, as for MEMOIR.fit()
#' @param effort A data-frame of annualised banding-effort, as for MEMOIR.fit()
#' @param wearRate A dataframe of wear-rate values, as for MEMOIR.fit()
#' @param niter A number of bootstrap iterations to run
#' @export

MEMOIR.boot <- function(records, effort, wearRate, breedingAge, zombieMode,
                        deadOnly, niter = 1000) {

    bootRecords <- data.frame(immSurv = c(rep(NaN, niter)),
                              adSurv = c(rep(NaN, niter)),
                              y1Surv = c(rep(NaN, niter))
                              )
    for(i in 1:niter) {
        try( {
        sampledRecords <- data.frame(records[sample(row(records), nrow(records),
                                                    replace = TRUE),])
        activeFit <- MEMOIR.fit(records = sampledRecords,
                                effort = effort,
                                wearRates = wearRate,
                                groups = sampledRecords$SCIENA[1]
                                )
        simplots <- MEMOIR.simplotterV2(x = activeFit[[1]],
                                        zombieMode = zombieMode,
                                        deadOnly = deadOnly)
        survrates <- MEMOIR.survrates(x = simplots[[1]],
                                      species = sampledRecords$SCIENA[1],
                                      breedingAges = breedingAge
                                          )
        bootRecords[i,1] <- survrates[[2]]$juvSurvRate[1] ## something from survRates
        bootRecords[i,2] <- survrates[[2]]$adSurvRate[1]
        bootRecords[i,3] <- survrates[[1]]$propSurv[1]
        })
    }
    strapSEs <- data.frame(immSurvSE = sd(bootRecords$immSurv, na.rm=TRUE),
                           adSurvSE = sd(bootRecords$adSurv, na.rm=TRUE),
                           y1SurvSE = sd(bootRecords$y1Surv, na.rm=TRUE))
    return(strapSEs)
}

