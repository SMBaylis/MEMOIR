#' Estimate survival rates through each year-class for a set of species, using curves
#' from MEMOIR.simplotter()
#'
#' Estimates juvenile and adult survival rates for each age-class, for each of a set of
#' species, using output from MEMOIR.simplotter(). Outputs a list with estimated survival
#' rates through each age-class for each species (as [[1]]) and N-weighted survival estimates
#' through all juvenile age-classes, and all adult age-classes, for each species (as [[2]]).
#' N-weighting is achieved by multiplying estimated survival through each age-class by the
#' (weighted) number of observations from that age-class, and dividing the sum of those
#' products by the sum of the (weighted) number of observations across all age-classes.
#'
#' @param x Output 'KMOuts' from MEMOIR.simplotter() or output 'outs' from MEMOIR.simplotter.
#' @param species A vector of species names to estimate survival rates for separately.
#' @param breedingAges A vector of ages at first breeding, of the same length as parameter
#'                     species. These are taken as the break-points between 'juvenile' and
#'                     'adult' survival. So, for a species with breeding age listed as 2,
#'                     'juvenile' survival will be calculated as n-weighted mean survival
#'                     across age-ranges 0 - 1 years and 1 - 2 years, and 'adult' survival
#'                     will be calculated as n-weighted mean survival across all age-ranges
#'                     2 - 3 years or greater.
#' @export

MEMOIR.survrates <- function(x, species, breedingAges) {
    ageClassDeaths <- data.frame(species = c("A"), ageClass = c(0), propSurv = c(1.5),
                                 atRisk = c(NaN))
    meanSurvRates <- data.frame(species = c("A"), juvSurvRate = c(0), adSurvRate = c(0))
    for (i in unique(x$iter)) {
        active <- subset(x, x$iter == i)
        active <- active[1:1001,]
        adultAge <- breedingAges[i]
        maxAge <- ceiling(max(active$ages) / 365.25)
        speciesFrame <- data.frame(species = c(rep(species[i], length(1:maxAge))),
                                   ageClass = c(1:maxAge),
                                   propSurv = c(rep(NaN, length(1:maxAge))),
                                   atRisk = c(rep(NaN, length(1:maxAge)))
                                   )
        for (j in 1:maxAge) {
            ageSet <- subset(active, active$ages > (j-1)*365.25 & active$ages < j*365.25)
            ageSurvival <- min(ageSet$propAlive) / max(ageSet$propAlive)
            speciesFrame$propSurv[j] <- ageSurvival
            speciesFrame$atRisk[j] <- max(ageSet$numberAlive)
        }
        speciesAdults <- subset(speciesFrame, ageClass > breedingAges[i])
        speciesJuvs <- subset(speciesFrame, ageClass <= breedingAges[i])
        ageClassDeaths <- rbind(ageClassDeaths, speciesFrame)
        adultSurv <-
            sum(speciesAdults$propSurv * speciesAdults$atRisk) / sum(speciesAdults$atRisk)
        juvSurv <-
            sum(speciesJuvs$propSurv * speciesJuvs$atRisk) / sum(speciesJuvs$atRisk)
        iterSurvRates <- data.frame(species = c(species[i]),
                                    juvSurvRate = juvSurv,
                                    adSurvRate = adultSurv
                                    )
        meanSurvRates <- rbind(meanSurvRates, iterSurvRates)
    }
    ageClassDeaths <- ageClassDeaths[-1,]
    meanSurvRates <- meanSurvRates[-1,]
    outs <- list(ageClassDeaths, meanSurvRates)
    return(outs)
}

