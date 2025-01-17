#' MEMOIR.senesc: estimate species' senescence rates from a MEMOIR.survrates() fit.

#' @param survrates frame [[1]] from a MEMOIR.survrates output. If this has glitched and
#'                  produced all NAs for 'species', it is necessary to edit the species
#'                  names in.
#' @param startAges a vector of ages at which senescence could start. Generally equal
#'                  to the age at first breeding, for each species.

#' Currently outputs a dataframe with species name (as in the 'survrates' input),
#' 'coef' (the estimated senescence parameter), and 'SE' (the SE of the senescence parameter).
#'
#' @export

MEMOIR.senesc <- function(survrates, startAge = 2) {
pdf(file = "senescencePlots.pdf")
    tick <- 1
    ratesOut <- data.frame(species = c("Animaux artificialis",
                                       levels(droplevels(survrates$species))),
                           coef = c(rep(1.000, length(levels(survrates$species)))),
                           SE = c(rep(1.000, length(levels(survrates$species))))
                           )
    startAges <- c(rep(startAge, nrow(survrates)))
    for (i in unique(survrates$species)) {
        active <- subset(survrates, survrates$species == i)
        active$integerRisk <- round(active$atRisk)
        analysisFrame <- data.frame(ages = c(rep(NA, sum(active$integerRisk))),
                                    deaths = c(rep(NaN, sum(active$integerRisk))))
        counter <- 1
        try( {
        for (j in 1:nrow(active)) {
            analysisFrame$ages[counter:(counter+active$integerRisk[j])] <-
                active$ageClass[j]
#            ACObs <- c(rep(NaN, active$integerRisk[j]))
            nDeaths <- round(active$integerRisk[j] * (1 - active$propSurv[j]))
            nSurvs <- active$integerRisk[j] - nDeaths
            obs <- c(rep(1, nDeaths), rep(0, nSurvs))
            analysisFrame$deaths[counter:(counter+active$integerRisk[j]-1)] <- 
                obs
            counter <- counter+active$integerRisk[j]
        }
        } )
        frame <- na.omit(analysisFrame)
        frame <- subset(frame, ages >= startAges[tick])
        frameLM <- with(frame, glm(deaths ~ ages, family = binomial))
        model <- summary(frameLM)

        newdata <- data.frame(
            ages = c(startAges[tick]:max(frame$ages))
            )
        tick <- tick+1
        predictions <- predict(frameLM, newdata = newdata, type = "response")
        plot(predictions ~ c(0:(length(predictions)-1)),
             ylim  = c(0,1), col = "black", main = active$species[1]
             , xlab = "Years older than age at first breeding", ylab = "Estimated annual mortality",
             type = "l")
        print(active$species[1])
        print(as.numeric(frameLM[[1]][2]))
        ratesOut[tick, 2] <- as.numeric(frameLM[[1]][2])
        ratesOut[tick, 3] <- model$coefficients[4]
    }
ratesOut <- ratesOut[-1,]
return(ratesOut)
dev.off()
}
        
