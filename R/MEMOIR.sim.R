#' Simulate National-Scale Mark-Recapture
#'
#' Simulates data collected by national-scale mark-recovery schemes
#' for a species with multiple populations with limited inter-population movement.
#' Includes support for a variable number of populations, setting 'true' death-curve,
#' setting the number of years the species has been (potentially) monitored for at
#' each location, setting the distribution of number of individuals marked each time
#' a research team goes to mark animals, setting the relative likelihood of recapture
#' by researchers, or recapture by casual observers, setting the species' band-wear 
#' rate, and setting the amount of wear which causes mark-loss.
#' @param locs Integer. Number of locations ('locodes', or equivalent) to simulate
#' @param line Single factor. Denotes the 'true' death curve to be used in simulations.
#'       One of c("A", "B", "C", "D", "E"). See Baylis et al (2014) for curves.
#' @param history Integer. Number of years between first (potential) band-application and
#'       the end of data-collection.
#' @param batchProb  Numeric. 0 < batchProb < Inf. Denotes the mean number of research
#'            projects initiated for the species in a given year. Number of
#'            studies initiated per year is a random draw from a Poisson distribution,
#'            batches ~ rpois(1, batchProb)
#' @param catchSize Integer. Max number of animals caught (banded) in a single research
#'            trip. Actual numbers caught on a trip are sampled from a Uniform dist,
#'            runif(1, 1, catchSize)
#' @param researchProp Numeric. Generally between zero and 1, but will accept values higher
#'               than one. Higher values increase the proportion of dead recaptures
#'               are research-related, and therefore increases the degree to which
#'               recaupture rate within animals from a location correlates with
#'               research effort at that location
#' @param wearRate Numeric. The mean rate at which bands lose mass to wear, expressed as a
#'           percentage of their starting weight per year.
#' @param maxWear Numeric. The percent mass-loss at which bands are lost to wear.
#' @param censorProb Numeric. The probability that an individual observation is 'censored',
#'           i.e., from an individual whose last resighting was a live-resighting. Reasonable
#'           values can be obtained as in Baylis et al (2014), as described for 'Proportion
#'           of live recaptures'.
#' @param liveRecapDeflator Numeric. Adjusts the probability of resighting a live,
#'           marked animal relative to the probability of observing that animal if it had
#'           died in that year. Can range from 0 to Inf, where 0 is 'there are no live
#'           resightings', and Inf is 'every individual will be resighted alive in every year
#'           that it is alive'. Live recaptures are modelled as arising only from research
#'           activity, as would be the case for species marked with only metal bands,
#'           requiring recapture for identification.
#' @param niter Numeric. The number of simulation iterations to be run. If the settings
#'                       result in some simulations where no animals are marked, those
#'                       iterations will go unrecorded in the output. After a simulation,
#'                       [niter] - length(unique([sim name][[1]]$recRepNo)) returns the
#'                       number of iterations in which no animals were marked.
#' @return The function returns a list containing two data frames.
#'    1: Records. Details for every mark that was recovered in the simulation
#'    1a: recRepNo: shows which iteration the band was from
#'    1b: BAND: A unique seven-digit integer for each banded animal
#'    1c: SCIENA: The 'scientific name' of the animal. Placeholder value for read-in
#'                to MEMOIR.fit or similar functions: 'Animalis artificialis'
#'    1d: BANDED: The date the animal was banded, as "%Y-%m-%d"
#'    1e: RECOVERED: The date the animal was recovered, as "%Y-%m-%d"
#'    1f: ELAPSED_DAYS: The number of days between BANDED and RECOVERED
#'    1g: BANDYEAR: The year the animal was banded, as "%Y"
#'    1h: BANDLOC: The location the animal was banded in, as numeric. Used as a 'locode'
#'    1i: RECOVERYEAR: The year the animal was recovered, as "%Y"
#'    2: Effort. Details of when and where animal-marking effort took place
#'    2a: eRepNo: Shows which iteration the rest of the line's data was from
#'    2b: YEAR: The year in which bands were applied, as "%Y"
#'    2c: CAVS: Depreciated. A vector of Poisson-distributed random numbers
#'    2d: SPECIES: The 'scientific name' of the animals. Placeholder for read-in to
#'                  MEMOIR.fit or similar functions: 'Animalis artificialis'
#'    2e: BANDED: The total number of animals banded in YEAR
#'    2f: X1 - Xn: The number of animals banded at each of n simulated locodes in YEAR
#' @export
#' @examples
#' MEMOIR.sim(locs=5, mortCurve=c("A"), history=64, batchProb=0.3, catchSize=50,
#'            researchProp=0.5, wearRate=0.22, maxWear=65, niter=1000)

MEMOIR.sim <- function(locs=5, mortCurve=c("A"), history=64, batchProb=0.4, catchSize=50, researchProp=0.5, wearRate=2.22, maxWear=65, censorProb=0.4, liveRecapDeflator = 0.5, niter=1000) {

require(survival)
## setting up some fake data
## First, some universals that will form the dataframe structure

numLocations <- locs  # set number of banding locations (locodes) here!
if (locs > 1) {
    outputEffort <- data.frame(eRepNo=c(0),YEAR=c(0),CAVS=rpois(1,20),
                     SPECIES=c(rep("Animalis artificialis",1)),
                     BANDED=c(0),matrix(data=0,nrow=1,ncol=numLocations))
} else {
    outputEffort <- data.frame(eRepNo=c(0),YEAR=c(0),CAVS=rpois(1,20),
                     SPECIES=c(rep("Animalis artificialis",1)),
                     BANDED=c(0),X1=c(0))
}
outputRecords <- data.frame(recRepNo=c(0), BAND=c(0),
                      SCIENA=c(rep("Animalis artificialis",1)),
                      BANDED=as.Date("1999-01-01"),
                      RECOVERED=as.Date("1999-01-01"),
                      ELAPSED_DAYS=c(0), BANDYEAR=c(1900+99),BANDLOC=c(0),
                      RECOVERYEAR=c(1900+99), CENS=c(0)
                      )

numRepeats <- niter
for(p in 1:numRepeats) {

###############   Set the length of the availability curve here!!!!   #####################
bandingRecords <- c(history) # how long the history of animal-marking is, in years
###########################################################################################
numSpecies <- c(1) # the number of different species to create a banding history for

## sumBanded <- c(rep(0,100))
## for(k in 1:100){  ## this bracing loop covers the whole numBanded-generating sequence,
                      # for comparing numbers banded in differing batching and metabatching
                      # scenarios

year <- c(1:bandingRecords)
numBatches <- c(rep(0,bandingRecords))
numBanded <- c(rep(0,bandingRecords))
ID <- c(0)
yearBanded <- c(0)
dayBanded <- c(0)
locationBanded <- c(0)
longevity <- c(0)
longevity_d <- c(0)
# speciesID <- c(0)
indiFrame <- data.frame(ID,yearBanded,dayBanded,locationBanded,longevity,longevity_d)
                                        # indiFrame is setup for a
                                         # dataframe of individual birds, the target of v0.2
batchesByLocation <- matrix(data=0,nrow=bandingRecords+20,ncol=numLocations)
bandedByLocation <- matrix(data=0,nrow=bandingRecords,ncol=numLocations)
                                        # ncol is the number of locations
                                        # banding has happened at. The +20 makes sure that
                                        # there is sufficient empty space at the end of the
                                        # matrix for all simulated banding-hypotheses,
                                        # and the years after the present year will be discarded
                                        # in a later step.

# for(s in 1:numSpecies) {

## 1) Setting up the number of banding trips for a species for each year, accounting for
 # some banding trips being multi-year and the possibility that multiple groups might
 # band the same species in the same year

for(i in 1:bandingRecords) {
    newBatches <- rpois(1,batchProb) # to represent the number of research groups initiating a
                               # banding study of a given species in a given year.
    numBatches[i] <- numBatches[i]+newBatches # add batches and first years of metabatches
    batchLocations <- floor(runif(newBatches,1,(numLocations+0.999999)))
    ##create as many random locations as there are new batches
    if(newBatches >= 1){  ## if there are new batches
        for(j in 1:length(batchLocations)){  ## then for every location,
            batchesByLocation[i,batchLocations[j]] <- batchesByLocation[i,batchLocations[j]]+1
            ## add one to the number of batches in that location
        }
    }else{
    }
    if (newBatches >= 1) {  ## if there are new batches,
        for(j in 1:newBatches) {  # then for each new batch,
            batchLocation <- batchLocations[j]
            isMetaBatch <- ifelse(runif(1,0,1)>0.9, 1, 0) # work out whether it is a metabatch
#            batchLocation <- floor(runif(1,1,5.999999)) ## and work out the location it tags at

            if(isMetaBatch==1){  # if it's a metabatch,
               metaLength <- rpois(1,3)  # work out its length
               if(metaLength>=2){ # if its length is greater than or equal to 2 (i.e., real
                                  # metabatch)
               numBatches[(i+1):(i+metaLength)] <- numBatches[(i+1):(i+metaLength)]+1 # add
                                                  # one to the numBatches for each year
                                                  # that it lasts
               batchesByLocation[(i+1):(i+metaLength),batchLocation] <- batchesByLocation[(i+1):(i+metaLength),batchLocation]+1  # and do the same for each location where a batch occurs in
                                 # that year.
               
              }
           }
        }
    }
}

numBatches <- numBatches[1:bandingRecords]
batchesByLocation <- batchesByLocation[1:bandingRecords,]  ## trim up the outputs.
# numBatches
# plot(numBatches,type="l")  ## plots the number of banding trips for that species per year
                            # over the history of banding
# numBatches <- numBatches[1:bandingRecords]

# 2) Converting those numbers of banding trips per annum into a number of banded birds per
# annum

#for (i in 1:length(numBatches)) { #for each year
#    if (numBatches[i]>0) {
#        for(j in 1:numBatches[i]) {  # and each batch within each year
#            ############################################################################
#            numInBatch <- floor(runif(1,1,50))  # the number of individuals in the batch
#            #############################################################################
#            numBanded[i] <- numBanded[i]+numInBatch  # and add that number to the number banded
#        }
#    }
#}
## a simpler phrasing,
 # batchesByLocation*floor(runif((nrow(batchesByLocation)*ncol(batchesByLocation)),1,50)),
 # was rejected because the sizes of batches within years were identical (and therefore
 # not independent) with this phrasing. Hence, the complicated three-part loop.

## duplicate this for individual locations
if (locs > 1) {
    for (i in 1:nrow(batchesByLocation)) {
            if (sum(batchesByLocation[i,])>0) {
                for (j in 1:ncol(batchesByLocation)) {
                    if(batchesByLocation[i,j]>0) {
                        for(k in 1:batchesByLocation[i,j]) {
                    ##########################################################################
                            numInBatch <- floor(runif(1,1,catchSize))
                            ## the number of individuals per batch
                    ##########################################################################
                            bandedByLocation[i,j] <- bandedByLocation[i,j]+numInBatch #as above
                        }
                    }
                }
            }
       }
} else {
    for (i in 1:length(numBatches)) { #for each year
            if (numBatches[i]>0) {
                        for(j in 1:numBatches[i]) {  # and each batch within each year
                            ##########################################################
                            numInBatch <- floor(runif(1,1,50))
                                        # the number of individuals in the batch
                            ##########################################################
                            bandedByLocation[i] <- numBanded[i]+numInBatch
                                        # and add that number to the number banded
                            }
                        }
            }
}
    
## sumBanded[k] <- sum(numBanded)
## }
numBanded <- apply(bandedByLocation,1,"sum")
year.df <- data.frame(year, numBatches, numBanded,bandedByLocation)

# plot(numBanded,type="l")

## next task: turn the numbers banded into a distribution of recoveries through time.
 # 3): turn numbers marked into life-histories
 # 4): apply a rediscovery rule, record discoveries

# make a dataframe of individuals, including the year they were banded
if(sum(numBanded)>0) {
    ID <- 1:sum(numBanded)
}else{
    ID <- integer(0)
}
    
yearBanded <- c(rep(0,sum(numBanded)))

n <- c(1)
for(i in 1:length(year.df$numBanded)){ # for each year
    add <- year.df$numBanded[i]  #take the number of banded individuals
    if(add>0){  # if that number is greater than zero
        yearBanded[n:(n+(add-1))] <- year.df$year[i] # repeat line by that many individuals
        n <- n+add
    }
}  # this loop creates a vector called yearBanded, with one datapoint for each band, indicating
   # the year the band was applied.
yearBanded <- yearBanded-1
dayBanded <- (yearBanded*365)+runif(sum(numBanded),0,365)

##next: create a factor called locationBanded, the compliment of yearBanded
locationBanded <- c(rep(0,length(ID)))
l <- c(1)
for(i in 1:nrow(bandedByLocation)){
    for(j in 1:ncol(bandedByLocation)){
            add <- bandedByLocation[i,j]
            if(add>0){
                locationBanded[l:(l+(add-1))] <- j
                l <- l+add
            }
        }
}
locationBanded <- factor(locationBanded)

#next: for each bird, generate a random number between zero and one, and use the death curves
 # from the previous paper to infer true longevities for each of them

x <- runif(sum(numBanded), 0,1)

###########  set longevity curve here!!   ##################################################
   
if (mortCurve == "A") {
    # Line A
    longevity_d <- (1-x)*20*365
    ## creates a vector of longevities for all banded animals, assuming
    #  a true maximum longevity of 20 years.
} else if (mortCurve == "B") {
    # Line B
    longevity_d <- (1-(x^2))*20*365
} else if (mortCurve == "C") {
    # Line C
    longevity_d <- (1-x+(x^2-x))*20*365
# alternate identity    (1-2*x+x^2)*20
} else if (mortCurve == "D") {
    # Line D
    longevity_d <- ((2/pi)*asin(1-2*x)+x)*20*365
} else if (mortCurve == "E") {
    # Line E
    longevity_d <- ((1/2)-1/2*sign(2*x-1)*(abs(2*x-1))^(1/3))*20*365
} else if (mortCurve %in% c("A","B","C","D","E") == FALSE) {
    print("The requested curve is not one of the known mortality curves!
          I'm using Curve A as a placeholder")
    longevity_d <- (1-x)*20*365
}

############################################################################################
longevity <- longevity_d/365

# speciesID <- rep(s,length(longevity))
tempFrame <- data.frame(ID,yearBanded,dayBanded,locationBanded,longevity,longevity_d)

indiFrame <- rbind(indiFrame,tempFrame)

year <- c(1:bandingRecords)
numBatches <- c(rep(0,bandingRecords))
numBanded <- c(rep(0,bandingRecords))
ID <- c(0)
yearBanded <- c(0)
dayBanded <- c(0)
longevity <- c(0)
longevity_d <- c(0)
#speciesID <- c(0)
n <- c(1)  ## this little collection resets all the modified intials so that the loop can
            # repeat for multiple species (from year <- to n <-)
#}
indiFrame <- indiFrame[-1,] ## note that yearBanded and dayBanded do not appear to match up,
                            ## as yearBanded starts at zero. yearBanded should be read as
                            ## 'this bird was banded when the banding programme was yearBanded
                            ## years old.'
## and then:
 # 5): Generate analysis, change code to simulate different scenarios

indiFrame$yearBanded <- indiFrame$yearBanded+1  ## corrects for the 'year zero' error where
                                                 # the first bird was apparently recovered
                                                 # before the first bird was banded
deathDay <- indiFrame$dayBanded+indiFrame$longevity_d
deathYear <- as.integer(indiFrame$yearBanded+indiFrame$longevity)
indiFrame <- data.frame(indiFrame,deathDay,deathYear)
#indiFrame$yearBanded <- indiFrame$yearBanded+1  ## corrects for the 'year zero' error where
                                                 # the first bird was apparently recovered
                                                 # before the first bird was banded
bandsApplied <- xtabs(~yearBanded,data=indiFrame)
yearsDied <- xtabs(~deathYear,data=indiFrame)

# generate some return rates, where
## nReturns(age, year)~bandingEffort(year)+numAvailable(year, age)
# or, in terms of individual probabilities
## pr(recapture) ~ bandingEffort(year)

zeroes <- data.frame(c(rep(0,20)))
for(i in numSpecies) {
    newRow <- rep(0,20)
    zeroes <- cbind(zeroes,newRow)
}
# nrow(yearsDied)-length(bandsApplied)

# need to find a way to put the number of bands applied in a year and the number of
# banded deaths in a year on the same dataframe

# indiFrame <- subset(indiFrame,speciesID==2)
if(nrow(indiFrame)>0){
    row.names(indiFrame) <- 1:nrow(indiFrame)
}

if(length(yearsDied)>bandingRecords) {
    years <- c(0:nrow(yearsDied))
} else {
    years <- c(0:bandingRecords)
}   ## current test at 25/06/15

bandedInYear <- c(rep(0,length(years)))
diedInYear <- c(rep(0,length(years)))

for(i in 1:nrow(indiFrame)){
    line <- indiFrame[i,]
    bandedInYear[line$yearBanded] <- bandedInYear[line$yearBanded]+1
    diedInYear[line$deathYear] <- diedInYear[line$deathYear]+1
}  #previously-stated goal (bands applied and banded deaths on same DF) achieved here.

##########################################################
#indiFrame$CENS <- rbinom(nrow(indiFrame), 1, censorProb)
#indiFrame$finalObsAge_d <- c(rep(0, nrow(indiFrame)))
#indiFrame$finalObsDay <- c(rep(0, nrow(indiFrame)))
#for(i in 1:nrow(indiFrame)) {
#    if(indiFrame$CENS[i] == 1) {
#        indiFrame$finalObsAge_d[i] <- runif(1, min = 0, max = indiFrame$longevity_d[i])
#        indiFrame$finalObsDay[i] <- indiFrame$finalObsAge_d[i] + indiFrame$dayBanded[i]
#    } else if(indiFrame$CENS[i] == 0) {
#        indiFrame$finalObsAge_d[i] <- indiFrame$longevity_d[i]
#        indiFrame$finalObsDay[i] <- indiFrame$deathDay[i]
#    }
#}
##########################################################

YBD <- data.frame(years,bandedInYear=na.omit(bandedInYear),diedInYear=na.omit(diedInYear))
maxBandedInYear <- max(YBD$bandedInYear)
###########################################################################################
prRecapInYear <- as.numeric(bandedInYear/maxBandedInYear) # for no geographic effects on
                                                            # recapture probability
prRecapInLocation <- matrix(0, nrow(bandedByLocation), ncol(bandedByLocation))
for (c in 1:ncol(bandedByLocation)) {
    prRecapInLocation[,c] <- bandedByLocation[,c]/max(bandedByLocation[,c])
}

blanks <- matrix(0,nrow=nrow(YBD)-nrow(prRecapInLocation),ncol=ncol(prRecapInLocation))
prRecapInLocation <- rbind(prRecapInLocation,blanks)
     # for geographic effects to enter recapture probability simulations
###########################################################################################
## it is envisaged that, in the final pipeline,
  # the prRecapInYear will be the output of a linear model relating the number of recaptures
  # in a year to the banding effort in that year. Here it is a function of the
  # number of individuals banded [at each location].

if (all(is.na(prRecapInYear) != TRUE)) {
    
YBD <- data.frame(YBD,prRecapInYear=na.omit(prRecapInYear))
YBD <- subset(YBD,years<=max(indiFrame$deathYear))

indiRecapProb <- runif(nrow(indiFrame),min=0,max=1)
yearRecapProb <- c(rep(0,nrow(indiFrame)))
locationRecapProb <- c(rep(0,nrow(indiFrame)))

for(i in 1:nrow(indiFrame)){
    yearRecapProb[i] <- prRecapInYear[indiFrame$deathYear[i]+1]
}  ## this version is not geographically-explicit

prRecapInLocation <- data.frame(prRecapInLocation)
for(i in 1:nrow(indiFrame)){
    locationRecapProb[i] <- prRecapInLocation[indiFrame$deathYear[i]+1,
                                          as.numeric(indiFrame$locationBanded[i])]
}    ## this version is geographically-explicit

## indiRecapProb gives a random number - if it exceeds the yearRecapProb in the individual's
 # year of death, then at baseline, it will be recaptured. Modified in next lines!
indiFrame <- data.frame(indiFrame,indiRecapProb,yearRecapProb,locationRecapProb)
###########################################################################################
## researchRecaptures <- ifelse(indiRecapProb<(0.4*yearRecapProb),1,0)
## or
researchRecaptures <- ifelse(indiRecapProb<(researchProp*locationRecapProb),1,0)
###########################################################################################
stochasticRecaptures <- rbinom(nrow(indiFrame),1,0.1) + researchRecaptures - researchRecaptures
                                        #controlling the ratio of
# researchRecaptures to stochasticRecaptures controls how many of the recoveries
# are a result of ongoing research in the simulation.
##########################################################################################

combinedRecaptures <- researchRecaptures+stochasticRecaptures
recaps_dead<- ifelse(combinedRecaptures>0,1,0)
indiFrame <- data.frame(indiFrame,recaps_dead)

### Bringing in location-specific live recaptures. Make the final product column be called
### 'recaps'.
## Parts: generate [allYears] x [allIndividuals] matrices, showing:
    ## 1) the number of days that the individual was alive during that year, called
    ##     probAvailable
    ## 2) the index of research effort for each individual's location each year, called
    ##     localEffort
    ## 3) generate a binary tick for 'captured' in years that each animal was captured
    ## 4) extract the final day in which an animal was captured alive

markedAndAlive.m <- matrix(data = c(0), nrow = nrow(indiFrame), ncol = nrow(YBD))
# alive.m <- matrix(data = c(0), nrow = nrow(indiFrame), ncol = nrow(YBD))
effort.m <- matrix(data = c(0), nrow = nrow(indiFrame),
                   ncol = length(unique(indiFrame$locationBanded)))

for (i in 1:nrow(indiFrame)) {
    for (y in 1:nrow(YBD)) {
        if (indiFrame$yearBanded[i] <= y && indiFrame$deathYear[i] >= y) {
            markedAndAlive.m[i,y] <- 1
#            alive.m[i,y] <- 1
        }
    }
}  ## this is going to be a very slow step to compute. Think about vectorising.
    
researchLiveRecapProb.m <- matrix(0, nrow = nrow(indiFrame), ncol = nrow(YBD))
stochasticLiveRecapsProb.m <- matrix((liveRecapDeflator*0.1), nrow = nrow(indiFrame), ncol = nrow(YBD))

for (i in 1:nrow(indiFrame)) {
    for (y in 1:nrow(YBD)) {
        researchLiveRecapProb.m[i,y] <-
            liveRecapDeflator*(prRecapInLocation[y, as.numeric(indiFrame$locationBanded[i])])
    } ## the 0.1s in previous lines are 'live recapture deflation factor', used to change
       # the relative likelihood of observing a dead thing in the year that it dies,
       # compared to observing a live thing in any year that it is alive. Name it and
       # add it to the function call.
}
    
researchLiveRandoms.m <- matrix(runif(nrow(indiFrame)*nrow(YBD), min = 0, max = 1),
                                nrow = nrow(indiFrame), ncol = nrow(YBD))
stochasticLiveRandoms.m <- matrix(runif(nrow(indiFrame)*nrow(YBD), min = 0, max = 1),
                                  nrow = nrow(indiFrame), ncol = nrow(YBD))

researchLiveRecaps.m <- matrix(0, nrow = nrow(indiFrame), ncol = nrow(YBD))
stochasticLiveRecaps.m <- matrix(0, nrow = nrow(indiFrame), ncol = nrow(YBD))

for (i in 1:nrow(indiFrame)) {
    for (y in 1:nrow(YBD)) {
        if (researchLiveRecapProb.m[i,y] > researchLiveRandoms.m[i,y]) {
            researchLiveRecaps.m[i,y] <- 1
        }
        if (stochasticLiveRecapsProb.m[i,y] > stochasticLiveRandoms.m[i,y]) {
            stochasticLiveRecaps.m[i,y] <- 1
        }
    }
}

researchLiveRecaps.m <- researchLiveRecaps.m * markedAndAlive.m
stochasticLiveRecaps.m <- stochasticLiveRecaps.m * markedAndAlive.m

allLiveRecaps.m <- researchLiveRecaps.m + stochasticLiveRecaps.m
for (i in 1:nrow(allLiveRecaps.m)) {
    for (y in 1:ncol(allLiveRecaps.m)) {
        if (allLiveRecaps.m[i,y] > 1) {
            allLiveRecaps.m[i,y] <- 1
        }
    }
}
    
liveRecapsDays.m <- matrix(0, nrow = nrow(allLiveRecaps.m), ncol = ncol(allLiveRecaps.m))
for (i in 1:nrow(allLiveRecaps.m)) {
    for (y in 1:ncol(allLiveRecaps.m)) {
        liveRecapsDays.m[i,y] <- (allLiveRecaps.m[i,y]*(((y-1)*365)+runif(1, 0, 365)))
    }
}
    
latestLiveRecap <- c(rep(0, nrow(indiFrame)))
secondLatestLiveRecap <- c(rep(0, nrow(indiFrame)))
for (i in 1:nrow(indiFrame)) {
    latestLiveRecap[i] <- max(liveRecapsDays.m[i,])
}
for (i in 1:nrow(indiFrame)) {
    n <- length(liveRecapsDays.m[i,])
    secondLatestLiveRecap[i] <- sort(liveRecapsDays.m[i,], partial=n-1)[n-1]
}

for (i in 1:length(latestLiveRecap)) {
    if (latestLiveRecap[i] > indiFrame$longevity_d[i]) {
        latestLiveRecap[i] <- secondLatestLiveRecap[i]
    }
}

indiFrame$latestLiveRecap <- latestLiveRecap
indiFrame <- subset(indiFrame, indiFrame$recaps_dead != "NA")

indiFrame$recaps <- c(rep(0, nrow(indiFrame)))
for (i in 1:nrow(indiFrame)) { 
    if (indiFrame$recaps_dead[i] == 1) {
        indiFrame$recaps[i] <- 1
    }
    if (indiFrame$latestLiveRecap[i] > 0 &&
        indiFrame$latestLiveRecap[i] > indiFrame$dayBanded[i]) {
        indiFrame$recaps[i] <- 1
    }
}

indiFrame$CENS <- c(rep(0, nrow(indiFrame)))
indiFrame$recordedLongevity <- c(rep(0, nrow(indiFrame)))

for (i in 1:nrow(indiFrame)) {
    if (indiFrame$recaps_dead[i] == 0) {
        indiFrame$CENS[i] <- 1
    }
    if (indiFrame$recaps_dead[i] == 1) {
        indiFrame$recordedLongevity[i] <- indiFrame$longevity_d[i]
    } else if(indiFrame$recaps_dead[i] == 0) {
        indiFrame$recordedLongevity[i] <- indiFrame$latestLiveRecap[i] - indiFrame$dayBanded[i]
    }
}

indiFrame$finalRecordYear <- floor(indiFrame$recordedLongevity / 365)
indiFrame$latestRecap <- indiFrame$dayBanded + indiFrame$recordedLongevity
    
########################################################################################
    
recaps <- subset(indiFrame,recaps==1)
#hist(recaps$longevity)
#hist(recaps$deathYear)
#now: generate a matrix of numbers available at each potential age in each potential year

row1 <- YBD$bandedInYear
newrow <- YBD$bandedInYear
for(i in 0:nrow(YBD)){
    newrow <- c(0,newrow[-nrow(YBD)])
    row1 <- rbind(row1,newrow)
}
row.names(row1) <- c(1:nrow(row1))
yearsByAvailability <- data.frame(row1)
if(ncol(yearsByAvailability)>1){
    names(yearsByAvailability) <- YBD$years
}
## this dataframe shows the year by column and the number
 # of individuals whose band is y years old by row.

trans <- t(yearsByAvailability)
recaptureProbMatrix <- t(trans*YBD$prRecapInYear)
## this matrix gives our best-informed estimate of the probability of finding an individual
 # of a specific age in a specific year, based only on the number of individuals banded in
 # each year and how old they would be, if they have survived. It represents the probability
 # density-function of encounters if deaths do not happen. In reality, deaths will happen,
 # so the number of observations should drop over increasing age, and the shape of that
 # drop should inform us of the structure of the population death curve.

# apply(recaptureProbMatrix,2,sum)
## records feed-ins
firstDay <- as.Date("1900-01-01")
BANDED <- firstDay+floor(recaps$dayBanded)
RECOVERED <- firstDay+floor(recaps$latestRecap)  ## needs to include dead recap days
ELAPSED_DAYS <- c(recaps$recordedLongevity)
BANDLOC <- c(recaps$locationBanded)
CENS <- recaps$CENS
    
## effort feed-ins
if(nrow(YBD) > nrow(bandedByLocation)) {    
    futureBands <- matrix(0,nrow=nrow(YBD)-nrow(bandedByLocation),ncol=ncol(bandedByLocation))
    bandedByLocation <- rbind(bandedByLocation,futureBands)
} else if (nrow(bandedByLocation) > nrow(YBD)) {
    emptyYearsBD <- matrix(0,nrow=nrow(bandedByLocation)-nrow(YBD),ncol=ncol(YBD))
    colnames(emptyYearsBD) <- names(YBD)
    YBD <- rbind(YBD,emptyYearsBD)
}

## creating frames that output for input into the looper!
potentialBands <- c(runif(nrow(recaps),1000000,9999999))
effort <- data.frame(YEAR=YBD$years+1900,CAVS=rpois(nrow(YBD),20),
                     SPECIES=c(rep("Animalis artificialis",nrow(YBD))),
                     BANDED=c(YBD$bandedInYear))
if (locs > 1) {
    effort <- cbind(effort, bandedByLocation)
} else {
    effort <- cbind(effort, X1 = bandedByLocation)
}
records <- data.frame(BAND=c(sample(potentialBands,nrow(recaps),replace=FALSE)),
                      SCIENA=c(rep("Animalis artificialis",nrow(recaps))),
                      BANDED=c(BANDED), RECOVERED=c(RECOVERED), ELAPSED_DAYS=c(ELAPSED_DAYS),
                      BANDYEAR=as.numeric(format.Date(BANDED,format="%Y")),
                      BANDLOC=c(BANDLOC),
                      RECOVERYEAR=as.numeric(format.Date(RECOVERED,format="%Y")),
                      CENS = c(CENS)
                      )

## set band-wear and band-loss variables for simulated data here!: #################
endpoint <- maxWear  # here the value given in Ludwig (1981), expressed as a percentage.
mwr <- wearRate  # insert mean wear-rate value for your species here!
####################################################################################

maxwr <- mwr+((2/3)*mwr)
minwr <- mwr-((2/3)*mwr)

myears <- endpoint/mwr
minyears <- endpoint/maxwr
maxyears <- endpoint/minwr

lmyears <- log(myears)
lminyears <- log(minyears)
lmaxyears <- log(maxyears)

maxsd <- (maxyears-myears)/1.96 # 'conservative'; a contrast of minyears to myears would
                                # give a smaller sd
minsd <- (myears-minyears)/1.96 # vice versa
msd <- (maxsd+minsd)/2          #fixed? Stands for 'mean sd'
lmsd <- (((lmaxyears-lmyears)/1.96)+((lmyears-lminyears)/1.96))/2 # the correct sd for
 # lognormal analyses
name <- records$SCIENA[1]

sampleAges <- c(1:100) ## replace with a vector of real ages-at-recovery!
plot(plnorm(sampleAges,meanlog=log(myears),sdlog=lmsd))
# mtext(records$SCIENA[1])
# plnorm(sampleAges,meanlog=log(myears),sdlog=lmsd)

if(nrow(records)>0) {
#    print("You Have Records!")
    PrBandLoss <- c(rep(0,nrow(records)))
}else{
    PrBandLoss <- numeric(0)
#    print("You Have No Records!")
}

if(length(PrBandLoss)>0){
for(i in 1:nrow(records)) {
    xage <- records$RECOVERYEAR[i]-records$BANDYEAR[i]
    xbandloss <- plnorm(xage,meanlog=log(myears),sdlog=lmsd)
    PrBandLoss[i] <- xbandloss
}
}## generates a probability that the band has fallen off by the year of recapture, for
   # all bands generated.

if(nrow(records)>0){
rand <- runif(nrow(records),0,1)
}else{
    rand <- numeric(0)
}
    
records$PrBandLoss <- PrBandLoss
records$rand <- rand
#records <- data.frame(records,PrBandLoss=PrBandLoss,rand=rand)
records <- subset(records, rand>PrBandLoss)
records <- subset(records, RECOVERYEAR <= 1900 + history)
records <- records[1:9]
records$ELAPSED_DAYS <- floor(records$ELAPSED_DAYS)
    ##################################################################################
# records$CENS <- rbinom(nrow(records), 1, censorProb)  ## is the recapture 'censored'?
    ##################################################################################
recRepNo <- c(rep(p,nrow(records)))
records <- data.frame(recRepNo,records)

effort <- subset(effort, BANDED>0)
eRepNo <- c(rep(p,nrow(effort)))
effort <- data.frame(eRepNo,effort)

outputRecords <- rbind(outputRecords, records)
outputEffort <- rbind(outputEffort, effort)

} ## this is the close-paren for 'if there were any animals marked in this iteration'
    
} ## this is the close-paren for repeating the simulation multiple times

#system("espeak --stdout 'Your data are all generated!' | aplay")
require(reshape2)
outputRecords <- outputRecords[-1,]
outputEffort <- outputEffort[-1,]
outputEffort <- melt(outputEffort, measure.vars=c(6:ncol(outputEffort)), value.name="BANDATLOC",
                     variable.name="LOC")
outputEffort <- data.frame(eRepNo=outputEffort$eRepNo, YEAR=outputEffort$YEAR,
                           CAVS=outputEffort$CAVS, SPECIES=outputEffort$SPECIES,
                           LOC=outputEffort$LOC, BANDED=outputEffort$BANDATLOC )
wearRates <- data.frame(SPECIES=c("Animalis artificialis"), usedRate=c(wearRate))
    
outs <- list(outputRecords, outputEffort, wearRates)
return(outs)

} ## closes the MEMOIR.sim function

