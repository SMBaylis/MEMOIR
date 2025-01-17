#' Fit a (set of) MEMOIR model(s) from simulated or real-world data
#'
#' Estimates weightings for observations based on a prior model of the probability of 
#' making each observation. Weightings estimated are Wi (the odds of missing an
#' animal because of low research intensity, relative to the animal observed under
#' the most intense research), Wa (the odds of missing an animal at this animal's
#' age-at-observation, relative to the age-of-observation at which the greatest number
#' of marked animals could have been observed), and Wb (the odds of missing an observation
#' at this animal's age-at-observation owing to mark loss from wear, relative to the age
#' at which no marks would have been lost to wear). Location-specific forms of Wa and Wi
#' (Wal and Wil, respectively), and combined-odds weights (Waib and Wbail) are
#' also calculated. Combined-odds weights are applied to points, and these are
#' used to estimate each species' mortality curve.
#' Note that no fit will be attempted on for species with fewer than 20 total recoveries.
#' This behaviour is hard-set at line 92 (in version 0.1).
#' Known (harmless) bug: You will likely see the error message:
#' In max(yset$n.risk) : no non-missing arguments to max; returning -Inf
#' This occurs when there is an age-class in which no individuals are recorded dead.
#' -Inf values are automatically detected and corrected.
#' @param records A dataframe of individual recapture records, formatted in the same
#'                way as a outs$outputRecords frame from MEMOIR.sim. 
#' @param effort A dataframe of annual (possibly location-specific) animal-marking effort,
#'               formatted the same way as a outs$outputEffort frame from MEMOIR.sim.
#' @param wearRates A dataframe of species names and band-wear rates, used to calculate mark-
#'                  loss.
#' @param groups Factor. One level for each group which you wish to be modelled separately.
#'               i.e., one level per species, if fitting species-specific estimates, or one
#'               level per sex within each species, if sex-specific species-specific if
#'               fitting separate curves for each sex within each species. If you are modelling
#'               simulated data (from MEMOIR.sim()), groups will most likely be iteration
#'               numbers, which can be extracted as unique(simName[[1]]$recRepNo).
#' @return The function returns a dataframe of weights for each observation under various
#'         weighting criteria: Wa, Wi, Wb, Wal, Wil, Waib, and Waibl.
#' @export
#' @examples
#' simName <- MEMOIR.sim()
#' fitName <- MEMOIR.fit(records=simName[[1]], effort=simName[[2]], wearRates=simName[[3]], groups=unique(simName[[1]]$recRepNo), fitLocs=TRUE)

MEMOIR.fit <- function(records, effort, wearRates, groups, fitLocs=TRUE) {

library(survival)
## setting up empty frames for storage.
## if you're recording all iterations of this loop, the loop starts here!
outrecoveries <- data.frame(repNum=c(0),BAND=c(0),SCIENA=c("Animaux artificialis"),
                         BANDED=as.POSIXlt("1888-11-11"),RECOVERED=as.POSIXlt("1888-11-11"),
                         ELAPSED_DAYS=c(0),
                         BANDYEAR=c(1887),BANDLOC=c(0),RECOVERYEAR=c(1887),CENS=c(0),
                         ELAPSED_YEARS=c(0),
                         recordsInclusions=c(FALSE),recordsNamedSpp=c(FALSE),
                         Wa=c(1.100000),Wal=c(1.10000),Wi=c(1.10000),Wil=c(1.10000),
                         Wb=c(1.10000),Waib=c(2.20000),Wail=c(2.20000),Wbail=c(2.20000),
                         EVENT=c(1),START_DAY=c(0))
outapplications <- data.frame(repNum=c(0),allYears=c(1887),SPECIES=c("Animaux artificials"),
                         BANDED=c(0),RECOVERED=c(0),predicted=c(10.01010))
#for(p in 1:max(records$recRepNo)) {
    
# switch between this read-in and the pair below to change between the real data (top) and
 # simulated data (bottom)
#records <- read.csv("~/Dropbox/PhD/Modelled Senescence/RealData/seabird_longevity.csv")
#effort <- read.csv("~/Dropbox/PhD/Modelled Senescence/RealData/seabird_longevity_numbers_banded_by_year.csv")

## simulated data read-in!
#records <- records
#effort <- effort
#activeRecords <- subset(outputRecords,recRepNo==p)
#records <- activeRecords[2:ncol(activeRecords)]
#activeEffort <- subset(outputEffort, eRepNo==p)
#effort <- activeEffort[2:ncol(activeEffort)]

ELAPSED_YEARS <- floor(records$ELAPSED_DAYS/365)
records <- data.frame(records,ELAPSED_YEARS)
#LADO <- subset(records, SCIENA=="Larus dominicanus")

#for(i in 1:nrow(names)){
#    print(names$SCIENA[i],quote=TRUE)
#    print("1")
#}

effortInclusions <- effort$SPECIES %in% records$SCIENA
recordsInclusions <- records$SCIENA %in% effort$SPECIES ## this pair of calls identifies
 # species that are listed in both 'effort' and 'records', as only those species in both
 # lists can be modelled.

records <- data.frame(records,recordsInclusions)
effort <- data.frame(effort,effortInclusions)

records <- records[recordsInclusions==TRUE,]
effort <- effort[effortInclusions==TRUE,]

if(length(records$SCIENA)>0){
records$SCIENA <- droplevels(records$SCIENA)
effort$SPECIES <- droplevels(effort$SPECIES)
                                        }# this trims datasets to just those species in
# both effort and records
scinames <- data.frame(xtabs(~SCIENA,data=records))
scinames <- subset(scinames,Freq>20)   #### hard-code of minimum 20 observations here
scinames$SCIENA <- droplevels(scinames$SCIENA)
  #trims the list of species down to only those with more than twenty collection events

effortNamedSpp <- effort$SPECIES %in% scinames$SCIENA
recordsNamedSpp <- records$SCIENA %in% scinames$SCIENA

records <- data.frame(records,recordsNamedSpp)
effort <- data.frame(effort,effortNamedSpp)

records <- records[recordsNamedSpp==TRUE,]
effort <- effort[effortNamedSpp==TRUE,]
records$SCIENA <- droplevels(records$SCIENA)
effort$SPECIES <- droplevels(effort$SPECIES)  # this trims datasets to just those species in
                                              # names (i.e., those with enough repeats)
### setting up empty dataframes for storage:

Weibulls <- list(scinames)
weightedWeibulls <- list(scinames)

levelIDs <- unique(groups)
levelIDs <- data.frame(index=c(1:length(levelIDs)), level=levelIDs)  ## 'levels' is the table form
                                                                # of 'groups', from input.
### LOOP STARTS HERE!!! HERE!!! HERE!!! ####

for(s in 1:nrow(levelIDs)) {

if(class(levelIDs$level) == "factor") {
    if(nrow(subset(effort,effort$SPECIES==levelIDs$level[s]))>0) {
        frameCheck <- c(TRUE)
    }
} else if(class(levelIDs$level) == "numeric") {
    if(nrow(subset(effort,eRepNo==levelIDs$level[s]))>0) {
        frameCheck <- c(TRUE)
    }
} else if (class(levelIDs$level) != "factor" && class(levelIDs$level) != "numeric") {
   print("the class of groups is neither factor or numeric, so cannot be interpreted")
} 

if (frameCheck == TRUE) {     ## if there is any actual data in this population...
  ## this will break unless levels are species (as text) or repeat numbers (as numeric)

## Part One: modelling the effects of non-uniform numbers of potential banded individuals
 # dying across the range of ages.
## Our active group is called 'SIGU' for historical reasons: Silver Gull was an abundant
 # species in the dataset on which MEMOIR was developed.

# species <- drop.levels(factor(print(names$SCIENA[s],quote=TRUE,max.levels=0),ordered=FALSE))    
if (class(levelIDs$level)=="numeric") {    
   SIGUeffort <- subset(effort, eRepNo==levelIDs$level[s])
} else if (class(levelIDs$level)=="factor") {
    SIGUeffort <- subset(effort, SPECIES==levelIDs$level[s])
}  ## this will break if groups are not species (differentiated by text) or repeats
    # differentiated by number in eRepNo. An example would be if a user defined a
    # custom grouping by number and did not also put that number in the eRepNo column.
## SIGUeffort BANDED = x$Freq  and YEAR = x$var

firstBand <- min(strptime(records$BANDED,format="%Y-%m-%d"))  ## this may have to be altered
latestRecap <- max(strptime(records$RECOVERED,format="%Y-%m-%d")) ## depending on date format  ### this is modified to prevent breakage during low-repetition runs.

presentYear <- as.numeric(format(latestRecap,"%Y"))
                     # If there is a record recovered after presentYear, this will break the
                     # estimate of Wi. This can be solved either by setting presentYear
                     # to the year in which the latest recovery happens, or by discarding
                     # recoveries after presentYear from the dataset.
firstYear <- min(SIGUeffort$YEAR)
                   #the first year of band-application in the Australian dataset
SIGUeffort <- subset(SIGUeffort, SIGUeffort$YEAR <= presentYear)

x <- data.frame((SIGUeffort$YEAR-(firstYear-1)), SIGUeffort$BANDED)
colnames(x) <- c("yearNumber","BANDED") # years start at Year 1, not Year 0.

allYears <- c(firstYear:presentYear)
zeroes <- c(rep(0,length(allYears)))

# zeroes[x$yearNumber] <- x$BANDED ## this tricky little line creates a vector of band
                                  # applications by year, including years with zero applications
for (l in 1:length(zeroes)) {
    zeroes[l] <- sum(subset(x$BANDED, x$yearNumber == l))
}  ## creates a vector of the number banded each year, safe for multiple locations

BANDED <- zeroes
SPECIES <- rep(as.factor(SIGUeffort[1,3]),length(BANDED))

bandApplications <- data.frame(allYears,SPECIES,BANDED)
## bandApplications is a frame with a line for each year, the species name, and the number
 # of that species banded in that year.
maxAge <- presentYear - min(SIGUeffort$YEAR) # maxAge is the oldest age that could theoretically
 # have been recorded
ages <- c(1:maxAge)
ageCoverage <- c(rep(0,maxAge))
for(i in 1:length(ageCoverage)){
    iSum <- sum(BANDED[1:i]) # iSum is the number of individuals that can have been recorded
                             # dying at age i, presuming all individuals survived to age i
    ageCoverage[maxAge-i] <- iSum
} 

ageCoverage <- data.frame(ages,ageCoverage) # ages in ageCoverage represent a range of one year
                                            # i.e., age 1 covers 0 - 1 years of age.
# with(ageCoverage, plot.default(ages,ageCoverage,type="l",
#             main=print(names$SCIENA[s])))

if (class(levelIDs$level) == "numeric") {    
   SIGUrecovery <- subset(records, recRepNo==levelIDs$level[s])
} else if (class(levelIDs$level) == "factor") {
    SIGUrecovery <- subset(records, SCIENA==levelIDs$level[s])
}  # this will break under the same conditions as the SIGUeffort subsetter.
SIGUrecovery <- subset(SIGUrecovery,ELAPSED_DAYS>0) # because there are some erroneous
                                                    # records of birds recaptured dead
                                                    # before they were banded alive, and that
                                                    # breaks the code.

SIGUeffort <- subset(SIGUeffort, SIGUeffort$LOC %in% SIGUrecovery$BANDLOC)
SIGUrecovery <- subset(SIGUrecovery, SIGUrecovery$BANDLOC %in% SIGUeffort$LOC)
SIGUeffort$LOC <- droplevels(SIGUeffort$LOC)
SIGUrecovery$BANDLOC <- droplevels(SIGUrecovery$BANDLOC)
    
SIGUlocations <- levels(SIGUrecovery$BANDLOC)
    
SIGUeffort$LOC <- droplevels(as.factor(as.numeric(SIGUeffort$LOC)))
SIGUrecovery$BANDLOC <- droplevels(as.factor(as.numeric(SIGUrecovery$BANDLOC)))
    
SIGUlocations <- data.frame(locNumber=c(1:length(SIGUlocations)),SIGUlocations)

    #######################################################################################
    ## from this line, to...
#    SIGUrecovery$BANDLOC <- as.numeric(SIGUrecovery$BANDLOC)

#     SIGUeffort$LOC <- as.numeric(SIGUeffort$BANDLOC)
    #######################################################################################

allLocsBanded.t <- xtabs(BANDED~YEAR+LOC,data=SIGUeffort) #CHECK THIS. Numbers appear wrong
    ## Problem ID'd: SIGUeffort is not currently being subsetted to the active iteration
     # and is therefore returning numbers banded over all iterations. Resolved at
     # 24-03-16 by correcting 'groups' on read-in. Documentation changed to clarify;
     # no change to code needed.
allLocsBanded.t <- data.frame(cbind(YEAR=as.numeric(row.names(allLocsBanded.t)),allLocsBanded.t))
row.names(allLocsBanded.t) <- 1:nrow(allLocsBanded.t)
if (ncol(allLocsBanded.t) > 2) {
    SIGUeffort <- data.frame(YEAR=allLocsBanded.t$YEAR
                        ,CAVS=c(rep(SIGUeffort$CAVS[1],nrow(allLocsBanded.t)))
                         ,SPECIES=rep(SIGUrecovery$SCIENA[1],nrow(allLocsBanded.t))
                         ,BANDED=as.vector(apply(allLocsBanded.t[,2:ncol(allLocsBanded.t)],1,sum))
                         ,allLocsBanded.t[,2:ncol(allLocsBanded.t)])
} else {
    SIGUeffort <- data.frame(YEAR=allLocsBanded.t$YEAR
                        ,CAVS=c(rep(SIGUeffort$CAVS[1],nrow(allLocsBanded.t)))
                         ,SPECIES=rep(SIGUrecovery$SCIENA[1],nrow(allLocsBanded.t))
                         ,BANDED=as.vector(allLocsBanded.t[,2])
                         ,X1=allLocsBanded.t[,2])
}
    
#allLocsBanded.t <- effort[c(2,3,4,ncol(effort),ncol(effort)-1)*-1]

recoveryBlanks <- data.frame(BAND=c(runif(ncol(allLocsBanded.t)-1,min=0,max=9999999)),
                             SCIENA=c(rep("Blank",ncol(allLocsBanded.t)-1)),
                             BANDED=c(rep(as.POSIXlt("1888-11-11"),ncol(allLocsBanded.t)-1)),
                             RECOVERED=c(rep(as.POSIXlt("1888-11-11"),ncol(allLocsBanded.t)-1)),
                             ELAPSED_DAYS=c(rep(0,ncol(allLocsBanded.t)-1)),
                             BANDYEAR=c(rep(1888,ncol(allLocsBanded.t)-1)),
                             BANDLOC=c(seq(1,ncol(allLocsBanded.t)-1,1)),
                             RECOVERYEAR=c(rep(1888,ncol(allLocsBanded.t)-1)),
                             CENS=c(rep(2,ncol(allLocsBanded.t)-1)),
                             ELAPSED_YEARS=c(rep(0,ncol(allLocsBanded.t)-1)),
                             recordsInclusions=c(rep(TRUE,ncol(allLocsBanded.t)-1)),
                             recordsNamedSpp=c(rep(TRUE,ncol(allLocsBanded.t)-1))
#                             ,Wa=c(rep(1,ncol(allLocsBanded.t)-1))
                             )
SIGUrecovery <- data.frame(BAND=SIGUrecovery$BAND,SCIENA=SIGUrecovery$SCIENA,
                           BANDED=SIGUrecovery$BANDED, RECOVERED=SIGUrecovery$RECOVERED,
                           ELAPSED_DAYS=SIGUrecovery$ELAPSED_DAYS, BANDYEAR=SIGUrecovery$BANDYEAR,
                           BANDLOC=SIGUrecovery$BANDLOC,RECOVERYEAR=SIGUrecovery$RECOVERYEAR,
                           CENS=SIGUrecovery$CENS,ELAPSED_YEARS=SIGUrecovery$ELAPSED_YEARS,
                           recordsInclusions=SIGUrecovery$recordsInclusions,
                           recordsNamedSpp=SIGUrecovery$recordsNamedSpp)
SIGUrecovery <- rbind(recoveryBlanks,SIGUrecovery)

allLocsRecovered.t <- with(SIGUrecovery, xtabs(~RECOVERYEAR+BANDLOC))
allLocsRecovered.t <- data.frame(cbind(YEAR=as.numeric(row.names(allLocsRecovered.t)),allLocsRecovered.t))
allLocsRecovered.t <- subset(allLocsRecovered.t,YEAR > 1888)
SIGUrecovery <- subset(SIGUrecovery, BANDYEAR > 1888) ## removes the values in recoveryBlanks.
                                                       # Also means the code will exclude
                                                       # data from prior to 1888.

allLocsBanded <- matrix(0,nrow=max(SIGUrecovery$RECOVERYEAR)-(min(SIGUeffort$YEAR)-1),ncol=ncol(allLocsBanded.t)-1)
allLocsBanded <- cbind(min(SIGUeffort$YEAR):max(SIGUrecovery$RECOVERYEAR),allLocsBanded)

allLocsRecovered <- matrix(0,nrow=max(SIGUrecovery$RECOVERYEAR)-(min(SIGUeffort$YEAR)-1),ncol=ncol(allLocsBanded.t)-1)
### swapped in
allLocsRecovered <- cbind(min(SIGUeffort$YEAR):max(SIGUrecovery$BANDYEAR),allLocsRecovered)
###########################################
for(i in 1:nrow(allLocsBanded)) {
    if(allLocsBanded[i,1] %in% allLocsBanded.t$YEAR==TRUE) {
        activeRow <- subset(allLocsBanded.t, YEAR==allLocsBanded[i,1])
        allLocsBanded[i,] <- as.matrix(activeRow)
    }else{
    }
}  # does the substitution for allLocsBanded

for(i in 1:nrow(allLocsRecovered)) {
    if(allLocsRecovered[i,1] %in% allLocsRecovered.t$YEAR==TRUE) {
        activeRow <- subset(allLocsRecovered.t, YEAR==allLocsRecovered[i,1])
        allLocsRecovered[i,] <- as.matrix(activeRow)
    }else{
    }
} ## ditto for allLocsRecovered
### trimmed 19/08/16:
#allLocsRecovered <- matrix(as.numeric(allLocsRecovered),nrow=nrow(allLocsBanded),
#                           ncol=ncol(allLocsBanded))
## to here 19/08/16

    
#for(i in 1:ncol(allLocsBanded)) {
#    print(cor(allLocsBanded[,i],allLocsRecovered[,i]))
#}
firstBandYear <- min(SIGUeffort$YEAR)
lastRecoverYear <- presentYear

allLocsRecovered <- subset(allLocsRecovered, allLocsRecovered[,1] >= firstBandYear)
allLocsRecovered <- subset(allLocsRecovered, allLocsRecovered[,1] <= lastRecoverYear)

allLocsBanded <- subset(allLocsBanded, allLocsBanded[,1] >= firstBandYear)
allLocsBanded <- subset(allLocsBanded, allLocsBanded[,1] <= lastRecoverYear)

SIGUrecovery <- subset(SIGUrecovery,SCIENA!="Blank")

## ... this line, is getting the numbers of banded and recaptured by year and location into
 # comparable formats. This has been moved up from the Wil precursor, so will probably be able
 # to be deleted from there.

Wa <- c(rep(0,nrow(SIGUrecovery)))
for(i in 1:nrow(SIGUrecovery)) {
    Wax <- 1/
        (ageCoverage$ageCoverage[SIGUrecovery$ELAPSED_YEARS[i]+1]
         /max(ageCoverage$ageCoverage))
  #  print(Wax)
    Wa[i] <- Wax
}

SIGUrecovery <- data.frame(SIGUrecovery,Wa)
    
## Part Two: modelling the relationship between number of recaptures and number of bands
 # applied in a year

# SIGUrecovery <- subset(records, SCIENA=="Chroicocephalus novaehollandiae")

x_2 <- data.frame(RECOVERYEAR = allYears,
                  RECOVERED = c(rep(0, length(allYears)))
                  )
for (l in 1:length(allYears)) {
    x_2$RECOVERED[l] <- nrow(subset(SIGUrecovery,
                                    SIGUrecovery$RECOVERYEAR == x_2$RECOVERYEAR[l]))
}

# x_2 <- data.frame(xtabs(~SIGUrecovery$RECOVERYEAR))
# colnames(x_2) <- c("RECOVERYEAR", "RECOVERED")

zeroes_2<- c(rep(0,length(allYears)))
zeroes_2 <- data.frame(allYears,zeroes_2)

for(i in 1:nrow(zeroes_2)) {
    activeYear <- zeroes_2$allYears[i]
    if(activeYear %in% x_2$RECOVERYEAR == TRUE) {
        year <- subset(x_2,RECOVERYEAR==activeYear)
        zeroes_2$zeroes_2[i] <- year[,2]
    }
}   ## could be achieved just be re-naming the x_2 columns to 'allYears' and 'zeroes_2',
    ## then assigning to zeroes_2. Why did I do the loop?
## zeroes_2 becomes active at line 369
## here, we set up location-specific Wa estimates, a.k.a. Wal estimates:

ageCoverageL <- matrix(0,nrow=maxAge,ncol=ncol(allLocsBanded)) # location-specific ageCoverage
ageCoverageL[,1] <- ages
for(j in 2:ncol(ageCoverageL)) {
    activeSite <- allLocsBanded[,j]
    for(i in 1:length(activeSite)){
        iSum <- sum(activeSite[1:i])
                             # iSum is the number of individuals that can have been recorded
                             # dying at age i, presuming all individuals survived to age i
        ageCoverageL[length(activeSite)-i,j] <- iSum
    }
}
ageCoverageL <- data.frame(ages=ageCoverageL[,1],ageCoverageL[,2:ncol(ageCoverageL)])
                # ages in ageCoverage represent a range of one year
                # i.e., age 1 covers 0 - 1 years of age.

Wals <- matrix(0,nrow(ageCoverageL),ncol(allLocsBanded)) ## blanks for holding preds
for(j in 2:ncol(Wals)) {
    for(i in 1:nrow(Wals)) {
        Wali <- 1/(ageCoverageL[i,j]/max(ageCoverageL[,j])) 
        Wals[i,j] <- Wali
    }
}
Wals <- rbind(Wals[1,],Wals)
Wals[,1] <- c(0:(nrow(Wals)-1))
Wals <- data.frame(ages=Wals[,1],Wals[,2:ncol(Wals)])

SIGUrecovery$BANDLOC <- as.numeric(SIGUrecovery$BANDLOC)
Wal <- rep(0,nrow(SIGUrecovery))
for (i in 1:length(Wal)) {
    Wal[i] <- Wals[SIGUrecovery$ELAPSED_YEARS[i]+1,SIGUrecovery$BANDLOC[i]+1]
}  ## the final Wal creation

SIGUrecovery <- data.frame(SIGUrecovery,Wal)

# zeroes_2[x_2$RECOVERYEAR] <- x_2$RECOVERED ## this tricky little line creates a vector of band
                                  # applications by year, including years with zero applications
## nope, scratch that. Couldn't make that line work without the for(if()) loop above
RECOVERED <- zeroes_2$zeroes_2
bandApplications <- data.frame(bandApplications, RECOVERED)
#aov1 <- glm(bandApplications$RECOVERED~bandApplications$BANDED
#           +I(bandApplications$BANDED^2)
#           ,family=poisson)

aov1 <- nls(bandApplications$RECOVERED#[1:nrow(allLocsBanded)]
            ~
                a + b * bandApplications$BANDED#[1:nrow(allLocsBanded)]
          , algorithm="port"
          ,start=c(a=5,b=0.1),lower=c(a=0.01,b=0),upper=c(a=Inf,b=Inf))
## modified 18/8/16 to remove the [1:nrow(allLocsBanded)], as this was causing misalignment.
    
summary(aov1)  ## the trim to the length of allLocsBanded ensures that only years
                # before the present year are used in this model; detected because
                # Wil was != Wi in simulations where locs = 1. Throws an odd error if
                # the data are read in as a trimmed form of bandApplications,
                # (Error in parse(text = x, keep.source = FALSE):
                #   <text>:2:0: unexpected end of input)
                # but doesn't
                # throw the error if the vectors are trimmed in the nls call itself.

predictions2 <- matrix(0,nrow(allLocsBanded),ncol(allLocsBanded)) ## blanks for holding preds
for(j in 2:ncol(allLocsBanded)){
    if(sum(allLocsBanded[,j])>=1){
        predictions2[,j] <- rep(mean(allLocsRecovered[,j]),length(predictions2[,j]))
    }else{
        predictions2[,j] <- rep(0.001,nrow(predictions2)) # vanishingly low placeholder prob.
    }  ## sets up plausible-but-not-sensible placeholders, in case of nls failure below.
}

for(j in 2:ncol(allLocsBanded)) {
     if(sum(allLocsBanded[,j])>=1) {
         if(class(try(nls(allLocsRecovered[,j]~a+b*allLocsBanded[,j],algorithm="port",
                     start=c(a=5,b=0.1),lower=c(a=0.01,b=0),
                     upper=c(a=Inf,b=Inf))))=="try-error") {
             print("A Wil model for a specific location has failed to resolve despite a non-zero number of recoveries. For that location, the modelled expected number of recoveries for each year is taken as the mean annual number of recoveries across all years. The iteration and location of this failure to resolve are:")
             print(c("Iteration number", p, "Location number", j), quote=FALSE)
         }else{
             aov2 <- nls(allLocsRecovered[,j]~a+b*allLocsBanded[,j],algorithm="port",
                    start=c(a=5,b=0.1),lower=c(a=0.01,b=0),upper=c(a=Inf,b=Inf))
             # the current model is strictly local, assuming essentially that movement between
             # sites does not happen. Modify in future to rec~a+b*localMarked+c*elsewhereMarked,
             # to account for movement.
             predictions2[,j] <- predict(aov2,type="response")
         }
     }
}
#}

predictions2[,1] <- allLocsBanded[,1]
## this loop gives a matrix of predictions of recapture number by location and year

# plot.default(log(bandApplications$RECOVERED)~log(bandApplications$BANDED))
# plot(residuals(aov1))
#  plot(aov1)
## the above line causes the script to break when run in batch, as it queues things after the
 # requirement for the user to press 'enter' five times.

#new.df <- data.frame(BANDED=c(max(bandApplications$BANDED),min(bandApplications$BANDED)
                            # ,bandApplications$BANDED[3:nrow(bandApplications)]
#                              ))
predictions <- predict(aov1,type="response")
# plot.default(bandApplications$RECOVERED~bandApplications$BANDED)
# points(predictions~bandApplications$BANDED+1, col=2)
# mtext(names$SCIENA[s])

#bandApplications <- bandApplications[1:nrow(allLocsRecovered),] ## dangerous subset!
    ######## trimmed 18/08/16
bandApplications$predicted <- predictions

# limPredictions <- predict(aov1, newdata=new.df,type="response")
## limPredictions[1] is the highest predicted number of band returns for a year in the
 # range of observed bands applied.
Wi <- c(rep(0,nrow(SIGUrecovery)))
for(i in 1:nrow(SIGUrecovery)) {
    xYear <- SIGUrecovery$RECOVERYEAR[i]
    xBandApplications <- subset(bandApplications,allYears==xYear)
    Wix <- 1/(xBandApplications$predicted[1]/max(predictions))  ##
    Wi[i] <- Wix
}

if (length(unique(records$BANDLOC)) == 1) {
   Wil <- Wi
} else {

Wil <- c(rep(0,nrow(SIGUrecovery)))
for(i in 1:nrow(SIGUrecovery)) {
    xYear <- SIGUrecovery$RECOVERYEAR[i]
    xYearPredictions <- subset(predictions2, predictions2[,1] == xYear)
    xLocation <- as.numeric(SIGUrecovery$BANDLOC[i])
    xPrediction <- xYearPredictions[xLocation+1]
    Wilx <- 1/(xPrediction/max(predictions2[,-1][,xLocation]))
    Wil[i] <- Wilx
}  ## note that, with one location, Wil will not necessarily equal Wi, because nls() may
    # converge on a subtly different model for the two scenarios. 
}

## extract the year of the current observation
## extract the number of bands applied in that year
## use the number of bands applied to predict a number of recoveries for that observation-year
## establish the ratio of that predicted number to the mamximum predicted number.        
SIGUrecovery <- data.frame(SIGUrecovery, Wi, Wil)
## append to the existing dataframe

# predict(aov1, newdata=max(bandApplications$BANDED+1))

## Part Three: modelling the band-loss rate
## here, we model band-loss as a log-normal distribution. Use other distributions as you
 # see fit.
SIGUrecovery$SCIENA <- droplevels(SIGUrecovery$SCIENA)
SIGUwear <- subset(wearRates, wearRates$SPECIES == paste(SIGUrecovery$SCIENA[1]))
## the list of names in lossrates must exactly match the list of names in names
    
endpoint <- 65  # here the value given in Ludwig (1981), expressed as a percentage.
#mwr <- 2.22  # insert mean wear-rate value for your species here!
mwr <- SIGUwear$usedRate  ## use this line if you are reading in a species-specific set of wear
                           # rates, or use the previous if not.
    
maxwr <- mwr+((2/3)*mwr)
minwr <- mwr-((2/3)*mwr)

myears <- endpoint/mwr
minyears <- endpoint/maxwr
maxyears <- endpoint/minwr

lmyears <- log(myears)
lminyears <- log(minyears)
lmaxyears <- log(maxyears)
## conversions for modelling as a lognormal
## running with the max-mean years as the 95% point; approximates Normal with decent band-
 # persistences (esp. if low variance).

maxsd <- (maxyears-myears)/1.96 # 'conservative'; a contrast of minyears to myears would
                                # give a smaller sd
minsd <- (myears-minyears)/1.96 # vice versa
msd <- (maxsd+minsd)/2          #fixed? Stands for 'mean sd', not 'Ministry of Social
                                # Development'
lmsd <- (((lmaxyears-lmyears)/1.96)+((lmyears-lminyears)/1.96))/2 # the correct sd for
 # lognormal analyses

# plot(plnorm(c(seq(0.01,maxyears*3,0.01)),meanlog=log(myears),sdlog=lmsd))
# mtext(names$SCIENA[s])
# name <- names$SCIENA[s]
# generates the cumulative density function for the lognormal distribution
# remember, when interpreting this curve, that all ages are log-transformed and need
# back-transformation before interpretation. ALSO: the sd in a lognormal curve is the
# standard deviation of the ln of the data, not the ln of the sd of the data. This makes
# rather a substantial difference!

## next step is for each observed age, calculate the prior probability that the band would
 # have fallen off by that age.

sampleAges <- c(1:100) ## replace with a vector of real ages-at-recovery!
# plot(plnorm(sampleAges,meanlog=log(myears),sdlog=lmsd))
# mtext(names$SCIENA[s])
# plnorm(sampleAges,meanlog=log(myears),sdlog=lmsd)

Wb <- c(rep(0,nrow(SIGUrecovery)))
for(i in 1:nrow(SIGUrecovery)) {
    xage <- SIGUrecovery$RECOVERYEAR[i]-SIGUrecovery$BANDYEAR[i]
    Wbx <- (1/(1-plnorm(xage,meanlog=lmyears,sdlog=lmsd)))[1]
    Wb[i] <- Wbx  # This variable is pronounced 'Weetbix'.
}

SIGUrecovery <- data.frame(SIGUrecovery,Wb)

Waib <- Wa*Wi*Wb
Wail <- Wal*Wil  ## this name only vaguely happened by accident; it's just a shame that it will
                 # not be the most commonly-used weighting.
Wbail <- Wb*Wal*Wil
SIGUrecovery <- data.frame(SIGUrecovery,Waib,Wail,Wbail)

# with(SIGUrecovery, plot.default(RECOVERYEAR, Waib))
# mtext(names$SCIENA[s])
## lognormal band-loss model: fitted.

# Next task: generating a weight for each datapoint in real data. Using SIGU

#hist(SIGUrecovery$ELAPSED_DAYS,xlim=c(0,17230),main="Histogram of durations between marking and discovery dead",xlab="elapsed days")
# mtext(names$SCIENA[s])

## select focal species
# SIGUeffort <- subset(effort, SPECIES==names$SCIENA[s]) 

## fitting a Kaplan-Meyer curve using Coxph

    ## From here, we can probably move things out to a 'modelplotter' or 'post-analysis'
     # function. Leaving them in probably slows things down.

BANDED <- strptime(SIGUrecovery$BANDED,"%d-%b-%Y")
RECOVERED <- strptime(SIGUrecovery$RECOVERED,"%d-%b-%Y")
SIGUrecovery$BANDED <- BANDED
SIGUrecovery$RECOVERED <- RECOVERED
START_DAY <- c(rep(0,nrow(SIGUrecovery)))
SIGUrecovery <- data.frame(SIGUrecovery,EVENT=c(rep(1,nrow(SIGUrecovery))),START_DAY)
## an 'event' score of 1 says that, yes, this bird was dead when it was found.

SIGUSurv <- with(SIGUrecovery, Surv(START_DAY,ELAPSED_DAYS, EVENT))
# plot(SIGUSurv, main=names$SCIENA[s])
surv.fit <- survfit(SIGUSurv~SIGUrecovery$EVENT,
                    # weights=SIGUrecovery$Waib,
                    conf.type="log-log") 
# 
#plot(surv.fit, main=names$SCIENA[s],
#     xlab="survival duration (days)",
#     ylab="Proportion survival to age, without weighting"
#     )  # the plot of that. PRIMARY OUTPUT

## surv.fit comes with the number at risk and number of events for each timepoint, so...
propDeaths <- surv.fit$n.event/surv.fit$n.risk
# plot(propDeaths~surv.fit$time)
# mtext(names$SCIENA[s])

### plot the predicted models using a Weibull:
#####  edit the censoring here, to fit censored or uncensored data as applicable!
    ####################################################################################
surviv <- with(SIGUrecovery,Surv(ELAPSED_DAYS,CENS))
    ####################################################################################
#plot kaplan-meier estimate
# fKM <- survfit(surviv ~ EVENT,data=SIGUrecovery)                                   ###
#plot(fKM,xlab="Age (years)",ylab="Proportion surviving to age x",axes=FALSE)
#axis(side=2)
#axis(side=1,at=seq(365.25,max(SIGUrecovery$ELAPSED_DAYS),(2*365.25)),
#     labels=seq(1,floor(max(SIGUrecovery$ELAPSED_DAYS))/365.25,2))
#box()
#mtext(name)
#plot Cox PH survival curves
###########################################  Set weighting method here!  ##################
#xCox <- coxph(surviv ~ EVENT,data=SIGUrecovery,weights=round(SIGUrecovery$Wbail))        ##
###########################################################################################
#lines(survfit(xCox,conf.int=FALSE,newdata=data.frame(EVENT=1)),col=rgb(0,1,0,alpha=0.3))
#SIGUwei <- survreg(surviv~EVENT,dist="weibull",data=SIGUrecovery)                       ##
#SIGUwwei <- survreg(surviv~EVENT,weights=round(SIGUrecovery$Waib),dist="weibull",data=SIGUrecovery)                                                                                     ## 
#lines(predict(SIGUwei, newdata=list(EVENT=1),type="quantile",p=seq(.01,.99,by=.01)),
#      seq(.99,.01,by=-.01),col="red")
#lines(predict(SIGUwwei, newdata=list(EVENT=1),type="quantile",p=seq(.01,.99,by=.01)),
#      seq(.99,.01,by=-.01),col="blue")
#legend(x=(max(SIGUrecovery$ELAPSED_DAYS)-0.4*max(SIGUrecovery$ELAPSED_DAYS)),y=1,
#          legend=c("without weighting","with weighting","unweighted Weibull predictions","weighted Weibull predictions"),
#       col=c("black","green","red","blue"),lty=c(1,1,1,1))
## same again, but on a scale where constant-hazards are linear
# tomorrow, maybe. Not a high priority, really

## exporting the model-fits for posterity.
# Weibulls[[s+1]] <- SIGUwei                                                              ##
# weightedWeibulls[[s+1]] <- SIGUwwei                                                     ##

## grouping the survival fit into years
maxyear <- ceiling(max(surv.fit$time)/365)
surv.fit <- data.frame(summary(surv.fit)[2:10])

range <- 1:maxyear
point <- 365

years <- data.frame(n.risk=c(0),n.event=c(0),surv=c(0)) 

for(i in range) {
    bot <- point-365
    xset <- subset(surv.fit,subset=surv.fit$time<point)
    yset <- subset(xset,subset=time>bot)
    suppressWarnings(n.risk <- max(yset$n.risk))
    n.event <- sum(yset$n.event)
    surv <- 1-(n.event/n.risk)
    append <- data.frame(n.risk,n.event,surv)
    years[i,] <- append
    point <- point+365
}

years <- data.frame(range,years)
if(years$n.risk[1]==-Inf) {
    years$n.risk[1] <- max(years$n.risk)
}
for(i in 1:nrow(years)) {
    if(years$n.risk[i]==-Inf){
        years$n.risk[i] <- years$n.risk[i-1]
    }
}  #fixing the annoying thing where, if no deaths occurred at an age, the number at risk
   # was recorded as negative infinity.
## the dataframe 'years' is an output

##goal 1 Graph
# with(years, plot(surv~range,xlab="age",ylab="proportion of surviving individuals that survive this age-class",main=print(SIGUrecovery$SCIENA[1])))
##this is a plot of the recorded annual survival rate across
                                # age-classes. It does not take into account the weightings.
## THIS IS A PRIMARY OUTPUT

## appending primary outputs

SIGUrecovery <- data.frame(repNum=c(rep(s,nrow(SIGUrecovery))),SIGUrecovery)
bandApplications <- data.frame(repNum=c(rep(s,nrow(bandApplications))),bandApplications)
outrecoveries <- rbind(outrecoveries, SIGUrecovery)
outapplications <- rbind(outapplications, bandApplications)

}
} ## this one closes the repeats for 1:s, where p is the number of simulation runs or species

# system("espeak --stdout 'Your models are all fitted!' | aplay")
# system('notify-send "Your models are all fitted"')

#dev.off()

# trimming up the table outputs
outrecoveries <- outrecoveries[-1,]
outapplications <- outapplications[-1,]

outs <- list(outrecoveries, outapplications)
return(outs)
    
#write.csv(outrecoveries, "currentRecoveries.csv")
#write.csv(outapplications, "currentApplications.csv")
    
}  ## this is the close for the MEMOIR.fit() function
