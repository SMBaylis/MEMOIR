#' Prepare a band-wear table for MEMOIR.fit read-in
#'
#' Takes a vector of species names and a single wear-rate or vector of wear-rates, and 
#' outputs it for input to MEMOIR.fit.
#' @param SPECIES A factor containing each species name for which fitted estimates are needed
#' @param rate Either a vector with the same length as SPECIES, or a single number,
#'             representing the wear rates for each species, or the shared wear-rate across
#'             all species.
#' @return The function returns a table formatted in the same way as MEMOIR.sim[[3]]
#'         c("SPECIES", "usedRate"), for input to MEMOIR.fit as 'records'.
#' @export
#' @examples
#' with(records, records.fitfeeder(BAND=bandNo, SCIENA=sciName, BANDED=bandDate,
#'      RECOVERED=recoverDate, BANDLOC=locode)) 

wear.fitfeeder <- function(SPECIES, rate) {
    if(length(SPECIES) == length(rate)) {
        rateTable <- data.frame(SPECIES=SPECIES, usedRate=rate)
    } else if (length(SPECIES) < length(rate)) {
        print("There are fewer Species than there are wear-rates!
               You may wish to check your inputs!")
    } else if (length(SPECIES) > length(rate) && length(rate)==1) {
        rateTable <- data.frame(SPECIES=SPECIES, usedRate=c(rep(rate, length(SPECIES))))
    }
}
