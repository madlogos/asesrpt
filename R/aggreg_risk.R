## -----------[[Get aggregate risk data source]]---------------------

#' Aggregated Modifiable Risk Stratification
#'
#' Stratify modifiable risks and aggregate the results.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param data Data frame fo raw data set.
#' @param riskvars Vector of variable names. Default \code{c("Dyslipidemia", "HighBP",
#' "ImpairBG", "OwtObe")}
#' @param grades A list defining values representing high, median and low risks.
#' Default \code{list(high=c(2,3), median=1, low=0))}
#'
#' @return A data frame.
#' @export
#' @importFrom reshape2 melt dcast
#'
#' @examples
#' \dontrun{
#' }
aggregRisk <- function(data, riskvars=c("Dyslipidemia", "HighBP", "ImpairBG", "OwtObe"),
                       grades=list(high=c(2, 3), median=1, low=0)){
	stopifnot(all(c("high", "median", "low") %in% names(grades)))
	stopifnot(all(riskvars %in% names(data)))
    set <- as.matrix(data[, riskvars])
    set_risky <- matrix(set %in% c(grades[['high']], grades[['median']]),
                        ncol = ncol(set))
    nrisk <- as.data.frame(table(rowSums(set_risky)))
    names(nrisk) <- c("Grade", "Freq")
    nrisk$Rate <- nrisk$Freq/sum(nrisk$Freq, na.rm=TRUE)
    trisk <- melt(set, id=NULL)
    trisk$value[trisk$value %in% grades[['low']]] <- 'low'
    trisk$value[trisk$value %in% grades[['median']]] <- 'median'
    trisk$value[trisk$value %in% grades[['high']]] <- 'high'
    trisk$value <- factor(trisk$value, levels=c("low", "median", "high"))
    trisk <- dcast(trisk,Var2~value, length)
    names(trisk) <- c("Risk", "low", "median", "high")
    trisk[, c("Low", "Median", "High")] <- trisk[, c("low", "median", "high")]/
        rowSums(trisk[, c("low", "median", "high")])
    return(list(nrisk, trisk[, c("Risk", "Low", "Median", "High", "low", "median", "high")]))
}
