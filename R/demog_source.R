## -----------[[Get demog data source]]---------------------
#' Demographic Data Source Table in Auto Reporting
#'
#' Only used for automated reporting. Get a demographic distribution.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param data Raw data frame.
#' @param imput Summarized data frame.
#' @param xvar Variable name of x.
#' @param gvar Variable name of grouping factor.
#' @param yvar Variable name of y.
#' @param tblxvar Data frame of x variable mapping data.
#' @param tblgvar Data frame of g variable mapping data.
#' @param lang 'cn' or 'en'.
#' @param sort A list. Default \code{list(col=1, desc=FALSE)}, meaning sort the
#' resutls ascendingly by col1.
#'
#' @return A data frame.
#' @export
#' @importFrom reshape2 dcast
#'
#' @examples
#'
tabDemogSource <- function(data, imput=NULL, xvar, gvar, yvar=NULL,
                           tblxvar=NULL, tblgvar=NULL,
                           sort=list(col=1, desc=FALSE), lang=LANG){

    popl <- melt(dcast(data, as.formula(paste(xvar , "~", gvar)),
                       value.var="RID", length), id=xvar)
    # popl$variable <- factor(popl$variable, c("M", "F"))
    if (!is.null(imput)) if (nrow(imput)==0) imput <- NULL
    if (!is.null(imput)){
        if (nrow(imput) > 0){
            imputTbl <- dcast(imput, RiskL2ID~., sum, value.var="NPcpt")
            maximput <- imputTbl[which(imputTbl$`.` == max(imputTbl$`.`, na.rm=TRUE)),
                                 "RiskL2ID"]
            imput <- imput[imput$RiskL2ID==maximput[1],]
            imput <- imput[, c(xvar, gvar, "NPcpt")]
            names(imput) <- c(xvar, "variable", "value")
            imput <- imput[imput[, xvar] %in% popl[, xvar] &&
                               imput$variable %in% popl$variable,]
            popl <- rbind(popl, imput)
        }
    }
    mapping <- data.frame(init=c("Gender", "BUID", "City"),
                          from=c("Gender", "BUID", "CityAlias"),
                          tocn=c("GenderCN", "BUCNCode", "CityAlias"),
                          toen=c("GenderEN", "BUENCode", "CityEN"))

    if (!is.null(tblxvar)){
        mapfrom <- as.character(mapping[mapping$init==xvar, 'from'])
        mapto <- as.character(mapping[mapping$init==xvar,
                                      ifelse(tolower(lang)=='cn', 'tocn', 'toen')])
        popl[, xvar] <- tblxvar[match(popl[, xvar], tblxvar[, mapfrom]), mapto]
    }
    if (!is.null(tblgvar)){
        mapfrom <- as.character(mapping[mapping$init==gvar, 'from'])
        mapto <- as.character(mapping[mapping$init==gvar,
                                      ifelse(tolower(lang)=='cn', 'tocn', 'toen')])
        popl[,'variable'] <- tblgvar[match(popl[, 'variable'], tblgvar[, mapfrom]), mapto]
        # popl$variable due to melt
    }

    if (ncol(popl)==3){
        names(popl) <- c(xvar, gvar, "value")
        levels(popl[, gvar]) <- enc2native(levels(popl[, gvar]))
    }
    if (is.numeric(sort[['col']]) || sort[['col']] %in% names(popl)){
        popl <- popl[order(popl[, sort[['col']]], decreasing=sort[['desc']]),]
        row.names(popl) <- seq_len(nrow(popl))
    }
    return(popl)
}
