##---------[[Get source dataset combining Raw and Summary Imput]]-------------
#' Data Frame for Tabulation in Auto Reporting
#'
#' Draw a prevalence table with raw data combining with summarized data, in
#' addition with table controlling parameters. The parameter pool data frames
#' are yielded using \code{setRptParam()}.
#'
#' It is more universal than \code{tabEpiPrev()}.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param data Raw dataset
#' @param imput Summarized data set
#' @param xvar Variable name of x in raw data set.
#' @param yvar Variable name of y in raw data set.
#' @param xvar.imput Variable name of x in summarized data set.
#' @param yvar.imput Variable name of y in summarized data set.
#' @param riskvar The risk variable
#' @param tblRisk Data frame of risk pool.
#' @param tblBU Data frame of BU pool.
#' @param tblGender Data frame of gender pool.
#' @param tblCity Data frame of city pool.
#' @param digits Number of digits in results.
#' @param lang Language, 'cn' or 'en'.
#' @param na.string A named vector \code{c(cn=..., en=...)}. How to present NAs
#' when \code{lang} is EN or CN. Default \code{getOption("na.string")}.
#' @param sort Sorting standard. If \code{sort}=list(col=1, desc=FALSE), then the
#' function sorts the data ascendingly by Col1. If \code{sort}=list(col=3,
#' desc=TRUE), the function sorts the data descendingly by Col3. You can also
#' directly assign a vector, e.g. \code{list(col=1, val=1:4)}.
#' @param row_levels A vector re-defining the factor level of the variable to be
#' put as the raw. Default null. If 'asis', keep the original order; if 'NULL',
#' reorder the columns; if a specific vector, sort columns by that order.
#' @param col.levels A vector re-defining the factor level of the variable to be
#' put as the column. Default null.
#' @param ... ignore
#'
#' @return A data frame restructured based on you requirements.
#' @export
#' @importFrom stringr str_replace_all
#'
#' @examples
#'
#'
tabSource <- function(data, imput=NULL, riskvar, xvar, yvar, xvar.imput=xvar,
                      yvar.imput=yvar, tblRisk, tblBU, tblGender, tblCity,
                      digits=1, lang=c('cn', 'en'), row_levels=NULL, col.levels=NULL,
                      sort=list(col=1, desc=FALSE),
                      na.string=getOption("na.string"), ...){
    # row_levels='asis' keep the original order,
    # =NULL reorder the column,
    # =vector assign the spec order
    #------------Get a mapping dataset--------------
    if (!is.factor(data[, xvar])) levx <- as.factor(unique(data[, xvar]))
    else levx <- levels(data[, xvar])
    lang <- match.arg(lang)
    na.string <- if (length(na.string) == 0) {
        getOption("na.string")
    }else if (length(na.string) == 1) {
        c(na.string, "N/A")
    }else {
        na.string[1:2]
    }
    col1Lang <- switch(xvar,
                       BUID=tblBU[, c("BUID", "BUCNCode", "BUENCode")],
                       City=tblCity[, c("CityAlias", "CityAlias", "CityEN")],
                       AgeG=data.frame(levx, levx, levx),
                       ProjectYear=data.frame(levx, levx, levx),
                       RiskCode=tblRisk[, c("RiskL2Code", "RiskL2CN", "RiskL2EN")],
                       RiskGrade=matrix(rep(0:3, 3), ncol=3))
    extraLang <- as.data.frame(matrix(c(
        "NA", na.string, "N/A", na.string, NA, na.string, enc2native("\u{2265}NA"),
        na.string, enc2native("\u{2264}NA"), na.string, "(all)",
        "\u{5408}\u{8BA1}", "Total"), byrow=TRUE, ncol=3))
    names(extraLang) <- names(col1Lang)
    col1Lang <- rbind(col1Lang, extraLang)
    names(col1Lang) <- c("Label", "CN", "EN")
    #---------------Get source data table--------------
    tab <- NULL
    if (!is.null(imput)) {
        names(imput) <- str_replace_all(
            str_replace_all(names(imput), xvar.imput, xvar), yvar.imput, yvar)
        imput <- restrImput(imput, xvar, yvar, riskvar, tblRisk)
        imput[, xvar][!imput[, xvar] %in% col1Lang[, ]] <- NA # Col1 -> NA if cannot be matched
    }
    data[, xvar][!data[, xvar] %in% col1Lang[, ]] <- NA  # Col1 -> NA if cannot be matched
    if (!is.null(row_levels)){
        if (length(row_levels)==1 && row_levels[1]=='asis'){
            tab <- tabEpiPrev(data, imput, rowvar=xvar, colvar=yvar, riskvar=riskvar,
                              margins=TRUE, col.levels=col.levels, sort=NULL,
                              pct.digits=digits, colvar.together=TRUE, empty.cols=TRUE)
        }else if (length(row_levels)>1 && is.null(sort[['val']])){
            tab <- tabEpiPrev(data, imput, rowvar=xvar, colvar=yvar, riskvar=riskvar,
                              margins=TRUE, col.levels=col.levels,
                              sort=list(col=1, val=row_levels),
                              pct.digits=digits, colvar.together=TRUE, empty.cols=TRUE)
        }
    }
    if (is.null(tab))
        tab <- tabEpiPrev(data, imput, rowvar=xvar, colvar=yvar, riskvar=riskvar,
                          margins=TRUE, col.levels=col.levels, sort=sort,
                          pct.digits=digits, colvar.together=TRUE, empty.cols=TRUE)

    if(!is.null(tab)){
        tab <- merge(tab, col1Lang, by.x=xvar, by.y="Label", all.x=TRUE, sort=FALSE)
        if (is.null(row_levels)){       # reorder Col1 by default order
            if (tab[nrow(tab)-1, ncol(tab)] %in% c("NA", "N/A", NA)){
                rorder <- c(order(tab[1:(nrow(tab)-2), toupper(lang)]),
                            (nrow(tab)-1):nrow(tab))
                tab <- tab[rorder, ]
            }
        }
        if (tolower(lang)=='cn') tab[, xvar] <- tab$CN
        else tab[, xvar] <- tab$EN
        tab <- tab[, 1:(ncol(tab)-2)]
        return(tab)
    }else{
        return(NULL)
    }
}

##---------[[Restructure Summary Imput Data]]-------------
#' Restructure Mannually Imput Summarized Data
#'
#' Work with \code{\link{tabSource}}. The summarized data extracted from CheckupRawData
#' system is not working directly. You have to restructure it with this function.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param imput Data frame. Summary input of checkup results
#' @param xvar Variable name to be crosstabed to x.
#' @param yvar Variable name to be crosstabed to y.
#' @param riskvar Risk variable name.
#' @param librisk Data frame of the risk pool.
#'
#' @return A restructured data frame.
#' @export
#' @importFrom reshape2 melt
#'
#' @examples
#'
restrImput <- function(imput, xvar, yvar, riskvar, librisk){
    if (!is.null(imput)){
        # if (riskvar %in% names(imput))
        imput <- imput[imput$RiskL2ID %in%
                           librisk$RiskL2ID[librisk$RiskL2Code %in% riskvar],
                       c(xvar, yvar, "NPcpt", "NAbn")]
        if (nrow(imput)>0){
            imput$`0` <- imput$NPcpt - imput$NAbn
            imput$`1` <- imput$NAbn
            imput <- melt(imput[, c(xvar, yvar, '0', '1')], id.var=c(xvar, yvar))
            names(imput) <- c(xvar, yvar, riskvar, "Freq")
        }else{
            imput <- NULL
        }
    }
    return(imput)
}
