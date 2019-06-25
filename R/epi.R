#------------[Epidemiologic tables and charts]-----------------

##------------[Prevalence, return a crosstab]---------
#' Epidemiologic Prevalence Table (data frame)
#'
#' Draw an prevalence table with raw data combining with summarized data, in
#' addition with table controlling parameters.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param data Raw dataset
#' @param imput Summarized data set
#' @param rowvar The variable used as rows
#' @param colvar The variable used as columns
#' @param riskvar The risk variable
#' @param neg Define which value represents negative. Default is 0.
#' @param margins Refer to margins argument in \code{reshape2::dcast}
#' @param lang language, "cn" or "en".
#' @param cols A vector. Which elements are to be included. Default is
#' \code{c("npos", "ntotal", "percent")}: number of positives, number of total, percent.
#' @param pct.digits Number of digits shown in percentages. If \code{pct.digits}==NULL,
#'  then the function does not coerce prevalence rates to percents.
#' @param colvar.together Logical.
#' @param empty.cols Logical. If TRUE, empty columns will be added in the table
#' to separate columns sectors.
#' @param na.string A named vector \code{c(cn=..., en=...)}. How to present NAs
#' when \code{lang} is 'cn' or 'en'. Default \code{.pkgPara()$na.string}.
#' @param sort Sorting standard. If \code{sort}=list(col=1, desc=FALSE), then the
#' function sorts the data ascendingly by Col1. If \code{sort}=list(col=3,
#' desc=TRUE), the function sorts the data descendingly by Col3. You can also
#' directly assign a vector, e.g. list(col=1, val=1:4)
#' @param col.levels A vector re-defining the factor level of the variable to be
#' put as the column. Default null.
#' @param ...
#'
#' @return A data frame restructured based on you requirements.
#' @export
#' @importFrom reshape2 dcast melt
#' @importFrom stringr str_replace_all str_detect
#'
#' @seealso \code{\link{dcast}}
#' @examples
#'
tabEpiPrev <- function(data, imput=NULL, rowvar, colvar, riskvar, neg="0",
                       margins=NULL, lang=c("cn", 'en'),
                       cols=c("npos", "ntotal", "percent"), pct.digits=0,
                       colvar.together=FALSE, empty.cols=FALSE,
                       na.string=.pkgPara()$na.string,
                       sort=list(col=1, desc=FALSE), col.levels=NULL, ...){
    # if pct.digits==NULL, then do not coerce prevalence rates to percents
    # sort=list(col=1, desc=FALSE) means ascending sort by Col1,
    ## while list(col=3, desc=TRUE) means descending sort by Col3
    ## You can also directly assign a vector, e.g. list(col=1, val=1:4)
    lang <- match.arg(lang)
    na.string <- if (lang %in% names(na.string)) na.string[lang] else "N/A"
    if (!all(rowvar %in% names(data))) stop("row variable not found!")
    if (!all(colvar %in% names(data))) stop("col variable not found!")
    if (!all(riskvar %in% names(data))) stop("risk variable not found!")
    if (length(cols)==0) stop("you have to output at least one col")
    data <- data[, c(rowvar, colvar, riskvar)]
    if (!is.null(imput)) if (nrow(imput)==0) imput <- NULL

    if (!is.null(col.levels)){
        if (nlevels(as.factor(data[, colvar]))==length(col.levels) &&
            all(col.levels %in% levels(as.factor(data[, colvar])))){
            data[, colvar] <- factor(data[, colvar], levels=col.levels, exclude=NULL)
        }
    }
    data[is.na(data[, riskvar]), riskvar] <- na.string
    dt <- as.data.frame(ftable(data, row.vars=rowvar, col.vars=c(colvar, riskvar)))
    if (nrow(dt)>0){
        dt <- dt[dt[, riskvar] %in% c(na.string, "0", "1", "2", "3"),]
        if (!is.null(imput)){       # merge imput aggregated numbers
            if (!all(names(imput) %in% names(dt)))
                stop("colnames of `imput` are not equal to `data`!")
            #imput[!imput[, rowvar] %in% dt[, rowvar], rowvar] <- na.string0
            #imput[!imput[, colvar] %in% dt[, colvar], colvar] <- na.string
            imput[, rowvar] <- factor(imput[, rowvar])
            imput[, colvar] <- factor(imput[, colvar])
            levels(dt[, rowvar]) <- c(
                levels(dt[, rowvar]),
                levels(imput[, rowvar])[!levels(imput[, rowvar]) %in% levels(dt[, rowvar])])
            levels(dt[, colvar]) <- c(
                levels(dt[, colvar]),
                levels(imput[, colvar])[!levels(imput[, colvar]) %in% levels(dt[, colvar])])
            levels(dt[, riskvar]) <- c(
                levels(dt[, riskvar]),
                levels(imput[, riskvar])[!levels(imput[, riskvar]) %in% levels(dt[, riskvar])])

            dt <- merge(dt, imput, by=c(rowvar, colvar, riskvar), all=TRUE, order=FALSE)
            if (!any(is.na(as.numeric(as.character(levels(dt[, colvar])))))){
                dt[, colvar] <- as.numeric(as.character(dt[, colvar]))
            }
            dt$Freq <- rowSums(dt[, c("Freq.x", "Freq.y")], na.rm=TRUE)
            dt <- dcast(dt, eval(parse(text=paste0(rowvar, "+", colvar, "+", riskvar, "~."))),
                        sum, value.var="Freq")
            names(dt) <- c(rowvar, colvar, riskvar, "Freq")

        }
        dt.expand <- expand.grid(levels(as.factor(dt[, rowvar])),
                                 levels(as.factor(dt[, colvar])),
                                 c(na.string, "0", "1", "2", "3"),0)
        names(dt.expand) <- names(dt)
        dt <- rbind(dt, dt.expand)
        dtpos <- try(dcast(dt, formula=eval(parse(text=paste0(
            paste(rowvar, collapse="+"), "~", paste(colvar, collapse="+")))),
            sum, subset=.(! eval(parse(text=riskvar)) %in% c(neg, na.string)),
            value.var="Freq", margins=margins), silent=TRUE)
        names(dtpos) <-
            c(names(dtpos)[1], paste0("n", names(dtpos)[2:length(names(dtpos))]))
        dttot <- try(dcast(dt, formula=eval(parse(text=paste0(
            paste(rowvar, collapse="+"), "~", paste(colvar, collapse="+")))),
            sum, subset=.(! eval(parse(text=riskvar)) %in% na.string),
            value.var="Freq", margins=margins), silent=TRUE)
        names(dttot) <- c(names(dttot)[1], paste0("N",
                                                  names(dttot)[2:length(names(dttot))]))
        dtpct <- cbind(dtpos[,1], dtpos[ ,2:ncol(dtpos)]/dttot[,2:ncol(dttot)])
        names(dtpct) <- names(dtpos)
        names(dtpct) <- str_replace_all(names(dtpct), "^n", "%")
        if (!is.null(pct.digits)){
            for (i in 2:ncol(dtpct)){
                dtpct[, i] <- convNum2Pct(dtpct[, i], pct.digits)
            }
        }
        if (empty.cols) {
            if (!colvar.together) {
                dtpos$Empty <- dttot$Empty <- ""
            }else {
                nlvl <- sum(str_detect(names(dtpct), "^%"))
                dtempty <- matrix(rep("", (nlvl-1)*nrow(dtpct)), nrow=nrow(dtpct))
                dtempty <- as.data.frame(dtempty)
                names(dtempty) <- str_replace_all(
                    names(dtpct)[2:(ncol(dtpct)-1)], "%", "_")
            }
        }
        if ("npos" %in% cols) output <- dtpos
        if ("ntotal" %in% cols) {
            if (ncol(output)>0) {
                output <- cbind(output, dttot[,2:ncol(dttot)])
            }else{
                output <- cbind(output, dttot)
            }
        }
        if ("percent" %in% cols) {
            if (ncol(output)>0) {
                output <- cbind(output, dtpct[ ,2:ncol(dtpct)])
            }else{
                output <- cbind(output, dtpct)
            }
        }
        if (colvar.together) {
            if (empty.cols) output <- cbind(output, dtempty)
            lvls <- str_replace_all(names(dtpct), "^%(.+$)", "\\1")
            vorder <- 1
            for (i in lvls){
                str <- which(str_detect(names(output), paste0(".{1}", i)))
                vorder <- c(vorder, str)
            }
            output <- output[, vorder]
        }
        if (!is.null(sort)){               # sort df based on sort parameter
            if (!is.null(sort[['val']])){  # sort by exact vector
                if (all(is.numeric(sort[['val']]))){  # numeric order
                    rorder <- sort[['val']]
                    if (!nrow(output) %in% sort[['val']])
                        rorder <- c(sort[['val']], nrow(output))
                    output <- output[rorder,]
                }else{                                # re-order by texts
                    if (all(sort[['val']] %in% output[ , sort[['col']]])){
                        rorder <- sort[['val']]
                        if (!output[nrow(output), sort[['col']]] %in% rorder)
                            # add 'total' to sort texts
                            rorder <- c(rorder, as.character(output[nrow(output),
                                                                   sort[['col']]]))
                        output[, sort[['col']]] <-
                            factor(as.character(output[, sort[['col']]]),
                                   levels=rorder)
                        output <- output[order(output[, sort[['col']]]),]
                    }
                }

            }else{  # sort asc/desc
                output_sub <- output[-nrow(output),]
                if (any(str_detect(output[, sort[['col']]], "\\d%"))){
                    output <- rbind(output_sub[order(convconvNum2Pct2Num(output_sub[, sort[['col']]]),
                                                     decreasing=sort[['desc']]),],
                                    output[nrow(output),])
                }else{
                    if (is.character(output[, sort[['col']]])){
                        rorder <- order(as.character(output_sub[, sort[['col']]]),
                                        decreasing=sort[['desc']])
                        output <- rbind(output_sub[rorder,],
                                        output[nrow(output),])
                    }else{
                        output <- rbind(output_sub[order(output_sub[, sort[['col']]],
                                                         decreasing=sort[['desc']]),],
                                        output[nrow(output),])
                    }
                }
            }
        }
        row.names(output) <- seq_len(nrow(output))
        output
    }else{
        NULL
    }
}


