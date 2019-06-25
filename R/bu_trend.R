##---------[[Table: BU x Pjt Year]]-------------
#' BU Trend Table in Auto Reporting
#'
#' Only used for automated reporting. Get an age-gender distribution.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param data Raw data frame.
#' @param imput Summarized data frame.
#' @param riskvar Variable name of the risk.
#' @param librisk Data frame of the risk pool.
#' @param tblBU Data frame of the BU pool.
#' @param tblGender Data frame of gender pool.
#' @param lang 'cn' or 'en'.
#' @param sort A list. Default \code{list(col=1, desc=FALSE)}, meaning sort the
#' results ascendingly by Col1.
#' @param digits Number of digits of the results. Default 1.
#' @param dynamic Interactive JavaScript charts/tables or not. If TRUE, call
#' \pkg{DT}::\code{\link{datatable}} to make the object. Default FALSE.
#' @param ...
#'
#' @return A \code{knitr} or \code{DT} object.
#' @export
#' @importFrom DT datatable
#' @importFrom stringr str_to_title
#' @seealso \code{\link{knitr}}  \code{\link{datatable}}
#'
#' @examples
#'
tabTrendBU <- function(data, imput=NULL, riskvar, librisk, tblBU, tblGender,
                       sort=list(col=1, desc=FALSE), digits=1,
                       lang=c('cn', 'en'), dynamic=FALSE, ...){
    lang <- match.arg(lang)
    riskname <- librisk[librisk$RiskL2Code==riskvar, paste0("RiskL2", toupper(lang))]
    tblTrend <- tabSource(data, imput, riskvar, "BUID", "ProjectYear", tblRisk=librisk,
                          tblBU=tblBU, tblGender=tblGender, tblCity=NULL,
                          lang=lang, col.levels=NULL, sort=sort)
    if (!is.null(tblTrend)){
        tblTrend <- tblTrend[, seq_len(ncol(tblTrend)-4)]
        if(!is.null(imput)) imput <- imput[imput$RiskL2ID %in%
                                               librisk$RiskL2ID[librisk$RiskL2Code %in% riskvar],]
        pjtyr <- levels(factor(c(unique(data$ProjectYear),
                                 unique(imput$ProjectYear))))
        piece <- as.vector(matrix(c(pjtyr, rep(NA, 3*length(pjtyr))),
                                  byrow=TRUE, nrow=4))
        piece <- piece[seq_len(length(piece)-1)]
        if (tolower(lang)=='cn'){
            head <- matrix(c(c("\u90E8\u95E8", piece),
                             rep(c(NA, "\u5F02\u5E38\u6570", "\u53C2\u68C0\u6570",
                                   "\u68C0\u51FA\u7387"), length(pjtyr))),
                           byrow=TRUE, nrow=2)
            caption=paste0("\u8868: ", riskname, "\u68C0\u51FA\u7387\u90E8\u95E8",
                           "\u5206\u5E03\u7684\u65F6\u5E8F\u8D8B\u52BF")
        }else if (tolower(lang)=='en'){
            head <- matrix(c(c("BU", piece),
                             rep(c(NA, "Abn'l", "All", "Rate"), length(pjtyr))),
                           byrow=TRUE, nrow=2)
            caption <- paste0("Table: Time Trend of ", str_to_title(riskname),
                              " Prevalence by BU")
        }
        if (dynamic){
            # loadPkg("DT")
            tblTrend[, ] <- enc2native(as.character(tblTrend[, ]))
            datatable(tblTrend, style='bootstrap', class="compact", rownames=FALSE,
                      container=sub('<table(.+?</thead>).+?$', '<table\\1</table>',
                                    reheadHTMLTable(tblTrend, head, align=c('l', 'c'))),
                      caption=caption, filter=list(position='top', clear=TRUE)) %>%
                formatStyle(2:ncol(tblTrend), textAlign="center")
        }else{
            print(reheadHTMLTable(tblTrend, head, footRows=1, caption=caption))
        }
    }else{
        return("")
    }
}

##---------[[Fig: BU x Pjt Year]]-------------
#' BU Trend Chart in Auto Reporting
#'
#' Only used for automated reporting. Get BU trend distribution.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param data Raw data frame.
#' @param imput Summarized data frame.
#' @param riskvar Variable name of the risk.
#' @param librisk Data frame of the risk pool.
#' @param tblBU Data frame of the BU pool.
#' @param lang 'cn' or 'en'.
#' @param sort A list. Default \code{list(col=1, desc=FALSE)}, meaning sort the
#' results ascendingly by Col1.
#' @param digits Number of digits of the results. Default 1.
#' @param na.string A named vector \code{c(cn=..., en=...)}. How to present NAs.
#' Default \code{getOption("na.string")}. You can define yours.
#' @param altfont Alternative font name. Default 'Microsoft YaHei'.
#' @param dynamic Interactive JavaScript charts/tables or not. If TRUE, call
#' \pkg{recharts}::\code{\link{echartR}} to make the object. Default FALSE.
#' @param ...
#'
#' @return A \code{ggplot2} object or \code{echarts} object.
#' @import recharts
#' @import ggplot2
#' @importFrom stringr str_to_title str_detect str_replace_all
#' @export
#' @seealso \code{\link{ggplot2}}  \code{\link{echartR}}
#' @examples
#'
figTrendBU <- function(data, imput=NULL, riskvar, tblBU, librisk,
                       sort=list(col=1, desc=FALSE), digits, lang=c('cn', 'en'),
                       na.string=getOption("na.string"),
                       altfont='Microsoft YaHei', dynamic=FALSE,...){
    lang <- match.arg(lang)
    na.string <- if (lang %in% names(na.string)) na.string[lang] else "NA"
    riskname <- librisk[librisk$RiskL2Code==riskvar, paste0("RiskL2", toupper(lang))]
    benchmark <- benchmark[
        which(benchmark$RiskL2ID==librisk$RiskL2ID[librisk$RiskL2Code==riskvar]),
        ]
    benchmark <- benchmark[which(benchmark$PubYear==max(benchmark$PubYear)),]
    valRef <- benchmark$Benchmark
    strRef <- list(cn=benchmark$RefCN, en=benchmark$RefEN)
    if (!is.null(imput))
        imput <- restrImput(imput, "BUID", "ProjectYear", riskvar, librisk)
    dfTrend <- tabEpiPrev(
        data, imput, rowvar="BUID", colvar="ProjectYear",
        riskvar=riskvar, margins=TRUE, col.levels=c("M", "F"),
        colvar.together=TRUE, pct.digits=NULL, sort=sort)
    if (!is.null(dfTrend)){
        BUs <- rbind(tblBU[tblBU$BUID %in% dfTrend$BUID,
                           c("BUID", "BUCNCode", "BUENCode")],
                     c("(all)", "\u5408\u8BA1", "Total"))
        dfTrend <- merge(dfTrend, BUs, by="BUID", all.x=TRUE, sort=FALSE)
        totalValue <- dfTrend[nrow(dfTrend), str_detect(names(dfTrend), '^%[^\\(]')]

        names(totalValue) <- str_replace_all(names(totalValue), "%", "")
        markLine <- as.data.frame(
            matrix(rep(c(switch(tolower(lang), cn=c('\u5408\u8BA1', '\u68C0\u51FA\u7387'),
                                en=c('Total', 'Rate'))),
                       length(totalValue)), byrow=TRUE, ncol=2),
            stringsAsFactors=FALSE
        )
        names(markLine) <- c("name1", "name2")
        markLine$value <- markLine$yAxis1 <- markLine$yAxis2 <- as.numeric(t(totalValue))
        markLine$xAxis1 <- -1
        markLine$xAxis2 <- length(unique(dfTrend$BUID))
        markLine$series <- names(totalValue)
        markLine <- markLine[, c("name1", "name2", "value", "xAxis1", "yAxis1",
                                "xAxis2", "yAxis2", "series")]
        if (tolower(lang)=='cn'){
            dfTrend$BUID <- dfTrend$BUCNCode
            dfTrend <- dfTrend[, 1:(ncol(dfTrend)-2)]
            dfTrend <- dfTrend[, str_detect(names(dfTrend), "^(BU|%).+")]
            names(dfTrend) <- str_replace_all(names(dfTrend), "^%", "")
            dfTrend[, ] <- str_replace_all(dfTrend[, ], "\\(all\\)", "\u5408\u8BA1")
            dfTrend <- melt(dfTrend[, 1:ncol(dfTrend)-1], id.var="BUID")
            names(dfTrend) <- str_replace_all(str_replace_all(
                names(dfTrend), "variable", "Year"), "BUID", "BU")
            caption <- paste0("\u56FE: ", riskname, "\u68C0\u51FA\u7387\u7684\u65F6\u5E8F\u8D8B\u52BF")

            dfTrend <- dfTrend[dfTrend[, 'BU'] != '\u5408\u8BA1',]
            if(dynamic){
                echartgg <- echartR(data=dfTrend, x=BU, y=value, t=Year, type='vbar') %>%
                setTheme(palette=getHexPal(getOption("init.pal"))[-2]) %>%
                setToolbox(TRUE, lang, pos=3) %>%
                setTitle(caption) %>%
                setYAxis(axisLine=list(lineStyle=list(color="darkgray", width=0)),
                         name="\u68C0\u51FA\u7387") %>%
                setXAxis(axisLine=list(lineStyle=list(color="darkgray")),
                         name="\u90E8\u95E8") %>%
                setSymbols('none') %>% addML(data=markLine)
            }else{
                gg <- chartLine(
                    dfTrend, xvar="Year", yvar="value", gvar="BU",
                    title=caption, xlab="\u9879\u76EE\u5E74\u5EA6",
                    ylab="\u68C0\u51FA\u7387",
                    annotate=paste0("percent", digits), palette=getOption("init.pal"),
                    alt.font=altfont, plot.background='grey95')

            }
        }else if (tolower(lang)=='en'){
            dfTrend$BUID <- dfTrend$BUENCode
            dfTrend <- dfTrend[, 1:(ncol(dfTrend)-2)]
            dfTrend <- dfTrend[, str_detect(names(dfTrend), "^(BU|%).+")]
            names(dfTrend) <- str_replace_all(names(dfTrend), "^%", "")
            dfTrend[, ] <- str_replace_all(dfTrend[, ], "\\(all\\)", "Total")
            dfTrend <- melt(dfTrend[, 1:ncol(dfTrend)-1], id.var="BUID")
            names(dfTrend) <- str_replace_all(str_replace_all(
                names(dfTrend), "variable", "Year"), "BUID", "BU")
            caption <- paste0("Fig: Time Trend - ", str_to_title(riskname))

            dfTrend <- dfTrend[dfTrend$BU != 'Total',]
            if (dynamic){
                echartgg <- echartR(data=dfTrend, x=BU, y=value, t=Year, type='vbar') %>%
                setTheme(palette=getHexPal(getOption("init.pal"))[-2]) %>%
                setToolbox(TRUE, lang, pos=3) %>%
                setTitle(caption) %>%
                setYAxis(axisLine=list(lineStyle=list(color="darkgray", width=0)),
                         name="Percent") %>%
                setYAxis(axisLine=list(lineStyle=list(color="darkgray")), name="BU") %>%
                setSymbols('none') %>% addML(data=markLine)
            }else{
                gg <- chartLine(
                    dfTrend, xvar="Year", yvar="value", gvar="BU",
                    title=caption, xlab="Project Year", ylab="Percent",
                    annotate=paste0("percent", digits), palette=getOption("init.pal"),
                    alt.font=altfont, plot.background='grey95')
            }
        }
        if (dynamic) {
            if ('echarts' %in% class(echartgg)){
                echartgg$sizingPolicy$padding <- echartgg$sizingPolicy$viewer$padding <-
                    echartgg$sizingPolicy$browser$padding <- 200
                #echartgg$width <- 788
                #echartgg$height <- 420
                echartgg$align <- 'right'
                echartgg$x$options[[1]]$yAxis[['axisLabel']][['formatter']] <-
                    JS(JStooltip("axis_pct", 0))
                echartgg$x$options[[1]]$tooltip[['formatter']] <-
                    JS(JStooltip('bar_mkline_pct', digits))
                for (i in seq_len(length(echartgg$x$options))) {
                    for (j in seq_len(length(echartgg$x$options[[i]]$series))){
                        if (!is.null(echartgg$x$options[[i]]$series[[j]]$markLine)){
                            echartgg$x$options[[i]]$series[[j]]$markLine[['itemStyle']][['normal']] <-
                                list(label=list(
                                    show=TRUE,
                                    formatter=JS(JStooltip('mkline_pct', digits))
                                )
                            )
                        }
                    }
                }
                if ((length(echartgg$x$options[[1]]$xAxis$data)>10 &
                     tolower(lang)=='en') |
                    (length(echartgg$x$options[[1]]$xAxis$data)>15 &
                     tolower(lang)=='cn')) {
                    echartgg$x$options[[1]]$xAxis$data <-
                        paste0(c("", "\n"), echartgg$x$options[[1]]$xAxis$data, c("\n", ""))
                    echartgg$x$options[[1]]$xAxis$axisLabel <-
                        append(echartgg$x$options[[1]]$xAxis$axisLabel, list(interval=0))
                    echartgg$x$options[[1]]$grid$y2 <- 140
                }
            }
            try(return(echartgg), silent=TRUE)
        }else{
            try(return(gg), silent=TRUE)
        }
    }else{
        return("")
    }
}
