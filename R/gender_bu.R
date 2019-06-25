##---------[[Table: BU x Gender]]-------------
#' BU-spec Table in Auto Reporting
#'
#' Only used for automated reporting. Get a BU distribution.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param data Raw data frame.
#' @param imput Summarized data frame.
#' @param riskvar Variable name of the risk.
#' @param librisk Data frame of the risk pool.
#' @param tblGender Data frame of gender pool.
#' @param tblBU Data frame of BU pool.
#' @param tblCity Data frame of city pool.
#' @param lang 'cn' or 'en'.
#' @param digits Number of digits of the results. Default 1.
#' @param dynamic Interactive JavaScript charts/tables or not. If TRUE, call
#' \pkg{DT}::\code{\link{datatable}} to make the object. Default FALSE.
#' @param ...
#'
#' @return A data frame.
#' @importFrom DT datatable
#' @export
#' @seealso \code{\link{knitr}}  \code{\link{datatable}}
#' @examples
#'
tabBU <- function(
    data, imput=NULL, riskvar, librisk, tblBU, tblGender, tblCity, digits=1,
    lang=c('cn', 'en'), dynamic=FALSE,
...){
    lang <- match.arg(lang)
    tblBUGender <- tabSource(data, imput, riskvar, "BUID", "Gender", tblRisk=librisk,
                             tblBU=tblBU, tblGender=tblGender, tblCity=NULL, lang=lang,
                             col.levels=c("M", "F"), row_levels=NULL)
    riskname <- librisk[librisk$RiskL2Code==riskvar, paste0("RiskL2", toupper(lang))]
    if (tolower(lang)=='cn'){
        if (ncol(tblBUGender)==12){
            head <- matrix(c(c("\u90E8\u95E8", "\u7537", NA, NA, NA, "\u5973", NA, NA, NA,
                               "\u5408\u8BA1", NA, NA),
                             c(NA, "\u5F02\u5E38\u6570", "\u53C2\u68C0\u6570",
                               "\u68C0\u51FA\u7387", NA, "\u5F02\u5E38\u6570",
                               "\u53C2\u68C0\u6570",
                               "\u68C0\u51FA\u7387", NA, "\u5F02\u5E38\u6570",
                               "\u53C2\u68C0\u6570", "\u68C0\u51FA\u7387")),
                           byrow=TRUE, nrow=2)
        }else if (ncol(tblBUGender)==16){
            head <- matrix(c(c("\u90E8\u95E8", "\u7537", NA, NA, NA, "\u5973", NA, NA, NA,
                               "\u4E0D\u8BE6", NA, NA, NA,
                               "\u5408\u8BA1", NA, NA),
                             c(NA, "\u5F02\u5E38\u6570", "\u53C2\u68C0\u6570",
                               "\u68C0\u51FA\u7387", NA, "\u5F02\u5E38\u6570",
                               "\u53C2\u68C0\u6570", "\u68C0\u51FA\u7387", NA,
                               "\u5F02\u5E38\u6570", "\u53C2\u68C0\u6570",
                               "\u68C0\u51FA\u7387", NA, "\u5F02\u5E38\u6570",
                               "\u53C2\u68C0\u6570", "\u68C0\u51FA\u7387")),
                           byrow=TRUE, nrow=2)
        }
        caption=paste0("\u8868: ", riskname, "\u7537\u5973\u5F02\u5E38\u68C0\u51FA",
                       "\u7387\u90E8\u95E8\u5206\u5E03 (", curYrs, ")")
    }else if (tolower(lang)=='en'){
        if (ncol(tblBUGender)==12){
            head <- matrix(c(c(
                "BU", "Male", NA, NA, NA, "Female", NA, NA, NA, "Total", NA, NA),
                c(NA, "Abnormal", "All", "Percent", NA, "Abnormal", "All", "Percent",
                  NA, "Abnormal", "All", "Percent")),
                byrow=TRUE, nrow=2)
        }else if (ncol(tblBUGender)==16){
            head <- matrix(c(c(
                "BU", "Male", NA, NA, NA, "Female", NA, NA, NA, "N/A", NA, NA, NA,
                "Total", NA, NA),
                c(NA, "Abnormal", "All", "Percent", NA, "Abnormal", "All", "Percent",
                  NA, "Abnormal", "All", "Percent", NA, "Abnormal", "All", "Percent")),
                byrow=TRUE, nrow=2)
        }
        caption=paste0("Table: ", str_to_title(riskname),  " by BU (", curYrs, ")")
    }
    if (dynamic){
        tblBUGender[, ] <- enc2native(as.character(tblBUGender[, ]))
        head <- gsub("Percent", "Rate", gsub("Abnormal", "Abn'l", head))

        datatable(tblBUGender, style='bootstrap', class="compact", rownames=FALSE,
                  container=enc2native(str_replace(
                      reheadHTMLTable(tblBUGender, head, align=c('l', 'c')),
                      '<table(.+?</thead>).+?$', '<table\\1</table>')),
                  caption=caption, filter=list(position='top', clear=TRUE)) %>%
            formatStyle(2:ncol(tblBUGender), textAlign="center")
    }else{
        print(reheadHTMLTable(tblBUGender, head, footRows=1, caption=caption))
    }
}

##---------[[Fig: BU x Gender]]-------------
#' BU-spec Chart in Auto Reporting
#'
#' Only used for automated reporting. Get a BU distribution.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param data Raw data frame.
#' @param imput Summarized data frame.
#' @param riskvar Variable name of the risk.
#' @param librisk Data frame of the risk pool.
#' @param tblBU Data frame of the BU pool.
#' @param curYrs String. The year range of this plot.
#' @param lang 'cn' or 'en'.
#' @param sort A list. Default \code{list(col=1, desc=FALSE)}, meaning sort the
#' results ascendingly by Col1.
#' @param digits Number of digits of the results. Default 1.
#' @param na.string A named vector \code{c(cn=..., en=...)}. How to present NAs.
#' Default \code{getOption("na.string")}. You can define yours.
#' @param altfont Alternative font name. Default 'Microsoft YaHei'.
#' @param dynamic Interactive JavaScript charts/tables or not. If TRUE, call
#' \pkg{recharts}::\code{\link{echartR}} to make the
#' object. Default FALSE.
#' @param ...
#'
#' @return A \code{ggplot2} object or \code{echarts} object.
#' @import recharts
#' @import ggplot2
#' @importFrom stringr str_to_title
#' @export
#'
#' @seealso \code{\link{ggplot2}}  \code{\link{echartR}}
#' @examples
#'
#'
#'
figBU <- function(data, imput=NULL, riskvar, librisk, tblBU, curYrs,
                  sort=list(col=1, desc=FALSE), digits, lang=c('cn', 'en'),
                  na.string=getOption("na.string"),
                  altfont='Microsoft YaHei', dynamic=FALSE,
...){
    lang <- match.arg(lang)
    na.string <- if (lang %in% names(na.string)) na.string[lang] else "NA"
    riskname <- librisk[librisk$RiskL2Code==riskvar, paste0("RiskL2", toupper(lang))]
    benchmark <- benchmark[
        which(benchmark$RiskL2ID == librisk$RiskL2ID[librisk$RiskL2Code==riskvar]),
        ]
    benchmark <- benchmark[which(benchmark$PubYear==max(benchmark$PubYear)),]
    valRef <- benchmark$Benchmark
    strRef <- list(cn=benchmark$RefCN, en=benchmark$RefEN)
    if (!is.null(imput)) imput <- restrImput(imput, "BUID", "Gender", riskvar, librisk)
    tblBUGender <- tabEpiPrev(data, imput, rowvar="BUID", colvar="Gender",
                              riskvar=riskvar, margins=TRUE, col.levels=c("M", "F"),
                              colvar.together=TRUE, empty.cols=TRUE,
                              sort=sort)
    if (!is.null(tblBUGender)){
        BUs <- rbind(tblBU[tblBU$BUID %in% tblBUGender$BUID,
                           c("BUID", "BUCNCode", "BUENCode")],
                     c("(all)", "\u5408\u8BA1", "Total"))
        dfBUGender <- tabEpiPrev(data, imput, rowvar="BUID", colvar="Gender",
                                 riskvar=riskvar, margins=TRUE, sort=sort,
                                 colvar.together=TRUE, pct.digits=NULL)
        dfBUGender <- merge(dfBUGender, BUs, by="BUID", all.x=TRUE, sort=FALSE)
        totalValue <- dfBUGender[nrow(dfBUGender), "%(all)"]
        totalValue <- as.numeric(totalValue)

        markLine <- data.frame(
            name1=ifelse(tolower(lang)=='cn', '\u5408\u8BA1', 'Total'),
            name2=ifelse(tolower(lang)=='cn', '\u68C0\u51FA\u7387', 'Rate'),
            value= t(totalValue), xAxis1=-1, yAxis1=t(totalValue),
            xAxis2=length(unique(dfBUGender$BUID)), yAxis2=t(totalValue),
            stringsAsFactors=FALSE)

        if (nrow(benchmark)>0)
            markLine <- rbind(markLine, data.frame(
                name1=ifelse(tolower(lang)=='cn', '\u5168\u56FD', 'China'),
                name2=ifelse(tolower(lang)=='cn', '\u68C0\u51FA\u7387', 'Rate'),
                value=valRef, xAxis1=-1, yAxis1=valRef,
                xAxis2=length(unique(dfBUGender$BUID)), yAxis2=valRef,
                stringsAsFactors=FALSE))

        if (tolower(lang)=='cn'){
            dfBUGender$BUID <- dfBUGender$BUCNCode
            dfBUGender <- dfBUGender[1:(nrow(dfBUGender)-1), c(1, ncol(dfBUGender)-2)]
            #dfBUGender[, ] <- as.numeric(convconvNum2Pct2Num(dfBUGender[, ]))
            names(dfBUGender) <- c("BU", "Percent")
            caption <- paste0(
                "\u56FE: ", riskname, "\u68C0\u51FA\u7387\u7684\u90E8\u95E8\u5206\u5E03 (",
                curYrs, ")")
            hline <- data.frame(label=c('\u5408\u8BA1'), linetype='solid', value=totalValue)
            if (nrow(benchmark)>0)
                hline <- rbind(hline, data.frame(label='\u5168\u56FD', linetype='solid',
                                                value=valRef))
            if (dynamic){
                echartgg <- echartR(data=dfBUGender, x=BU, y=Percent, type='vbar') %>%
                setTheme(palette=getHexPal(getOption("init.pal"))[-2]) %>%
                setTitle(caption) %>%
                setXAxis(axisLine=list(lineStyle=list(color='darkgrey')), name="\u90E8\u95E8") %>%
                setYAxis(axisLine=list(lineStyle=list(color='darkgrey', width=0)),
                         name="\u6BD4\u4F8B", max=ifelse(
                             max(dfAgeGender$Percent, na.rm=TRUE)==1, 1.2, 1)) %>%
                setToolbox(TRUE, lang, pos=3) %>% addML(data=markLine)
            }else{
                gg <- chartColumn(
                    data=dfBUGender, xvar="BU", yvar="Percent",
                    title=caption, xlab="\u90E8\u95E8", ylab="\u6BD4\u4F8B",
                    annotate=paste0("percent", digits),
                    palette=getHexPal(getOption("init.pal"))[c(1, 4, 3)],
                    hline=hline, alt.font=altfont, plot.background='grey95')
            }
        }else if (tolower(lang)=='en'){
            dfBUGender$BUID <- dfBUGender$BUENCode
            dfBUGender <- dfBUGender[1:(nrow(dfBUGender)-1), c(1, ncol(dfBUGender)-2)]
            #dfBUGender[, ] <- as.numeric(convconvNum2Pct2Num(dfBUGender[, ]))
            names(dfBUGender) <- c("Business_Unit", "Percent")
            caption <- paste0("Fig: ", str_to_title(riskname), " by BU (", curYrs, ")")
            hline <- data.frame(label=c('Total'), linetype='solid', value=totalValue)
            if (nrow(benchmark)>0)
                hline <- rbind(hline, data.frame(label='China', linetype='solid',
                                                value=valRef))
            if (dynamic){
                echartgg <- echartR(
                    data=dfBUGender, x=Business_Unit, y=Percent, type='vbar') %>%
                setTheme(palette=getHexPal(getOption("init.pal"))[-2]) %>%
                setTitle(caption) %>%
                setYAxis(axisLine=list(lineStyle=list(color="darkgrey", width=0)),
                         name="Percent", max=ifelse(
                             max(dfAgeGender$Percent, na.rm=TRUE)==1, 1.2, 1)) %>%
                setXAxis(axisLine=list(lineStyle=list(color="darkgrey")), name="BU") %>%
                setToolbox(TRUE, lang, pos=3) %>% addML(data=markLine)
            }else{
                gg <- chartColumn(
                    data=dfBUGender, xvar="Business_Unit", yvar="Percent",
                    title=caption, xlab="BU", ylab="Percent",
                    annotate=paste0("percent", digits),
                    palette=getHexPal(getOption("init.pal"))[c(1, 4, 3)],
                    hline=hline,
                    alt.font=altfont, plot.background='grey95')
            }
        }
        if (dynamic){
            if ('echarts' %in% class(echartgg)){
                echartgg$sizingPolicy$padding <- echartgg$sizingPolicy$viewer$padding <-
                    echartgg$sizingPolicy$browser$padding <- c(0, 0, 0, 200)
                #echartgg$width <- 788
                #echartgg$height <- 420
                echartgg$align <- 'right'
                echartgg$x$yAxis[[1]][['axisLabel']][['formatter']] <-
                    JS(JStooltip("axis_pct", 0))
                echartgg$x$tooltip[['formatter']] <- JS(JStooltip('bar_mkline_pct', digits))
                for (i in seq_len(length(echartgg$x$series))) {
                    if (!is.null(echartgg$x$series[[i]]$markLine)){
                        echartgg$x$series[[i]]$markLine[['itemStyle']][['normal']] <-
                            list(label=list(
                                show=TRUE, formatter=JS(JStooltip('mkline_pct', digits)))
                            )
                    }
                }
                if ((length(echartgg$x$xAxis[[1]]$data)>10 & tolower(lang)=='en') |
                    (length(echartgg$x$xAxis[[1]]$data)>15 & tolower(lang)=='cn')) {
                    echartgg$x$xAxis[[1]]$data <-
                        paste0(c("", "\n"), echartgg$x$xAxis[[1]]$data, c("\n", ""))
                    echartgg$x$xAxis[[1]]$axisLabel <-
                        append(echartgg$x$xAxis[[1]]$axisLabel, list(interval=0))
                    echartgg$x$grid$y2 <- 80
                }
            }
            try(return(echartgg), silent=TRUE)
        }else{
            try(return(gg), silent=TRUE)
        }
    }else{
        return('')
    }
}

