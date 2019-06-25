##---------[[Table: City x Gender]]-------------
#' City-spec Table in Auto Reporting
#'
#' Only used for automated reporting. Get an age-gender distribution.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param data Raw data frame.
#' @param imput Summarized data frame.
#' @param riskvar Variable name of the risk.
#' @param librisk Data frame of the risk pool.
#' @param tblCity Data frame of the city pool.
#' @param tblGender Data frame of gender pool.
#' @param curYrs String. The year range of this plot.
#' @param lang 'cn' or 'en'.
#' @param sort A list. Default \code{list(col=1, desc=FALSE)}, meaning sort the
#' results ascendingly by Col1.
#' @param digits Number of digits of the results. Default 1.
#' @param dynamic Interactive JavaScript charts/tables or not. If TRUE, call
#' \pkg{DT}::\code{\link{datatable}} to make the object. Default FALSE.
#' @param ...
#'
#' @return A \code{knitr} or \code{DT} object.
#' @importFrom DT datatable
#' @importFrom stringr str_to_title
#' @export
#'
#' @seealso \code{\link{knitr}}, \code{\link{datatable}}
#' @examples
#'
tabCity <- function(data, imput=NULL, riskvar, librisk, tblCity, tblGender, curYrs,
                    sort=list(col=1, desc=FALSE), digits=1,
                    lang=c('cn', 'en'), dynamic=FALSE,...){
    lang <- match.arg(lang)
    tblCityGender <- tabSource(data, imput, riskvar, "City", "Gender", tblRisk=librisk,
                               tblBU=NULL, tblGender=tblGender, tblCity=tblCity,
                               lang=lang, col.levels=c("M", "F"), sort=sort)
    if (!is.null(tblCityGender)){
        riskname <- librisk[librisk$RiskL2Code==riskvar, paste0("RiskL2", toupper(lang))]
        if (tolower(lang)=='cn'){
            if (ncol(tblCityGender)==12){
                head <- matrix(c(c("\u57CE\u5E02", "\u7537", NA, NA, NA, "\u5973", NA, NA, NA,
                                   "\u5408\u8BA1", NA, NA),
                                 c(NA, "\u5F02\u5E38\u6570", "\u53C2\u68C0\u6570",
                                   "\u68C0\u51FA\u7387", NA, "\u5F02\u5E38\u6570",
                                   "\u53C2\u68C0\u6570",
                                   "\u68C0\u51FA\u7387", NA, "\u5F02\u5E38\u6570",
                                   "\u53C2\u68C0\u6570", "\u68C0\u51FA\u7387")),
                               byrow=TRUE, nrow=2)
            }else if (ncol(tblCityGender)==16){
                head <- matrix(c(c("\u57CE\u5E02", "\u7537", NA, NA, NA, "\u5973", NA, NA, NA,
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
            caption=paste0("\u8868: ", riskname, "\u7537\u5973\u5F02\u5E38\u68C0",
                           "\u51FA\u7387\u57CE\u5E02\u5206\u5E03 (", curYrs, ")")
        }else if (tolower(lang)=='en'){
            if (ncol(tblCityGender)==12){
                head <- matrix(c(c("City", "Male", NA, NA, NA, "Female", NA, NA, NA,
                                   "Total", NA, NA),
                                 c(NA, "Abnormal", "All", "Percent", NA,
                                   "Abnormal", "All", "Percent",
                                   NA, "Abnormal", "All", "Percent")),
                               byrow=TRUE, nrow=2)
            }else if (ncol(tblCityGender)==16){
                head <- matrix(c(c("City", "Male", NA, NA, NA, "Female", NA, NA, NA, "N/A",
                                   NA, NA, NA, "Total", NA, NA),
                                 c(NA, "Abnormal", "All", "Percent", NA, "Abnormal",
                                   "All", "Percent",
                                   NA, "Abnormal", "All", "Percent", NA, "Abnormal",
                                   "All", "Percent")),
                               byrow=TRUE, nrow=2)
            }
            caption=paste0("Table: ", str_to_title(riskname),
                           " Prevalence by City (", curYrs, ")")
        }
        if (dynamic){

            tblCityGender[, ] <- enc2native(as.character(tblCityGender[, ]))
            head <- gsub("Percent", "Rate", gsub("Abnormal", "Abn'l", head))
            datatable(tblCityGender, style='bootstrap', class="compact", rownames=FALSE,
                      container=sub('<table(.+?</thead>).+?$', '<table\\1</table>',
                                    reheadHTMLTable(tblCityGender, head, align=c('l', 'c'))),
                      caption=caption, filter=list(position='top', clear=TRUE)) %>%
                formatStyle(2:ncol(tblCityGender), textAlign="center")
        }else{
            print(reheadHTMLTable(tblCityGender, head, footRows=1, caption=caption))
        }
    }else{
        return("")
    }
}

##---------[[Fig: City x Gender]]-------------
#' City-spec Chart in Auto Reporting
#'
#' Only used for automated reporting. Get a city distribution.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param data Raw data frame.
#' @param imput Summarized data frame.
#' @param riskvar Variable name of the risk.
#' @param librisk Data frame of the risk pool.
#' @param curYrs String. The year range of this plot.
#' @param lang 'cn' or 'en'.
#' @param sort A list. Default \code{list(col=1, desc=FALSE)}, meaning sort the
#' results ascendingly by Col1.
#' @param digits Number of digits of the results. Default 1.
#' @param Cities Data frame of the city pool.
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
#' @importFrom stringr str_to_title
#' @export
#'
#' @seealso \code{\link{ggplot2}} \code{\link{echartR}}
#' @examples
#'
figCity <- function(data, imput=NULL, riskvar, librisk, curYrs,
                    sort=list(col=1, desc=FALSE), digits, Cities=NULL,
                    lang=c('cn', 'en'), na.string=getOption("na.string"),
                    altfont='Microsoft YaHei', dynamic=FALSE,
...){
    lang <- match.arg(lang)
    na.string <- if (lang %in% names(na.string)) na.string[lang] else "NA"
    if (!is.null(imput)) imput <- restrImput(imput, "City", "Gender", riskvar, librisk)
    riskname <- librisk[librisk$RiskL2Code==riskvar, paste0("RiskL2", toupper(lang))]
    benchmark <- benchmark[
        which(benchmark$RiskL2ID==librisk$RiskL2ID[librisk$RiskL2Code==riskvar]),
        ]
    benchmark <- benchmark[which(benchmark$PubYear==max(benchmark$PubYear)),]
    valRef <- benchmark$Benchmark
    strRef <- list(cn=benchmark$RefCN, en=benchmark$RefEN)
    tblCityGender <- tabEpiPrev(
        data, imput, rowvar="City", colvar="Gender",
        riskvar=riskvar, margins=TRUE, col.levels=c("M", "F"),
        colvar.together=TRUE, empty.cols=TRUE, sort=sort)
    dfCityGender <- tabEpiPrev(
        data, imput, rowvar="City", colvar="Gender",
        riskvar=riskvar, margins=TRUE, col.levels=c("M", "F"),
        colvar.together=TRUE, pct.digits=NULL, sort=sort)
    if (!is.null(tblCityGender)){
        totalValue <- dfCityGender[nrow(dfCityGender), "%(all)"]
        totalValue <- as.numeric(totalValue)
        markLine <- data.frame(
            name1=ifelse(tolower(lang) == 'cn', '\u5408\u8BA1', 'Total'),
            name2=ifelse(tolower(lang) == 'cn', '\u68C0\u51FA\u7387', 'Rate'),
            value=t(totalValue), xAxis1=-1, yAxis1=t(totalValue),
            xAxis2=length(unique(dfCityGender$City)),
            yAxis2=t(totalValue), stringsAsFactors = FALSE)
        if (nrow(benchmark)>0)
            markLine <- rbind(markLine, data.frame(
                name1=ifelse(tolower(lang)=="cn", '\u5168\u56FD', "China"),
                name2=ifelse(tolower(lang)=="cn", '\u68C0\u51FA\u7387', "Rate"),
                value=valRef, xAxis1=-1, yAxis1=valRef,
                xAxis2=length(unique(dfCityGender$City)), yAxis2=valRef,
                stringsAsFactors = FALSE))

        if (tolower(lang)=='cn'){
            dfCityGender <- dfCityGender[1:(nrow(dfCityGender)-1),
                                         c(1, ncol(dfCityGender))]
            #dfCityGender[, ] <- as.numeric(convconvNum2Pct2Num(dfCityGender[, ]))
            names(dfCityGender) <- c("City", "Percent")
            caption <- paste0("\u56FE: ", riskname, "\u68C0\u51FA\u7387\u7684",
                              "\u57CE\u5E02\u5206\u5E03 (", curYrs, ")")
            hline <- data.frame(label=c('\u5408\u8BA1'), linetype='solid', value=totalValue)
            if (nrow(benchmark)>0)
                hline <- rbind(hline, data.frame(label='\u5168\u56FD', linetype='solid',
                                                value=valRef))

            if (dynamic){
                echartgg <- echartR(data=dfCityGender, x=City, y=Percent, type='vbar') %>%
                setTheme(palette=getHexPal(getOption("init.pal"))[c(1, 4, 3)]) %>%
                setTitle(caption) %>%
                setYAxis(axisLine=list(lineStyle=list(color="darkgray", width=0)),
                         name="\u6BD4\u4F8B", max=ifelse(
                             max(dfCityeGender$Percent, na.rm=TRUE)==1, 1.2, 1)) %>%
                setXAxis(axisLine=list(lineStyle=list(color="darkgray")),
                         name="\u57CE\u5E02") %>%
                setToolbox(TRUE, lang, pos=3) %>% addML(data=markLine)
            }else{
                gg <- chartColumn(
                    data=dfCityGender, xvar="City", yvar="Percent", title=caption,
                    xlab="\u57CE\u5E02", ylab="\u6BD4\u4F8B",
                    annotate=paste0("percent", digits),
                    palette=getHexPal(getOption("init.pal"))[c(1, 4, 3)],
                    hline=hline, alt.font=altfont, plot.background='grey95')
            }
        }else if (tolower(lang)=='en'){
            dfCityGender <- merge(dfCityGender, Cities[, c("CityAlias", "CityEN")],
                                  by.x="City", by.y="CityAlias", all.x=TRUE,
                                  sort=FALSE)
            dfCityGender$City <- dfCityGender$CityEN
            dfCityGender <- dfCityGender[1:(nrow(dfCityGender)-1),
                                         c(1, ncol(dfCityGender)-1)]
            #dfCityGender[, ] <- as.numeric(convNum2Pct2Num(dfCityGender[, ]))
            names(dfCityGender) <- c("City", "Percent")
            caption <- paste0("Fig: ", str_to_title(riskname), " By City (", curYrs, ")")
            hline <- data.frame(label=c('Total'), linetype='solid', value=totalValue)
            if (nrow(benchmark)>0)
                hline <- rbind(hline, data.frame(label='China', linetype='solid',
                                                value=valRef))

            if (dynamic){
                echartgg <- echartR(data=dfCityGender, x=City, y=Percent, type='vbar') %>%
                setTheme(palette=getHexPal(getOption("init.pal"))[-2]) %>%
                setTitle(caption) %>%
                setYAxis(axisLine=list(lineStyle=list(color="darkgray", width=0)),
                         name="Percent", max=ifelse(
                             max(dfCityGender$Percent, na.rm=TRUE)==1, 1.2, 1)) %>%
                setYAxis(axisLine=list(lineStyle=list(color="darkgray")),
                         name="City") %>%
                setToolbox(TRUE, lang, pos=3) %>% addML(data=markLine)
            }else{
                gg <- chartColumn(
                    data=dfCityGender, xvar="City", yvar="Percent", title=caption,
                    xlab="City", ylab="Percent",
                    annotate=paste0("percent", digits),
                    palette=getHexPal(getOption("init.pal"))[c(1, 4, 3)],
                    hline=hline, alt.font=altfont, plot.background='grey95')
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
                echartgg$x$tooltip[['formatter']]=JS(JStooltip('bar_mkline_pct', digits))
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
