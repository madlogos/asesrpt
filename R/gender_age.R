##---------[[Table: Age x Gender]]-------------
#' Age-Gender Table in Auto Reporting
#'
#' Only used for automated reporting. Get an age-gender distribution.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param data Raw data frame.
#' @param imput Summarized data frame.
#' @param riskvar Variable name of the risk.
#' @param librisk Data frame of the risk pool.
#' @param lang 'cn' or 'en'.
#' @param sort A list. Default \code{list(col=1, desc=FALSE)}, meaning sort the
#' results ascendingly by Col1.
#' @param digits Number of digits of the results. Default 1.
#' @param dynamic Interactive JavaScript charts/tables or not. Default FALSE.
#' @param ...
#'
#' @return A \code{knitr} or \code{DT} object.
#' @export
#' @importFrom DT datatable
#' @importFrom stringr str_replace str_to_title
#'
#' @seealso \code{\link{knitr}}, \code{DT}
#'
#' @examples
#'
tabAge <- function(data, imput=NULL, riskvar, librisk, lang=c('cn', 'en'),
                   sort=list(col=1, desc=FALSE), digits=1, dynamic=FALSE,
...){
    lang <- match.arg(lang)
    riskname <- librisk[librisk$RiskL2Code==riskvar, paste0("RiskL2", toupper(lang))]
    tabAgeGender <- tabSource(data, imput, riskvar, "AgeG", "Gender",
                              tblRisk=librisk, tblBU=NULL, tblCity=NULL, lang=lang,
                              col.levels=c("M", "F"), sort=sort)
    if (!is.null(tabAgeGender)){
        if (tolower(lang)=='cn'){
            if (ncol(tabAgeGender)==12){
                head <- matrix(c(c(
                    "\u{5E74}\u{9F84}\u{6BB5}", "\u{7537}", NA, NA, NA, "\u{5973}",
                    NA, NA, NA, "\u{5408}\u{8BA1}", NA, NA),
                    c(NA, "\u{5F02}\u{5E38}\u{6570}", "\u{53C2}\u{68C0}\u{6570}",
                      "\u{68C0}\u{51FA}\u{7387}", NA, "\u{5F02}\u{5E38}\u{6570}",
                      "\u{53C2}\u{68C0}\u{6570}",
                      "\u{68C0}\u{51FA}\u{7387}", NA, "\u{5F02}\u{5E38}\u{6570}",
                      "\u{53C2}\u{68C0}\u{6570}", "\u{68C0}\u{51FA}\u{7387}")),
                               byrow=TRUE, nrow=2)
            }else if (ncol(tabAgeGender)==16){
                head <- matrix(c(c(
                    "\u{5E74}\u{9F84}\u{6BB5}", "\u{7537}", NA, NA, NA, "\u{5973}", NA, NA, NA,
                    "\u{4E0D}\u{8BE6}", NA, NA, NA, "\u{5408}\u{8BA1}", NA, NA),
                    c(NA, "\u{5F02}\u{5E38}\u{6570}", "\u{53C2}\u{68C0}\u{6570}",
                      "\u{68C0}\u{51FA}\u{7387}", NA, "\u{5F02}\u{5E38}\u{6570}",
                      "\u{53C2}\u{68C0}\u{6570}", "\u{68C0}\u{51FA}\u{7387}", NA,
                      "\u{5F02}\u{5E38}\u{6570}", "\u{53C2}\u{68C0}\u{6570}",
                      "\u{68C0}\u{51FA}\u{7387}", NA, "\u{5F02}\u{5E38}\u{6570}",
                      "\u{53C2}\u{68C0}\u{6570}", "\u{68C0}\u{51FA}\u{7387}")),
                    byrow=TRUE, nrow=2)
            }
            caption=paste0("\u{8868}: ", riskname,
                           "\u{68C0}\u{51FA}\u{7387}\u{7684}\u{5E74}\u{9F84}",
                           "\u{6027}\u{522B}\u{5206}\u{5E03} (", curYrs, ")")
        }else if (tolower(lang)=='en'){
            if (ncol(tabAgeGender)==12){
                head <- matrix(c("Age Group", "Male", NA, NA, NA, "Female", NA, NA, NA, "Total",
                                 NA, NA, NA, "Abnormal", "All", "Percent", NA, "Abnormal", "All",
                                 "Percent", NA, "Abnormal", "All", "Percent"),
                               byrow=TRUE, nrow=2)
            }else if (ncol(tabAgeGender)==16){
                head <- matrix(c(c("Age Group", "Male", NA, NA, NA, "Female", NA, NA, NA, "N/A",
                                   NA, NA, NA, "Total", NA, NA),
                                 c(NA, "Abnormal", "All", "Percent", NA, "Abnormal",
                                   "All", "Percent", NA, "Abnormal", "All",
                                   "Percent", NA, "Abnormal",
                                   "All", "Percent")),
                               byrow=TRUE, nrow=2)
            }
            caption=paste0("Table: ", str_to_title(riskname),
                           " Prevalence by Age-group And Gender (", curYrs, ")")
        }
        if (dynamic){
            tabAgeGender[, ] <- enc2native(as.character(tabAgeGender[, ]))
            head <- gsub("Percent", "Rate", gsub("Abnormal", "Abn'l", head))
            datatable(tabAgeGender, style='bootstrap', class="compact",
                      rownames=FALSE, container=str_replace(
                          reheadHTMLTable(
                              tabAgeGender, head, align=c('l', 'c'), caption=caption),
                          '<table(.+?</thead>).+?$', '<table\\1</table>'),
                      filter=list(position='top', clear=TRUE)) %>%
                formatStyle(2:ncol(tabAgeGender), textAlign="center")
        }else{
            print(reheadHTMLTable(tabAgeGender, head, footRows=1, caption=caption))
        }
    }else{
        return("")
    }
}

##---------[[Fig: Age x Gender]]-------------
#' Age-Gender Chart in Auto Reporting
#'
#' Only used for automated reporting. Get an age-gender distribution.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param data Raw data frame.
#' @param imput Summarized data frame.
#' @param riskvar Variable name of the risk.
#' @param librisk Data frame of the risk pool.
#' @param tblGender Data frame of gender pool.
#' @param curYrs String. The year range of this plot.
#' @param lang 'cn' or 'en'.
#' @param sort A list. Default \code{list(col=1, desc=FALSE)}, meaning sort the
#' results ascendingly by Col1.
#' @param digits Number of digits of the results. Default 1.
#' @param na.string A named vector \code{c(cn=..., en=...)}. How to present NAs.
#' Default \code{getOption("na.string")}. You can define yours.
#' @param altfont Alternative font name. Default 'Microsoft YaHei'.
#' @param dynamic Interactive JavaScript charts/tables or not. If TRUE, call
#' \pkg{recharts}::\code{echartR} to make the object. Default FALSE.
#' @param ... ignore
#'
#' @return A \code{ggplot2} object or \code{echarts} object.
#' @import recharts
#' @import ggplot2
#' @export
#'
#' @seealso \code{\link{ggplot2}}  \code{\link{echartr}}
#' @examples
#'
figAge <- function(data, imput=NULL, riskvar, librisk, tblGender, curYrs,
                   sort=list(col=1, desc=FALSE), digits, lang=c('cn', 'en'),
                   na.string=getOption("na.string"),
                   altfont='Microsoft YaHei', dynamic=FALSE,...){
    lang <- match.arg(lang)
    na.string <- if (lang %in% names(na.string)) na.string[lang] else "NA"
    riskname <- librisk[librisk$RiskL2Code==riskvar, paste0("RiskL2", toupper(lang))]
    if (!is.null(imput))
        imput <- restrImput(imput, "AgeG", "Gender", riskvar, librisk)
    tblAgeGender <- tabEpiPrev(data, imput, rowvar="AgeG", colvar="Gender",
                               riskvar=riskvar, pct.digits=NULL, col.levels=c("M", "F"),
                               sort=sort)
    if (!is.null(tblAgeGender)){
        dfAgeGender <- tblAgeGender[, c(1,(ncol(tblAgeGender)-1):ncol(tblAgeGender))]
        names(dfAgeGender) <- str_replace_all(names(dfAgeGender), "^[nN%]", "")
        dfAgeGender <- melt(dfAgeGender, id.vars="AgeG")
        dfAgeGender <- merge(dfAgeGender, tblGender, by.x="variable", by.y="Gender",
                             sort=FALSE)
        dfAgeTotal <- data.frame(x=tblAgeGender$AgeG,
                                 y=rowSums(tblAgeGender[, 1:3], na.rm=TRUE)/
                                     rowSums(tblAgeGender[, 1:5], na.rm=TRUE))

        if (tolower(lang)=='cn'){
            dfAgeGender$GenderCN <-
                factor(dfAgeGender$GenderCN, levels=tblGender$GenderCN)
            caption <- paste0("\u56FE: ", riskname, "\u68C0\u51FA\u7387\u7684\u5E74",
                              "\u9F84\u6027\u522B\u5206\u5E03 (", curYrs, ")")
            if (dynamic){
                echartgg <- echartR(data=dfAgeGender, x=AgeG, y=value,
                                    series=GenderCN, type='vbar') %>%
                    setTheme(palette=getOption("init.pal")) %>%
                    setYAxis(axisLine=list(lineStyle=list(color='darkgrey', width=0)),
                             name="\u6BD4\u4F8B", max=ifelse(
                                 max(dfAgeGender$value, na.rm=TRUE)==1, 1.2, 1)) %>%
                    setXAxis(axisLine=list(color='darkgrey'), name="\u5E74\u9F84\u7EC4") %>%
                    setToolbox(TRUE, lang, pos=3) %>% setTitle(caption)
                if ('echarts' %in% class(echartgg)){
                    echartgg$x$series <- append(
                        echartgg$x$series, list(list(
                            type = 'line', data = dfAgeTotal$y, itemStyle = list(
                                normal = list(lineStyle=list(type='dashed'))),
                            symbol='none', large=FALSE, name="\u5408\u8BA1")))
                    echartgg$x$legend$data <- c(echartgg$x$legend$data, "\u5408\u8BA1")
                }
            }else{
                gg <- chartColumn(
                    data=dfAgeGender, xvar="AgeG", yvar="value", gvar="GenderCN",
                    title=caption, xlab="\u5E74\u9F84\u7EC4", ylab="\u6BD4\u4F8B",
                    annotate=paste0("percent", digits),
                    palette=getOption("init.pal"),
                    line=list(list(label='\u5408\u8BA1', linetype='dashed', arrow=FALSE,
                                   value=dfAgeTotal)),
                    alt.font=altfont, plot.background='grey95')
            }
        }else if (tolower(lang)=='en'){
            dfAgeGender$GenderEN <-
                factor(dfAgeGender$GenderEN, levels=tblGender$GenderEN)
            caption=paste0("Fig: ", str_to_title(riskname),
                           " by Age & Gender (", curYrs, ")")

            if (dynamic){
                echartgg <-echartR(data=dfAgeGender, x=AgeG, y=value,
                                   series=GenderEN, type='vbar') %>%
                    setTheme(palette=getOption("init.pal")) %>%
                    setToolbox(TRUE, lang, pos=3) %>% setTitle(caption) %>%
                    setYAxis(axisLine=list(lineStyle=list(width=0, color='darkgrey')),
                             name="Percent") %>%
                    setXAxis(axisLine=list(lineStyle=list(color='darkgrey')), name="Age\ngroup")
                if ('echarts' %in% class(echartgg)){
                    echartgg$x$series <- append(echartgg$x$series, list(
                        list(type='line', data=dfAgeTotal$y,
                             itemStyle=list(
                                 normal=list(
                                     lineStyle=list(type='dashed'))),
                             symbol='none', large=FALSE, name="Total")))
                    echartgg$x$legend$data <- c(echartgg$x$legend$data, "Total")
                }
            }else{
                gg <- chartColumn(
                    data=dfAgeGender, xvar="AgeG", yvar="value", gvar="GenderEN",
                    title=caption, xlab="Age group", ylab="Percent",
                    annotate=paste0("percent", digits),
                    palette=getOption("init.pal"),
                    line=list(list(label='Total', linetype='dashed', arrow=FALSE,
                                   value=dfAgeTotal)),
                    alt.font=altfont, plot.background='grey95')
            }
        }
        if (dynamic){
            if ('echarts' %in% class(echartgg)){
                echartgg$sizingPolicy$padding <- echartgg$sizingPolicy$viewer$padding <-
                    echartgg$sizingPolicy$browser$padding <- c(0, 0, 0, 200)
                echartgg$width <- 788
                echartgg$height <- 420
                echartgg$align <- 'right'
                echartgg$x$yAxis[[1]][['axisLabel']][['formatter']] <-
                    JS(JStooltip("axis_pct", 0))
                echartgg$x$tooltip[['formatter']] <- JS(JStooltip('tile_bar_pct', digits))
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
