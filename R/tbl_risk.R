##---------[[Table: Cascaded Risk Rates List]]-------------
#' Risk Aggregation Stats in Auto Reporting
#'
#' Only used for automated reporting. Get a summarized risk distribution.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param data Raw data frame.
#' @param imput Summarized data frame.
#' @param librisk Data frame of the risk pool.
#' @param lang 'cn' or 'en'.
#' @param curYr Integer. Current year used for reporting.
#' @param digits Number of digits of the results. Default 1.
#' @param dynamic Interactive JavaScript charts/tables or not. If TRUE, call
#' \pkg{DT}::\code{\link{datatable}} to make the object. Default FALSE.
#' @param ...
#'
#' @return A data frame.
#' @importFrom DT datatable
#' @importFrom reshape2 melt dcast
#' @export
#' @seealso \code{\link{knitr}}  \code{\link{datatable}}
#' @examples
#'
tabRisks <- function(data, imput=NULL, librisk, lang=c('cn', 'en'), curYr,
                     digits=1, dynamic=FALSE,
...){

    Data <- data
    Imput <- imput
    lang <- match.arg(lang)
    if (!is.null(curYr)) {          # only get this year's data
        data <- data[data$ProjectYear %in% curYr,]
        if (!is.null(imput)) {
            imput <- imput[imput$ProjectYear %in% curYr,]
            if (nrow(imput)==0) imput <- NULL
        }
    }
    riskvar1 <- names(data)[which(names(data) %in% librisk$RiskL2Code)]
    data <- melt(data, id="Gender", measure=riskvar1, na.rm=TRUE)
    names(data) <- c("Gender", "RiskCode", "RiskGrade")
    if (!is.null(imput))
        imput <- merge(imput[, c("RiskL2ID", "Gender", "NPcpt", "NAbn")],
                       risk[, c("RiskL2ID", "RiskL2Code")], by="RiskL2ID", all.x=TRUE)
    tabRisk <- tabSource(data, imput, "RiskGrade", "RiskCode", "Gender", "RiskL2Code",
                         tblRisk=librisk, tblBU=NULL, tblCity=NULL, lang=lang,
                         col.levels=c("M", "F"), row_levels=NULL)

    if (tolower(lang)=='cn'){
        tabRisk <- merge(tabRisk, librisk[, c("RiskCatgCN", "RiskCatgID", "OrderID",
                                            "RiskL2CN", "RiskL1CN")],
                         by.x="RiskCode", by.y="RiskL2CN", all.x=TRUE, sort=FALSE)
        tabRisk <- tabRisk[seq_len(nrow(tabRisk)-1), ]   # drop sum row
        tabRisk[, ] <- as.character(tabRisk[, ])
        tabRisk <- tabRisk[order(tabRisk$RiskCatgID, tabRisk$OrderID), ]
        tabRisk <- tabRisk[, c(ncol(tabRisk)-3, ncol(tabRisk), 1:(ncol(tabRisk)-4))]
        if (ncol(tabRisk)==14){
            head <- matrix(c(c(
                "\u7C7B\u522B", NA, "\u98CE\u9669", "\u7537", NA, NA, NA, "\u5973",
                NA, NA, NA, "\u5408\u8BA1", NA, NA),
                c(NA, NA, NA, "\u5F02\u5E38\u6570", "\u53C2\u68C0\u6570",
                  "\u68C0\u51FA\u7387", NA, "\u5F02\u5E38\u6570",
                  "\u53C2\u68C0\u6570", "\u68C0\u51FA\u7387", NA,
                  "\u5F02\u5E38\u6570", "\u53C2\u68C0\u6570", "\u68C0\u51FA\u7387")),
                byrow=TRUE, nrow=2)
        }else if (ncol(tabRisk)==18){
            head <- matrix(c(c(
                "\u7C7B\u522B", NA, "\u98CE\u9669", "\u7537", NA, NA, NA, "\u5973",
                NA, NA, NA, "\u4E0D\u8BE6", NA, NA, NA, "\u5408\u8BA1", NA, NA),
                c(NA, NA, NA, "\u5F02\u5E38\u6570", "\u53C2\u68C0\u6570",
                  "\u68C0\u51FA\u7387", NA, "\u5F02\u5E38\u6570",
                  "\u53C2\u68C0\u6570", "\u68C0\u51FA\u7387", NA,
                  "\u5F02\u5E38\u6570", "\u53C2\u68C0\u6570",
                  "\u68C0\u51FA\u7387", NA, "\u5F02\u5E38\u6570",
                  "\u53C2\u68C0\u6570", "\u68C0\u51FA\u7387")),
                byrow=TRUE, nrow=2)
        }
        caption=paste0("\u8868: \u5404\u98CE\u9669\u68C0\u51FA\u7387\u7684\u6027",
                       "\u522B\u5206\u5E03 (", curYrs, ")")
    }else if (tolower(lang)=='en'){
        tabRisk <- merge(tabRisk, librisk[, c("RiskCatgEN", "RiskCatgID", "OrderID",
                                            "RiskL2EN", "RiskL1EN")],
                         by.x="RiskCode", by.y="RiskL2EN", all.x=TRUE, order=FALSE)
        tabRisk <- tabRisk[seq_len(nrow(tabRisk)-1),]
        tabRisk[, 1] <- as.character(tabRisk[, 1])
        tabRisk <- tabRisk[order(tabRisk$RiskCatgID, tabRisk$OrderID),]
        tabRisk <- tabRisk[, c(ncol(tabRisk)-3, ncol(tabRisk), 1:(ncol(tabRisk)-4))]
        if (ncol(tabRisk)==14){
            head <- matrix(c("Type", NA, "Risk", "Male", NA, NA, NA, "Female", NA, NA, NA,
                             "Total", NA, NA, NA, NA, NA, "Abnormal", "All", "Percent", NA,
                             "Abnormal", "All", "Percent", NA, "Abnormal", "All", "Percent"),
                           byrow=TRUE, nrow=2)
        }else if (ncol(tabRisk)==18){
            head <- matrix(c(c("Type", NA, "Risk", "Male", NA, NA, NA, "Female", NA, NA, NA,
                               "N/A", NA, NA, NA, "Total", NA, NA),
                             c(NA, NA, NA, "Abnormal", "All", "Percent", NA, "Abnormal", "All",
                               "Percent", NA, "Abnormal", "All", "Percent", NA, "Abnormal",
                               "All", "Percent")),
                           byrow=TRUE, nrow=2)
        }
        caption=paste0("Table: Risk Prevalence by Gender (", curYrs, ")")
    }
    if (dynamic){
        tabRisk[, ] <- enc2native(as.character(tabRisk[, ]))
        head <- gsub("Percent", "Rate", gsub("Abnormal", "Abn'l", head))
        datatable(tabRisk, style='bootstrap', class="compact", rownames=FALSE,
                  container=sub('<table(.+?</thead>).+?$', '<table\\1</table>',
                                reheadHTMLTable(tabRisk, head, align=c(rep('l', 3), 'c'),
                                                caption=caption)),
                  filter=list(position='top', clear=TRUE)) %>%
            formatStyle(2:ncol(tabRisk), textAlign="center")
    }else{
        print(reheadHTMLTable(tabRisk, head, footRows=0, caption=caption, concatCol=1:2,
                              align=c(rep("l", 3), "c")))
    }
}
