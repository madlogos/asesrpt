##------------[Interpretation characters]-------------

#' Structured Interpretation
#'
#' In an automated report, use this function to structure the elements you want
#' to put in each section.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param librisk Data frame of the health risks
#' @param riskvar Variable name of the specific risk
#' @param risk.level The level of the risk. \cr
#' \describe{
#'  \item{0}{section title}
#'  \item{1}{risk group}
#'  \item{2}{specific risk}
#' }
#' @param title.headlevel If \code{risk.level} = 1, then \code{title.headlevel} should
#' be 2, and in markdown document, it is shown as "##"
#' @param lang Language, "cn" or "en"
#' @param suppl A list. Supplemental wording pre- or post- each element of the
#' section.
#' @param ...
#'
#' @return A list containing all the wording elements in the list.
#' @importFrom stringr str_to_title
#' @export
#'
#' @examples
#' \dontrun{
#' getStrInterpret(libRisk, "Owt", suppl=list(posttitle="Sub title", ending="Close"))
#' }
getStrInterpret <- function(
    librisk, riskvar, risk.level=2, title.headlevel=risk.level+1, lang=c('cn', 'en'),
    suppl=list(posttitle=NULL, pretabBU=NULL, posttabBU=NULL,
               prefigAge=NULL, postfigAge=NULL, pretabAge=NULL,
               posttabAge=NULL, prefigBU=NULL, postfigBU=NULL,
               pretabCity=NULL, posttabCity=NULL,
               prefigCity=NULL, postfigCity=NULL,
               pretabTrendBU=NULL, posttabTrendBU=NULL,
               prefigTrendBU=NULL, postfigTrendBU=NULL, ending=NULL),
...){
    lang <- tolower(match.arg(lang))
    riskrec <- switch(risk.level+1,
                      librisk[librisk$RiskL2Code==riskvar | librisk$RiskL2CN==riskvar |
                                  librisk$RiskL2EN == riskvar | librisk$RiskCatgCN==riskvar |
                                  librisk$RiskCatgEN==riskvar,],
                      librisk[librisk$RiskL2Code==riskvar | librisk$RiskL2CN==riskvar |
                                  librisk$RiskL2EN == riskvar | librisk$RiskL1CN==riskvar |
                                  librisk$RiskL1EN==riskvar,],
                      librisk[librisk$RiskL2Code==riskvar | librisk$RiskL2CN==riskvar |
                                  librisk$RiskL2EN == riskvar,]
    )
    if (nrow(riskrec)>1)  riskrec <- riskrec[which(riskrec$IsL2Sum==1),]
    if (tolower(lang)=='cn'){
        title <- paste0(paste(rep("#", title.headlevel), collapse=""),
                        as.character(switch(risk.level+1, riskrec$RiskCatgCN,
                                            riskrec$RiskL1CN, riskrec$RiskL2CN)),
                        " (", str_to_title(as.character(switch(risk.level+1, riskrec$RiskCatgEN,
                                                           riskrec$RiskL1EN, riskrec$RiskL2EN))), ")")
        interpret <- as.character(switch(risk.level+1, riskrec$RiskCatgIntptCN,
                                         riskrec$RiskL1IntptCN,
                                         riskrec$RiskL2IntptCN))
    }else if (tolower(lang)=="en"){
        title <- str_to_title(paste(paste(rep("#", title.headlevel), collapse=""),
                                 as.character(switch(risk.level+1, riskrec$RiskCatgEN,
                                                     riskrec$RiskL1EN, riskrec$RiskL2EN))))
        interpret <- as.character(switch(risk.level+1, riskrec$RiskCatgIntptEN,
                                         riskrec$RiskL1IntptEN,
                                         riskrec$RiskL2IntptEN))
    }else{
        stop("language only supports 'cn' and 'en'!")
    }
    if (!all(is.null(unlist(suppl))))
        suppl <- mergeList(list(posttitle=NULL, pretabAge=NULL, posttabAge=NULL,
                                prefigAge=NULL, postfigAge=NULL, pretabBU=NULL,
                                posttabBU=NULL, prefigBU=NULL, postfigBU=NULL,
                                pretabCity=NULL, posttabCity=NULL,
                                prefigCity=NULL, postfigCity=NULL,
                                pretabTrendBU=NULL, posttabTrendBU=NULL,
                                prefigTrendBU=NULL, postfigTrendBU=NULL, ending=NULL),
                           suppl)
    return(mergeList(list(title=title, interpret=interpret), suppl))
}