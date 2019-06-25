##---------[[Label JS classes for echartR]]-------------
#' JavaScript Tooltip for Echarts
#'
#' Get specific JS tooltip scripts according to your needs. The scripts are used
#' for echarts.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param type The script type name. \cr
#' \itemize{
#'  \item axis_pct - Turn value to percents;\cr
#'  \item axis_abs_pct - Turn value to abs values and then percents;\cr
#'  \item tile_bar_pct - Turn value to percents by groups;\cr
#'  \item tile_bar_abs_pct - Turn value to abs values and then percents by groups;\cr
#'  \item bar_mkline_pct - Percentize the grouped values for bars and marklines;\cr
#'  \item mkline_pct - Percentize the grouped values for marklines
#' }
#' @param digits Number of digits, if applicable.
#' @param ... ignore
#'
#' @return String. JS tooltip scripts.
#' @export
#'
#' @examples
#' JStooltip("axis_pct", 1)
JStooltip <- function(type, digits,...){ # return JS function body as a string
    switch(type,
           axis_pct=paste0(
               "function(value){return Number(100*value).toFixed(", digits, ") + '%';}"),
           axis_abs_pct=paste0(
               "function(value){",
               "return Math.abs(Number(100*value)).toFixed(", digits,
               ") + '%';}"),
           tile_bar_pct=paste0(
               "function(params){ ",
               "var res = params[0].name; ",
               "for (var i = 0, l = params.length; i < l; i++) { ",
               "if (isNaN(params[i].value)){",
               "res += '<br/>' + params[i].seriesName + ': ';",
               "}else{",
               "res += '<br/>' + params[i].seriesName + ': '",
               "+ Number(100*params[i].value).toFixed(", digits, ") + '%';",
               "}",
               "}",
               "return res;}"),
           tile_bar_abs_pct=paste0(
               "function(params){",
               "var res = params[0].name;",
               "for (var i = 0, l = params.length; i < l; i++) {",
               "if (isNaN(params[i].value)){",
               "res += '<br/>' + params[i].seriesName + ': ';",
               "}else{",
               "res += '<br/>' + params[i].seriesName + ': '",
               "+ Math.abs(Number(100*params[i].value)).toFixed(", digits, ") + '%';",
               "}",
               "}",
               "return res;}"),
           bar_mkline_pct=paste0(
               "function(params){",
               "if (params.length == 1){",
               "return params.seriesName + ': '",
               "+ Number(100*params.value).toFixed(", digits, ") +'%';",
               "}else{",
               "var res = params[0].name;",
               "if (isNaN(params[0].value)){",
               "res += ': ';",
               "}else{",
               "res += ': '",
               "+ Number(100*params[0].value).toFixed(", digits, ") + '%';",
               "}",
               "for (var i = 1, l = params.length; i < l; i++) {",
               "if (isNaN(params[i].value)){",
               "res += '<br/>' + params[i].seriesName",
               "+ ': ';",
               "}else{",
               "res += '<br/>' + params[i].seriesName + ': '",
               "+ Number(100*params[i].value).toFixed(", digits, ")",
               "+ '%';",
               "}",
               "}",
               "return res;}",
               "}"),
          mkline_pct=paste0(
              "function(params){",
              "return (100*params.value).toFixed(", digits,
              ") +'%';}")
    )
}