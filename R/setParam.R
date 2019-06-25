#' Set Report Parameters in GUI
#'
#' Set report parameters in a GUI wizard for automatic checkup reporting.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param db the full path to the database. If Null, a wizard will guide you there.
#' @param ...
#'
#' @return A list containing company, companyid, bu, buid, proj, projid,
#' agegrp, lang, curyr, dynamic, rankby
#'
#' @export
#' @import RODBC
#' @import gWidgets2
#' @import gWidgets2tcltk
#' @import stringr
#' @examples
#' \dontrun{
#' setRptParam()
#' }
setRptParam <- function(db=NULL, ...){
    
    .tmpEnv <- new.env()
    machine <- Sys.info()[['machine']]
    l <- list(...)
    if (is.null(db)) 
        db <- gfile("Find the Db file ...", type="open", filter=c("Access Db"="accdb"))
    if ("pwd" %in% names(l)){
        pwd <- l$pwd
    }else{
        pwd <- decryptPwd(inputPwd(prompt="Input the password to the Database:"))
    }
    dbInd <- sub("(.+\\/).+$", "\\1Industry.accdb", db)
    #---------- odbcConnect only supports 32-bit Access
    if (machine == "x86-64"){
        queryAccess32(dbInd, NULL, c(
            "tblIndustryL1", "tblIndustryL2", "tblIndustryL3", "tblIndustryL4"),
            c("IndL1", "IndL2", "IndL3", "IndL4"))
        load("~/acc32.Rdata")
        meta.tbl <- c("Company", "Project", "BU", "AgeGrpRecipe")
        names(meta.tbl) <- c("dtCompany", "dtProject", "dtBU", "dtAgeg")
        meta.req <- ! sapply(meta.tbl, exists, mode="list")
        if (any(meta.req)){
            queryAccess32(db, pwd, meta.tbl[meta.req], names(meta.tbl)[meta.req])
            load("~/acc32.Rdata")
        }
        dtCompany <- dtCompany[order(dtCompany$CompanyEN),]
        dtProj <- dtProject[order(dtProject$ProjectYear), c("ProjectYear","CompanyID")]
        dtBUs <- dtBU[order(dtBU$BUENFull), c("BUENFull","CompanyID")]
    }else{
        conn <- odbcConnectAccess2007(db, pwd=pwd)
        connInd <- odbcConnectAccess2007(dbInd)
        IndL1 <- sqlFetch(connInd, "tblIndustryL1", stringsAsFactors=FALSE)
        IndL2 <- sqlFetch(connInd, "tblIndustryL2", stringsAsFactors=FALSE)
        IndL3 <- sqlFetch(connInd, "tblIndustryL3", stringsAsFactors=FALSE)
        IndL4 <- sqlFetch(connInd, "tblIndustryL4", stringsAsFactors=FALSE)
        if (!exists("dtCompany", mode='list')) 
            dtCompany <- sqlFetch(conn, "Company", stringsAsFactors=FALSE)
        dtCompany <- dtCompany[order(dtCompany$CompanyEN),]
        if (!exists("dtProject", mode='list')) 
            dtProject <- sqlFetch(conn, "Project", stringsAsFactors=FALSE)
        dtProj <- dtProject[order(dtProject$ProjectYear), c("ProjectYear", "CompanyID")]
        if (!exists("dtBU", mode='list')) 
            dtBU <- sqlFetch(conn, "BU", stringsAsFactors=FALSE)
        dtBUs <- dtBU[order(dtBU$BUENFull), c("BUENFull", "CompanyID")]
        if (!exists("dtAgeg", mode='list')) 
            dtAgeg <- sqlFetch(conn, "AgeGrpRecipe", stringsAsFactors=FALSE)
        odbcClose(connInd)
    }
    #-------------------------
    Ind <- merge(merge(merge(
        IndL1, IndL2, by="SectorID", all.y=TRUE), IndL3, by="IndGrpID", all.y=TRUE), 
        IndL4, by="IndID", all.y=TRUE)
    
    #------GUI-----
    gwin <- gbasicdialog("Set Report Parameters", do.buttons=TRUE, action=actionOK)
    invisible(gwin$set_size(1000, 800))
    # addHandlerChanged(gwin, handler=function(...){
    #     try(odbcClose(conn), silent=TRUE)
    #     #gtkMainQuit()
    # })
    box <- gvbox(cont=gwin)
    # box$set_borderwidth(2)
    ggrp1 <- ggroup(horizontal=FALSE, cont=box, spacing=1)
    # ggrp1$set_borderwidth(2)
    gfrm1 <- gframe("Select company and age group", cont=ggrp1, horizontal=FALSE)
    ggrp11 <- ggroup(horizontal=TRUE, cont=gfrm1, spacing=0.5)
    ggrp12 <- ggroup(horizontal=TRUE, cont=gfrm1, spacing=0.5)
    ggrp13 <- ggroup(horizontal=TRUE, cont=gfrm1, spacing=0.5)
    ggrp14 <- ggroup(horizontal=TRUE, cont=gfrm1, spacing=0.5)
    ggrp15 <- ggroup(horizontal=TRUE, cont=gfrm1, spacing=0.5)
    ggrp16 <- ggroup(horizontal=TRUE, cont=gfrm1, spacing=0.5)
    # ggrp11$set_borderwidth(1)
    # ggrp12$set_borderwidth(1)
    # ggrp13$set_borderwidth(1)
    # ggrp14$set_borderwidth(1)
    # ggrp15$set_borderwidth(1)
    # ggrp16$set_borderwidth(1)

    ggrp2 <- ggroup(cont=box)
    gfrm2 <- gframe("Projects", cont=ggrp2, horizontal=FALSE, spacing=1)
    chkProj <- gcheckboxgroup(items="", use.table=TRUE, cont=gfrm2)
    size(chkProj) <- c(350, 300)
    size(gfrm2) <- size(chkProj) + 2
    gfrm3 <- gframe("Business units", cont=ggrp2, horizontal=FALSE, spacing=1)
    
    chkBU <- gcheckboxgroup(items="", use.table=TRUE, cont=gfrm3)
    size(chkBU) <- c(450, 300)
    size(gfrm3) <- size(chkBU) + 2
    lbl1 <- glabel("Industry L1", editable=TRUE, cont=ggrp11)
    lbl2 <- glabel("Industry L2", editable=TRUE, cont=ggrp12)
    lbl3 <- glabel("Industry L3", editable=TRUE, cont=ggrp13)
    lbl4 <- glabel("Industry L4", editable=TRUE, cont=ggrp14)
    chkIndL1 <- gcombobox(items=IndL1$SectorEN, selected=FALSE, cont=ggrp11)
    chkIndL2 <- gcombobox(items=IndL2$IndGrpEN, selected=FALSE, cont=ggrp12)
    chkIndL3 <- gcombobox(items=IndL3$IndustryEN, selected=FALSE, cont=ggrp13)
    chkIndL4 <- gcombobox(items=IndL4$SubIndustryEN, selected=FALSE, cont=ggrp14)
    
    label1 <- glabel(" Company ", cont=ggrp15)
    chkCompany <- gcombobox(items=dtCompany$CompanyEN,
                            selected=FALSE, cont=ggrp15)
    size(chkIndL1) <- size(chkIndL2) <- size(chkIndL3) <- size(chkIndL4) <- 
        size(chkCompany) <- c(1200, 20)
    addHandlerChanged(chkIndL1, handler=function(h,...){
        if (length(chkIndL1$get_value()) > 0){
            chkIndL2$set_items(unique(
                Ind$IndGrpEN[Ind$SectorEN %in% chkIndL1$get_value()]))
            chkIndL3$set_items(unique(
                Ind$IndustryEN[Ind$SectorEN %in% chkIndL1$get_value()]))
            chkIndL4$set_items(unique(
                Ind$SubIndustryEN[Ind$SectorEN %in% chkIndL1$get_value()]))
            .tmpEnv$companies <<-
                dtCompany[dtCompany$SubIndID %in%
                    (Ind$SubIndID[Ind$SectorEN %in% chkIndL1$get_value()]),]
            chkCompany$set_items(.tmpEnv$companies$CompanyEN)
        }else{
            chkIndL2$set_items(IndL2$IndGrpEN)
        }
    })
    addHandlerChanged(chkIndL2, handler=function(h, ...){
        if (length(chkIndL2$get_value())>0){
            chkIndL3$set_items(unique(
                Ind$IndustryEN[Ind$IndGrpEN %in% chkIndL2$get_value()]))
            chkIndL4$set_items(unique(
                Ind$SubIndustryEN[Ind$IndGrpEN %in% chkIndL2$get_value()]))
            .tmpEnv$companies <<-
                dtCompany[dtCompany$SubIndID %in%
                            Ind$SubIndID[Ind$IndGrpEN %in% chkIndL2$get_value()],]
            chkCompany$set_items(.tmpEnv$companies$CompanyEN)
        }else{
            chkIndL3$set_items(IndL3$IndustryEN)
        }
    })
    addHandlerChanged(chkIndL3, handler=function(h,...){
        if (length(chkIndL3$get_value())>0){
            chkIndL4$set_items(unique(
                Ind$SubIndustryEN[Ind$IndustryEN %in% chkIndL3$get_value()]))
            .tmpEnv$companies <<-
                dtCompany[dtCompany$SubIndID %in%
                              Ind$SubIndID[Ind$IndustryEN %in% chkIndL3$get_value()],]
            chkCompany$set_items(.tmpEnv$companies$CompanyEN)
        }else{
            chkIndL4$set_items(IndL4$SubIndustryEN)
        }
    })
    addHandlerChanged(chkIndL4, handler=function(h,...){
        if (length(chkIndL4$get_value())>0){
            .tmpEnv$companies <-
                dtCompany[dtCompany$SubIndID %in%
                              Ind$SubIndID[Ind$SubIndustryEN %in% chkIndL4$get_value()],]
            chkCompany$set_items(.tmpEnv$companies$CompanyEN)
        }else{
            chkCompany$set_items(dtCompany$CompanyEN)
        }
    })
    addHandlerChanged(chkCompany, handler=function(h,...){
        if (!is.na(chkCompany$get_value())){
            CorpID <- dtCompany$CompanyID[which(dtCompany$CompanyEN %in%
                                                    chkCompany$get_value())]
            chkProj$set_items(data.frame(
                items=dtProj$ProjectYear[dtProj$CompanyID %in% CorpID]))
            chkBU$set_items(data.frame(
                items=dtBUs$BUENFull[dtBUs$CompanyID %in% CorpID]))
            size(chkProj)[2] <- size(chkBU)[2] <- 295
            invisible(gwin$set_size(1000, 800))
            if (length(chkBU$get_items())>0) 
                svalue(chkBU) <- chkBU$get_items()
            if (length(chkProj$get_items())>0)
                svalue(chkProj) <- chkProj$get_items()
        }
    })
    addHandlerChanged(chkProj, handler=function(h, ...){
        if (length(chkProj$get_value())>0){
            chkCuryr$set_items(chkProj$get_value())
            if (length(chkCuryr$get_value())==0)
                svalue(chkCuryr) <- chkCuryr$get_items()[length(chkCuryr$get_items())]
        }
    })
    label2 <- glabel("Age Group", cont=ggrp16)
    chkAgeg <- gcombobox(items=dtAgeg$RecipeName[order(dtAgeg$RecipeName)],
                         editable=TRUE, cont=ggrp16)
    addHandlerChanged(chkAgeg, handler=function(h, ...){
        if (! str_detect(chkAgeg$get_value(), paste0(
            "(\\d+),[[:space:]]*\\((\\d+)[[:space:]]*\\[(\\d+)\\]",
            "[[:space:]]*(\\d+)\\),[[:space:]]*(\\d+)"))){
            galert(paste0("The format of the Age Group is not valid!\n","Should be ",
                          "`Min (Begin [Interval] End) Max`."),
                   "Invalid format", delay=5)
            size(galert) <- c(80, 10)
        }
    })
    size(chkAgeg) <- size(chkCompany)
    invisible(gwin$set_size(1000, 800))
    ###-----Function that parse env values-----------
    fetchCtrlVal <- function(dtCompany, dtBU, dtProj, ...){
        .tmpEnv$agegrp <<- str_replace_all(chkAgeg$get_value(),
                                paste0("(\\d+),[[:space:]]*\\((\\d+)[[:space:]]*",
                                       "\\[(\\d+)\\][[:space:]]*(\\d+)\\),",
                                       "[[:space:]]*(\\d+)"),
                                "\\1 \\2 \\3 \\4 \\5")
        .tmpEnv$agegrp <<- as.numeric(unlist(strsplit(.tmpEnv$agegrp, " ")))
        .tmpEnv$lang <<- chkLang$get_value()
        .tmpEnv$dynamic <<- chkDynamic$get_value()
        .tmpEnv$curyr <<- chkCuryr$get_value()
        .tmpEnv$rankby <<- chkRankby$get_value()
        .tmpEnv$rank <<- chkRank$get_value()
        if (length(chkCompany$get_value())==0){
            .tmpEnv$company <<- chkCompany$get_items()
            .tmpEnv$companyid <<-
                dtCompany$CompanyID[dtCompany$CompanyEN %in% .tmpEnv$company]
            .tmpEnv$projid <<-
                dtProj$PjtID[dtProj$CompanyID %in% .tmpEnv$companyid]
            .tmpEnv$proj <<-
                dtProj$ProjectYear[dtProj$CompanyID %in% .tmpEnv$companyid]
            .tmpEnv$buid <<- dtBU$BUID[dtBU$CompanyID %in% .tmpEnv$companyid]
            .tmpEnv$bu <<- dtBU$BUENFull[dtBU$CompanyID %in% .tmpEnv$companyid]
        }else{
            .tmpEnv$company <<- chkCompany$get_value()
            .tmpEnv$companyid <<-
                dtCompany$CompanyID[dtCompany$CompanyEN %in% .tmpEnv$company]
            if (length(chkProj$get_value())==0){
                .tmpEnv$proj <<- NULL
                .tmpEnv$projid <<- NULL
            }else{
                .tmpEnv$proj <<- chkProj$get_value()
                .tmpEnv$projid <<-
                    dtProj$PjtID[dtProj$CompanyID %in% .tmpEnv$companyid &
                                     dtProj$ProjectYear %in% .tmpEnv$proj]
            }
            if (length(chkBU$get_value())==0){
                .tmpEnv$bu <<- NULL
                .tmpEnv$buid <<- NULL
            }else{
                .tmpEnv$bu <<- chkBU$get_value()
                .tmpEnv$buid <<-
                    dtBU$BUID[dtBU$CompanyID %in% .tmpEnv$companyid &
                                  dtBU$BUENFull %in% .tmpEnv$bu]
            }
        }
    }

    group3 <- ggroup(horizontal=TRUE, spacing=1, cont=box)
    frame4 <- gframe("Select reporting parameters", cont=group3, horizontal=TRUE)
    size(frame4) <- c(750, 60)
    group31 <- ggroup(cont=frame4, spacing=1, horizontal=TRUE)
    frame41 <- gframe("Language", cont=group31, horizontal=TRUE)
    group32 <- ggroup(cont=frame4, spacing=1, horizontal=TRUE)
    frame42 <- gframe("Current project year", cont=group32, horizontal=TRUE)
    size(frame42) <- c(600, 60)
    group33 <- ggroup(cont=frame4, spacing=1, horizontal=TRUE)
    chkLang <- gradio(c("cn", "en"), selected=1, horizontal=FALSE, cont=frame41)
    chkCuryr <- gcheckboxgroup(items=NA, use.table=FALSE, horizontal=TRUE,
                               cont=frame42)
    chkDynamic <- gcheckbox("Interactive\nJS chart", checked=FALSE,
                            cont=group33)
    group4 <- ggroup(horizontal=TRUE, spacing=1, cont=box)
    frame41 <- gframe("Rank dataset by", cont=group4, horizontal=TRUE)
    chkRankby <- gradio(c("Col_1", "n_pos", "n_total", "%_pos"),
                        selected=1, horizontal=TRUE, cont=frame41)
    frame42 <- gframe("Rank dataset by", cont=group4, horizontal=TRUE)
    chkRank <- gradio(c("ascencding","descending","as.is"),
                      selected=3, horizontal=TRUE, cont=frame42)
    group5 <- ggroup(horizontal=TRUE, spacing=1, cont=box)
    box51 <- gvbox(cont=group5)
    box52 <- gvbox(cont=group5)
    box53 <- gvbox(cont=group5)
    box54 <- gvbox(cont=group5)
    output <- NULL
    actionOK <- gaction(" Confirm ", "OK", icon="ok", handler=function(h, ...){
        if (length(chkCompany$get_value())==0){
            cfm_msg <- paste0(
                "You selected:\n\n",
                length(.tmpEnv$companies$CompanyEN), " Companies: ",
                paste(.tmpEnv$companies$CompanyEN, collapse=","), "\n\n",
                "All their Projects and BUs\n\n",
                "Age Group: ", paste(chkAgeg$get_value(), collapse=","))
        }else{
            cfm_msg <- paste0(
                "You selected:\n\n", length(chkCompany$get_value()), " Companies: ",
                str_wrap(paste(chkCompany$get_value(), collapse=", "), 40), "\n\n",
                length(chkProj$get_value()), " Projects: ",
                str_wrap(paste(chkProj$get_value(), collapse=", "), 40), "\n\n",
                length(chkBU$get_value()), " BUs: ",
                str_wrap(paste(chkBU$get_value(), collapse=", "), 40), "\n\n",
                "Age Group: ", str_wrap(paste(chkAgeg$get_value(), collapse=", "), 40))
        }
        cfm <- gconfirm(cfm_msg, "Parameters Defined", icon="info")
        size(cfm) <- c(300, 40)
        if (cfm){
            fetchCtrlVal(dtCompany, dtBU, dtProject)
            output <<- TRUE
            invisible(gwin$dispose_window())
        }
    })
    ok <- gbutton(action=actionOK, cont=box51)
    actionCls <- gaction("Clear", "Clear all", icon="delete", handler=function(h, ...){
        chkProj$set_items(NULL)
        chkBU$set_items(NULL)
    })
    cls <- gbutton(action=actionCls, cont=box52)
    actionCancel <- gaction("Cancel", "Cancel", icon="cancel", handler=function(h, ...){
        output <<- FALSE
        invisible(gwin$dispose_window())
    })
    cancel <- gbutton(action=actionCancel, cont=box53)
    actionHist <- gaction(
        "Distribution","Distribution of Age",icon="info",
        handler=function(h,company=dtCompany,
                         project=dtProject,BU=dtBU,...){
            sql <- "SELECT Age, BU, Gender FROM Raw0 WHERE "
                  fetchCtrlVal(dtCompany, dtBU, dtProject)
                  if (length(.tmpEnv$companyid) > 1){
                      sql <- paste0(sql," Raw0.Company IN ('",
                                    paste(.tmpEnv$companyid, collapse="','"), "')")
                  }else if (length(.tmpEnv$companyid)==1){
                      if (length(.tmpEnv$buid) > 0){
                          sql <- paste0(sql," Raw0.BU IN ('",
                                        paste(.tmpEnv$buid, collapse="','"), "')")
                      }else{
                          sql <- paste0(sql," Raw0.BU IN ('0')")
                      }
                      if (length(.tmpEnv$projid) > 0) {
                          sql <- paste0(sql," AND Raw0.PjtID IN (",
                                        paste(.tmpEnv$projid, collapse=","), ")")
                      }else{
                          sql <- paste0(sql," AND Raw0.PjtID IN (0)")
                      }
                  }
                  if (machine == "x86-64"){
                      queryAccess32(db, pwd, sql, "dfage")
                      load("~/acc32.Rdata")
                  }else{
                      conn <- odbcConnectAccess2007(db, pwd=pwd)
                      dfage <- sqlQuery(conn, sql)
                      odbcCloseAll()
                  }
                  
                  dfage <- merge(dfage, dtBU[, c("BUID","BUENFull")],
                                 by.x="BU", by.y="BUID", all.x=TRUE)
                  if (nrow(dfage)>0) {
                      #label <- paste0('<html><table><tr><td><b>Company: </b></td><td>',
                      #                paste(.tmpEnv$company,collapse=', '),'</td></tr>',
                      #                '<tr><td><b>Project: </b></td><td>',
                      #                paste(.tmpEnv$proj,collapse=', '),'</td></tr>',
                      #                '<tr><td><b>Bus Unit: </b></td><td>',
                      #                paste(.tmpEnv$bu,collapse=', '),"</td></tr></table></html>")
                      lblTbl <- data.frame(
                          Param=c("Company","Project","Bus Unit"),
                          Value=c(paste(.tmpEnv$company, collapse=', '),
                                  paste(.tmpEnv$proj, collapse=', '),
                                  paste(.tmpEnv$bu, collapse=', ')))
                      histAge(x=dfage$BUENFull, y=dfage$Age,
                              gender=dfage$Gender, label=lblTbl,
                              parent=gwin,
                              agegrp=c(.tmpEnv$agegrp[1], seq(
                                  .tmpEnv$agegrp[2], .tmpEnv$agegrp[4],
                                  .tmpEnv$agegrp[3]), .tmpEnv$agegrp[5]))
                  }else{
                      galert(paste("Cannot find data requested. Please double check.\n",sql),
                             "Return No Results", parent=gwin, delay=5)
                  }
              })
    hist <- gbutton(action=actionHist, cont=box54)
    # size(ok) <- size(cls) <- size(cancel) <- size(hist) <- c(150, 30)
    
    visible(gwin)  # hold the main window
    if (is.null(output)){
        stop("Reporting process is cancelled!!")
    }else{
        try(return(list(company=.tmpEnv$company, companyid=.tmpEnv$companyid,
                   bu=.tmpEnv$bu, buid=.tmpEnv$buid, proj=.tmpEnv$proj,
                   projid=.tmpEnv$projid,
                   agegrp=c(.tmpEnv$agegrp[1], seq(.tmpEnv$agegrp[2],
                                                   .tmpEnv$agegrp[4],
                                                   .tmpEnv$agegrp[3]),
                             .tmpEnv$agegrp[5]),
                   lang=.tmpEnv$lang, curyr=.tmpEnv$curyr,
                   dynamic=.tmpEnv$dynamic,
                   rankby=.tmpEnv$rankby, rank=.tmpEnv$rank)),
        silent=TRUE)
    }
    rm(.tmpEnv)
    if (file.exists("~/acc32.Rdata")) unlink("~/acc32.Rdata")
}


#' Get Data from 32-bit Access with 64-bit R
#'
#' When x64 Access driver is missing, you cannot directly fetch data from MS Access 
#' using \pkg{RODBC} right now. This is a work-around function to initiate an 
#' independent x86 R session to fetch the data, and save the data.frames as an
#' \code{acc32.Rdata} file in your work directory.
#' 
#' @param db.path charcter, MS Access db full path
#' @param pwd character, the password to the Access db if applicable. If there is
#' no password, set it NULL (default).
#' @param qry.tbl character vector, SQL queries or tables name. 
#' @param out.names character vector, names of the output objects. Must be of the same
#' length as qry.tbl.
#' @param silent logical, if avoid any message. Deafult FALSE.
#' 
#' @return An Rdata file (acc32.Rdata) in your work directory.
#' @export
#'
#' @examples
#' \dontrun{
#' queryAccess32("~/test.accdb", qry.tbl=c("User", "select * from Deal;"))
#' }
#' 
queryAccess32 <- function(db.path, pwd=NULL, qry.tbl=NULL, out.names=NULL, silent=FALSE) {
    
    if (is.null(qry.tbl)){
        return(structure("do not know which tables/queries you want to get.",
                         class="try-error"))
    }else{
        type <- vector(length=length(qry.tbl))
        type[grepl("^select ", tolower(qry.tbl))] <- "query"
        type[!grepl(" ", tolower(qry.tbl))] <- "table"
        if (is.null(out.names)){
            out.names <- vector(length=length(qyr.tbl))
            out.names[type=="query"] <- gsub('^.+from (\\S+).*$', '\\1', qry.tbl[type=="query"])
            out.names[type=="table"] <- qry.tbl[type=="table"]
        }else{
            if (length(qry.tbl) != length(out.names))
                stop("qry.tbl must be of the same length as out.names.")
        }
    }

    # variables to make values uniform
    # sock.port <- 8642L
    # sock.con <- "sv_con"
    ODBC.con <- "a32_con"
    
    if (file.exists(db.path)) {
        
        # build ODBC string
        ODBC.str <- local({
            s <- list()
            s$path <- paste0("DBQ=", gsub("(/|\\\\)+", "/", path.expand(db.path)))
            s$driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)}"
            s$threads <- "Threads=4"
            s$buffer <- "MaxBufferSize=4096"
            s$timeout <- "PageTimeout=5"
            if (!is.null(pwd)) s$pwd <- paste0("PWD=", pwd)
            paste(s, collapse=";")
        })
        
        # start socket server to transfer data to 32 bit session
        # proven to be unable to run
        #startSocketServer(port=sock.port, server.name="access_query_32", local=TRUE)
        
        # build expression to pass to 32 bit R session
        #expr <- "library(svSocket)"
        expr <- "library(RODBC)"
        expr <- c(expr, sprintf("%s <- odbcDriverConnect('%s')", ODBC.con, ODBC.str))
        
        if (any(type == "table")){
            expr <- c(expr, sprintf(paste(
                "if('%1$s' %%in%% sqlTables(%2$s)$TABLE_NAME) {",
                "%3$s <- sqlFetch(%2$s, '%1$s', stringsAsFactors=FALSE) ",
                "} else {",
                "%3$s <- 'table %1$s not found'}"), 
                qry.tbl[type=="table"], ODBC.con, out.names[type=="table"]))
        }
        if (any(type == "query")){
            expr <- c(expr, sprintf(paste(
                "%3$s <- sqlQuery(%2$s, \"%1$s\", as.is=TRUE, stringsAsFactors=FALSE)"
                ), 
                qry.tbl[type=="query"], 
                ODBC.con, out.names[type=="query"]))
        }
        
        #expr <- c(expr, sprintf("%s <- socketConnection(port=%i)", sock.con, sock.port))
        #expr <- c(expr, sprintf("evalServer(%s, %s, %s)", sock.con, table.out, qry))
        expr <- c(expr, "odbcCloseAll()")
        #expr <- c(expr, sprintf("close(%s)", sock.con))
        expr <- c(expr, "save(list=ls(), file='~/acc32.Rdata')")
        
        expr <- paste(expr, collapse=";")
        
        # launch 32 bit R session and run expressions
        prog <- file.path(R.home(), "bin", "i386", "Rscript.exe")
        system2(prog, args=c("-e", shQuote(expr)), stdout=NULL, wait=TRUE, invisible=TRUE)
        
        # stop socket server
        #try(stopSocketServer(port=sock.port), silent=TRUE)
        
        # display table fields
        if (!silent) invisible(
            message("acc32.rdata has been saved in work dir: ", getwd()))
    }else{
        warning("database not found: ", db.path)
    }
}
