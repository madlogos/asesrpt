#------------[Run distribution charts from Report Configure window]------
#' Histogram of Age + Gender
#'
#' Generate histogram of age + gender. It is used in \code{setRptParam()} function to
#' look into the age distribution.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param x BU or city vector
#' @param y Age vector
#' @param gender Gender vector
#' @param label A data frame to show in the parameter table
#' @param parent Parent GUI
#' @param agegrp Age grouping standard
#' @param ...
#'
#' @return Nothing but a GUI showing histograms
#'
#' @import ggplot2
#' @import ggthemes
#' @import gWidgets2
#' @importFrom stringr str_replace_all
#'
#' @examples
#' \dontrun{
#' histAge(y=Age, label=Label)
#' }
histAge <- function(x=NULL, y, gender=NULL, label, parent=NULL, agegrp=NULL, ...){    
    # FIXME: Shiny
    win <- gbasicdialog("Histogram of Age", parent=parent, do.buttons=FALSE)
    size(win) <- c(800, 800)
    box <- gvbox(cont=win)
    layout <- glayout(cont=box, spacing=5)
    label[,2] <- str_replace_all(label[,2], "(.{80})", "\\1\n")
    layout[1,1] <- tbl <- gtable(label, cont=layout, expand=TRUE, fill=TRUE)
    size(tbl) <- c(620, 120)
    layout[2,1] <- nb <- gnotebook(cont=layout)
    size(nb) <- c(620, 420)
    gg1 <- ggraphics(width=600, height=400, cont=nb, label='Histogram of Age',
                     visible=FALSE)
    df <- data.frame(Age=y)
    if (!is.null(x)) {df$BU <- as.factor(x)}
    if (!is.null(gender)) df$gender <- gender <- as.factor(gender)
    bin_width <- floor((max(df$Age, na.rm=TRUE) - min(df$Age, na.rm=TRUE))/15)
    print(ggplot(df,aes(x=Age))+
              geom_histogram(binwidth=bin_width,
                             fill="#00BCE4", color="gray75", alpha=0.5)+
              ggtitle("Age distribution") + xlab("Age") + ylab("Count")+
              theme_wsj(base_size=8, title_family="sans")+
              theme(title=element_text(size=12), legend.title=element_blank(),
                    axis.title=element_text(family='sans',vjust=0.5, hjust=0.5,size=10),
                    axis.title.y=element_text(angle=90),
                    panel.grid.major=element_line(colour='gray90'),
                    plot.background=element_rect(fill=rgb(255, 239, 219, 100, max=255)),
                    panel.background=element_rect(fill='white')
              ))
    visible(gg1) <- TRUE
    gg2 <- ggraphics(width=600, height=400, cont=nb, label='Box Plot of Age',
                     visible=FALSE)
    df1 <- df
    if (ncol(df1)==2) df1$BU <- "All"
    gph <- ggplot(rbind(df1, df), aes(x=BU, y=Age))+
        geom_boxplot(fill="#00BCE4", color="gray75", alpha=0.5)+
        ggtitle("Age distribution") + xlab("Business Unit") + ylab("Age")+
        theme_wsj(base_size=8, title_family="sans")+
        theme(title=element_text(size=12), legend.title=element_blank(),
              axis.title=element_text(family='sans', vjust=0.5,  hjust=0.5, size=10),
              axis.title.y=element_text(angle= 90),
              axis.text.x=element_text(vjust= -0.25),
              axis.text.y=element_text(hjust= -0.25),
              panel.grid.major=element_line(colour='gray90'),
              plot.background=element_rect(fill=rgb(255, 239, 219, 100, max=255)),
              panel.background=element_rect(fill='white')
        )
    if (is.factor(rbind(df1, df)$BU) && sum(nchar(levels(rbind(df1, df)$BU))) >= 40){
        scale_x_lab <- paste0(c("","\n"), unique(rbind(df1,df)$BU), c("\n",""))
        gph <- gph + scale_x_discrete(labels=scale_x_lab)
    }
    print(gph)
    visible(gg2) <- TRUE
    if (!is.null(agegrp)){
        gg3 <- ggraphics(width=600, height=400, cont=nb, 
                                 label='Gender-Age Pyramid', visible=FALSE)
        ageg <- cut(y, agegrp, right=FALSE)
        aetPal <- c("#7AC143","#7D3F98","#F47721","#D20962","#00A78E","#00BCE4")
        levels(ageg) <- renameRange(levels(ageg), ">=", right.substract = 1)
        df$ageg <- ageg
        df$gender <- as.factor(df$gender)
        levels(df$gender) <- str_replace_all(levels(df$gender), "\u{7537}", "Male")
        levels(df$gender) <- str_replace_all(levels(df$gender), "\u{5973}", "Female")
        df$gender <- relevel(df$gender,ref="Male")
        print(chartTornado(df[,2:4], "ageg", NULL, "gender",
                           title="Age-group and gender distribution",
                           annotate="percent0", xlab="Age Group", ylab="Percent",
                           plot.background=rgb(255, 239, 219, 100, max=255)))
        visible(gg3) <- TRUE
        #         win.graph(height=4,width=6)
        #         print(gph)
    }
    gtkMain()
    nb$set_value(1) # go back to tab 1
}
