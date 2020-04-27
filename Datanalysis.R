#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ggplot2)
require(ggExtra)
#equire(plotly)

#options(width=200)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Genetech Data Analysis"),
    mainPanel(
        tabsetPanel(
            tabPanel("Main", 
                     sidebarLayout(
                         sidebarPanel(
                             fileInput("file1", "Choose Random_PatientLevelInfo_2020.tsv File",
                                       accept = c(
                                           ".tsv")
                             ),
                             fileInput("file2", "Choose Random_LabValuesInfo_2020.tsv  File",
                                         accept = c(
                                             ".tsv")
                             ),
                             actionButton("okButton", "Load Data"),
                             h4("Please select the files as named in text"),
                             h4("Then press the 'Load Data' button"),
                             h4("The data will upload and merge"),
                             h4("the column names are assumed")),
                     mainPanel(fluidRow(
                         column(4,
                                plotOutput("plotAge")),
                         column(4,
                               plotOutput("plotSex")),
                         column(4,
                               plotOutput("plotRace")))))),
            tabPanel("Data Analysis",
    fluidRow(
    # Sidebar with a slider input for number of bins 
    column(6,
           plotOutput("distPlot")),
    column(6,
           plotOutput("pairs"))),
   # column(10,
          # verbatimTextOutput("summary")),
           dataTableOutput("dataval"))
)))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dataInput <- 
        eventReactive(input$okButton,{
        
        
        file1 <- input$file1
        file2 <- input$file2
        
        if (is.null(file1)) {
            return(NULL)
        }
        
        if (is.null(file2)) {
            return(NULL)
        }
        
        Patdat <- read.csv(file1$datapath, sep="\t")
        Labdat <- read.csv(file2$datapath, sep="\t")
        results=merge(Patdat,Labdat,by=c("STUDYID","USUBJID"))
        })
    
    
    # during development used .RData to save time.
    #setwd("D:\\ryanicky\\workspace\\GT_analysis")
    #load(".RData")
    output$dataval <- renderDataTable({
        dataInput()},
        class = "display nowrap compact",
        filter = "top",
        options = list(
            pageLength =10
        ))
    
    output$distPlot <- renderPlot({
        
        mydat <- dataInput()
        
        if (is.null(mydat)) {
            return(NULL)
        }
        
        
        p2<-ggplot(mydat[input$dataval_rows_all,], aes(AVISIT,AVAL)) + geom_boxplot() 
        p2 <- p2 +  facet_grid(~ACTARM)
        p2 <- p2 + theme(axis.text.x = element_text(angle=45, hjust=1))
        print(p2)
    })
    
    output$pairs <- renderPlot({
       
        
        mydat <- dataInput()
        
        if (is.null(mydat)) {
            return(NULL)
        }
        
        plot1 <- ggplot(mydat,aes(x=AGE,y=AVAL,color=SEX,Size = SEX)) + geom_point() +
                 theme(legend.position = c(0.8,0.9))
        
        plot2 <- ggMarginal(plot1, type="histogram", size = 10)
        
        plot2
        #plot1 <- plot_ly(pairs(~AVISIT+BMRKR1+AGE+SEX,data=results,col=results$ACTARMCD))
    })
    
    output$plotAge <- renderPlot({
        
        mydat <- dataInput()
        
        if (is.null(mydat)) {
            return(NULL)
        }
        
        plot1 <- hist(mydat$AGE,main="",xlab="AGE")
        text(plot1$mids,plot1$counts,labels=plot1$counts, adj=c(0.5, -0.5))
        print(plot1)
        
    })
    
    output$plotSex <- renderPlot({
        
        mydat <- dataInput()
        
        if (is.null(mydat)) {
            return(NULL)
        }
        
        
        plot1 <- barplot(table(mydat$SEX),main="",las=2,cex.names=0.7)
        print(plot1)
        
    })
    
    output$plotRace <- renderPlot({
        
        mydat <- dataInput()
        
        if (is.null(mydat)) {
            return(NULL)
        }
        
        
        plot1 <- barplot(table(mydat$RACE),main="",las=2,cex.names=0.7)
        print(plot1)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
