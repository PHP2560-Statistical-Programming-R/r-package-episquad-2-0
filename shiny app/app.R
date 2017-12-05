#install.packages("shiny")
#library(shiny)
library(ggplot2)
library(forestplot)
library(readxl)
#appData <- read_excel("C:/Users/drsad/Google Drive/Brown/Thesis/forestplotdata.xlsx")

ui <- fluidPage(
  titlePanel("Forest plot of relative risks"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose xlsx file',
                  accept = c(".xlsx")),
      sliderInput("width", "Graph Width:",
                  min = 1, max = 15,
                  value = 6, pre = "cm"),
      radioButtons("table", "Side Table",
                   choices = c("YES", "NO"),
                   selected = "YES"),
      selectInput("group", "Grouped Plot",
                  choices = c("TRUE", "FALSE")),
      selectInput("p", "p-value",
                  choices = c("TRUE", "FALSE")),
      downloadButton('down',"download plot")
    ),
  mainPanel(plotOutput("forestplot"))
  )
 )


server <- function(input, output){
  dataInput <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data <- read_excel(inFile$datapath)
    if (input$group[1] == TRUE ) {
      #maybe i shoud use an indentation column in input data to identify the subgroups
    subgps <- c(which(data$Indent == 1)) #use the row numbers that you want to be indented
    data$Variable[subgps] <- paste("  ",data$Variable[subgps])
    } else {data$Variable <- data$Variable}
    data
  })
 
  plotInput <- reactive({
    ## Labels defining subgroups that are a little indented!
    ## Combine the HR, high and low column for the side table
    hr <- ifelse(!is.na(dataInput()$HR), paste(format(dataInput()$HR, digits=4), " (", format(dataInput()$Low, digits=4) 
                                             , " to ", format(dataInput()$High,digits=4) , ")",sep=""), NA)
    ## The rest of the columns in the table. 
    if (input$table[1] == "YES"){
      tabletext <- cbind(c("Events","\n",dataInput()$Variable), 
                         c("HR (95% CI)","\n", hr))
    } else {
      tabletext <- cbind(c("Events","\n",dataInput()$Variable))
    }
    
    
    
   forestplot(labeltext=tabletext, graph.pos=2, 
               mean=c(NA,NA,dataInput()$HR), 
               lower=c(NA,NA,dataInput()$Low), 
               upper=c(NA,NA,dataInput()$High),
               graphwidth = unit(input$width[1], "cm"),
               confintNormalFn = fpDrawPointCI,
               line.margin = .3,
               lty.ci = 1,
               txt_gp = fpTxtGp(ticks=gpar(cex=1.1)),
              if (input$group[1] == TRUE ) {
                subgps <- c(which(fp$Indent == 1))
                for (i in 1:length(subgps)){
                  hrzl_lines=list("subgps[1]" = gpar(lwd=1, col="black"), 
                                  "subgps[2]" = gpar(lwd=59, lineend="butt", columns=c(1:3), col="#99999922"),
                                  "subgps[3]" = gpar(lwd=59, lineend="butt", columns=c(1:3), col="#99999922"),
                                  "subgps[4]" = gpar(lwd=59, lineend="butt", columns=c(1:3), col="#99999922"),
                                  "subgps[5]" = gpar(lwd=59, lineend="butt", columns=c(1:3), col="#99999922"),
                                  "subgps[6]" = gpar(lwd=59, lineend="butt", columns=c(1:3), col="#99999922"))
                }}
               col=fpColors(box="gray75", lines="black", zero = "gray50"), new_page = TRUE,
               zero=1, cex=0.9, lineheight = unit(0.503, "cm"), boxsize=0.8, colgap=unit(6,"mm"),
              lwd.ci=2, ci.vertices=FALSE, ci.vertices.height = 0.4)
    
  })
   
  output$forestplot <- renderPlot({
    print(plotInput())
    })
  
  
  
  output$down <- downloadHandler(
    filename = "Shinyplot.png",
    content = function(file) {
      png(file)
      print(plotInput())
      dev.off()
    }) 
  
  
  }
  


shinyApp(ui = ui, server = server)

