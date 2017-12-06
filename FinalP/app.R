library(shiny)
ui <-  navbarPage(title = "Epi Wizard",
                  tabPanel(title = "Introduction",
                           verbatimTextOutput("introduction")),
              
                  
                   navbarMenu(title = "Person-Time data",
                            
                     tabPanel(title = "Rate Difference",
                              numericInput(inputId = "exposeddisease1", label = "exposed people with disease",value = 1),
                              numericInput(inputId = "unexposeddisease1", label = "unexposed people with disease",value = 1),
                              numericInput(inputId = "exposedpt1", label = "exposed person time",value = 1),
                              numericInput(inputId = "ubexposedpt1", label = "unexposed person time",value = 1),      
                        textOutput("ratedifference")
                                            
                                       
                              ),
                            tabPanel(title = "Rate Ratio",
                                     numericInput(inputId = "exposeddisease2", label = "exposed people with disease",value = 1),
                                     numericInput(inputId = "unexposeddisease2", label = "unexposed people with disease",value = 1),
                                     numericInput(inputId = "exposedpt2", label = "exposed person time",value = 1),
                                     numericInput(inputId = "ubexposedpt2", label = "unexposed person time",value = 1),      
                                      textOutput("rateratio")
                                     
                            )
                  ),
                  navbarMenu(title = "Case-Control data",
                      
                             tabPanel(title = "Risk Ratio",
                                      numericInput(inputId = "exposeddisease3", label = "exposed people with disease",value = 1),
                                      numericInput(inputId = "unexposeddisease3", label = "unexposed people with disease",value = 1),
                                      numericInput(inputId = "exposednondisease3", label = "exposed people without disease",value = 1),
                                      numericInput(inputId = "unexposednondisease3", label = "unexposed people without disease",value = 1),
                                      textOutput("riskratio")
                                    
                             ),
                             tabPanel(title = "Odds Ratio",
                                      numericInput(inputId = "exposeddisease4", label = "exposed people with disease",value = 1),
                                      numericInput(inputId = "unexposeddisease4", label = "unexposed people with disease",value = 1),
                                      numericInput(inputId = "exposednondisease4", label = "exposed people without disease",value = 1),
                                      numericInput(inputId = "unexposednondisease4", label = "unexposed people without disease",value = 1),
                                      textOutput("oddsratio")
                                      
                             ),
                             tabPanel(title = "Risk Difference",
                                      numericInput(inputId = "exposeddisease5", label = "exposed people with disease",value = 1),
                                      numericInput(inputId = "unexposeddisease5", label = "unexposed people with disease",value = 1),
                                      numericInput(inputId = "exposednondisease5", label = "exposed people without disease",value = 1),
                                      numericInput(inputId = "ubexposednondisease5", label = "unexposed people without disease",value = 1),
                                      textOutput("riskdifference")
                                      
                             ),
                             tabPanel(title = "AR%",
                                      numericInput(inputId = "exposeddisease", label = "exposed people with disease",value = 1),
                                      numericInput(inputId = "unexposeddisease", label = "unexposed people with disease",value = 1),
                                      numericInput(inputId = "exposednondisease", label = "exposed people without disease",value = 1),
                                      numericInput(inputId = "ubexposednondisease", label = "unexposed people without disease",value = 1),  
                                      plotOutput("attributableriskpercent")
                                      
                             ),
                             tabPanel(title = "PAR",
                                      numericInput(inputId = "exposeddisease", label = "exposed people with disease",value = 1),
                                      numericInput(inputId = "unexposeddisease", label = "unexposed people with disease",value = 1),
                                      numericInput(inputId = "exposednondisease", label = "exposed people without disease",value = 1),
                                      numericInput(inputId = "ubexposednondisease", label = "unexposed people without disease",value = 1),
                                      plotOutput("populationar")
                                      
                             ),
                             tabPanel(title = "PAR%",
                                      numericInput(inputId = "exposeddisease", label = "exposed people with disease",value = 1),
                                      numericInput(inputId = "unexposeddisease", label = "unexposed people with disease",value = 1),
                                      numericInput(inputId = "exposednondisease", label = "exposed people without disease",value = 1),
                                      numericInput(inputId = "ubexposednondisease", label = "unexposed people without disease",value = 1),
                                      plotOutput("populationarpercent")
                                      
                             )
                             
                  ),
                  tabPanel(title = "Graphs")
                  
)



server <- function(input, output) 
  {
  output$introduction<-renderText({"Epidemiology is the study and analysis of the patterns, causes, and effects of health and disease conditions in defined populations. It is the cornerstone of public health, and shapes policy decisions and evidence-based practice by identifying risk factors for disease and targets for preventive healthcare."})
output$rateratio<-renderText({cbind("Rate Ratio",crude.Rate(crude.table(input$exposeddisease2,input$unexposeddisease2,input$exposedpt2,input$ubexposedpt2),measure = "IRR"),rate.ci(crude.table(input$exposeddisease2,input$unexposeddisease2,input$exposedpt2,input$ubexposedpt2),95,measure = "IRR"))})
output$ratedifference<-renderText({cbind("Rate Difference",crude.Rate(crude.table(input$exposeddisease1,input$unexposeddisease1,input$exposedpt1,input$ubexposedpt1),measure = "IRD"),rate.ci(crude.table(input$exposeddisease1,input$unexposeddisease1,input$exposedpt1,input$ubexposedpt1),95,measure = "IRD"))})
output$oddsratio<- renderText({OR(input$exposeddisease4,input$unexposeddisease4,input$exposednondisease4,input$unexposednondisease4)})
}

shinyApp(ui = ui, server = server)