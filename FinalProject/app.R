library(shiny)
ui <-  navbarPage(title = "Epi Wizard",
                  tabPanel(title = "Introduction",
                           verbatimTextOutput("introduction")),
              
                  
                   navbarMenu(title = "Rate data",
                            
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
                  navbarMenu(title = "Risk data",
                      
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
                             tabPanel(title = "AR(Risk Difference)",
                                      numericInput(inputId = "exposeddisease5", label = "exposed people with disease",value = 1),
                                      numericInput(inputId = "unexposeddisease5", label = "unexposed people with disease",value = 1),
                                      numericInput(inputId = "exposednondisease5", label = "exposed people without disease",value = 1),
                                      numericInput(inputId = "unexposednondisease5", label = "unexposed people without disease",value = 1),
                                      textOutput("attributablerisk")
                                      
                             ),
                             tabPanel(title = "AR%",
                                      numericInput(inputId = "exposeddisease6", label = "exposed people with disease",value = 1),
                                      numericInput(inputId = "unexposeddisease6", label = "unexposed people with disease",value = 1),
                                      numericInput(inputId = "exposednondisease6", label = "exposed people without disease",value = 1),
                                      numericInput(inputId = "unexposednondisease6", label = "unexposed people without disease",value = 1),  
                                      textOutput("attributableriskpercent")
                                      
                             ),
                             tabPanel(title = "PAR",
                                      numericInput(inputId = "exposeddisease7", label = "exposed people with disease",value = 1),
                                      numericInput(inputId = "unexposeddisease7", label = "unexposed people with disease",value = 1),
                                      numericInput(inputId = "exposednondisease7", label = "exposed people without disease",value = 1),
                                      numericInput(inputId = "unexposednondisease7", label = "unexposed people without disease",value = 1),
                                      textOutput("populationar")
                                      
                             ),
                             tabPanel(title = "PAR%",
                                      numericInput(inputId = "exposeddisease8", label = "exposed people with disease",value = 1),
                                      numericInput(inputId = "unexposeddisease8", label = "unexposed people with disease",value = 1),
                                      numericInput(inputId = "exposednondisease8", label = "exposed people without disease",value = 1),
                                      numericInput(inputId = "unexposednondisease8", label = "unexposed people without disease",value = 1),
                                      textOutput("populationarpercent")
                                      
                             )
                             
                  ),
                  tabPanel(title = "Graphs")
                  
)



server <- function(input, output) 
  {
  output$introduction<-renderText({"Epidemiology is the study and analysis of the patterns, causes, and effects of health and disease conditions in defined populations. It is the cornerstone of public health, and shapes policy decisions and evidence-based practice by identifying risk factors for disease and targets for preventive healthcare."})
output$rateratio<-renderText({cbind("Rate Ratio",crude.Rate(crude.table(input$exposeddisease2,input$unexposeddisease2,input$exposedpt2,input$ubexposedpt2),measure = "IRR"), ", ",rate.ci(crude.table(input$exposeddisease2,input$unexposeddisease2,input$exposedpt2,input$ubexposedpt2),95,measure = "IRR"))})
output$ratedifference<-renderText({cbind("Rate Difference",crude.Rate(crude.table(input$exposeddisease1,input$unexposeddisease1,input$exposedpt1,input$ubexposedpt1),measure = "IRD"), ", ",rate.ci(crude.table(input$exposeddisease1,input$unexposeddisease1,input$exposedpt1,input$ubexposedpt1),95,measure = "IRD"))})
output$oddsratio<- renderText({OR(tablex(input$exposeddisease4,input$unexposeddisease4,input$exposednondisease4,input$unexposednondisease4))})
output$attributablerisk<-renderText({AR(tablex(input$exposeddisease5,input$unexposeddisease5,input$exposednondisease5,input$unexposednondisease5))})
output$attributableriskpercent<-renderText({ARpercent(tablex(input$exposeddisease6,input$unexposeddisease6,input$exposednondisease6,input$unexposednondisease6))})
output$populationar<-renderText({PAR(tablex(input$exposeddisease7,input$unexposeddisease7,input$exposednondisease7,input$unexposednondisease7))})
output$populationarpercent<-renderText({PARpercent(tablex(input$exposeddisease8,input$unexposeddisease8,input$exposednondisease8,input$unexposednondisease8))})

}

shinyApp(ui = ui, server = server)