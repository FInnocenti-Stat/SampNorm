rm(list=ls())
library(shiny)
library(shinydashboard)
source("R_functions_ver02.R")

options(digits = 3)

# Define UI ----
ui <- fluidPage(
  titlePanel("Multivariate norming"),
  navbarPage(
    h4(em("Norming")),
    #################################
    ##-----First Page - Method-----##
    #################################
    tabPanel(strong("Method choice"),
             fluidRow(column(width=4, offset = 0,
                             h3("Approach choice"),
                             radioButtons("approach", label="Approach:",
                                          choices = c("Hypothesis Testing Approach",
                                                      "Confidence Interval Approach"
                                                      ),
                                          selected = "Hypothesis Testing Approach"
                                          )
                             ),
                      column(width=4,offset=0,h3("Goal"),
                             conditionalPanel(condition="input.approach=='Hypothesis Testing Approach'",
                                              radioButtons("aimHT",label="Aim:",
                                                           choices = c("Sample Size",
                                                                       "Power"),
                                                           selected="Sample Size"
                                                           )
                                              ),
                             conditionalPanel(condition="input.approach=='Confidence Interval Approach'",
                                              radioButtons("aimCI",label="Aim:",
                                                           choices = c("Sample Size",
                                                                       "Margin of Error"),
                                                           selected="Sample Size"
                                                           )
                                              )
                             )#),
                      #column(width=4,offset=0,h3("Design"),
                             #conditionalPanel(condition="input.approach=='Confidence Interval Approach'",
                                              #conditionalPanel(condition="input.aimCI=='Sample Size'",
                                                               #radioButtons("desHT",label="Design:",
                                                                            #choices = c("Optimal",
                                                                                        #"Uniform",
                                                                                        #"Equidistant"),
                                                                            #selected="Optimal"
                                                                            #)
                                                               #)
                                              #)
                             #)
                      )
             ),
    ######################################
    ##-----Second Page - Parameters-----##
    ######################################
    tabPanel(strong("Input Parameters"),
             fixedPage(
                      sidebarPanel(width=7,offset=0,h4("General Parameters"),
                                   sliderInput("alpha",label=h3(HTML("Set the probability of type I error (&alpha;)")),min=0,max=0.5,value=0.05,step=0.001),
                                   numericInput("k", label = "Number of Predictors:",min = 1, max=10, step= 1, value = 1),
                                   numericInput("Mc",label="Mahalanobis score:",min=0,max=100,step=0.01,value=0)
                                   ),
                      sidebarPanel(width=5,offset=7,h4("Approach Specific Parameters"),
                                   conditionalPanel(condition="input.approach=='Hypothesis Testing Approach'",
                                                    conditionalPanel(condition="input.aimHT=='Sample Size'",
                                                                     sliderInput("gamma",label=h3(HTML("Set the power of the test (1-&gamma;)")), min = 0.5, max = 0.999, step = 0.05, value=0.9)
                                                                     ),
                                                    conditionalPanel(condition="input.aimHT=='Power'",
                                                                     numericInput("N",label=h3("Sample Size"),min=0,max=Inf,step=1,value=100)
                                                                     ),
                                                    numericInput("tau",label=h3(HTML("Effect Size (&tau;) on the Mahalanobis score")),min=0,max=8,step=0.1,value=1)
                                                    ),
                                   conditionalPanel(condition="input.approach=='Confidence Interval Approach'",
                                                    conditionalPanel(condition="input.aimCI=='Sample Size'",
                                                                     sliderInput("assP",label=h3(HTML("Set the assurance probability (1-&beta;)")), min = 0.5, max = 0.999, step = 0.05, value=0.5),
                                                                     numericInput("tau_CI",label=h3(HTML("Effect Size (&tau;) on the Mahalanobis score")),min=0,max=8,step=0.1,value=1)
                                                                     ),
                                                    conditionalPanel(condition="input.aimCI=='Margin of Error'",
                                                                     sliderInput("assP_Moe",label=h3(HTML("Set the assurance probability (1-&beta;)")), min = 0.5, max = 0.999, step = 0.05, value=0.5),
                                                                     numericInput("N_CI",label=h3("Sample Size"),min=0,max=Inf,step=1,value=100)
                                                                     )
                                                    )
                                   )
                      )
             ),
    ##################################
    ##-----Third Page - Results-----##
    ##################################
    tabPanel(strong("Results"),
             absolutePanel(#See here for icons https://fontawesome.com/search?o=r&m=free&f=classic%2Cbrands%2Cduotone%2Csharp%2Csharp-duotone
               actionButton(inputId = "AB", label = "Go!",icon = icon("calculator")),bottom="10%",height="5%",left="47%",width="6%" #icon = icon("thumbs-up"), other options laptop, desktop, floppy-disk, computer
             ),
             absolutePanel(
               actionButton(inputId = "Quit", label="STOP!",icon = icon("power-off")),bottom="5%",height="5%",left="46%",width="6%"#other options circle-stop, x-mark, rectangle-xmark, circle-xmark, square x-mark
             ),
             fluidRow(column(width=4, offset = 0,
                             #####################################
                             ##----Output Hypothesis Testing----##
                             #####################################
                             conditionalPanel(condition="input.approach=='Hypothesis Testing Approach'",
                                              conditionalPanel(condition="input.aimHT=='Sample Size'",
                                                               textOutput("SSHT")
                                                               ),
                                              conditionalPanel(condition="input.aimHT=='Power'",
                                                               textOutput("PHT")
                                              )
                                              ),
                             ######################################
                             ##----Output Confidence Interval----##
                             ######################################
                             conditionalPanel(condition="input.approach=='Confidence Interval Approach'",
                                              conditionalPanel(condition="input.aimCI=='Sample Size'",
                                                               textOutput("SSCI")
                                              ),
                                              conditionalPanel(condition="input.aimCI=='Margin of Error'",
                                                               textOutput("MoECI")
                                              )
                             )
                             )
                      )
             )
    )
)

# Define server logic ----
server <- function(input, output) {
  
  observe({
    if (input$Quit) 
      stopApp()
  })
  
observeEvent(input$AB,{
  #############################################
  ##----Prepare Output Hypothesis Testing----##
  #############################################
  if(input$approach=='Hypothesis Testing Approach'){
    if(input$aimHT=='Sample Size'){
        out=N_star_HypTest_Mahala(alpha=input$alpha,gamma=1-input$gamma,k=input$k,MD_c=input$Mc,tau=input$tau)
        output$SSHT=renderText({paste("Number of participants required = ",out)})
        #output$SSHT=renderText("HT - SS - z")
      }else if(input$aimHT=='Power'){
          out=Power_Mahala(alpha=input$alpha,k=input$k,MD_c=input$Mc,tau=input$tau,N=input$N)
          output$PHT=renderText({paste("Power = ",out)})
          #output$PHT=renderText("HT - P - z")
      }
    }
  ##############################################
  ##----Prepare Output Confidence Interval----##
  ##############################################
  if(input$approach=='Confidence Interval Approach'){
    if(input$aimCI=='Sample Size'){
      #if(input$desHT=="Optimal"){
        out=Samp_MD_AP(alpha=input$alpha,k=input$k,MD0=input$Mc,tau=input$tau_CI,beta=1-input$assP)
        output$SSCI=renderText({paste("Number of participants required = ",out)})
        #output$SSCI=renderText("CI - SS - z")
      #}else if(input$desHT=="Uniform"){
        #output$SSCI=renderText("CI - SS - z - Uniform")
      #}else if(input$desHT=="Equidistant"){
        #output$SSCI=renderText("CI - SS - z - Equidistant")
      #}
    }else if(input$aimCI=='Margin of Error'){
        out=Tau_CI_AP_Mahala(alpha=input$alpha,k=input$k,MD0=input$Mc,N=input$N_CI,beta=1-input$assP_Moe)
        output$MoECI=renderText({paste("Margin of Error = ",out)})
        #output$MoECI=renderText("CI - P - PR")
    }
  }
})

}

# Run the app ----
shinyApp(ui = ui, server = server)
