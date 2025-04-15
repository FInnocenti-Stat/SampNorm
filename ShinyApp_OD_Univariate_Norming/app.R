rm(list=ls())
library(shiny)
library(shinydashboard)
source("R_functions_ver02.R")

options(digits = 3)

# Define UI ----
ui <- fluidPage(
  titlePanel("Univariate norming"),
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
                                          ),
                             radioButtons("score",label="Norm statistic of Interest:",
                                          choices = c("z-score",
                                                      "Percentile Rank"
                                                      )
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
                     #%column(width=4,offset=0,h3("Design"),
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
                                   conditionalPanel(condition="input.score=='z-score'",
                                                    numericInput("zc",label="z-score:",min=-4,max=4,step=0.01,value=0)
                                                    ),
                                   conditionalPanel(condition="input.score=='Percentile Rank'",
                                                    numericInput("pr",label="Percentile Rank:",min=0,max=100,step=1,value=50)
                                                    )
                                   ),
                      sidebarPanel(width=5,offset=7,h4("Approach Specific Parameters"),
                                   conditionalPanel(condition="input.approach=='Hypothesis Testing Approach'",
                                                    conditionalPanel(condition="input.aimHT=='Sample Size'",
                                                                     sliderInput("gamma",label=h3(HTML("Set the power of the test (1-&gamma;)")), min = 0.5, max = 0.999, step = 0.05, value=0.9)
                                                                     ),
                                                    conditionalPanel(condition="input.aimHT=='Power'",
                                                                     numericInput("N",label=h3("Sample Size"),min=0,max=Inf,step=1,value=100)
                                                                     ),
                                                    numericInput("delta",label=h3(HTML("Effect Size (&tau;)")),min=0,max=8,step=0.1,value=1)
                                                    ),
                                   conditionalPanel(condition="input.approach=='Confidence Interval Approach'",
                                                    conditionalPanel(condition="input.aimCI=='Sample Size'",
                                                                     sliderInput("assP",label=h3(HTML("Set the assurance probability (1-&beta;)")), min = 0.5, max = 0.999, step = 0.05, value=0.5),
                                                                     numericInput("tau",label=h3(HTML("Margin of Error")),min=0,max=8,step=0.1,value=1)
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
      if(input$score=="z-score"){
        out=N_star_HypTest_Z(alpha=input$alpha,gamma=1-input$gamma,k=input$k,Zc=input$zc,delta=input$delta)
        output$SSHT=renderText({paste("Number of participants required = ",out)})
        #output$SSHT=renderText("HT - SS - z")
      }else if(input$score=="Percentile Rank"){
        out=N_star_HypTest_PR(alpha=input$alpha,gamma=1-input$gamma,k=input$k,PRc=input$pr,delta=input$delta)
        output$SSHT=renderText({paste("Number of participants required = ",out)})
        #output$SSHT=renderText("HT - SS - PR")
          }
      }else if(input$aimHT=='Power'){
        if(input$score=="z-score"){
          out=Power_Z(alpha=input$alpha,N=input$N,k=input$k,Zc=input$zc,delta=input$delta)
          output$PHT=renderText({paste("Power = ",out)})
          #output$PHT=renderText("HT - P - z")
        }else if(input$score=="Percentile Rank"){
          out=Power_PR(alpha=input$alpha,N=input$N,k=input$k,PRc=input$pr,delta=input$delta)
          output$PHT=renderText({paste("Power = ",out)})
          #output$PHT=renderText("HT - P - PR")
        }
      }
    }
  ##############################################
  ##----Prepare Output Confidence Interval----##
  ##############################################
  if(input$approach=='Confidence Interval Approach'){
    if(input$aimCI=='Sample Size'){
      if(input$score=="z-score"){
        #if(input$desHT=="Optimal"){
        #output$SSCI=renderText({paste("alpha",input$alpha,"k",input$k,"z0",input$zc,"margin of error",input$tau)})
          out=Samp_Z_AP(alpha=input$alpha,k=input$k,Z0=input$zc,tau=input$tau,beta=1-input$assP)
          ##print(out)
          output$SSCI=renderText({paste("Number of participants required = ",out)})
          #output$SSCI=renderText("CI - SS - z")
        #}else if(input$desHT=="Uniform"){
         # output$SSCI=renderText("CI - SS - z - Uniform")
        #}else if(input$desHT=="Equidistant"){
          #output$SSCI=renderText("CI - SS - z - Equidistant")
        #}
      }else if(input$score=="Percentile Rank"){
        #if(input$desHT=="Optimal"){
          out=Samp_PR_AP(alpha=input$alpha,k=input$k,PR0=input$pr,tau=input$tau,beta=1-input$assP)
          output$SSCI=renderText({paste("Number of participants required = ",out)})
          #output$SSCI=renderText("CI - SS - PR")
        #}else if(input$desHT=="Uniform"){
          #output$SSCI=renderText("CI - SS - PR - Uniform")
        #}else if(input$desHT=="Equidistant"){
          #output$SSCI=renderText("CI - SS - PR - Equidistant")
        #}
      }
    }else if(input$aimCI=='Margin of Error'){
      if(input$score=="z-score"){
        out=Tau_CI_AP_Z(alpha=input$alpha,k=input$k,Z0=input$zc,N=input$N_CI,beta=1-input$assP_Moe)
        output$MoECI=renderText({paste("Margin of Error = ",out)})
        #output$MoECI=renderText("CI - P - z")
      }else if(input$score=="Percentile Rank"){
        out=Tau_AP_CI_PR(alpha=input$alpha,k=input$k,PR0=input$pr,N=input$N_CI,beta=1-input$assP_Moe)
        output$MoECI=renderText({paste("Margin of Error = ",out)})
        #output$MoECI=renderText("CI - P - PR")
      }
    }
  }
})

}

# Run the app ----
shinyApp(ui = ui, server = server)
