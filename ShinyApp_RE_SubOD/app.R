rm(list=ls())
library(shiny)
library(shinydashboard)
source("R_functions.R")

options(digits = 3)

# Define UI ----
ui <- fluidPage(
  titlePanel("Sub-optimal Designs"),
  navbarPage(
    h4(em("Relative efficiency")),
    #################################
    ##-----First Page - Method-----##
    #################################
    tabPanel(strong("Model choice"),
             fluidRow(column(width=3, offset = 0,
                             h3("Variables choice"),
                             radioButtons("age", label="Age (quantitative variable) included as",
                                          choices = c("Linear",
                                                      "Quadratic"
                                                      ),
                                          selected = "Linear"
                                          ),
                             radioButtons("sex", label="Sex (categorical variable)",
                                          choices = c("Yes",
                                                      "No"
                                          ),
                                          selected = "No"
                             ),
                             radioButtons("education", label="Education (categorical or quantitative variable)",
                                          choices = c("Yes",
                                                      "No"
                                          ),
                                          selected = "No"
                             ),
                             conditionalPanel(condition="input.education=='Yes'",
                                              radioButtons("educationQ", label="Education",
                                                           choices = c("Quantitative",
                                                                       "Categorical"
                                                                       ),
                                                           selected = "Categorical"
                                                          )
                                              )
                             ),
                      column(width=3,offset=0,h3("Variable levels"),
                             radioButtons("agelev", label="Age levels",
                                          choices = c("Uniform",
                                                      "Equidistant"
                                                      ),
                                          selected = "Uniform"
                                          ),
                             conditionalPanel(condition="input.education=='Yes'",
                                              conditionalPanel(condition="input.educationQ=='Quantitative'",
                                                               radioButtons("edulev", label="Education levels",
                                                                            choices = c("Uniform",
                                                                                        "Equidistant"
                                                                                        ),
                                                                            selected = "Uniform"
                                                                            )
                                                               )
                                              )
                             ),
                      column(width=3,offset=0,h3("Effects choice"),
                             conditionalPanel(condition="input.education=='Yes'",
                                              conditionalPanel(condition="input.educationQ=='Quantitative'",
                                                               radioButtons("quadratic",label="Quadratic effects (education)",
                                                                            choices = c("Yes",
                                                                                        "No"
                                                                                        ),
                                                                            selected = "No"
                                                                            )
                                                               )
                                              ),
                             conditionalPanel(condition="input.education=='Yes' | input.sex=='Yes'",
                                              radioButtons("interactions",label="Interaction effect(s)?",
                                                           choices = c("Yes, model with all possible interactions",
                                                                       "No"
                                                                       ),
                                                           selected = "No"
                                                           )
                                              
                                              )
                             ),
                      column(width=3,offset=0,h3("Norm statistic choice"),
                             radioButtons("score",label="Score of Interest:",
                                          choices = c("z-score",
                                                      "Percentile Rank",
                                                      "Mahalanobis Distance"
                                                      ),
                                          selected = "z-score"
                                          )
                             )
                      )
             ),
    ######################################
    ##-----Second Page - Parameters-----##
    ######################################
     tabPanel(strong("Input Parameters"),
              fixedPage(
#                absolutePanel(
#                  actionButton(inputId = "AB", label = "Go!"),bottom="5%",height="5%",left="47%",width="6%"
#                  ),
                sidebarPanel(width=7,offset=0,h4("General Parameters"),
                             conditionalPanel(condition="input.score=='z-score'",
                               numericInput("z_score",label="z-score:",min=-6,max=6,step=0.001,value=1.96)
                             ),
                             conditionalPanel(condition="input.score=='Percentile Rank'",
                                              numericInput("pr",label="Percentile Rank:",min=-0,max=100,step=0.01,value=95)
                             ),
                             conditionalPanel(condition="input.score=='Mahalanobis Distance'",
                                              numericInput("MD",label="Mahalanobis Distance:",min=0,max=Inf,step=0.001,value=10)
                             )
                             ),
                sidebarPanel(width=7,offset=0,h4("Age Parameters"),
                             numericInput("age_min", label = "Minimum age:",min = 1, max=100, step= 1, value = 20),
                             numericInput("age_max", label = "Maximum age:",min = 1, max=100, step= 1, value = 80),
                             conditionalPanel(condition = "input.agelev=='Uniform'",
                                              numericInput("age_granularity",label="Age granularity (be careful, the app may crash if you set large values because of memory overload):", min=2,max=Inf,step=1,value=10)
                                              ),
                             conditionalPanel(condition = "input.agelev=='Equidistant'",
                                              numericInput("g_age",label="Number of age groups:", min=2,max=Inf,step=1,value=10)
                                              ),
                             conditionalPanel(condition="input.agelev=='Equidistant' && input.education=='Yes' && input.educationQ=='Categorical'",
                                              numericInput("radius",label="Histogram radius from number of age groups",min=2,max=10,value=5)
                                              )
                             ),
                sidebarPanel(width=5,offset=0,h4("Education Parameters"),
                             conditionalPanel(condition = "input.education=='Yes'",
                                              conditionalPanel(condition="input.educationQ=='Quantitative'",
                                                               numericInput("edu_min", label = "Minimum education:",min = 1, max=100, step= 1, value = 6),
                                                               numericInput("edu_max", label = "Maximum education:",min = 1, max=100, step= 1, value = 25),
                                                               conditionalPanel(condition = "input.edulev=='Uniform'",
                                                                                numericInput("edu_granularity",label = "Education granularity (be careful, the app may crash if you set large values because of memory overload):", min=2,max=Inf,step= 1, value=10)
                                                                                ),
                                                               
                                                               conditionalPanel(condition = "input.edulev=='Equidistant'",
                                                                                numericInput("g_edu",label="Number of education groups:", min=2,max=Inf,step=1,value=5)
                                                                                )
                                                               ),
                                              conditionalPanel(condition="input.educationQ=='Categorical'",
                                                               sliderInput("qedu",label="Number of education levels:",min=2,max=10,value=5,step=1)
                                                               )
                                              )
                             ),
                sidebarPanel(width=5,offset=0,h4("Sex Parameters"),
                             conditionalPanel(condition = "input.sex=='Yes'",
                                              sliderInput("qsex",label="Number of sex levels:",min=0,max=10,value=2,step=1)
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
                              textOutput("RE")
                              )
                       ),
              fluidRow(column(width=4, offset = 0,
                              textOutput("IN")
                              )
                       ),
              plotOutput(outputId = "SubOptPlot")
              # conditionalPanel(condition="input.AB>0 && input.agelev=='Equidistant' && input.education=='Yes' && input.educationQ=='Categorical' ",
              #   box(title="Relative efficiency ",#for varying values of the number of groups",
              #     #status= "warning",
              #     #background = "teal",
              #     #solidHeader = T,
              #     plotOutput(outputId = "SubOptPlot"),
              #     width=12,
              #     #collapsible=T,
              #     #collapsed=T,
              #     footer="The numbers above the observed points indicate the required increase of sample size as compared to the optimal design (in percentage)"
              #   )
              # )
              )
    )
 )

# Define server logic ----
server <- function(input, output) {
  #observe({
  observe({
    if (input$Quit) 
      stopApp()
  })
  observeEvent(input$AB,{
    #### ----------------------------------------------------------------------- ####
    ## Create model in text format. Feed it to the final function see lines 294 on ##
    #### ----------------------------------------------------------------------- ####
    if(input$age=='Linear'){
      model="~Age"#args<-list(model=eval(parse(text=model)), min_age=20, max_age=80, Age_levels=c("Uniform"), Z=1.96)
      #do.call(RelEff,args)
      if(input$sex=='Yes'){
        model=paste(model,"+ factor(Sex)")
        if(input$education=='Yes'){
          if(input$educationQ=='Categorical'){
            model=paste(model,"+ factor(Education)")
            if(input$interactions=="Yes, model with all possible interactions"){
              model=paste(model,"+ Age*factor(Education) + Age*factor(Sex) + factor(Sex)*factor(Education) + Age*factor(Sex)*factor(Education)")
            }
          }
          if(input$educationQ=='Quantitative'){
            model=paste(model,"+ Education")
            if(input$interactions=="Yes, model with all possible interactions"){
              model=paste(model,"+ Age*Education + Age*factor(Sex) + factor(Sex)*Education + Age*factor(Sex)*Education")
            }
            if(input$quadratic=='Yes'){
              model=paste(model,"+ I(Education^2)")
              if(input$interactions=="Yes, model with all possible interactions"){
                model=paste(model,"+ Age*I(Education^2) + factor(Sex)*I(Education^2) + Age*factor(Sex)*I(Education^2)")
              }
            }
          }else{
            }
        }else{
          if(input$interactions=="Yes, model with all possible interactions"){
            model=paste(model,"+ Age*factor(Sex)")
          }
        }
      }else{
        if(input$education=='Yes'){
          if(input$educationQ=='Categorical'){
            model=paste(model,"+ factor(Education)")
            if(input$interactions=="Yes, model with all possible interactions"){
              model=paste(model,"+ Age*factor(Education)")
            }
          }
          if(input$educationQ=='Quantitative'){
            model=paste(model,"+ Education")
            if(input$interactions=="Yes, model with all possible interactions"){
              model=paste(model,"+ Age*Education")
            }
            if(input$quadratic=='Yes'){
              model=paste(model,"+ I(Education^2)")
              if(input$interactions=="Yes, model with all possible interactions"){
                model=paste(model,"+ Age*I(Education^2)")
              }
            }
          }#else{}
        }
      }
    }else if(input$age=='Quadratic'){
      model="~Age+I(Age^2)"
      if(input$sex=='Yes'){
        model=paste(model,"+ factor(Sex)")
        if(input$education=='Yes'){
          if(input$educationQ=='Categorical'){
            model=paste(model,"+ factor(Education)")
            if(input$interactions=="Yes, model with all possible interactions"){
              model=paste(model,"+ Age*factor(Education) + Age*factor(Sex) + factor(Sex)*factor(Education) + Age*factor(Sex)*factor(Education) + I(Age^2)*factor(Education) + I(Age^2)*factor(Sex) + I(Age^2)*factor(Sex)*factor(Education)")
              }
          }
          if(input$educationQ=='Quantitative'){
            model=paste(model,"+ Education")
            if(input$interactions=="Yes, model with all possible interactions"){
              model=paste(model,"+ Age*Education + Age*factor(Sex) + factor(Sex)*Education + Age*factor(Sex)*Education + I(Age^2)*Education + I(Age^2)*factor(Sex) + I(Age^2)*factor(Sex)*Education")
            }
            if(input$quadratic=='Yes'){
              model=paste(model,"+ I(Education^2)")
              if(input$interactions=="Yes, model with all possible interactions"){
                model=paste(model,"+ Age*I(Education^2) + factor(Sex)*I(Education^2) + Age*factor(Sex)*I(Education^2) + I(Age^2)*factor(Sex)*I(Education^2)")
              }
            }
          }#else{}
        }else{
          if(input$interactions=="Yes, model with all possible interactions"){
            model=paste(model,"+ Age*factor(Sex) + I(Age^2)*factor(Sex)")
          }
        }
      }else{
        if(input$education=='Yes'){
          if(input$educationQ=='Categorical'){
            model=paste(model,"+ factor(Education)")
            if(input$interactions=="Yes, model with all possible interactions"){
              model=paste(model,"+ Age*factor(Education) + I(Age^2)*factor(Education)")
            }
          }
          if(input$educationQ=='Quantitative'){
            model=paste(model,"+ Education")
            if(input$interactions=="Yes, model with all possible interactions"){
              model=paste(model,"+ Age*Education + I(Age^2)*Education")
            }
            if(input$quadratic=='Yes'){
              model=paste(model,"+ I(Education^2)")
              if(input$interactions=="Yes, model with all possible interactions"){
                model=paste(model,"+ Age*I(Education^2) + I(Age^2)*I(Education^2)")
              }
            }
          }#else{}
        }
      }
    }
    output$RE=renderText(paste("Relative Efficiency = ",model))
    #### ----------------------------------------------------------------------- ####
    ##   Probably not the best way of doing this. Create another if structure to   ##
    ##   pass the model and all the parameters to the main function                ##
    #### ----------------------------------------------------------------------- ####
    
    if(input$agelev=="Equidistant"){
      ##############################
      ##          Case 1          ##
      ##############################
      if(input$sex=="No" && input$education=="No"){
        if(input$score=="z-score"){
          args<-list(model=eval(parse(text=model)),
                     min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, Z=input$z_score)
          aux=do.call(RelEff,args)
          output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
          output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
        }else if(input$score=="Percentile Rank"){
          args<-list(model=eval(parse(text=model)),
                     min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, PR=input$pr)
          aux=do.call(RelEff,args)
          output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
          output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
        }else {
          args<-list(model=eval(parse(text=model)),
                     min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, Mahala=input$MD)
          aux=do.call(RelEff,args)
          output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
          output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
        }
      }
      ##############################
      ##          Case 3          ##
      ##############################
      if(input$sex=="Yes" && input$education=="No"){
        if(input$score=="z-score"){
          args<-list(model=eval(parse(text=model)),
                     min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, Z=input$z_score,q_sex=input$qsex)
          aux=do.call(RelEff,args)
          output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
          output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
        }else if(input$score=="Percentile Rank"){
          args<-list(model=eval(parse(text=model)),
                     min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, PR=input$pr,q_sex=input$qsex)
          aux=do.call(RelEff,args)
          output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
          output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
        }else{
          args<-list(model=eval(parse(text=model)),
                     min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, Mahala=input$MD,q_sex=input$qsex)
          aux=do.call(RelEff,args)
          output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
          output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
        }
      }
      ##############################
      ##        Cases 5,6,7       ##
      ##############################
      if(input$sex=="No" && input$education=="Yes"){
        ##############################
        ##          Case 5          ##
        ##############################
        if(input$educationQ=="Categorical"){
          if(input$score=="z-score"){
            args<-list(model=eval(parse(text=model)),
                       min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, Z=input$z_score,
                       min_edu=input$edu_min,max_edu=input$edu_max,q_edu=input$qedu)
            aux=do.call(RelEff,args)
            output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
            output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
          }else if(input$score=="Percentile Rank"){
            args<-list(model=eval(parse(text=model)),
                       min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, PR=input$pr,
                       min_edu=input$edu_min,max_edu=input$edu_max,q_edu=input$qedu)
            aux=do.call(RelEff,args)
            output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
            output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
          }else {
            args<-list(model=eval(parse(text=model)),
                       min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, Mahala=input$MD,
                       min_edu=input$edu_min,max_edu=input$edu_max,q_edu=input$qedu)
            aux=do.call(RelEff,args)
            output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
            output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
          }
        }else{
          if(input$edulev=="Equidistant"){
            ##############################
            ##          Case 6          ##
            ##############################
            if(input$score=="z-score"){
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, Z=input$z_score,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),g_edu=input$g_edu)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }else if(input$score=="Percentile Rank"){
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, PR=input$pr,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),g_edu=input$g_edu)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }else {
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, Mahala=input$MD,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),g_edu=input$g_edu)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }
          }else{
            ##############################
            ##          Case 7          ##
            ##############################
            if(input$score=="z-score"){
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, Z=input$z_score,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),edu_granularity=input$edu_granularity)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }else if(input$score=="Percentile Rank"){
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, PR=input$pr,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),edu_granularity=input$edu_granularity)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }else {
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, Mahala=input$MD,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),edu_granularity=input$edu_granularity)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }
          }
        }
      }
      ##############################
      ##       Cases 11,12,13     ##
      ##############################
      if(input$sex=="Yes" && input$education=="Yes"){
        ##############################
        ##          Case 11         ##
        ##############################
        if(input$educationQ=="Categorical"){
          if(input$score=="z-score"){
            args<-list(model=eval(parse(text=model)),
                       min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, Z=input$z_score,
                       min_edu=input$edu_min,max_edu=input$edu_max,q_edu=input$qedu,q_sex=input$qsex)
            aux=do.call(RelEff,args)
            output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
            output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
          }else if(input$score=="Percentile Rank"){
            args<-list(model=eval(parse(text=model)),
                       min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, PR=input$pr,
                       min_edu=input$edu_min,max_edu=input$edu_max,q_edu=input$qedu,q_sex=input$qsex)
            aux=do.call(RelEff,args)
            output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
            output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
          }else {
            args<-list(model=eval(parse(text=model)),
                       min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, Mahala=input$MD,
                       min_edu=input$edu_min,max_edu=input$edu_max,q_edu=input$qedu,q_sex=input$qsex)
            aux=do.call(RelEff,args)
            output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
            output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
          }
        }else{
          if(input$edulev=="Equidistant"){
            ##############################
            ##          Case 12         ##
            ##############################
            if(input$score=="z-score"){
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, Z=input$z_score,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),g_edu=input$g_edu,q_sex=input$qsex)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }else if(input$score=="Percentile Rank"){
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, PR=input$pr,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),g_edu=input$g_edu,q_sex=input$qsex)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }else {
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, Mahala=input$MD,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),g_edu=input$g_edu,q_sex=input$qsex)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }
          }else{
            ##############################
            ##          Case 13         ##
            ##############################
            if(input$score=="z-score"){
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, Z=input$z_score,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),edu_granularity=input$edu_granularity,
                         q_sex=input$qsex)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }else if(input$score=="Percentile Rank"){
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, PR=input$pr,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),edu_granularity=input$edu_granularity,
                         q_sex=input$qsex)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }else {
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),g_age=input$g_age, Mahala=input$MD,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),edu_granularity=input$edu_granularity,
                         q_sex=input$qsex)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }
          }
        }
      }
    }
    if(input$agelev=="Uniform"){
      ##############################
      ##          Case 2          ##
      ##############################
      if(input$sex=="No" && input$education=="No"){
        if(input$score=="z-score"){
          args<-list(model=eval(parse(text=model)),
                     min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, Z=input$z_score)
          aux=do.call(RelEff,args)
          output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
          output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
        }else if(input$score=="Percentile Rank"){
          args<-list(model=eval(parse(text=model)),
                     min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, PR=input$pr)
          aux=do.call(RelEff,args)
          output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
          output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
        }else {
          args<-list(model=eval(parse(text=model)),
                     min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, Mahala=input$MD)
          aux=do.call(RelEff,args)
          output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
          output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
        }
      }
      ##############################
      ##          Case 4          ##
      ##############################
      if(input$sex=="Yes" && input$education=="No"){
        if(input$score=="z-score"){
          args<-list(model=eval(parse(text=model)),
                     min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, Z=input$z_score,q_sex=input$qsex)
          aux=do.call(RelEff,args)
          output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
          output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
        }else if(input$score=="Percentile Rank"){
          args<-list(model=eval(parse(text=model)),
                     min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, PR=input$pr,q_sex=input$qsex)
          aux=do.call(RelEff,args)
          output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
          output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
        }else{
          args<-list(model=eval(parse(text=model)),
                     min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, Mahala=input$MD,q_sex=input$qsex)
          aux=do.call(RelEff,args)
          output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
          output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
        }
      }
      ##############################
      ##       Cases 8,9,10       ##
      ##############################
      if(input$sex=="No" && input$education=="Yes"){
        ##############################
        ##          Case 8          ##
        ##############################
        if(input$educationQ=="Categorical"){
          if(input$score=="z-score"){
            args<-list(model=eval(parse(text=model)),
                       min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, Z=input$z_score,
                       min_edu=input$edu_min,max_edu=input$edu_max,q_edu=input$qedu)
            aux=do.call(RelEff,args)
            output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
            output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
          }else if(input$score=="Percentile Rank"){
            args<-list(model=eval(parse(text=model)),
                       min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, PR=input$pr,
                       min_edu=input$edu_min,max_edu=input$edu_max,q_edu=input$qedu)
            aux=do.call(RelEff,args)
            output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
            output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
          }else {
            args<-list(model=eval(parse(text=model)),
                       min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, Mahala=input$MD,
                       min_edu=input$edu_min,max_edu=input$edu_max,q_edu=input$qedu)
            aux=do.call(RelEff,args)
            output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
            output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
          }
        }else{
          if(input$edulev=="Equidistant"){
            ##############################
            ##          Case 9          ##
            ##############################
            if(input$score=="z-score"){
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, Z=input$z_score,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),g_edu=input$g_edu)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }else if(input$score=="Percentile Rank"){
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, PR=input$pr,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),g_edu=input$g_edu)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }else {
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, Mahala=input$MD,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),g_edu=input$g_edu)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }
          }else{
            ##############################
            ##          Case 10          ##
            ##############################
            if(input$score=="z-score"){
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, Z=input$z_score,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),edu_granularity=input$edu_granularity)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }else if(input$score=="Percentile Rank"){
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, PR=input$pr,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),edu_granularity=input$edu_granularity)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }else {
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, Mahala=input$MD,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),edu_granularity=input$edu_granularity)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }
          }
        }
      }
      ##############################
      ##       Cases 14,15,16     ##
      ##############################
      if(input$sex=="Yes" && input$education=="Yes"){
        ##############################
        ##          Case 14         ##
        ##############################
        if(input$educationQ=="Categorical"){
          if(input$score=="z-score"){
            args<-list(model=eval(parse(text=model)),
                       min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, Z=input$z_score,
                       min_edu=input$edu_min,max_edu=input$edu_max,q_edu=input$qedu,q_sex=input$qsex)
            aux=do.call(RelEff,args)
            output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
            output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
          }else if(input$score=="Percentile Rank"){
            args<-list(model=eval(parse(text=model)),
                       min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, PR=input$pr,
                       min_edu=input$edu_min,max_edu=input$edu_max,q_edu=input$qedu,q_sex=input$qsex)
            aux=do.call(RelEff,args)
            output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
            output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
          }else {
            args<-list(model=eval(parse(text=model)),
                       min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, Mahala=input$MD,
                       min_edu=input$edu_min,max_edu=input$edu_max,q_edu=input$qedu,q_sex=input$qsex)
            aux=do.call(RelEff,args)
            output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
            output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
          }
        }else{
          if(input$edulev=="Equidistant"){
            ##############################
            ##          Case 15         ##
            ##############################
            if(input$score=="z-score"){
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, Z=input$z_score,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),g_edu=input$g_edu,q_sex=input$qsex)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }else if(input$score=="Percentile Rank"){
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, PR=input$pr,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),g_edu=input$g_edu,q_sex=input$qsex)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }else {
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, Mahala=input$MD,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),g_edu=input$g_edu,q_sex=input$qsex)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }
          }else{
            ##############################
            ##          Case 16         ##
            ##############################
            if(input$score=="z-score"){
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, Z=input$z_score,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),edu_granularity=input$edu_granularity,
                         q_sex=input$qsex)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }else if(input$score=="Percentile Rank"){
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, PR=input$pr,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),edu_granularity=input$edu_granularity,
                         q_sex=input$qsex)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }else {
              args<-list(model=eval(parse(text=model)),
                         min_age=input$age_min, max_age=input$age_max, Age_levels=c(input$agelev),age_granularity=input$age_granularity, Mahala=input$MD,
                         min_edu=input$edu_min,max_edu=input$edu_max,Edu_levels=c(input$edulev),edu_granularity=input$edu_granularity,
                         q_sex=input$qsex)
              aux=do.call(RelEff,args)
              output$RE<-renderText(paste("Relative Efficiency = ",aux[1]))
              output$IN<-renderText(paste("Increase sample size by ",aux[2],"percent"))
            }
          }
        }
      }
    }
    if(input$agelev=='Equidistant' && input$education=='Yes' && input$educationQ=='Categorical'){
        output$SubOptPlot <- renderPlot({
          isolate({
            if(input$score=="z-score"){
              if(input$sex=="Yes"){
                REplot(model=eval(parse(text=model)), min_age=input$age_min, max_age=input$age_max,
                      agegroup=input$g_age, q_sex=input$qsex, q_edu=input$qedu,Z=input$z_score, radius=input$radius)
              }else{
                REplot(model=eval(parse(text=model)), min_age=input$age_min, max_age=input$age_max,
                     agegroup=input$g_age, q_edu=input$qedu, Z=input$z_score, radius=input$radius)
              }
            }else if(input$score=="Percentile Rank"){
              if(input$sex=="Yes"){
                REplot(model=eval(parse(text=model)), min_age=input$age_min, max_age=input$age_max,
                      agegroup=input$g_age, q_sex=input$qsex, q_edu=input$qedu,PR=input$pr, radius=input$radius)
              }else{
                REplot(model=eval(parse(text=model)), min_age=input$age_min, max_age=input$age_max,
                      agegroup=input$g_age, q_edu=input$qedu, PR=input$pr, radius=input$radius)
              }
            }else{
              if(input$sex=="Yes"){
                REplot(model=eval(parse(text=model)), min_age=input$age_min, max_age=input$age_max,
                      agegroup=input$g_age, q_sex=input$qsex, q_edu=input$qedu, Mahala=input$MD, radius=input$radius)
              }else{
                REplot(model=eval(parse(text=model)), min_age=input$age_min, max_age=input$age_max,
                      agegroup=input$g_age, q_edu=input$qedu, Mahala=input$MD, radius=input$radius)
              }
            }
          })
        })
    }else{output$SubOptPlot <- NULL}
  }
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)
