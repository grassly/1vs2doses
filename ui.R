library(shiny)
library(markdown)
library(tidyr)
library(ggplot2)
library(DT)
library(shinyWidgets)

#pre-calcs and read in default data
ve_range<-seq(from =100, to = 0, by=-1)
risk_groups<-read.csv("risk_groups.csv", stringsAsFactors = F)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Impact of COVID-19 vaccination on mortality for 1 or 2 dose prioritisation strategies"),
        
        sidebarLayout(
            
            sidebarPanel(
                sliderTextInput("ve1_p", 
                                h4("1-dose efficacy against mortality (%)*"), 
                                choices = ve_range,
                                selected = ve_range[c(26,51)],
                                grid = TRUE
                ),

                sliderTextInput("ve2_p", 
                                h4("2-dose efficacy against mortality (%)*"), 
                                choices = ve_range,
                                selected = ve_range[c(6,11)],
                                grid = TRUE
                ),
                
                h5("*slider range indicates efficacy at 3-11 weeks (leftmost) and at 12+ weeks (rightmost)"),
                
                sliderTextInput("mixed_2nd_dose", 
                                h4("For mixed strategy, prioritise 2nd doses up to"), 
                                choices = risk_groups$Risk.group,
                                selected = risk_groups$Risk.group[2],
                                grid = TRUE
                ),
                
                sliderInput("foi", "Average weekly infection attack rate (%)",
                            min = 0.1, max = 2,
                            value = 1, step = 0.1),
                
                awesomeRadio(
                    inputId = "foi_trend",
                    label = "Trend in attack rate over time", 
                    choices = c("constant", "increasing", "decreasing"),
                    selected = "constant"
                ),
                
                h6("Note: this model is for guidance only and does not imply endorsement by WHO or others for a particular vaccination strategy. See 'About' for further details.")
                
            ),
            
            mainPanel(
                tabsetPanel(
                    tabPanel("Groups",
                             DT::DTOutput("rg_datatable")
                             ), 
                    tabPanel("Supply",
                             DTOutput("supply_datatable")
                    ),                     
                    tabPanel("Coverage",
                             fluidRow(
                                 column(width = 6,
                                        plotOutput("plot_cov1")
                                        ),
                                 column(width = 6,
                                        plotOutput("plot_cov2")
                                 ),
                                 column(width = 6,
                                        plotOutput("plot_cov3")
                                 ),
                                 column(width = 6,
                                        plotOutput("plot_cov4")
                                 ),
                                 column(width = 6,
                                        plotOutput("plot_cov5")
                                 ),
                                 column(width = 6,
                                        plotOutput("plot_cov6")
                                 )
                             )
                    ), 
                    tabPanel("Impact",
                             h3("Deaths averted plot"),
                             plotOutput("plot_impact"),
                             br(),
                             h3("Deaths averted table"),
                             tableOutput("tab_impact"),
                             br(),
                             h3("Coverage achieved in each group with 1 and 2 doses"),
                             plotOutput("plot_cov_final")
                    ), 
                    tabPanel("About",
                             withMathJax(includeMarkdown("about.md"))
                    )
                )
            )
        )
))
