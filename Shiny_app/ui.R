library(shiny)
library(plotly)
library(shinyjs)
library(shinycssloaders)

#Set the station names for the UI.
CIRES_statnames <- c("SWISSCAMP_01","CRAWFORD_02","NASAU_03","HUMBOLDT_05","SUMMIT_06","TUNUN_07","DYE2_08","JAR_09","SADDLE_10",
                     "SOUTHDOME_11","NASAE_12","NGRIP_14","NASASE_15","JAR2_17")

#Set up the user interface with file upload.
ui <- fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
    tags$style(
      HTML(".shiny-notification {
              height: 100px;
              width: 600px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 400px);;
            }
           "
      )
    )
    ),

    #Add a title Panel to the app.
    titlePanel("Climate model evaluation tool: A fuzzy changepoint based approach"),

    #Create tabs at the top of the app to hold an explorer tab and a data download tab. 
    tabsetPanel(id = "mainTabset",
      tabPanel("Changepoint explorer", value = "explorer", 

        fluidRow(
	      column(3,h3("Changepoint explorer"),
               selectInput(inputId = "station_name",label = "Choose station",choices=as.list(CIRES_statnames),selected=as.list(CIRES_statnames[[1]])),
               actionButton("getmodeldata",label="Extract",style="color: white; background-color: grey; border-color: black; font-style: bold"),
               actionButton("showhelp",label="Help",style="color: white; background-color: grey; border-color: black; font-style: bold"),
               h3(),
               numericInput(inputId = "N_reps", label = "Number of samples to generate CI", value=1000, min=500, max=2000),
               actionButton(inputId= 'run_boot', 'Analyze', style="color: white; background-color: grey; border-color: black; font-style: bold"),
               actionButton("showhelp2",label="Help",style="color: white; background-color: grey; border-color: black; font-style: bold")
              ),
        column(3,h3("Station Locations"),withSpinner(plotlyOutput(outputId = "station_map")))
	      ),
    
	    #Output - Changepoint comparison plot.
	    fluidRow(column(12,h3("Climate model versus observations"))),
      fluidRow(column(6,h3(),dateRangeInput(inputId = 'plt_date_rng', label="Plot date range", start="2000-01-01", end="2017-12-31", min="2000-01-01", max="2017-12-31",format="dd/mm/yyyy",separator="to"))),           
      fluidRow(column(12,h3(),withSpinner(plotOutput(outputId = "tsPlot_MODvsOBS", height=700)))),

      #Show the overall performance of the climate model at the selected site.
      fluidRow(
        column(3,h3("Overall model performance"),DT::dataTableOutput("Seg_stats"))
      ),

      #Show the evaluation weights at each cpt.
      fluidRow(
        column(9,h3("Changepoint evaluation weights"),DT::dataTableOutput("Wgt_stats"))
      )


      #Close the first tab on the ui.
      )

    #Close the main tabset.
    )

#Close the UI.
)