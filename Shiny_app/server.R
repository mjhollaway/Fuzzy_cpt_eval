#Load some functions required for initialisation.
library(rmarkdown)
library(plotly)
library(grid)

#Set the station names for the UI.
CIRES_statnames <- c("SWISSCAMP_01","CRAWFORD_02","NASAU_03","HUMBOLDT_05","SUMMIT_06","TUNUN_07","DYE2_08","JAR_09","SADDLE_10",
                     "SOUTHDOME_11","NASAE_12","NGRIP_14","NASASE_15","JAR2_17")

#Render the markdown notebooks so that the underlying functions are avialable to the shiny app.
#Now render the notebook to make its functions available.
tempReport <- file.path(tempdir(), "Fuzzy_cpt_single_site.Rmd")
file.copy("../notebooks/Fuzzy_cpt_single_site.Rmd",tempReport,overwrite=TRUE)
rmarkdown::render(tempReport,quiet=TRUE)

#Also load in the simple map data for greenland.
#Format for plotting also.
data(wrld_simpl)
greenland <-  wrld_simpl[wrld_simpl$NAME == "Greenland", ] 
green_area=extent(-74,-10,58,85)
green_area360=extent(286,350,58,85)

#Read in the pre-extracted model data and make available to the shiny session.
data_allsite_file <- '../data/CPTS_DATA_PREPROC.csv'
data_allsite_read <- read.table(data_allsite_file,sep=',',header=TRUE)

#Function to format the data for plotting.
cpt_poly_plt <- function(df_fzz_in,ts_date_first,ts_date_last){

     #Define a numeric array to hold the cpt plotting points for the fuzzy triangles.
     cpt_fuzzy_plt <- ts_date_first
 
     #Loop over the number of cpts (rows) in the current df and format accordingly.
     for (ii in 1:nrow(df_fzz_in)){
        cpt_fuzzy_plt <- c(cpt_fuzzy_plt,c(as.Date(df_fzz_in$CPT_Date_left[ii]),as.Date(df_fzz_in$CPT_Date[ii]),as.Date(df_fzz_in$CPT_Date_right[ii])))
     }

     #Now append the full t_series length as a final point on the end.
     cpt_fuzzy_plt <- c(cpt_fuzzy_plt,ts_date_last)

     #Create an output df.
     df_fuzzy_out <- data.frame(x=cpt_fuzzy_plt,y=c(0,rep(c(0,1,0),nrow(df_fzz_in)),0))

     return(df_fuzzy_out)

}

#Read in the site information for plotting station locations on map. 
Site_info <- read.table("../data/CIRES_statinfo.csv",sep=',',header=TRUE)

#Server function to process the changepoint analysis.
#This holds all of the relevant R shiny processes.
#This abstracts all the functions from the shiny code (could put the calling functions in a seperate module.) 
server <- function(input, output) {
   
   #Initialise the reactive values.
   cpts_proc_all <- reactiveValues(cpts_CI_site_mod=data.frame(),cpts_CI_site_obs=data.frame(),mod_tseries_stats=data.frame(),
                                   curr_station_plt="",cpts_mod_eval_metrics=data.frame(),
                                   create_plot=FALSE,publish_summary=FALSE,show_analysis_but=FALSE,show_downld_tab=FALSE)

   #Create an reactive object to read in the data.
   data_in <- reactiveValues(df_in=data.frame())

   #Create a reactive object to hold the processed data for download.
   data_out <- reactiveValues(obs_cpt_summ=data.frame(),mod_cpt_summ=data.frame(),mod_eval_summary=data.frame(),mod_sumstats_main=data.frame(),
                              obs_seg_summ=data.frame(),mod_seg_summ=data.frame(),app_settings=data.frame())

   #Create an observe event to modify the reactive values based on settings.
   observeEvent(input$getmodeldata, {
 
     #Extract the data for the chosen model/station.
     #Use a with progress loop to track timing on data extract. 
     withProgress(message = paste('Extracting model data and obs data for: ',input$station_name,sep=''), value = 0, {
       #Now extract the data.frame to process for the current site/model.
       #Can drop the site name here for processing.
       Site_ext_ind  <- which(data_allsite_read$Site_Name == input$station_name)
       data_in$df_in <- data.frame(Dates=as.Date(data_allsite_read$Dates[Site_ext_ind]),
                                  OBS_t2m=data_allsite_read$OBS_t2m[Site_ext_ind],
                                  MOD_t2m=data_allsite_read$ERA5_t2m[Site_ext_ind]
                                  )

       #For output also fit the AR1 model using ARIMA and add the residuals to the df_in dataframe.
       obs_ar1 <- arima(data_in$df_in$OBS_t2m[which(!is.na(data_in$df_in$OBS_t2m))],order=c(1,0,0),method='CSS-ML')
       mod_ar1 <- arima(data_in$df_in$MOD_t2m[which(!is.na(data_in$df_in$OBS_t2m))],order=c(1,0,0),method='CSS-ML') #Use the missing obs here as only obs has missing data.
       data_in$df_in$obs_resid                                        <- rep(NA,length(data_in$df_in$OBS_t2m))
       data_in$df_in$obs_resid[which(!is.na(data_in$df_in$OBS_t2m))] <- residuals(obs_ar1)
       data_in$df_in$mod_resid                                        <- rep(NA,length(data_in$df_in$MOD_t2m))
       data_in$df_in$mod_resid[which(!is.na(data_in$df_in$OBS_t2m))] <- residuals(mod_ar1)

     }) 

     #Reset the plots.
     cpts_proc_all$show_analysis_but <- TRUE

     #Also populate the current station reactive variable.
     cpts_proc_all$curr_station_plt <- input$station_name

   #Close the observe event
   })
   
   #Add an observe event that resets the publish summary logical (clears all plots) if station name is changed.
   #This will also trigger the disabling of the analysis and download action buttons.
   #The download tab will also be removed here. 
   observe({

      if (cpts_proc_all$curr_station_plt != input$station_name){
        cpts_proc_all$publish_summary   <- FALSE
        cpts_proc_all$show_analysis_but <- FALSE
        cpts_proc_all$create_plot       <- FALSE
        cpts_proc_all$show_downld_tab   <- FALSE
      }

      #Also update the station information for the map.
      #Use the station name input to pick out the lat/lon of the current station.
	    #This will move on the map when the station is changed.
      curr_station_id        <- which(Site_info$Name == input$station_name)
      curr_station_loc_plt	 <- data.frame(lon=Site_info$lon[curr_station_id],lat=Site_info$lat[curr_station_id],Name=input$station_name)

      #Can also update the map here to display the selected station.
      output$station_map        <-  renderPlotly({

        g <- ggplot() +
             geom_polygon(data=greenland,  aes(x=long, y=lat, group=group),fill=NA, color="black") +                                         #Add GRL map.
             geom_point(data=Site_info, aes(x=lon, y=lat, group=Name), size=3, colour="red", show.legend=TRUE) +                             #Add the station points.
		   	     geom_point(data=curr_station_loc_plt, aes(x=lon, y=lat, group=Name), size=5, colour="blue", show.legend=TRUE) +                 #Add the station that is currently selected.
		         theme_bw() 

        #Show the plot.
	    	ggplotly(g)

      })

   #Close the observe event.
   })  

   #Add the help text box to the app. 
   observeEvent(input$showhelp, {
      showModal(modalDialog(
        title = "Help",
        h4("Choose model and station to compare"),
        p("Please choose which site to process the analysis for."),
        p("The map shows the available sites with the current selected site highlighted in blue."),
        p("The data is retrieved by pressing the extract button."),
        p("Pressing the extract button will enable the analyze button."),
        p("Everytime a new site is selected the app will reset and all graphs and tables will be cleared."),
        p("The extract button will need to be pressed again to enable analysis to be run for the new site."),
        p("Note: The download tab will disappear if a new site is selected and analysis will need to be re-run to enable results and code download")
        #easyClose = TRUE,
        #footer = NULL
      ))
   })

   #Add help text for analysis button.
   observeEvent(input$showhelp2, {
      showModal(modalDialog(
        title = "Help",
        p("Please select the number of samples to generate uncertainty intervals on changepoint locations."),
        p("To run the analysis press the Analyze button."),
        p("On completion of the analysis the results and code will be available from the downloads tab."),
        p("Note: The analysis button will be disabled if the data extract step has not been completed first.")
      ))
   })

   #Add help text for the select cpt to look at more closely.
   observeEvent(input$showhelp3, {
      showModal(modalDialog(
        title = "Help",
        p("Please select the format of the code notebook you wish to download and enter your chosen filename."),
        p("Available formats are RMarkdown (.Rmd) or Jupyter (.ipynb)")
      ))
   })

   #Add help text for the download tab.
   observeEvent(input$showhelp4, {
      showModal(modalDialog(
        title = "Help",
        p("Please input chosen filename and press download button to obtain an spreadsheet containing results for the selected site."),
      ))
   })

   

   #Create an observed event instance that edits the reactive values if the input site it changed.
   observeEvent(input$run_boot, {

     #Run the analysis in the following order:
     #1) Process the observations cpts and associated CIS
     #2) Process the model cpts and associated CIS
     #3) Evaluate the timing of the cpts.

     #Display the progress bar of the computation - OBS
     withProgress(message = 'Processing confidence intervals for obs dataset', value = 0, {

       #Call the function that processes the cpts for the desired site.
       #For this version of the app the settings are hardwired to that of the paper (AR1, cpts on variance, 100 min seg len).
       OBS_currsite_proc <- Sites_CI_proc(data_in$df_in,'OBS',100,1,'variance',input$N_reps,input$station_name)   
    
     #Close the with progress bar.
     })
	                                            
     #Display the progress bar of the computation - MOD
     withProgress(message = 'Processing confidence intervals for mod dataset', value = 0, {

       #Call the function that processes the cpts for the desired site.
       MOD_currsite_proc <- Sites_CI_proc(data_in$df_in,'MOD',100,1,'variance',input$N_reps,input$station_name)   
    
     #Close the with progress bar.
     })

     #Display the progress bar for the evaluation.
     withProgress(message = 'Processing model evaluation', value = 0, {

       #Call the function that processes the cpts for the desired site.
       MODvsOBS_eval <- Sites_CI_eval(data_in$df_in,OBS_currsite_proc$all_cpt_summ,MOD_currsite_proc$all_cpt_summ)
    
     #Close the with progress bar.
     })


     #Add to the reactive values.
     cpts_proc_all$cpts_mod_eval_metrics     <- MODvsOBS_eval$mod_eval_summary   #Evaluation metrics
     cpts_proc_all$cpts_CI_site_mod          <- MOD_currsite_proc$all_cpt_summ   #Model cpts
     cpts_proc_all$cpts_CI_site_obs          <- OBS_currsite_proc$all_cpt_summ   #Obs cpts
	   cpts_proc_all$mod_tseries_stats         <- MODvsOBS_eval$mod_sumstats_main	 #Summary stats for current model.

     #Also pass the data to the data_out reactive values for downloading the data.
     data_out$obs_cpt_summ        <- OBS_currsite_proc$all_cpt_summ
     data_out$mod_cpt_summ        <- MOD_currsite_proc$all_cpt_summ
     data_out$obs_seg_summ        <- OBS_currsite_proc$all_seg_summ
     data_out$mod_seg_summ        <- MOD_currsite_proc$all_seg_summ
     data_out$mod_eval_summary    <- MODvsOBS_eval$mod_eval_summary
     data_out$mod_sumstats_main   <- MODvsOBS_eval$mod_sumstats_main

     #Also record the current settings from the app for output.
     data_out$app_settings        <- data.frame(Current_site=input$station_name,N_reps=input$N_reps)

     #Set the create plot logical to TRUE.
     cpts_proc_all$create_plot <- TRUE

     #Also set the publish summary logical to be TRUE.
     cpts_proc_all$publish_summary <- TRUE

     #Also show the download data tab.
     cpts_proc_all$show_downld_tab <- TRUE

   #Close the observe event.
   })

   #Write out the output table.
    
   observe({

      #Use the publish_summary logical to decide whether or not to add the tablulated data to the screen.
      #This is reset if a new file is loaded in.
      #This will work for all tables produced by the analysis.
      #This call will also enable/disable the download button.
      if (cpts_proc_all$publish_summary == TRUE){

        #Create a data frame that holds the observed cpts locations(and CIs), the intersecting model cpts (if any) and the corresponding evluation weight.
        wgt_stats_df <- data.frame(obs_ci_lwr=character(),obs_cpt=character(),obs_ci_upr=character(),
                                   mod_ci_lwr=character(),mod_cpt=character(),mod_ci_upr=character(),
                                   evaluation_score=numeric())

        #Loop over the evaluation weights df and populate the table for output.
        for (cc in 1:nrow(cpts_proc_all$cpts_mod_eval_metrics)){

           #Check to see if there is a model intersect, if not populate the table with no intersect. 
           if (cpts_proc_all$cpts_mod_eval_metrics$mod_cpt_insct[cc] == 0){

             wgt_stats_temp <- data.frame(obs_ci_lwr=as.character(cpts_proc_all$cpts_CI_site_obs$CPT_Date_left[cpts_proc_all$cpts_mod_eval_metrics$obs_cpt_no[cc]]),
                                          obs_cpt   =as.character(cpts_proc_all$cpts_CI_site_obs$CPT_Date[cpts_proc_all$cpts_mod_eval_metrics$obs_cpt_no[cc]]),
                                          obs_ci_upr=as.character(cpts_proc_all$cpts_CI_site_obs$CPT_Date_right[cpts_proc_all$cpts_mod_eval_metrics$obs_cpt_no[cc]]),
                                          mod_ci_lwr="No Intersect",
                                          mod_cpt   ="No Intersect",
                                          mod_ci_upr="No Intersect",
                                          evaluation_score=round(cpts_proc_all$cpts_mod_eval_metrics$cpt_eval_score[cc],2)
                                          )


           } else {

            wgt_stats_temp <- data.frame(obs_ci_lwr=as.character(cpts_proc_all$cpts_CI_site_obs$CPT_Date_left[cpts_proc_all$cpts_mod_eval_metrics$obs_cpt_no[cc]]),
                                         obs_cpt   =as.character(cpts_proc_all$cpts_CI_site_obs$CPT_Date[cpts_proc_all$cpts_mod_eval_metrics$obs_cpt_no[cc]]),
                                         obs_ci_upr=as.character(cpts_proc_all$cpts_CI_site_obs$CPT_Date_right[cpts_proc_all$cpts_mod_eval_metrics$obs_cpt_no[cc]]),
                                         mod_ci_lwr=as.character(cpts_proc_all$cpts_CI_site_mod$CPT_Date_left[cpts_proc_all$cpts_mod_eval_metrics$mod_cpt_insct[cc]]),
                                         mod_cpt   =as.character(cpts_proc_all$cpts_CI_site_mod$CPT_Date[cpts_proc_all$cpts_mod_eval_metrics$mod_cpt_insct[cc]]),
                                         mod_ci_upr=as.character(cpts_proc_all$cpts_CI_site_mod$CPT_Date_right[cpts_proc_all$cpts_mod_eval_metrics$mod_cpt_insct[cc]]),
                                         evaluation_score=round(cpts_proc_all$cpts_mod_eval_metrics$cpt_eval_score[cc],2)
                                         )

           }

           #Now append to the main data.frame,
           wgt_stats_df <- rbind(wgt_stats_df,wgt_stats_temp)

           #Finally filter out the dates based on the chosen plot range.
           #wgt_stats_df <- wgt_stats_df[which((as.Date(wgt_stats_df$obs_cpt) >= input$plt_date_rng[1]) & (as.Date(wgt_stats_df$obs_cpt) <= input$plt_date_rng[2])),]

        }


        #Output summary evaluation weights.
        output$Wgt_stats          <- DT :: renderDataTable(wgt_stats_df,options=list(pageLength = 5))

        #Output the summary statistics
	      output$Seg_stats          <- DT :: renderDataTable(format(cpts_proc_all$mod_tseries_stats, digits=2, nsmall=2),options=list(pageLength = 5))       

      #Otherwise publish a blank table.
      } else {

        #Output summary evaluation weights.
        output$Wgt_stats          <- DT :: renderDataTable(NULL,options=list(pageLength = 5))

        #Output the summary statistics
	      output$Seg_stats          <- DT :: renderDataTable(NULL,options=list(pageLength = 5))

      }

      #Enable/disable the anaysis action button based on station input change.
      if (cpts_proc_all$show_analysis_but == TRUE){
        shinyjs::enable("run_boot") 
      } else {
        shinyjs::disable("run_boot")
      }

      #Enable/disable the download button based on chosen outputs.
      if (cpts_proc_all$show_downld_tab == TRUE){
        appendTab(inputId = "mainTabset",
        tabPanel("Download", value = "downloader",
           column(3,h3("Code Notebook download"),
               textInput(inputId = "nb_name",label="Enter name for output notebook",value="notebook_name"),
               selectInput(inputId = "nb_type",label = "Available code notebooks",choices=list('RMarkdown','jupyter'),selected=as.list('RMarkdown')),
               downloadButton("downloadNb", "Download selected notebook",style="color: white; background-color: grey; border-color: black; font-style: bold"),
               actionButton("showhelp3",label="Help",style="color: white; background-color: grey; border-color: black; font-style: bold"),
               h3(""),
               textInput("fnameout",label="Enter output file name",value="filename"),
               downloadButton("downloadData", "Download results summary",style="color: white; background-color: grey; border-color: black; font-style: bold"),
               actionButton("showhelp4",label="Help",style="color: white; background-color: grey; border-color: black; font-style: bold")
              )
        )
        )
      } else {
        
        removeTab(inputId = "mainTabset", target = "downloader")
      }

      output$tsPlot_MODvsOBS    <- renderPlot({

        #Check to see if the CIs have been processed. 
          
        #If they have create the plot if do nothing.

        if (cpts_proc_all$create_plot == TRUE){
           
           #Process the cpts locations for plotting.
           OBS_cpt_fuzzy <- cpt_poly_plt(cpts_proc_all$cpts_CI_site_obs,data_in$df_in$Dates[1],data_in$df_in$Dates[nrow(data_in$df_in)])
           MOD_cpt_fuzzy <- cpt_poly_plt(cpts_proc_all$cpts_CI_site_mod,data_in$df_in$Dates[1],data_in$df_in$Dates[nrow(data_in$df_in)])

           #Extract the data for plotting.
           OBS_df_plt <- data.frame(Dates=data_in$df_in$Dates,Air_Temp=data_in$df_in$OBS_t2m,Time_series='OBS')
           MOD_df_plt <- data.frame(Dates=data_in$df_in$Dates,Air_Temp=data_in$df_in$MOD_t2m,Time_series='ERA5')

           #Now create a series of plots to summarise the time series.
           #These will be as follows: 1) Model versus obs TS, 2) Obs residuals and CPTS, 3) Mod residuals and CPTS and 4) CPT fuzzy evaluation.
       
           #Create and display the original time series plot.
           ts_comp_orig <- ggplot() + 
                           geom_line(MOD_df_plt, mapping=aes(x=Dates, y=Air_Temp, colour=Time_series), size=1, show.legend = TRUE) + 
                           geom_line(OBS_df_plt, mapping=aes(x=Dates, y=Air_Temp, colour=Time_series), size=1, show.legend = TRUE) + 
                           scale_colour_manual(values=c("blue","red")) +
                           scale_y_continuous(name='Temperature (\u00b0C)',
                                              breaks=c(-60,-40,-20,0,20), 
                                              labels=c('-60','-40','-20','0','20'), 
                                              limits=c(-60,20),
                                              expand=c(0,0)) +
                           scale_x_date(name='Date', limits=c(input$plt_date_rng[1],input$plt_date_rng[2]),expand=c(0,0)) +
                           labs(title="Original Time Series") + 
                           theme_classic()                 +
                           theme(axis.text.x       = element_text(face='bold',size=12),
                                 axis.title.x      = element_text(face='bold', size=14),
                                 axis.text.y       = element_text(face='bold',size=12),
                                 axis.title.y      = element_text(face='bold', size=14),
                           )
           
           #Now create the osb resid plot with corresponding cpts.
           #Now create the obs resid plot with the corresponding cpts.
           ts_resid_obs <- ggplot() +
                           geom_line(data_in$df_in, mapping=aes(x=Dates, y=obs_resid), size=1, colour='black') +
                           geom_vline(cpts_proc_all$cpts_CI_site_obs, mapping=aes(xintercept=as.Date(CPT_Date)), color='red',size=1) +
                           scale_y_continuous(name='Temperature (\u00b0C)',
                                              breaks=c(-30,-20,-10,0,10,20,30), 
                                              labels=c('-30','-20','-10','0','10','20','30'), 
                                              limits=c(-30,30),
                                              expand=c(0,0)) +
                           scale_x_date(name='Date', limits=c(input$plt_date_rng[1],input$plt_date_rng[2]),expand=c(0,0)) + 
                           labs(title="Time series residuals (observations)") +
                           theme_classic()                 +
                           theme(axis.text.x       = element_text(face='bold',size=12),
                                 axis.title.x      = element_text(face='bold', size=14),
                                 axis.text.y       = element_text(face='bold',size=12),
                                 axis.title.y      = element_text(face='bold', size=14),
                           )

           #Do the same for the model.
           ts_resid_mod <- ggplot() +
                           geom_line(data_in$df_in, mapping=aes(x=Dates, y=mod_resid), size=1, colour='black') +
                           geom_vline(cpts_proc_all$cpts_CI_site_mod, mapping=aes(xintercept=as.Date(CPT_Date)), color='blue',size=1) +
                           scale_y_continuous(name='Temperature (\u00b0C)',
                                              breaks=c(-30,-20,-10,0,10,20,30), 
                                              labels=c('-30','-20','-10','0','10','20','30'), 
                                              limits=c(-30,30),
                                              expand=c(0,0)) +
                           scale_x_date(name='Date', limits=c(input$plt_date_rng[1],input$plt_date_rng[2]),expand=c(0,0)) +
                           labs(title="Time series residuals (climate model)") + 
                           theme_classic()                 +
                           theme(axis.text.x       = element_text(face='bold',size=12),
                                 axis.title.x      = element_text(face='bold', size=14),
                                 axis.text.y       = element_text(face='bold',size=12),
                                 axis.title.y      = element_text(face='bold', size=14),
                           )

           #Finally produce the cpts fuzzy plot for model evaluation.
           cpt_fuzzy <- ggplot() + 
                        geom_polygon(MOD_cpt_fuzzy, mapping=aes(x=x, y=y), col='blue', fill='blue') +
                        geom_polygon(OBS_cpt_fuzzy, mapping=aes(x=x, y=y), col='red', fill='red', alpha=0.5) +
                        geom_path(MOD_cpt_fuzzy, mapping=aes(x=x, y=y), col='black') +
                        geom_path(OBS_cpt_fuzzy, mapping=aes(x=x, y=y), col='black') +
                        scale_y_continuous(name='Membership',
                                           breaks=c(0.0,0.5,1.0), 
                                           labels=c('0.0','0.5','1.0'), 
                                           limits=c(0.0,1.0),
                                           expand=c(0,0)) +
                        scale_x_date(name='Date', limits=c(input$plt_date_rng[1],input$plt_date_rng[2]),expand=c(0,0)) +
                        labs(title="Fuzzy changepoint evaluation") +
                        theme_classic()                 +
                        theme(axis.text.x       = element_text(face='bold',size=12),
                              axis.title.x      = element_text(face='bold', size=14),
                              axis.text.y       = element_text(face='bold',size=12),
                              axis.title.y      = element_text(face='bold', size=14),
                        )

           #Covert the plots to grob objects and make the same width to align them.
           ts_comp_orig_grob <- ggplotGrob(ts_comp_orig)
           ts_resid_obs_grob <- ggplotGrob(ts_resid_obs)     
           ts_resid_mod_grob <- ggplotGrob(ts_resid_mod)      
           cpt_fuzzy_grob    <- ggplotGrob(cpt_fuzzy)
  
           ts_resid_obs_grob$widths <- ts_comp_orig_grob$widths
           ts_resid_mod_grob$widths <- ts_comp_orig_grob$widths
           cpt_fuzzy_grob$widths    <- ts_comp_orig_grob$widths

           #Combine into a column of plots and output to shiny app.
           g_out = rbind(ts_comp_orig_grob, ts_resid_obs_grob, ts_resid_mod_grob, cpt_fuzzy_grob, size = "last")
  
           #Display the plot.
           grid.draw(g_out)

        } else {

           #Display a placeholder plot.
           ts_comp_plot <- ggplot() +
                           labs(title='PRESS ANALYZE BUTTON TO GENERATE PLOT') +     
                           theme(plot.title  = element_text(face='bold', hjust=0.5, size=20),
                                 axis.line=element_blank(),
                                 axis.text.x=element_blank(),
                                 axis.text.y=element_blank(),
                                 axis.ticks=element_blank(),
                                 axis.title.x=element_blank(),
                                 axis.title.y=element_blank(),
                                 legend.position="none",
                                 panel.background=element_blank(),
                                 panel.border=element_blank(),
                                 panel.grid.major=element_blank(),
                                 panel.grid.minor=element_blank(),
                                 plot.background=element_blank())

          #Display the plot
          ts_comp_plot

        }


      #Close the render plot.   
       
      })      

    #Download the results summary.
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$fnameout,".xlsx", sep="")
      },
      content = function(file) {
      
       #Create the excel workbook and write to file.
       wb <- createWorkbook(creator='Me',title='Cpts Shiny App Output')
       #Add the worksheets
       addWorksheet(wb,"SETTINGS")
       addWorksheet(wb,"TIME_SERIES")
       addWorksheet(wb,"OBS_CPT_SUMMARY")
       addWorksheet(wb,"MOD_CPT_SUMMARY")
       addWorksheet(wb,"OBS_SEG_SUMMARY")
       addWorksheet(wb,"MOD_SEG_SUMMARY")
       addWorksheet(wb,"MOD_EVAL_SUMMARY")
       addWorksheet(wb,"MOD_SUMSTATS")
       #Write the data.
       writeData(wb, "SETTINGS", data_out$app_settings)
       writeData(wb, "TIME_SERIES", data_in$df_in)
       writeData(wb, "OBS_CPT_SUMMARY",  data_out$obs_cpt_summ)
       writeData(wb, "MOD_CPT_SUMMARY",  data_out$mod_cpt_summ)
       writeData(wb, "OBS_SEG_SUMMARY",  data_out$obs_seg_summ)
       writeData(wb, "MOD_SEG_SUMMARY",  data_out$mod_seg_summ)
       writeData(wb, "MOD_EVAL_SUMMARY", data_out$mod_eval_summary)
       writeData(wb, "MOD_SUMSTATS",     data_out$mod_sumstats_main)
       #Save the workbook.
       saveWorkbook(wb, file, overwrite=TRUE)
      }
    )

    #Download the R notebooks.
    output$downloadNb <- downloadHandler(
      filename = function(){
        #Use if statement to decide on output file name based on chosen type.
        if (input$nb_type == 'RMarkdown'){
          paste(input$nb_name,".Rmd",sep="")
        } else if (input$nb_type == 'jupyter'){
          paste(input$nb_name,".ipynb",sep="")
        }
      },
      content = function(file) {

          #Now check the notebook type for output and copy the correct one for download.
          if (input$nb_type == 'RMarkdown'){
            file.copy("../notebooks/Fuzzy_cpt_single_site.Rmd", file, overwrite=TRUE)
          } else if (input$nb_type == 'jupyter'){
            file.copy("../notebooks/Fuzzy_cpt_single_site.ipynb", file, overwrite=TRUE)
          }

      }
    )

   #Close the observe statement.
   })

#Close the server function
}