#Functions to determine the changepoints in the time series, calculate the associated confidence intervals and evaluate the timing of changepoint capture.

#Load all required packages.
library(tseries)
library(lubridate)
library(changepoint)
library(dplyr)
library(sets)
library(openxlsx)


#**Function 1:** - Function to return fuzzy intersection of 2 numbers.
#Generic function used in code.
cpts_insct_siml <- function(fuzzy_int_1,fuzzy_int_2,t_series_len){

    #To conserve speed set the universe to be the max/min of the 2 cpts being compared.
    fuzzy_uni_min <- min(fuzzy_int_1$ci.left,fuzzy_int_2$ci.left)
    fuzzy_uni_max <- max(fuzzy_int_1$ci.right,fuzzy_int_2$ci.right)

    #Convert the intervals into fuzzy numbers.
    fuzzy_num_1 <- fuzzy_triangular_gset(corners=c(fuzzy_int_1$ci.left,
                                         fuzzy_int_1$cpt,fuzzy_int_1$ci.right),
                                         universe=seq(fuzzy_uni_min,fuzzy_uni_max,0.1))
 
    fuzzy_num_2 <- fuzzy_triangular_gset(corners=c(fuzzy_int_2$ci.left,
                                         fuzzy_int_2$cpt,fuzzy_int_2$ci.right),
                                         universe=seq(fuzzy_uni_min,fuzzy_uni_max,0.1))

    return(gset_similarity(fuzzy_num_1,fuzzy_num_2))


}

#**Function 2:** Function to calculate the confidence intervals (CIs) on cpts locations.
#This uses a bootstrap appraoch. 
#Essentially the time series is split into segments a each changepoint and sampled to the
#left anf right of each fitted changepoint. The AMOC method is then used to estimate the new location based
#on the new sample. confidence intervals are estimated for a chosen level for each cpt.    
cpts_confint <- function(x_series,x_series_orig,x_cpts,N_reps,n_cpts,cpt_method,AR_order){

  #First use the identified cpts to set upper and lower bnds for each 
  #segment.
  #Just use the length of the AR tseries (x_series) here.
  #Need to start from zero as segments are defined by +1
  #to end of previous seg. At start this is 0.
  x_cpts_segs_bnds <- c(0,x_cpts[1:length(x_cpts)],length(x_series))
  
  #Find the number of segments in the data to be processed.
  x_nseg <- n_cpts+1
  
  #Create a data frame to hold the summary of each segment.
  #This will also hold the summary of the changepoint locations at the start and end of each segment.
  cpts_CI_all <- data.frame(seg_lwr=as.integer(rep(NA,x_nseg)),
                          seg_lwr.ci.left=as.integer(rep(NA,x_nseg)),
                          seg_lwr.ci.right=as.integer(rep(NA,x_nseg)),
                          seg_upr=as.integer(rep(NA,x_nseg)),
                          seg_upr.ci.left=as.integer(rep(NA,x_nseg)),
                          seg_upr.ci.right=as.integer(rep(NA,x_nseg)),
                          is_cpt_lwr=as.character(rep('',x_nseg)),
                          is_cpt_upr=as.character(rep('',x_nseg)),
                          seg_mean=as.numeric(rep(NA,x_nseg)),
                          seg_mean.ci.left=as.numeric(rep(NA,x_nseg)),
                          seg_mean.ci.right=as.numeric(rep(NA,x_nseg)),
                          seg_sd=as.numeric(rep(NA,x_nseg)),
                          seg_sd.ci.left=as.numeric(rep(NA,x_nseg)),
                          seg_sd.ci.right=as.numeric(rep(NA,x_nseg)),
                          stringsAsFactors = FALSE
                          )
                            
                            
  #Define a function that bootstraps the cpts for each segment.
  #The function also returns the summary stats of each segment from the 
  #resulting cpt location for each sample. 
  cpt_boot <- function(seg_1,seg_2,cpt_method,AR_order){
          
      #Gernerate new samples for each segment with replacement.      
      seg_1_boot <- sample(seg_1, length(seg_1), replace=T)
      seg_2_boot <- sample(seg_2, length(seg_2), replace=T)
          
      #Now estimate the new changepoint (fix at 1 using AMOC).
	  #Also need to define this based on chosen cpt method.
	  if (cpt_method == 'mean'){
	    cpt_samp <- cpt.mean(c(seg_1_boot,seg_2_boot), method='AMOC', penalty='MBIC')
	  } else if (cpt_method == 'variance'){
	    cpt_samp <- cpt.var(c(seg_1_boot,seg_2_boot), method='AMOC', penalty='MBIC')
	  } else if (cpt_method == 'mean+variance'){
	    cpt_samp <- cpt.meanvar(c(seg_1_boot,seg_2_boot), method='AMOC', penalty='MBIC')
	  } 
      
      return(cpts(cpt_samp))

  #Close the bootstrap function.    
  }

  #Now loop over the changepoints and estimate the confidence intervals based on bootstrap sample.
  #Create a matrix to hold the boostrap cpt locations.
  cpt_samp_matrix              <- matrix(0,nrow=N_reps,ncol=n_cpts+2)
  cpt_samp_matrix[,1]                     <- 0
  cpt_samp_matrix[,ncol(cpt_samp_matrix)] <- length(x_series_orig)
  
  #Loop over the number of desired reps and bootstrap cpts.
  for(mm in 1:N_reps){
  
    summary_cpts <- numeric()
  
    #Bootstrap each cpt location. 
    for (ll in 1:n_cpts){

     #Get the segments for the current sample.
     #If the 1st cpt.
     if((ll == 1) & (n_cpts > 1)){
        x1_l <- 1
        x1_r <- x_cpts[ll]
        x2_l <- x_cpts[ll]+1
        x2_r <- x_cpts[ll+1]
     #if the last cpt.
     } else if ((ll > 1) & (ll == n_cpts)){
        x1_l <- x_cpts[ll-1]+1
        x1_r <- x_cpts[ll]
        x2_l <- x_cpts[ll]+1
        x2_r <- length(x_series)
     #Case where there is only 1 cpt in the segment.
     } else if ((ll == 1) & (n_cpts == 1)){
        x1_l <- 1
        x1_r <- x_cpts[ll]
        x2_l <- x_cpts[ll]+1
        x2_r <- length(x_series)
     #If any other cpt.
     } else {
        x1_l <- x_cpts[ll-1]+1
        x1_r <- x_cpts[ll]
        x2_l <- x_cpts[ll]+1
        x2_r <- x_cpts[ll+1]
     }
         
     #Extract the segments from the time series.       
     seg_1_proc <- x_series[x1_l:x1_r]
     seg_2_proc <- x_series[x2_l:x2_r]

     #Bootstrap the cpt locations.
     summary_cpts_tmp <- cpt_boot(seg_1_proc,seg_2_proc,
                                  cpt_method,AR_order)
     if (length(summary_cpts_tmp) > 0){
       summary_cpts <- append(summary_cpts,(summary_cpts_tmp+(x1_l-1)))
     } else {
       summary_cpts   <- append(summary_cpts,NA)
     }
     

    #Close loop around cpts.
    }
  
    #Append the bootstrapped cpts locations to the master array.
    cpt_samp_matrix[mm,2:(ncol(cpt_samp_matrix)-1)] <- summary_cpts
        
  #Close the loop around the number of samples.
  }
     
  #Function to calculate summary stats for each segment (mean,sd,min and max)
  seg_sumstats_fun <- function(df_row,x_tseries_orig,sumstat_proc){

    #Find the number of segments.
    nseg <- length(df_row)-1

    #Set blank variable to hold mean.
    tmpstat <- NULL

    #Loop over segments and determine mean.
    for(ll in 1:nseg){
      if ((!is.na(df_row[ll]) == TRUE) & (!is.na(df_row[ll+1]) == TRUE)){
        tmpstat[ll] <- eval(parse(text=paste(sumstat_proc,'(x_tseries_orig[(df_row[ll]+1):(df_row[ll+1])])',sep='')))
      } else {
        tmpstat[ll] <- NA
      }
    }

    return(tmpstat)

  }
  
  #Get the segment means,sd,min and max.
  #Returned matrix seems to be flipped to transpose to get 
  #correct orientation.
  cpt_samp_segmeans <- t(apply(cpt_samp_matrix,1,seg_sumstats_fun,x_series_orig,'mean'))
  cpt_samp_segsds   <- t(apply(cpt_samp_matrix,1,seg_sumstats_fun,x_series_orig,'sd'))
  
  #Get the segment means for the original set of cpts.
  x_seg_means_orig <- seg_sumstats_fun(x_cpts_segs_bnds,x_series_orig,'mean')
  x_seg_sds_orig   <- seg_sumstats_fun(x_cpts_segs_bnds,x_series_orig,'sd')
     
  #Loop over segments and calculate CIs of upper/lower ends + sumstats
  for (nn in 1:nrow(cpts_CI_all)){
       
     #Populate the master array.
     if (nn == 1){
       #Fix the lower bounds at 1 for the first segment.
       #This requires adding one to the lowest segment start (0).
       #This is because of the way the segment means are calculated.
       cpts_CI_all$seg_lwr[nn]          <- x_cpts_segs_bnds[nn]+1
       cpts_CI_all$seg_lwr.ci.left[nn]  <- x_cpts_segs_bnds[nn]+1
       cpts_CI_all$seg_lwr.ci.right[nn] <- x_cpts_segs_bnds[nn]+1
       cpts_CI_all$is_cpt_lwr[nn]       <- 'NO'
       cpts_CI_all$seg_upr[nn]          <- x_cpts_segs_bnds[nn+1]
       cpts_CI_all$seg_upr.ci.left[nn]  <- quantile(cpt_samp_matrix[,nn+1],c(0.025,0.975),type=1,na.rm=TRUE)[1]
       cpts_CI_all$seg_upr.ci.right[nn] <- quantile(cpt_samp_matrix[,nn+1],c(0.025,0.975),type=1,na.rm=TRUE)[2]
       cpts_CI_all$is_cpt_upr[nn]       <- 'YES'
     } else if (nn == length(x_cpts_segs_bnds)-1){
       cpts_CI_all$seg_lwr[nn]          <- x_cpts_segs_bnds[nn]
       cpts_CI_all$seg_lwr.ci.left[nn]  <- quantile(cpt_samp_matrix[,nn],c(0.025,0.975),type=1,na.rm=TRUE)[1]
       cpts_CI_all$seg_lwr.ci.right[nn] <- quantile(cpt_samp_matrix[,nn],c(0.025,0.975),type=1,na.rm=TRUE)[2]
       cpts_CI_all$is_cpt_lwr[nn]       <- 'YES'
       cpts_CI_all$seg_upr[nn]          <- x_cpts_segs_bnds[nn+1]
       cpts_CI_all$seg_upr.ci.left[nn]  <- x_cpts_segs_bnds[nn+1]
       cpts_CI_all$seg_upr.ci.right[nn] <- x_cpts_segs_bnds[nn+1]
       cpts_CI_all$is_cpt_upr[nn]       <- 'NO'
     } else {  
       cpts_CI_all$seg_lwr[nn]          <- x_cpts_segs_bnds[nn]
       cpts_CI_all$seg_lwr.ci.left[nn]  <- quantile(cpt_samp_matrix[,nn],c(0.025,0.975),type=1,na.rm=TRUE)[1]
       cpts_CI_all$seg_lwr.ci.right[nn] <- quantile(cpt_samp_matrix[,nn],c(0.025,0.975),type=1,na.rm=TRUE)[2]
       cpts_CI_all$is_cpt_lwr[nn]       <- 'YES'
       cpts_CI_all$seg_upr[nn]          <- x_cpts_segs_bnds[nn+1]
       cpts_CI_all$seg_upr.ci.left[nn]  <- quantile(cpt_samp_matrix[,nn+1],c(0.025,0.975),type=1,na.rm=TRUE)[1]
       cpts_CI_all$seg_upr.ci.right[nn] <- quantile(cpt_samp_matrix[,nn+1],c(0.025,0.975),type=1,na.rm=TRUE)[2]
       cpts_CI_all$is_cpt_upr[nn]       <- 'YES'
     #Close the if statement.
     }
     #Get the segment sumstats.
     #Means
     cpts_CI_all$seg_mean[nn]           <- x_seg_means_orig[nn]
     cpts_CI_all$seg_mean.ci.left[nn]   <- quantile(cpt_samp_segmeans[,nn],c(0.025,0.975),type=1,na.rm=TRUE)[1]
     cpts_CI_all$seg_mean.ci.right[nn]  <- quantile(cpt_samp_segmeans[,nn],c(0.025,0.975),type=1,na.rm=TRUE)[2]
     #SDs
     cpts_CI_all$seg_sd[nn]           <- x_seg_sds_orig[nn]
     cpts_CI_all$seg_sd.ci.left[nn]   <- quantile(cpt_samp_segsds[,nn],c(0.025,0.975),type=1,na.rm=TRUE)[1]
     cpts_CI_all$seg_sd.ci.right[nn]  <- quantile(cpt_samp_segsds[,nn],c(0.025,0.975),type=1,na.rm=TRUE)[2]
  #Close loop around segments.    
  }
     
  #Pass out the results.
  return(cpts_CI_all)

#Close the function.
}

#**Function 3:** Fucntion to process the data for the current site. 
#This function fits the AR models, determines the changepoint locations in the time series pass in. The second part runs the evluation metrics.
cpts_currts_CI <- function(data_in_proc,min_seg_proc,TS_AR_ARIMA,CPT_METHOD_PROC,N_reps_proc,ts_type){

   #Get the observed and climate model time series for the current station.
   #Only the chosen ts will be evaluated (defined by ts_type) but need both for consitency in missing data.
   obs_tseries            <- data_in_proc$OBS_t2m
   mod_tseries            <- data_in_proc$MOD_t2m

   #Pick out the actual time series to process.
   if (ts_type == 'OBS'){
     proc_tseries <- data_in_proc$OBS_t2m
   } else if (ts_type == 'MOD'){
     proc_tseries <- data_in_proc$MOD_t2m
   }
   
	 #Also extract the dates for plotting and determining time of cpts.
	 mod_dates_plt          <- data_in_proc$Dates
	
   #Find out where we have data present.
	 #Only need to do this for the obs - the model time series will be segmented in the same way for consistency.
	 #As some of the models dont go all the way to 2017 also filter out these from the analysis.
   obs_pres_logicals      <- (!is.na(obs_tseries) & !is.na(mod_tseries))
	
   #Get the segment ids.
	 obs_seg_no <- obs_pres_seg_proc(obs_pres_logicals)
	
	 #Find the individual number of segments.
	 obs_pres_segs <- sort(unique(obs_seg_no[which(obs_seg_no > 0)]))
	
   #Set up blank data frames to hold the changpoints over all segs and their confidence intervals.
   cpts_CI_site_currts <- data.frame(seg_lwr=as.integer(),
                          seg_lwr.ci.left=as.integer(),
                          seg_lwr.ci.right=as.integer(),
                          seg_upr=as.integer(),
                          seg_upr.ci.left=as.integer(),
                          seg_upr.ci.right=as.integer(),
                          is_cpt_lwr=as.character(),
                          is_cpt_upr=as.character(),
                          seg_mean=as.numeric(),
                          seg_mean.ci.left=as.numeric(),
                          seg_mean.ci.right=as.numeric(),
                          seg_sd=as.numeric(),
                          seg_sd.ci.left=as.numeric(),
                          seg_sd.ci.right=as.numeric(),
                          stringsAsFactors = FALSE
                          )

   #Loop over the segments and identify the changepoints for that segment.
   for (kk in 1:length(obs_pres_segs)){
     
      #Calculate the % we are through all segments to show on progress bar.
	    pct_proc <- 100.0*(kk/length(obs_pres_segs))
	
	    #If shiny is running increment the progress bar. 
      if (shiny::isRunning()){
	      incProgress(1/length(obs_pres_segs), detail = paste("% Complete:", round(pct_proc,0)))
      }
     
      #Find the current segment length.
      curr_seg_ids <- which(obs_seg_no == obs_pres_segs[kk])

      #If the seg length is above the minimum then find the changepoints.
      if (length(curr_seg_ids) > min_seg_proc){

        #Extract the current segment for both model and obs tseries.
        curr_seg_proc_ts <- proc_tseries[curr_seg_ids]
 
        #Fit the chosen AR model to smooth out seasonlity.
        #This is set in the UI for each time series.
		    ts_arima <- arima(curr_seg_proc_ts,c(as.numeric(TS_AR_ARIMA),0,0),method='CSS-ML')
		     
        #Get the changepoints for the current segment.
		    #Use the chosen CPT_METHOD to dermine location of changepoints.
		    if (CPT_METHOD_PROC == "mean"){
          curr_seg_cpts_ts <- cpt.mean(residuals(ts_arima),method='PELT',penalty='MBIC', minseglen =                                            min_seg_proc*0.50)
        } else if (CPT_METHOD_PROC == "variance"){
		      curr_seg_cpts_ts <- cpt.var(residuals(ts_arima),method='PELT',penalty='MBIC', minseglen =                                            min_seg_proc*0.50)
		    } else if (CPT_METHOD_PROC == "mean+variance"){
		      curr_seg_cpts_ts <- cpt.meanvar(residuals(ts_arima),method='PELT',penalty='MBIC', minseglen =                                            min_seg_proc*0.50)
		    } 
		 
        #Check to see if there are any changepoints - of there are append to the main array.
        #Also compute the cpts CIs if any exist.

        if(ncpts(curr_seg_cpts_ts) > 0){
		        curr_seg_CI_ts  <- cpts_confint(residuals(ts_arima),curr_seg_proc_ts,cpts(curr_seg_cpts_ts),
		                                         N_reps_proc,ncpts(curr_seg_cpts_ts),CPT_METHOD_PROC,TS_AR_ARIMA)
			      
		        #As the segment processing code works on individual
		        #segments need to add the base id to cpts locations
		        #to make sure they are correct.
		        
		        #Use column names to get correct cols to correct
		        seg_lwr_cols  <- grepl("seg_lwr",names(curr_seg_CI_ts))
		        seg_upr_cols  <- grepl("seg_upr",names(curr_seg_CI_ts))
		        seg_cols_corr <- which((seg_lwr_cols == TRUE) |
		                               (seg_upr_cols == TRUE))
		        curr_seg_CI_ts[,seg_cols_corr] <- curr_seg_CI_ts[,seg_cols_corr] + (curr_seg_ids[1]-1)
		        
		        cpts_CI_site_currts <- rbind(cpts_CI_site_currts,curr_seg_CI_ts)
            
        }

      #Close the check over segment length.
      }
		
   #Close loop over segments.
   }

   #Pick out the original cpts along with their CIS.
   cpts_CI_summary <- cpts_CI_site_currts[which(cpts_CI_site_currts$is_cpt_upr == 'YES'),which(seg_upr_cols == TRUE)]
   #Rename the columns names for output.
   names(cpts_CI_summary) <- c('cpt','ci.left','ci.right')
	
   #Append the dates timings of the changepoint to the data frame.    
   cpts_CI_summary$CPT_Date       <- mod_dates_plt[cpts_CI_summary$cpt]
   cpts_CI_summary$CPT_Date_left  <- mod_dates_plt[cpts_CI_summary$ci.left]    
   cpts_CI_summary$CPT_Date_right <- mod_dates_plt[cpts_CI_summary$ci.right]

   #Also add the segment upper/lower dates.
   cpts_CI_site_currts$seg_lwr_DATE       <- mod_dates_plt[cpts_CI_site_currts$seg_lwr]
   cpts_CI_site_currts$seg_lwr_DATE_left  <- mod_dates_plt[cpts_CI_site_currts$seg_lwr.ci.left]
   cpts_CI_site_currts$seg_lwr_DATE_right <- mod_dates_plt[cpts_CI_site_currts$seg_lwr.ci.right]
   cpts_CI_site_currts$seg_upr_DATE       <- mod_dates_plt[cpts_CI_site_currts$seg_upr]
   cpts_CI_site_currts$seg_upr_DATE_left  <- mod_dates_plt[cpts_CI_site_currts$seg_upr.ci.left]
   cpts_CI_site_currts$seg_upr_DATE_right <- mod_dates_plt[cpts_CI_site_currts$seg_upr.ci.right]
   
   #Pass out the results
   return(list(seg_summary=cpts_CI_site_currts,cpt_summary=cpts_CI_summary))
 
#Close the function.      
}

cpts_currts_eval <- function(data_proc_in,cpts_CI_site_obs,cpts_CI_site_mod){

   	
	 #Now compute the overlap for each observed changepoint assuming the obs cpts are truth.
	 #First Use the CIs to find the cpts that overlap.
   cpts_overlap <- matrix(1,nrow=nrow(cpts_CI_site_obs),ncol=nrow(cpts_CI_site_mod))

   for (aa in 1:nrow(cpts_CI_site_obs)){

      for (bb in 1:nrow(cpts_CI_site_mod)){

         ci_overlap <- cbind(cpts_CI_site_obs$ci.left[aa] - cpts_CI_site_mod$ci.right[bb], cpts_CI_site_obs$ci.right[aa] - cpts_CI_site_mod$ci.right[bb], 
                             cpts_CI_site_obs$ci.right[aa] - cpts_CI_site_mod$ci.left[bb])

         if ((ci_overlap[,1] > 0 & ci_overlap[,2] > 0 & ci_overlap[,3] > 0) |
             (ci_overlap[,1] < 0 & ci_overlap[,2] < 0 & ci_overlap[,3] < 0)){
           
               cpts_overlap[aa,bb] <- 0  #I.e. Cis do not overlap.

         }

      }

   }

   #Find all model cpts that intercept the CIs of each obs cpt.
   obs_cpt_insct <- list()
   for (cc in 1:nrow(cpts_overlap)){
      cpts_insct <- which(cpts_overlap[cc,] == 1)
      if (length(cpts_insct) != 0){
        obs_cpt_insct[[cc]] <- cpts_insct
      } else {
        obs_cpt_insct[[cc]] <- 0
      }
   }

   #Determine the fuzzy similarity of each intersecting point.
   #At the same time find the point that gives the maximum intersection.
   #This is either the giev point if there is one or the max of the multiples if there are more than one.
   cpts_fuzzy_eval   <-   list()
   obs_cpt_insct_max <- numeric()

   for (dd in 1:length(obs_cpt_insct)){

      if ((length(obs_cpt_insct[[dd]]) == 1) & (obs_cpt_insct[[dd]][1] != 0)){
        cpts_fuzzy_eval[[dd]] <- cpts_insct_siml(cpts_CI_site_obs[dd,],cpts_CI_site_mod[obs_cpt_insct[[dd]][1],],length(mod_tseries))
        obs_cpt_insct_max[dd] <- obs_cpt_insct[[dd]][1]
      } else if ((length(obs_cpt_insct[[dd]]) == 1) & (obs_cpt_insct[[dd]][1] == 0)){
        cpts_fuzzy_eval[[dd]] <- 0
        obs_cpt_insct_max[dd] <- 0
      } else if (length(obs_cpt_insct[[dd]]) > 1){
        temp_fuzzy_eval <- numeric()
        for (ee in 1:length(obs_cpt_insct[[dd]])){
           temp_fuzzy_eval[ee] <- cpts_insct_siml(cpts_CI_site_obs[dd,],cpts_CI_site_mod[obs_cpt_insct[[dd]][ee],],length(mod_tseries))
        }
        cpts_fuzzy_eval[[dd]] <- temp_fuzzy_eval
        obs_cpt_insct_max[dd] <- obs_cpt_insct[[dd]][which.max(temp_fuzzy_eval)]
      }
   }

   #Find the unique list of maximum intersections.
   #This gives the model cpt ind that has maximum intersection with each obs cpt.
   #There will be duplicates here where more than one model cpt intersects. 
   rep_cpt <- unique(obs_cpt_insct_max[obs_cpt_insct_max != 0])

   #Find the max intserctions to each obs cpt.
   #Set the duplicates to 0.
   obs_wgts_max <- numeric(length=length(obs_cpt_insct_max))
   for (yy in 1:length(rep_cpt)){
      obs_ind_mlt <- which(obs_cpt_insct_max == rep_cpt[yy])
      #If we have a single location having max intersect just populate with that value.
      #If we have more than one find the maxium score and that gives the max intersect.
      if (length(obs_ind_mlt) == 1){
        obs_wgts_max[obs_ind_mlt] <- rep_cpt[yy]
      } else if (length(obs_ind_mlt) > 1){
        obs_ind_max <- obs_ind_mlt[which.max(unlist(cpts_fuzzy_eval)[which(unlist(obs_cpt_insct) == rep_cpt[yy])])]
        obs_wgts_max[obs_ind_max] <- rep_cpt[yy]
      }
   }

   #Where we dont have maximum intersections there might be another cpt that does intersect to a lesser extent.
   #Check for these.

   #Find the remaining points to check for alternative intsersections
   obs_ind_rmn <- which(obs_wgts_max == 0)

   #Get the corresponding alternatives from the main list.
   obs_alt_check <- obs_cpt_insct[obs_ind_rmn]

   #Now loop over the remaining alternative intserctions (if any) and identify.
   #This code accounts for the fact that the only available insection may be a cpt that has already been taken by the maxima.
   for (xx in 1:length(obs_alt_check)){
      curr_alt <- obs_alt_check[[xx]]
      curr_alt_rmn <- curr_alt[!(curr_alt %in% obs_wgts_max)]  #strips out the maxima already taken.
      if (length(curr_alt_rmn) > 0){
        obs_alt_check[[xx]] <- curr_alt_rmn
      } else {
        obs_alt_check[[xx]] <- 0
      }
   }

   #Now map the alternatives back into the main array.
   obs_wgts_max[obs_ind_rmn] <- unlist(obs_alt_check)

   #Extract the corresponding intersection weights.
   cpts_wgts_val <- numeric()
   for (zz in 1:length(obs_wgts_max)){
      if (obs_wgts_max[zz] == 0){
        cpts_wgts_val[zz] <- 0
      } else {
        cpts_wgts_val[zz] <- cpts_fuzzy_eval[[zz]][which(obs_cpt_insct[[zz]] == obs_wgts_max[zz])]
      }
   }

   #Create an output data frame to write to table.
	 cpts_mod_eval_metrics <- data.frame(obs_cpt_no=1:length(obs_wgts_max),mod_cpt_insct=obs_wgts_max,cpt_eval_score=cpts_wgts_val)

   #Round off the evluation score to 2DP.
   #cpts_mod_eval_metrics <- cpts_mod_eval_metrics %>% mutate_at(vars(c('cpt_eval_score')), funs(round(., 4)))

   #Also determine a global metric to compare cpt evaluation method to (R2 in this case)
   #First determine where we have both model and obs data present.
   MOD_eval_pres <- which(!is.na(data_proc_in$MOD_t2m) & !is.na(data_proc_in$OBS_t2m))

   #Now determine the Coefficient of determination (R2)
   MOD_R2   <- summary(lm(data_proc_in$OBS_t2m[MOD_eval_pres] ~
                          data_proc_in$MOD_t2m[MOD_eval_pres]))$r.squared     

   #Also get the summed changepoint evaluation metric.
   MOD_CPTS_EVAL <- sum(cpts_mod_eval_metrics$cpt_eval_score)
   
   #Normalise the changepoint evaluation metric to 0-1 scale. 
   MOD_CPTS_EVAL <- MOD_CPTS_EVAL/length(cpts_mod_eval_metrics$cpt_eval_score)
   
   #Add all the metrics to a summary data.frames.
   mod_tseries_stats <- data.frame(CPT_TIME_METRIC=MOD_CPTS_EVAL,R2=MOD_R2)
                                   
   #Format the data frame to output at 4dp for each variable.
   #mod_tseries_stats <- mod_tseries_stats %>% mutate_at(vars(c('Value')), funs(round(., 2)))

   #Pass out the results.
   cpts_proc_out <- list(cpts_mod_eval_metrics=cpts_mod_eval_metrics,mod_tseries_stats=mod_tseries_stats)
   return(cpts_proc_out)

#Close the function.
}

#**Function 4:** Function to determine segments of continuous data.
obs_pres_seg_proc <- function(obs_pres){

   seg_no  <- array(0,length(obs_pres))
   ini_seg <- 1

   for (ii in 1:length(obs_pres)){
      if(ii < length(obs_pres)){
        if ((obs_pres[ii] == TRUE) & (obs_pres[ii+1] == TRUE)){
          seg_no[ii] <- ini_seg
        } else if ((obs_pres[ii] == TRUE) & (obs_pres[ii+1] == FALSE)){
          seg_no[ii] <- ini_seg
          ini_seg    <- ini_seg+1  
        } else if ((obs_pres[ii] == FALSE) & (obs_pres[ii+1] == FALSE)){
          seg_no[ii] <- 0
        } else if ((obs_pres[ii] == FALSE) & (obs_pres[ii+1] == TRUE)){
          seg_no[ii] <- 0
        }
      } else {
        if(obs_pres[ii] == TRUE){
          seg_no[ii] <- ini_seg
        } else {
          seg_no[ii] <- 0
        }
      }
   }

   return(seg_no)

#Close the function. 
}

#**Function 5:** Function to process data for the selected site for output.
Sites_CI_proc <- function(Site_df_ext,ts_proc_out,min_seg_len_out,TS_AR_ARIMA_out,CPT_METHOD_out,N_reps_out){
  
 
    #Now process the cpts analysis based on the UI settings.
    #Need to put the check for shiny in here in case model is running standalone.
    prog_message <- paste('Processing confidence intervals: ',sep='')
    if (shiny::isRunning()){
      withProgress(message = prog_message, value = 0, {
      Curr_site_data <- cpts_currts_CI(Site_df_ext,min_seg_len_out,TS_AR_ARIMA_out,
                                       CPT_METHOD_out,N_reps_out,ts_proc_out)
      })
    } else {
      Curr_site_data <- cpts_currts_CI(Site_df_ext,min_seg_len_out,TS_AR_ARIMA_out,
                                       CPT_METHOD_out,N_reps_out,ts_proc_out)
    }
    
    #Pass out results.
    return(list(all_cpt_summ=Curr_site_data$cpt_summary,all_seg_summ=Curr_site_data$seg_summary))
  
}

#**Function 6:** Function that determines the model evaluation metrics by comparing the calculated confidence intervals on changepoints for each time series.
Sites_CI_eval <- function(Site_df_ext,obs_cpt_summ,mod_cpt_summ){
    
  #Now call the evaluation routine.
  Curr_site_eval_data <- cpts_currts_eval(Site_df_ext,obs_cpt_summ,mod_cpt_summ)
  
  #Pass out the results.
  return(list(mod_eval_summary=Curr_site_eval_data$cpts_mod_eval_metrics,
              mod_sumstats_main=Curr_site_eval_data$mod_tseries_stats,
              Site_df_ext=Site_df_ext))
  
  
}
