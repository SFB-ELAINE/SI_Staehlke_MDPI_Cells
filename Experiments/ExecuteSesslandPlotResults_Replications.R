  #--------------------------------------------------------------------------#
# Scrip   t for producing the plots of Wnt Pillars paper
# Authors:      Kai Budde, Fiete Haack
# Created:      2019-07-04
# Last changed: 2020-01-06
# Used version of Sessl2R: 0.1.3
#--------------------------------------------------------------------------#

# Preface ##################################################################
remove(list = ls())
library(grid)
# Set the list of experiments to be run ####################################

list_of_directories <- c(
  "./Simulation_P5",
  "./Simulation_Ref"
)

# Determine how to plot the results
with_legend <- TRUE
with_y_label <- TRUE
    
# set if you want to start new experiments

sim <- TRUE

# Set the current directory ################################################
directory_of_R_script <- rstudioapi::getSourceEditorContext()$path
directory_of_R_script <- gsub(pattern = "ExecuteSesslandPlotResults_Replications.R",
                              replacement = "",
                              x = directory_of_R_script)
old_directory <- getwd()
setwd(directory_of_R_script)


# Install and load necessary R package if not already installed ############

list.of.packages <- c("devtools", "ggplot2", "gridExtra", "dplyr")
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = TRUE))


# Install the R package for using Sessl output data ########################
devtools::install_github("SFB-ELAINE/Sessl2R", ref = "v0.1.3")
require(Sessl2R)

# Load reference data ######################################################
df_Axin <- read.csv(file = "../Data/AxinRefExpression.csv",
                    stringsAsFactors = FALSE)
names(df_Axin)[2] <- "AxinRefExpression"
df_Axin$'Data Source' <- "RefExpression"

# Run Sessl scripts and plot experiment results ############################

for(i in 1:length(list_of_directories)){
  print(paste0("Change working directory to ", getwd(), "/",
               list_of_directories[i]))
  setwd(list_of_directories[i])
  
  # Find scala script an find scan variable
  scala_file <- grep(pattern = "*.scala", list.files(), value = TRUE)
  scan_command <- grep("scan", readLines(scala_file), value = TRUE)
  
  # Only use scan line that was not commented out
  scan_command <- grep("^\\s?//", scan_command,
                       invert = TRUE, value = TRUE)
  
  if(length(grep("^\\s?//", scan_command, invert = TRUE, value = TRUE)) > 1){
    print("We have more than one scan command.")
    
    # Combining the parameters into one
    scan_command <- gsub(pattern = ".*\\(\"",replacement = "", scan_command)
    scan_command <- gsub(pattern = "\".*",replacement = "", scan_command)
    
    parameter_names <- scan_command
  }else if(length(scan_command) == 1){
    parameter_names <- strsplit(scan_command, split = "\"")[[1]][2]
  }else{
    parameter_names <- NA
    print("No parameters existent.")
  }
  
  # Run Sessl script
  if(sim == TRUE){
     if(.Platform$OS.type == "unix") {
       system(command = "./run.sh", wait = TRUE)
     } else {
       system(command = "./run.bat", wait = TRUE)
    }
  }
  
  # Find most recent results directory
  
  # I do have to repeat the following line because otherwise it would not
  # work properly (length(0) otherwise for i=2). I do not know why this
  # happens.
  subdirectories <- list.dirs(recursive = FALSE)
  subdirectories <- list.dirs(recursive = FALSE)
  experiment_result_dirs <- grep(pattern = "results",
                                 x = subdirectories,
                                 value = TRUE)
  
  recent_result <- experiment_result_dirs[length(experiment_result_dirs)]
  
  # Load data from experiments
  # Save a dataframe with the experiment result
  print(paste0("Folder with most recent SESSL results: ", recent_result))
  
  setwd(recent_result)
  # TODO: This will not be necessary for the new version of Sessl2R.
  getData(input_dir = getwd())
  
  # Load the data frame and copy loaded df to default name
  df_files <- list.files()
  df_files <- df_files[grepl(pattern = "rda", x = df_files)]
  
  df_multi <- vector("list", length(df_files))
  
  # Go through every data frame saved in the directory and store the results
  # in df_complete
  for(j in 1:length(df_files)){
    
    load(df_files[j])
    loaded_dataframe <- gsub("\\.rda", "", df_files[j])
    
    df <- get(loaded_dataframe)
    
    df$Expression <- (df$Axin+10) / (df_Axin$AxinRefExpression+10)
    df$bCat_Cyt_r <- (df$bCat_Cyt/13000)
    df$bCat_Nuc_r <- (df$bCat_Nuc/5282)
    df$bCat_t <- (df$bCat_Cyt + df$bCat_Nuc) / 18300
    
    if(j > 1){
      df_complete <- rbind(df_complete, df)
    }else{
      df_complete <- df
    }
  }

  # Make parameter (combinations) as factor
  if(length(parameter_names) > 1){
    copy_parameter_names <- parameter_names
    parameter_names <- paste0(parameter_names, collapse = ",\n")
    df_complete[[parameter_names]] <-
      apply(df_complete[ , copy_parameter_names ] , 1 , paste ,
            collapse = ", " )
    
    # Delete all NA parameter names
    df_complete[[parameter_names]][
      grep(pattern = "NA", x = df_complete[[parameter_names]],
           ignore.case = TRUE)] <- NA
  }
  
  df_complete[[parameter_names]] <- as.factor(df_complete[[parameter_names]])
  
  result_min <- 1
  result_max <- 0
  
  for(k in 1:length(levels(df_complete[[parameter_names]])) ){
    
    result <- df_complete$bCat_Cyt[
      df_complete$time == 1440 &
        df_complete[[parameter_names]] == levels(
          df_complete[[parameter_names]])[k] &
        !is.na(df_complete[[parameter_names]])]
    if(is.na(result)){
      print("Something went wrong wit the calculation of the result.")
    }
    
    if(result < result_min){
      result_min <- result
      parameter_line_min <- levels(df_complete[[parameter_names]])[k]
    }
    if(result > result_max){
      result_max <- result
      parameter_line_max <- levels(df_complete[[parameter_names]])[k]
    }
    df_multi[[j]] <- df
    
  }

  df_complete$time2 <- as.factor(df_complete$time)
  bCat_Cyt_r_rep <- data.frame(time = rep(df_complete$time[1:length(levels(df_complete$time2))], 3), 
                               data = rep(1, length(levels(df_complete$time2))*3), 
                               value = c(rep("min", length(levels(df_complete$time2))), 
                                         rep("mean", length(levels(df_complete$time2))), 
                                         rep("max", length(levels(df_complete$time2)))), 
                               stringsAsFactors = F)
  
  bCat_Nuc_r_rep <- data.frame(time = rep(df_complete$time[1:length(levels(df_complete$time2))], 3), 
                               data = rep(1, length(levels(df_complete$time2))*3), 
                               value = c(rep("min", length(levels(df_complete$time2))), 
                                         rep("mean", length(levels(df_complete$time2))), 
                                         rep("max", length(levels(df_complete$time2)))), 
                               stringsAsFactors = F)
  
  bCat_t_rep <- data.frame(time = rep(df_complete$time[1:length(levels(df_complete$time2))], 3), 
                               data = rep(1, length(levels(df_complete$time2))*3), 
                               value = c(rep("min", length(levels(df_complete$time2))), 
                                         rep("mean", length(levels(df_complete$time2))), 
                                         rep("max", length(levels(df_complete$time2)))), 
                               stringsAsFactors = F)
  
  
  axin_r_rep <- data.frame(time = rep(df_complete$time[1:length(levels(df_complete$time2))], 3), 
                               data = rep(1, length(levels(df_complete$time2))*3), 
                               value = c(rep("min", length(levels(df_complete$time2))), 
                                         rep("mean", length(levels(df_complete$time2))), 
                                         rep("max", length(levels(df_complete$time2)))), 
                               stringsAsFactors = F)
  
  # Create plot
  # Data from Sessl Experiment
  for(k in 1:length(levels(df_complete$time2))){
    bCat_Cyt_r_rep$data[bCat_Cyt_r_rep$value == "min"][k] <- min(df_complete$bCat_Cyt_r[df_complete$time2 == levels(df_complete$time2)[k]])
    bCat_Cyt_r_rep$data[bCat_Cyt_r_rep$value == "max"][k] <- max(df_complete$bCat_Cyt_r[df_complete$time2 == levels(df_complete$time2)[k]])
    bCat_Cyt_r_rep$data[bCat_Cyt_r_rep$value == "mean"][k] <- mean(df_complete$bCat_Cyt_r[df_complete$time2 == levels(df_complete$time2)[k]])
    
    bCat_Nuc_r_rep$data[bCat_Nuc_r_rep$value == "min"][k] <- min(df_complete$bCat_Nuc_r[df_complete$time2 == levels(df_complete$time2)[k]])
    bCat_Nuc_r_rep$data[bCat_Nuc_r_rep$value == "max"][k] <- max(df_complete$bCat_Nuc_r[df_complete$time2 == levels(df_complete$time2)[k]])
    bCat_Nuc_r_rep$data[bCat_Nuc_r_rep$value == "mean"][k] <- mean(df_complete$bCat_Nuc_r[df_complete$time2 == levels(df_complete$time2)[k]])
    
    bCat_t_rep$data[bCat_t_rep$value == "min"][k] <- min(df_complete$bCat_t[df_complete$time2 == levels(df_complete$time2)[k]])
    bCat_t_rep$data[bCat_t_rep$value == "max"][k] <- max(df_complete$bCat_t[df_complete$time2 == levels(df_complete$time2)[k]])
    bCat_t_rep$data[bCat_t_rep$value == "mean"][k] <- mean(df_complete$bCat_t[df_complete$time2 == levels(df_complete$time2)[k]])
    
    axin_r_rep$data[axin_r_rep$value == "min"][k] <- min(df_complete$Expression[df_complete$time2 == levels(df_complete$time2)[k]])
    axin_r_rep$data[axin_r_rep$value == "max"][k] <- max(df_complete$Expression[df_complete$time2 == levels(df_complete$time2)[k]])
    axin_r_rep$data[axin_r_rep$value == "mean"][k] <- mean(df_complete$Expression[df_complete$time2 == levels(df_complete$time2)[k]])
  }
  
  
    p <- ggplot(data = bCat_t_rep) + 
      geom_line(data=bCat_t_rep[which(bCat_t_rep$value=="mean"),], mapping = aes(x=time/60, y=data, color='bcat')) + 
      geom_ribbon(data = bCat_t_rep[which(bCat_t_rep$value=="max"),], 
                  mapping = aes(x=time/60, 
                      ymax=bCat_t_rep$data[which(bCat_t_rep$value=="max")], 
                      ymin=bCat_t_rep$data[which(bCat_t_rep$value=="min")]), 
                  fill="darkgrey", alpha=.25) +
      
      geom_line(data=axin_r_rep[which(axin_r_rep$value=="mean"),], mapping = aes(x=time/60, y=data, color='axin')) +
      geom_ribbon(data=axin_r_rep[which(axin_r_rep$value=="mean"),], 
                  mapping = aes(x=time/60, 
                                ymax=axin_r_rep$data[which(axin_r_rep$value=="max")], 
                                ymin=axin_r_rep$data[which(axin_r_rep$value=="min")]), 
                  fill="darkgrey", alpha=.2) +
    theme_bw() +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    xlab("Time [hours]" ) +
    scale_colour_brewer(palette = "Set1")

  # Add y_label and legend if requested
  if(with_y_label){
    p <- p + ylab("fold change")
  }else{
    p <- p + theme(axis.title.y = element_blank())
  }
  if(with_legend){
    p <- p + theme(legend.title = element_text(size=rel(0.7)),
                   legend.text=element_text(size=rel(0.6)),
                   legend.key.size = unit(0.8, 'lines'),
                   legend.margin = margin(t = 0, unit='cm'))
  }else{
    p <- p + theme(legend.position="none")
  }
  
  p <- p + theme(axis.text  = element_text(size=14),
                 axis.title = element_text(size=14),
                 plot.margin=unit(c(0.5,0.2,0.2,0.2),"cm"))
  
#  print(p)
  
  # Save plot
  setwd(directory_of_R_script)
  output_path <- "Plots"
  dir.create(path = output_path, showWarnings = FALSE)
   
  output_name <- paste(list_of_directories[i], ".pdf", sep="")
   
  ggsave(filename = paste(output_path, "/", output_name, sep=""),
          plot = p, width = 11.4, height = 6.4, units = "cm")
  
}

# Set working directory to defeault ########################################
setwd(old_directory)
