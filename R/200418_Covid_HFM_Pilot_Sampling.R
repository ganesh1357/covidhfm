# Sampling for Covid-19 HFM Pilot Project in TS
# Language: R
# Author: Ganesh Rao (GR), ganesh@cegis.org
# Date: 18-04-2020
# Modified on: ...
# References:

# ----------------------------------------------------------------------------------------------
# Part 0: Clear
# ----------------------------------------------------------------------------------------------    

  rm(list=ls())

# ----------------------------------------------------------------------------------------------
# Part 1: Load packages
# ----------------------------------------------------------------------------------------------    

# Enter 1 if you want to install packages
    PACKAGES             <- 0
  
  # Enter list of libraries you want to load / packages you want to install 
    packages <- c("dplyr", "ggplot2", "readxl", "tidyverse")
  
  # If you selected the option to install packages, install them
    if (PACKAGES) {
      install.packages(packages,
                       dependencies = TRUE)
    }
  
  # Load all packages -- this is equivalent to using library(package) for each 
  # package listed before
    sapply(packages, library, character.only = TRUE)

# ----------------------------------------------------------------------------------------------    
# Part 2: Set folder paths
# ----------------------------------------------------------------------------------------------    
    
  # Add your username and folder path here (for Windows computers)
  # To find out what your username is, type Sys.getenv("USERNAME")
  if (Sys.getenv("USERNAME") == "Ganesh Rao") {
    
    projectFolder <- "G:/My Drive/CEGIS Drive/02 Outcome Measurement/023 HFM/Covid-19 Survey/Sampling/Sampling/Pilot/R"
    
  }
  
  # Project subfolders
    
    taskdate = "200418" # update task date
    project = "Covid_HFM_Pilot" # update project name
    purpose = "Sampling" # update purpose 
    
    Input             <- file.path(projectFolder,"Input")
    Output            <- file.path(projectFolder,"Output")
    
    inputfile1 <- file.path(Input,"MANDAL_WISE_MB_DTLS.xlsx")
    
#   Unable to solve xlsx writer - outputfile1 <- file.path(Output,paste0(paste(taskdate, project, purpose, sep="_"), "output", ".xlsx"))
    outputfile1 <- file.path(Output,paste0(paste(taskdate, project, purpose, "idinfo", sep="_"), ".csv"))
    outputfile2 <- file.path(Output,paste0(paste(taskdate, project, purpose, "summ_dist", sep="_"), ".csv"))
    outputfile3 <- file.path(Output,paste0(paste(taskdate, project, purpose, "summ_mand", sep="_"), ".csv"))
    outputfile4 <- file.path(Output,paste0(paste(taskdate, project, purpose, "mod_data", sep="_"), ".csv"))
    
    outputfile5 <- file.path(Output,paste0(paste(taskdate, project, purpose, "pilot_pop", sep="_"), ".csv"))
    outputfile6 <- file.path(Output,paste0(paste(taskdate, project, purpose, "pilot_sample", sep="_"), ".csv"))

# ----------------------------------------------------------------------------------------------    
# Part 3: Load and clean the dataset
# ----------------------------------------------------------------------------------------------    
    
  # Load input and summarize
    rawdf <- read_xlsx(inputfile1,
                      col_names = TRUE)
    summary(rawdf)
    
  # Sort by phone number
    rawdf <- rawdf[order(rawdf$`DISTRICT NAME`, rawdf$`MANDAL NAME`, rawdf$`PANCHAYAT NAME`), ]  

  # Remove obs with duplicate phone numbers
    # No priority has been given to which number is retained among a set of dup numbers
    # Some are genuine cases of multiple members of the same HH, but some are errors
    moddf <- rawdf[!duplicated(rawdf$`MOBILE NUMBER`), ]
  
  # Convert text to numeric phone number
    moddf$mob_num <- as.numeric(moddf$`MOBILE NUMBER`)
    moddf$JOBCARD <- as.character(moddf$JOBCARD)
    
  # Remove invalid numbers
    moddf <- moddf[!(moddf$mob_num<=6000000000), ]
  
  # Create a function for encoding string vars
    encode_ordinal <- function(x, order = unique(x)) {
      x <- as.numeric(factor(x, levels = order, exclude = NULL))
      x
    }
    
  # Create a District ID
    moddf$id_dist <- encode_ordinal(moddf$`DISTRICT NAME`)
    moddf$id_mand <- encode_ordinal(moddf$`MANDAL NAME`) # ideally this is ordered within district but given only few mandals covered, any ID is fine
    moddf$id_phon <- ave(moddf$mob_num, moddf$id_mand, FUN=seq_along) # Ordered ID within each Mandal
    
  # Create a Mandal ID
    moddf$id_dist2 <- sprintf("%02d",  moddf$id_dist)
    moddf$id_mand2 <- sprintf("%03d",  moddf$id_mand)
    moddf$id_phon2 <- sprintf("%05d",  moddf$id_phon)
  
  # Create a final unique ID
    moddf$id_resp <- paste(moddf$id_dist2, moddf$id_mand2, moddf$id_phon2, sep="-")
      
  # Subset a table for indicating IDs used
    id_info <- select(filter(moddf,),c(id_dist2, id_mand2, `DISTRICT NAME`, `MANDAL NAME`))
    id_info <- id_info[!duplicated(id_info$id_mand2), ]

  # Summarize by district
    distsumm <- moddf %>%
      group_by(id_dist) %>%
      summarise(dist_name = first(`DISTRICT NAME`), num_dist = n_distinct(id_resp))
    distsumm <- distsumm[order(distsumm$num_dist), ]  
    
  # Summarize by Mandal
    mandsumm <- moddf %>%
      group_by(id_mand) %>%
      summarise(dist_name = first(`DISTRICT NAME`),
                mand_name = first(`MANDAL NAME`),
                num_mand = n_distinct(id_resp))
    mandsumm <- mandsumm[order(mandsumm$num_mand), ]  
  
  # Order the df
    moddf <- moddf[order(moddf$id_dist, moddf$id_mand, moddf$`PANCHAYAT NAME`, moddf$mob_num), ]
  
  # Drop the numeric phone number var
    moddf$mob_num<-NULL
    
  # Write results to CSVs
    write_csv(id_info, outputfile1, na = "NA", append = FALSE)
    write_csv(distsumm, outputfile2, na = "NA", append = FALSE)
    write_csv(mandsumm, outputfile3, na = "NA", append = FALSE)
    write_csv(moddf, outputfile4, na = "NA", append = FALSE) # Jobcard precision is lost if you open in Excel but it's safe
    
# ----------------------------------------------------------------------------------------------    
# Part 4: Simple random sampling (SRS) for 2 mandals
# ----------------------------------------------------------------------------------------------    

  # D = Yadadri Bhuvanagiri, M = Valigonda, id_mand = 56, Has highest number of obs
    pilot_pop <- select(filter(moddf, id_mand==56), colnames(moddf))
   
  # Write results to CSVs
    write_csv(moddf, outputfile5, na = "NA", append = FALSE)
    
  # We could choose to stratify by panchayats, there are 36
  #   pancsumm <- moddf %>%
  #     group_by(`MANDAL NAME`) %>%
  #     summarise(dist_name = first(`DISTRICT NAME`),
  #               mand_name = first(`MANDAL NAME`),
  #               panc_name = first(`PANCHAYAT NAME`),
  #               num_panc = n_distinct(`PANCHAYAT NAME`))
    
  # Set seed for random number
    set.seed(20418)
   
  # Sample by mandal. This will help when we actually have multiple mandals
    by_mand <- pilot_pop %>% group_by(id_mand)

  # Sample, we need 400
    pilot_n <- 400
    pilot_sample <- sample_n(by_mand, pilot_n)
    pilot_sample$pw1 <- 1/(pilot_n/nrow(by_mand))
  
  # Distribute by enumerator ID
    enum_n <- 14
    pilot_sample$enum_id <- gl(enum_n, (pilot_n/enum_n))
    
  # Output the sample
    pilot_sample$resp_name <- toupper(paste(pilot_sample$"WAGESEEKER NAME", 
                                    pilot_sample$"HOUSEHOLD NAME", sep=" "))
    
    col_order <- c("enum_id", "id_resp", "id_dist2", "DISTRICT NAME", 
                   "id_mand2", "MANDAL NAME", "PANCHAYAT NAME", 
                   "HABITATION NAME", "resp_name", "MOBILE NUMBER")
    pilot_sample_out <- pilot_sample[, col_order]
    
  # Rename the columns to single words
    pilot_sample_out <- pilot_sample_out %>% 
                          rename(
                            dist_name = `DISTRICT NAME`,
                            mand_name = `MANDAL NAME`,
                            panc_name = `PANCHAYAT NAME`,
                            vill_name = `HABITATION NAME`,
                            mobi_num1 = `MOBILE NUMBER`,
                            dist_id = id_dist2,
                            mand_id = id_mand2
                          )
    
  # Write results to CSVs
    write_csv(pilot_sample_out, outputfile6, na = "NA", append = FALSE)

# ----------------------------------------------------------------------------------------------    
# THE END
# ----------------------------------------------------------------------------------------------    