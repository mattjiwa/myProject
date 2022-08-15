# Read in the data and output a dataframe of relevant info

# Install relevant packages (if you haven't already)
# install.packages("here", "stringr", "pracma", "dplyr")

library(here)
library(stringr)
library(pracma)
library(dplyr)

# Read in list of IDs
source(here::here("analysis", "myProj_IDlist.R"))

# Pre-assign dataframes
demographics <- tibble()
allTrials <- tibble()

# Loop over each ID
for (ID in IDs) {
  
  # Load next participant's data
  allData <- read.csv(here::here("data", paste("unreliable-info-7-sources_", toString(ID), ".csv", sep = "")), header=T)
  
  # Collect demographics ----------------------------------------------------
  
  if ("{\"Gender\":\"Male\"}" %in% allData$response) {
    Gender <- "M"
  } else if ("{\"Gender\":\"Female\"}" %in% allData$response) {
    Gender <- "F"
  } else {
    Gender <- "O"
  }
  
  Age <- as.numeric(str_replace_all(string = allData$response[3], pattern = "\\D+", replacement = ""))
  
  demogData <- tibble(ID = ID, Gender = Gender, Age = Age)
  
  # Find trial data ---------------------------------------------------------
  
  # Find the bids. We only want the last 84 because the first few are practice trials
  numBids <- sum(allData$trial_name == "infoBid")
  bidData <- allData[allData$trial_name == "infoBid",][(numBids-83):numBids,]
  # Find out which trial each bid corresponds to
  trialData <- allData$trial_name[which(allData$trial_name == "infoBid")[(numBids-83):numBids]-1]
  trialData <- str_remove_all(trialData, "_")
  
  # Preallocate arrays
  pData <- tibble(
    ID = rep(ID, length(trialData)),
    diff = rep(NA, length(trialData)),
    finalCol = rep(NA, length(trialData)),
    bid = as.numeric(bidData$response)
  )
  
  for (t in 1:length(trialData)) {
    
    # Find difference between colours
    if (str_detect(trialData[t], "Z")) {
      pData$diff[t] <- 0
    } else {
      pData$diff[t] <- as.numeric(substr(trialData[t], 2, 2))
    }
    
    # Check final colour
    # 1 if dominant, 0 if non-dominant colour
    if (strcmp(substr(trialData[t], 1, 1), substr(trialData[t], 4, 4))) {
      pData$finalCol[t] <- 1
    } else {
      pData$finalCol[t] <- 0
    }
    
  }
  
  # Collate data ------------------------------------------------------------
  
  demographics <- rbind(demographics, demogData)
  allTrials <- rbind(allTrials, pData)
  
  

}

# Remove excess variables
rm(Age, Gender, ID, t, trialData, pData, demogData, bidData, 
   allData, numBids)

