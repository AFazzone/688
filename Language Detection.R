# Analyze MLD GS Language Detection
rm(list=ls()); cat("\014") # Clear Workspace and Console
library("cld2"); library("cld3")
library("readr"); library("magrittr")

#--------------------------------------------------------
# Run for cld2
#--------------------------------------------------------

# 1) Your Code HERE: Load the file names that need to be processed into "files.txt"
files.txt <- list.files(path = "Data/sentence data")
#Check that file names are correct
files.txt

Result.DF <- data.frame()
for (ff in 1:length(files.txt)) { 
  fn <- files.txt[ff]
  txt.data <- read_tsv(fn)
  Lg.Data.Code <- cld2::detect_language(txt.data[[1]]) # Get Language Code
  Lg.Data.Lg <- cld2::detect_language(txt.data[[1]], lang_code = FALSE) # Get Language
  Lg.Data.Code <- Lg.Data.Code[!is.na(Lg.Data.Code)]; Lg.Data.Lg <- Lg.Data.Lg[!is.na(Lg.Data.Lg)]  # Remove NA From Language Code & Language
  
  Predicted.Lg.Code <- names(sort(table(Lg.Data.Code), decreasing=TRUE)[1]) # Find Dominant Language Code 
  Predicted.Lg <- names(sort(table(Lg.Data.Lg), decreasing=TRUE)[1]) # Find Dominant Language 
  
  # 2) Your Code HERE: Extract the language 2 letter code from the file name ()
  GT.Lg <- sub(".*_","", files.txt )
  GT.Lg <- sub(".txt.*", "", GT.Lg) # Extract the ground truth Language code from the file name (i.e. get "ar" from "sentence_ar.txt")
  #Check that GT.Lg has two letters for the currency code
  GT.Lg
  
  Correct <- GT.Lg == Predicted.Lg.Code
  
  temp1 <- data.frame(GS_Lg_Code=GT.Lg, Pred_Lg_Code=Predicted.Lg.Code, Correct=Correct, Pred_Lg=Predicted.Lg, stringsAsFactors = FALSE)
  Result.DF <- rbind(Result.DF, temp1)
}

knitr::kable(Result.DF) # Display result of Language Detection

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#Rerun again for  cld3
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------



# Analyze MLD GS Language Detection
rm(list=ls()); cat("\014") # Clear Workspace and Console
library("cld2"); library("cld3")
library("readr"); library("magrittr")




# 1) Your Code HERE: Load the file names that need to be processed into "files.txt"
#files.txt <- load.files(path="data/sentence data", all.files = FALSE, full.names = FALSE)
files.txt <- list.files(path = "Data/sentence data")
#Check that file names are correct
files.txt


Result.DF <- data.frame()
for (ff in 1:length(files.txt)) { 
  fn <- files.txt[ff]
  txt.data <- read_tsv(fn)
  Lg.Data.Code <- cld3::detect_language(txt.data[[1]]) # Get Language Code
  Lg.Data.Lg <- cld3::detect_language(txt.data[[1]]) # Get Language
  Lg.Data.Code <- Lg.Data.Code[!is.na(Lg.Data.Code)]; Lg.Data.Lg <- Lg.Data.Lg[!is.na(Lg.Data.Lg)]  # Remove NA From Language Code & Language
  
  Predicted.Lg.Code <- names(sort(table(Lg.Data.Code), decreasing=TRUE)[1]) # Find Dominant Language Code 
  Predicted.Lg <- names(sort(table(Lg.Data.Lg), decreasing=TRUE)[1]) # Find Dominant Language 
  
  # 2) Your Code HERE: Extract the language 2 letter code from the file name ()
  GT.Lg <- sub(".*_","", files.txt )
  GT.Lg <- sub(".txt.*", "", GT.Lg) # Extract the ground truth Language code from the file name (i.e. get "ar" from "sentence_ar.txt")
  #Check that GT.Lg has two letters for the currency code
  GT.Lg
  Correct <- GT.Lg == Predicted.Lg.Code
  
  temp1 <- data.frame(GS_Lg_Code=GT.Lg, Pred_Lg_Code=Predicted.Lg.Code, Correct=Correct, Pred_Lg=Predicted.Lg, stringsAsFactors = FALSE)
  Result.DF <- rbind(Result.DF, temp1)
}

knitr::kable(Result.DF) # Display result of Language Detection
