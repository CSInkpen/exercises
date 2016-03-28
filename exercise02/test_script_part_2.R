
# Part 2 test run #

gc(rm(list=ls()))
graphics.off()

#invisible(suppressMessages(install.packages('pacman')))

library_list <- c("DBI", "RMySQL", "plyr", "data.table", "rms", "foreign", "ggplot2", "MASS", "reshape2", "pastecs", "Hmisc", "dummies", "Matching", 
                  "DataCombine", "stringr", "data.table", "stats", "plm", "ineq", "lubridate", "mallet", "XML", "rjson", "gridExtra", "rworldmap",
                  "openNLP", "wordcloud")

# Install and Load pacman, a package-installing package in R that allows you to load
# packages without output and check if a requested package is installed and install it if it is
# present.



suppressMessages(library(pacman))
invisible(suppressMessages(p_load(library_list, char, install=TRUE, character.only = TRUE)))





##############################################################
#### Part 2 - Bringing in JSON Data for Narative Analysis ####
##############################################################

setwd('/Users/christopherinkpen/PSU_Class_Documents/PSU_Class_Documents/Job_Market_Materials/RTI/exercises/exercise02')



inputDir <- "data/"
files.v <- dir(path=inputDir, pattern=".*json")


class(files.v[1])
files.v[1]

setwd('/Users/christopherinkpen/PSU_Class_Documents/PSU_Class_Documents/Job_Market_Materials/RTI/exercises/exercise02/data')

text_data <- data.frame()

for(i in 1:length(files.v)){
  print(files.v[[i]])
  x <- fromJSON(file = files.v[[i]])
  print(length(x[[1]]))
  x_size <- length(x[[1]])
  new_doc <- data.frame(matrix(unlist(x), nrow=x_size, byrow=T),stringsAsFactors=FALSE)
  text_data <-rbind(text_data, new_doc)
}


names(text_data) <- c("EventId","Narrative_Text","Probable_Cause")

text_data[text_data==""] <- NA

class(avi_data$EventId)
class(text_data$EventId)

merged_data <- merge(avi_data, text_data,by="EventId")

