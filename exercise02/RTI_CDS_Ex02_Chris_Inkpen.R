############################################
#### RTI - CDS - Exercise 2 Script #########
############################################

gc(rm(list=ls()))
graphics.off()


library_list <- c("DBI", "RMySQL", "plyr", "data.table", "rms", "foreign", "ggplot2", "MASS", "reshape2", "pastecs", "Hmisc", "dummies", "Matching", 
                  "DataCombine", "stringr", "data.table", "stats", "plm", "ineq", "lubridate", "mallet", "XML", "rjson", "gridExtra", "rworldmap",
                  "openNLP", "wordcloud", "qdap", "tm", "SnowballCC", "RColorBrewer", "biclust", "cluster", "igraph", "fpc")

# Install and Load pacman, a package-installing package in R that allows you to load
# packages without output and check if a requested package is installed and install it if it is
# present.
# invisible(suppressMessages(install.packages('pacman')))


suppressMessages(library(pacman))
invisible(suppressMessages(p_load(library_list, char, install=TRUE, character.only = TRUE)))


setwd('/Users/christopherinkpen/PSU_Class_Documents/PSU_Class_Documents/Job_Market_Materials/RTI/exercises/exercise02')

# Ok, first I'm going to bring in the xml data object using the R package XML. Specifically, I'll be using the xmlTreeParse
# function on the xml file.
doc <- xmlTreeParse("data/AviationData.xml")
# Taking a look at the class of the object
class(doc)
# [1] "XMLDocument"         "XMLAbstractDocument"
# Taking a look at the size of the XMLdoc
xmlSize(doc)
# 1 - so one child node.

# Now I'll get the root node of the XML doc using the xmlRoot function.
xmltop <- xmlRoot(doc)
class(xmltop)
# This is an XMLNode
xmlSize(xmltop)
# One child node
xmlName(xmltop)
# The name of the node is "DATA" (as we could see by inspecting the xml document in SublimeText3 or XCode)
# taking a look at the name (which should be ROWS) of the child node of <DATA>
xmlName(xmltop[[1]])
# Called <ROWS>
# Taking a look at the amount of child nodes within the sub-node <ROWS>
xmlSize(xmltop[[1]])
# 77,257 sub-child nodes (the number of flight incidents) within the child node <ROWS>
# Seeing if there are any sub-sub-child nodes
xmlSize(xmltop[[1]][[1]])
# 0 sub-sub child nodes.
xmlName(xmltop[[1]][[1]])
# The sub-child-nodes are called ROW, as we could tell from visually inspecting the file.

# We can take a look at one of the ROW observations by printing it to the console.
print(xmltop[[1]][[1]])
# separating ROWS node form DATA node
child_nodes <- xmlChildren(xmltop)
class(child_nodes)
# This is an XMLNodeList.

# Taking a look at what the xmlAttrs function returns when I apply it to one of the ROW sub-child nodes
xmlAttrs(xmltop[[1]][[1]])
# Ok, this returns what I want.

### Now I want to unlist the xmlnodelist of child nodes.
# What I want is the attributes, since the data appears to be stored in attributes as opposed to individual sub-nodes 
# (which would be more properly formatted and thus easier to convert to a dataframe)
# I'm going to try doing xmlAttrs for just the first row (so xmltop[[1]][[1]]) just to make sure this works
attr_list <- xmlAttrs(xmltop[[1]][[1]])
class(attr_list)
# This returns a character list with the first observation.
# Now I'll try unlisting this to convert it to a dataframe as a test.
test_df <- data.frame(matrix(unlist(attr_list), nrow=1, byrow = TRUE), stringsAsFactors = FALSE)
# Now I'll try it on the node list if i can, the child nodes.
# Ok so this gave me 1 observation with 31 variables (some missing)

# As a note, looking at the data, I can see that it's improperly/sloppily formatted, because instead of having individual subnodes
# for every feature, they have a subnode for each observation called <ROW> and the 'variable names' and 'values' are
# all listed as attributes.
# Since all the data is stored in the one and only child node of the root node <DATA> called <ROWS>, I'll
# apply the function xmlAttrs to the child node (here written as xmltop[[1]]) to extract the attributes from the
# <ROWS> child  node.

xml_childnode_attr <- xmlApply(xmltop[[1]], xmlAttrs)
# Ok, th is gives me a Large list of 77,257 elements (the number of incidents), so I'm
# going to convert this into a dataframe by first "unlisting" the Large list, specifying the number
# of rows (77,257) and saying I want the data unlisted by row. Then I'll convert it into a matrix, keeping
# strings as character variables (not autmoatically factors). Then I'll convert it to a dataframe.

avi_data <- data.frame(matrix(unlist(xml_childnode_attr), nrow=77257, byrow=T),stringsAsFactors=FALSE)

# Now I have the XML data, all 77,257 incidents as a dataframe. 
# Now I want to clean this up a bit, set the blank space data (originally listed as "" in the XML document when missing info on the attribute)
# and explore prior to appending the JSON data.

# i'll first add in the variable names that didn't get carried over during the unlist procedure.

names(avi_data) <- c("EventId", "InvestigationType", "AccidentNumber", "EventDate", "Location", "Country", 
                     "Latitude", "Longitude", "AirportCode", "AirportName", "InjurySeverity", "AircraftDamage",
                     "AircraftCategory", "RegistrationNumber", "Make", "Model", "AmateurBuilt", "NumberofEngines",
                     "EngineType", "FARDescription", "Schedule", "PurposeOfFlight", "AirCarrier", "TotalFatalInjuries",
                     "TotalSeriousInjuries", "TotalMinorInjuries", "TotalUninjured", "WeatherCondition", "BroadPhaseOfFlight",
                     "ReportStatus", "PublicationDate")



# checking for missing data
avi_data[!complete.cases(avi_data),]
# 0 rows with missing data, so the blank spaces weren't converted to NA in the unlisting/dataframe procedure.
# So now I'll set all the cells in the dataframe that are blank ("") to NA
avi_data[avi_data==""] <- NA
avi_data[avi_data=="N/A"] <- NA

# Now I'll just take a look at some of the variables to make sure they're in the format I want them in.
class(avi_data$EventId)
# Character, although these are numbers, but they're ID numbers so it really doesn't matter and they'll most likely
# be brought in as characters from the JSON files.

class(avi_data$InvestigationType)
# character but probably best as factor.
avi_data$InvestigationType <- as.factor(avi_data$InvestigationType)
table(avi_data$InvestigationType, useNA = "ifany")
# 74,207 accidents, 3050 incidents.

# AccidentNumber
class(avi_data$AccidentNumber)
# That's character, and we can just leave it as that.

# EventDate
class(avi_data$EventDate)
# changing EventDate to date format in R.
avi_data$EventDate <- mdy(avi_data$EventDate)
class(avi_data$EventDate)
# Now event date is a POSIXct format, not sure if that'll be helpful later.
sorted_date_table <- sort(table(avi_data$EventDate, useNA = "ifany"), decreasing = TRUE)
sorted_date_table[1:10]
# This shows the top ten dates when events happened. The max number of events on any one day is 25. Nothing really interesting here.

# Location is a character, and given the fact that it's unlikely there are locations where incidents happened many many times (and
# we'd probably have a lot of 1 frequencies) I'm just going to leave this as a character variable. Although there are some issues
# with missing data, I can split this by the comma using the strsplit function, then find the frequencies of elements (i.e. locations)
# to see where flights happen the most.
locations <- strsplit(avi_data$Location, ",")
# turn this into a dataframe of individual elements
loc_data <- data.frame(matrix(unlist(locations)),stringsAsFactors=FALSE)

locs_table <- sort(table(loc_data, useNA = "ifany"), decreasing = TRUE)
locs_table[1:10]

# We can create a simple visualization of the percentages of incidents by location, looking at the top ten
# by dividing the frequencies by the number of total incidents (includes NA for location)
loc_table_percentages <- 100*(locs_table/nrow(avi_data))
# Now, we can plot out the percentages and list the location name on the X axis.
graph_name_1 <- "incident_locations.png"
png(file = graph_name_1)
plot(loc_table_percentages[1:10], type="b", main="Top Ten Incident Locations", xlab="Top Ten Incident Locations", ylab = "Percentage of Total Incidents", xaxt = "n")
axis(1,1:10, labels=names(loc_table_percentages[1:10]))
dev.off()
# Here we can see California, Florida, Texas, and Arkansas are the big contributers (which makes sense, at least given the population
# and number of airports in California, Florida, and Texas)

# Country
avi_data$Country <- as.factor(avi_data$Country)
sorted_country_table <- sort(table(avi_data$Country, useNA = "ifany"), decreasing = TRUE)
sorted_country_table[1:10]
# Top ten countries. The USA has 73,076 (which makes sense since it's a U.S. national data collection effort)
# followed by NA for country (510), Canada (225), Mexico (197), Brazil (191), Bahamas(188), UK (188), Australia (178)
# France (153), and Germany (143)
# So NTSB is good at collecting data in their region, and from developed countries. 
# We can plot this out, and given the count, it's probably best to do it as a percentage of the overall count of incidents.c
sorted_country_incident_percentages <- 100*(sorted_country_table/sum(sorted_country_table))
sorted_country_incident_percentages[1:10]
plot(sorted_country_incident_percentages[1:10], type="b", xlab="Top Ten Countries", ylab = "Percentage of Incidents", xaxt = "n")
axis(1,1:10, labels=names(sorted_country_incident_percentages[1:10]))
# Just a simple visualization to show that USA has majority of percentage of incidents, 
# but then the following are in the same region/development level.


# AirportName
avi_data$AirportName <- as.factor(avi_data$AirportName)
sorted_airport_table <- sort(table(avi_data$AirportName, useNA = "ifany"), decreasing = TRUE)
sorted_airport_table[1:10]
# Ok, so here we note that 31,136 cases don't explicitly list the airport name
# Then the rest of the top ten are some derivation of being a "Private Airstrip" or listing "None" for name.
# in 9th place is Municipal airport (most likely a catchall), and then finally 75 incidents at MERRILL FIELD
sorted_airport_table[10:20]
# This shows a few more of the actual airports, and the list itself gives us an idea of the airports that report
# accidents most frequently.



##### Quick Mapping of Lat and Long #####

# Here I'll make use of the lat and long variables to produce a quick map of that data that do have locational data points.
# I'll bring in the package rworldmap (and it's dependencies [requires Xcode] to plot the point data)
graphics.off()
path = as.character(getwd())
graph_name <- "world_flight_incidents.pdf"
pdf(file = graph_name)
newmap <- getMap(resolution = "low")
plot(newmap, asp = 1, main="Map of Flight Incidents")
points(avi_data$Longitude, avi_data$Latitude, cex = .1)
dev.off()

# This shows what we knew already, that the overwhelming majority of the incidents occurred in the United States.
# However, here we can see that there were plenty of events that occurred at sea (and not just at the 0, 0 lat long point
# that sometimes causes odd events to be found in the middle of the sea.). 

## Injury Severity ##
avi_data$InjurySeverity <- as.factor(avi_data$InjurySeverity)
sorted_injury_severity_table <- sort(table(avi_data$InjurySeverity, useNA = "ifany"), decreasing = TRUE)
sorted_injury_severity_table[1:10]
# Here I'll plot out the top ten as well to show the most likely injury severity including missing data.
sorted_injury_sev_percentages <- 100*(sorted_injury_severity_table/nrow(avi_data))
sorted_injury_sev_percentages[1:10]
plot(sorted_injury_sev_percentages[1:10], type="b", xlab="Top Ten Injury Severity Types", ylab = "Percentage of Incidents", xaxt = "n")
axis(1,1:10, labels=names(sorted_injury_sev_percentages[1:10]))

sorted_injury_sev_percentages[1]
sorted_injury_sev_percentages[2]
sorted_injury_sev_percentages[3]
sorted_injury_sev_percentages[4]
sorted_injury_sev_percentages[5]
sorted_injury_sev_percentages[6]
sorted_injury_sev_percentages[7]
sorted_injury_sev_percentages[8]
sorted_injury_sev_percentages[9]
sorted_injury_sev_percentages[10]



sum(sorted_injury_sev_percentages[2:3], sorted_injury_sev_percentages[5:7], sorted_injury_sev_percentages[9])

# So here we can see the most common injury severity (76%) is non-fatal. Single fatality accidents
# make up roughly 10% of cases (most likely a sole pilot), double fatality comprise 6% of accidents.
# Then incidents (which presumably aren't the same as an accident) make up 4%. This pattern continues 
# so we can see that Non-fatal accidents are the overwhelming majority (76%), and incidents (no fatalities) make
# up 4%. When we omit NA (106 cases) and data points where the fatality number was unavailable, we can see that 
# the great majority of accidents where there were even any fatalities list under 6 fatalities (indicating a small plane)
# and so large fatality plane crashes (here anything over 6 fatalities, even when aggregated, make up less than 1% of all observations.
sum(sorted_injury_sev_percentages[11:length(sorted_injury_sev_percentages)])

## Aircraft Damage ##
avi_data$AircraftDamage <- as.factor(avi_data$AircraftDamage)
aircraft_damage_table <- sort(table(avi_data$AircraftDamage, useNA = "ifany"), decreasing = TRUE)
aircraft_damage_pct_table <- 100*(aircraft_damage_table/nrow(avi_data))
aircraft_damage_pct_table
# Here we can see 72% of observations had substantial damage to the aircraft, 22% of observations
# had an accident that resulted in the plane being destroyed. 3% had minor damage, and in 3% of cases 
# data was missing on the severity of the aircraft damage.


## Aircraft Category ##
avi_data$AircraftCategory <- as.factor(avi_data$AircraftCategory)
table(avi_data$AircraftCategory, useNA = "ifany")
aircraft_cat_table <- sort(table(avi_data$AircraftCategory, useNA = "no"), decreasing = TRUE)
aircraft_cat_table
# missing data is 60,737 on this one.
aircraft_cat_pct_table <- 100*(aircraft_cat_table/sum(aircraft_cat_table))
aircraft_cat_pct_table

# quick look at how much is missing
count(is.na(avi_data$AircraftCategory))/nrow(avi_data)
# 79% of cases missing data here.
# omitting observations with missing data on this variable (79%), of the remaining cases
# 86% are airplanes, followed by 11% helicopters. This also makes sense since airplanes
# are the most common commercial flying vehicle.

## Make of the plane ##
avi_data$Make <- as.factor(avi_data$Make)

make_table <- sort(table(avi_data$Make, useNA = "ifany"), decreasing = TRUE)
make_pct_table <- 100*(make_table/nrow(avi_data))
make_pct_table[1:10]
sum(make_pct_table[1:6])
# 55% of the planes involved in incidents are made by Cessna, Piper, or Beech
# manufacturers of small airplanes (max passengers between 11-14 depending on 
# company)

fatal_data <- avi_data[which(avi_data$InjurySeverity!="Non-Fatal" & avi_data$InjurySeverity!="Incident" & is.na(avi_data$InjurySeverity)==FALSE),]
# These are all fatal accidents. Now I'll look to see if the make of planes differs.
fatal_make_table <- sort(table(fatal_data$Make, useNA = "ifany"), decreasing = TRUE)
fat_make_pct_table <- 100*(fatal_make_table/nrow(fatal_data))
fat_make_pct_table[1:10]
sum(fat_make_pct_table[1:6])
# The top six for fatal accidents were again some derivation (lower or uppercase)
# of Cessna, Piper, or Beech, comprising 53% of fatal accident plane makes.

## amateur built ##

avi_data$AmateurBuilt <- as.factor(avi_data$AmateurBuilt)
table(avi_data$AmateurBuilt, useNA = "ifany")
100*table(avi_data$AmateurBuilt, useNA = "ifany")/nrow(avi_data)
# 90% not amateur built in complete data
fatal_data$AmateurBuilt <- as.factor(fatal_data$AmateurBuilt)
100*table(fatal_data$AmateurBuilt, useNA = "ifany")/nrow(fatal_data)
# 85% not amateur built in fatal accidents, so a slight increase in amateur-built
# proportions but not a huge shift.

# number of engines #
class(avi_data$NumberofEngines)
# character, need to switch to numeric.
avi_data$NumberofEngines <- as.numeric(avi_data$NumberofEngines)
mean(avi_data$NumberofEngines, na.rm = TRUE)
range(avi_data$NumberofEngines, na.rm = TRUE)
unique(avi_data$NumberofEngines)
# so the average is 1.15 engines, but they range from 0 (hot air ballon/glider)
# to 24.



## Purpose of Flight ##
avi_data$PurposeOfFlight <- as.factor(avi_data$PurposeOfFlight)
table(avi_data$PurposeOfFlight)
purpose_table <- sort(table(avi_data$PurposeOfFlight, useNA = "ifany"), decreasing = TRUE)
purpose_table_pct <- 100*purpose_table/nrow(avi_data)
purpose_table_pct[1:10]
# 56% personal flights, 12% instructional.
fatal_data$PurposeOfFlight <- as.factor(fatal_data$PurposeOfFlight)
purpose_table_fatal <- sort(table(fatal_data$PurposeOfFlight, useNA = "ifany"), decreasing = TRUE)
purpose_table_pct_fatal <- 100*purpose_table_fatal/nrow(fatal_data)
purpose_table_pct_fatal[1:10]
# In fatalities, we have 59% personal, 10% unknown, with a lower percentage of
# instructional flights.

## Total Fatal Injuries ##
avi_data$TotalFatalInjuries <- as.numeric(avi_data$TotalFatalInjuries)
sum(avi_data$TotalFatalInjuries, na.rm = TRUE)
# all told, we're looking at 44,017 fatalities


## Weather Conditions ##
avi_data$WeatherCondition <- as.factor(avi_data$WeatherCondition)
table(avi_data$WeatherCondition, useNA = "ifany")
100*table(avi_data$WeatherCondition, useNA = "ifany")/nrow(avi_data)
# So in 7% of incidents, there was instrument meteorological conditions.
# in 1% conditions were unknown, in 89% there were visual meteorological conditions
# in the full dataset.

# fatal data
fatal_data$WeatherCondition <- as.factor(fatal_data$WeatherCondition)
100*table(fatal_data$WeatherCondition, useNA = "ifany")/nrow(fatal_data)
# Here, in fatal accidents, we can see there were a higher percentage of 
# instrument meteorological conditions, so it's likely that instrument failure
# leads to a higher probability of fatality.



## Broad Phase of Flight ##
avi_data$BroadPhaseOfFlight <- as.factor(avi_data$BroadPhaseOfFlight)
table(avi_data$BroadPhaseOfFlight, useNA = "ifany")
phase_pct_table <- 100*sort(table(avi_data$BroadPhaseOfFlight, useNA = "ifany"), decreasing = TRUE)/nrow(avi_data)
sum(phase_pct_table[1:2])
# Here, we can see that 43% of accidents/incidents occurred during takeoff or landing (moreso
# landing than takeoff). 14% occurred in Cruise, 12% while maneuvering, 10% during approach.

# Fatal Accidents Phase #
fatal_data$BroadPhaseOfFlight <- as.factor(fatal_data$BroadPhaseOfFlight)
fatal_phase_pct_table <- 100*sort(table(fatal_data$BroadPhaseOfFlight, useNA = "ifany"), decreasing = TRUE)/nrow(fatal_data)
fatal_phase_pct_table
# For fatal accidents, however, 24% of accidents occurred while maneuvering
# 19% during cruise, with 15% of the data understandably missing. Only 16% of fatal
# accidents occurred during takeoff or landing, with 14% coming during takeoff.


#### Measure of Fatal Flight or Not ####

# It may be useful to eventually have an indicator of whether an accident/incident flight
# was fatal or not, so that we'll have some variation to look at as far indicators go.
# Now, for predictive modeling, this won't really work, since we're only looking at a
# universe (assuming we have all accident/incident flights) or sample of flights that had
# some sort of problem, not just any flight (which would be a massive data collection effort). 
# However, if we have a measure of fatal or not (and exclude the NA's or "None's") then among
# problem flights, we at least still have some variation for a potential dependent variable. 

sorted_injury_severity_table
# ok so looking at the different categories for injury severity, there are only three (omitting NA's)
# that are not fatal: "Non-Fatal", "Incident", or "Unavailable". So first I want to simply create a measure of 
# fatal or not (so unavailable's would be "not", since they're not specifically marked as fatal - and they're only 193 cases of 77K).
# To create this variable, I can use an ifelse condition.

avi_data$fatal <- ifelse(avi_data$InjurySeverity=="Non-Fatal", 0, 
                         ifelse(avi_data$InjurySeverity=="Incident", 0, 
                                ifelse(avi_data$InjurySeverity=="Unavailable", 0, 
                                       1)))

table(avi_data$fatal, useNA = "ifany")
class(avi_data$fatal)
# Now I'll make this a factor.
avi_data$fatal <- as.factor(avi_data$fatal)
# getting percentages
100*table(avi_data$fatal, useNA = "ifany")/nrow(avi_data)
# So as we knew before, 79.9% non-fatal, 19.9% Fatal, 0.12% NA.


### Creating a Categorial Measure of Injury Severity ###

# Now it might be useful to have a categorical variable that shows the dispersion of deaths.
table(avi_data$TotalFatalInjuries, useNA = "ifany")
# Here we can see we have 40,363 flights with 0 fatalities, but this is lower than our
# number of cases that were non-fatal, incidents, or unavaiable (58499 + 3050 + 193 = 61,742)
# This is most likely a data-entry issue, since we have 21,466 NA's for the TotalFatalInjury
# variable, and only 106 NA for the InjurySeverity question, so what I'll want to do is
# create a new TotalFatalInjury variable, and if an observation has a value of Non-Fatal, 
# Incident, or Unvailable for the InjurySeverity variable, and has a value of NA for the TotalFatalInjury
# variable, I'll assign it a 0. So I'd like to have only 106 NA on this new variable to match that from 
# the InjurySeverity variable. So I can do this with a nested ifelse statement again.

avi_data$total_fatalities <- (ifelse(avi_data$InjurySeverity=="Non-Fatal", 0,
                                     ifelse(avi_data$InjurySeverity=="Incident", 0,
                                            ifelse(avi_data$InjurySeverity=="Unavailable", 0,
                                                   avi_data$TotalFatalInjuries))))

table(avi_data$total_fatalities, useNA = "ifany")
class(avi_data$total_fatalities)
# Ok, now I feel we have a relatively good indicator of total fatalities that takes care of the NA problem (now we have 106
# NA just like in the InjurySeverity variable). From here we can make a categorical/ordinal variable. Now this 
# won't be a standard bucket type category, because I think there are differences between 1 fatality (single pilot, probably small plain)
# and 2 fatalities. Since looking at the top makes of planes involved in accidents/incidents and fatalities, it's clear they 
# are made by small-plane manufacturers (Cessna, Piper, Beech ,Bell, etc.) that have at the largest, a capacity of 14 (based
# on inspection of their current line from their website, admittedly they could have made larger planes in the past).
# So what I'll do is make a variable with the following categories.
# 0 = 0 fatalities.
# 1 = 1 fatality
# 2 = 2 fatalities
# 3 = 3 - 14 fatalities
# 4 = 15+

# This will be a broad variable, but it will give us some sense in the distribution of fatalities in all accident-flights,
# and distribution of fatalities in fatal flights.

avi_data$fatality_num <- ifelse(avi_data$total_fatalities==0, 0, 
                                ifelse(avi_data$total_fatalities==1, 1,
                                       ifelse(avi_data$total_fatalities==2, 2,
                                              ifelse(avi_data$total_fatalities>=3 & avi_data$total_fatalities<=14, 3,
                                                     4))))


avi_data$fatality_cat <- ifelse(avi_data$total_fatalities==0, "0 Deaths", 
                                ifelse(avi_data$total_fatalities==1, "1 Death",
                                       ifelse(avi_data$total_fatalities==2, "2 Deaths",
                                              ifelse(avi_data$total_fatalities>=3 & avi_data$total_fatalities<=14, "Between 3 to 14 Deaths",
                                                     "Fifteen or More Deaths"))))

avi_data$fatality_cat <- as.factor(avi_data$fatality_cat)

table(avi_data$fatality_cat, useNA = "ifany")

hist_deaths <- ggplot(avi_data, aes(x = fatality_cat))
hist_deaths + geom_bar() + ggtitle("Aviation Fatalities") + ylab("Count") + xlab("Fatalities")

fatality_pct_table <- 100*sort(table(avi_data$fatality_cat, useNA = "ifany"), decreasing = TRUE)/nrow(avi_data)
fatality_pct_table
plot(fatality_pct_table, type="b", main="Aviation Fatalities as Percent of Total Incidents", xlab="Aviation Fatalities", ylab = "Percentage of Overall Incidents", xaxt = "n")
axis(1,1:6, labels=names(fatality_pct_table[1:6]))

##############################################################
#### Part 2 - Bringing in JSON Data for Narative Analysis ####
##############################################################

# Now I'll be bringing in the Narrative and Probable Cause Text Data in JSON format.

setwd('/Users/christopherinkpen/PSU_Class_Documents/PSU_Class_Documents/Job_Market_Materials/RTI/exercises/exercise02')

# Since I've already set my working directory as the exercises02 folder so I can save graphs/results 
# there.
# I'm first going to create a path variable just to direct to the data folder
# Then create a character vector of the names of the json files by pulling in
# all files with the .json extension.
inputDir <- "data/"
files.v <- dir(path=inputDir, pattern=".*json")

# just checking to make sure everything loaded in the correct character
# format and location
class(files.v[1])
files.v[1]

# Now I'll reset my working directory to the exercise02/data folder to pull files directly from there.
setwd('/Users/christopherinkpen/PSU_Class_Documents/PSU_Class_Documents/Job_Market_Materials/RTI/exercises/exercise02/data')

# creating an empty data frame to append the json files to.
text_data <- data.frame()

# Now I'll write a quick for loop that that will use the fromJSON function to bring
# the individual files into R, unlist them (since they come in as a "Large list)
# convert them to a data frame (using the nrow function after querying the size
# of the list), and then append them to the empty data frame.
for(i in 1:length(files.v)){
  print(files.v[[i]])
  x <- fromJSON(file = files.v[[i]])
  print(length(x[[1]]))
  x_size <- length(x[[1]])
  new_doc <- data.frame(matrix(unlist(x), nrow=x_size, byrow=T),stringsAsFactors=FALSE)
  text_data <-rbind(text_data, new_doc)
}

# Now I'll rename the three variables so i can merge with the avi_data
# data frame and differentiate between the narrative_text and probable_cause
# fields.
names(text_data) <- c("EventId","Narrative_Text","Probable_Cause")

# setting white space to NA
text_data[text_data==""] <- NA

# checking that both are characters.
class(avi_data$EventId)
class(text_data$EventId)

# merging the datasets.
merged_data <- merge(avi_data, text_data,by="EventId")

# resetting the working directory to save graphs/model output.
setwd('/Users/christopherinkpen/PSU_Class_Documents/PSU_Class_Documents/Job_Market_Materials/RTI/exercises/exercise02')

#### Cleaning Text Data ####

# Now I'm going to do some cleaning of the text data so
# that I can check word frequencies and perform topic modeling.
# First I'll change everything to lower case.

#### Narrative Text Variable ####

# First I'll convert all the text to lower case text
Narrative_Text <- tolower(merged_data$Narrative_Text)


# Turn this into a dataframe of 77,257 rows to apply some functions to it.
Narrative_Text <- data.frame(matrix(unlist(Narrative_Text), nrow=77257, byrow=T),stringsAsFactors=FALSE)

# rename the dataframe as Narrative_Text
names(Narrative_Text) <- c("Narrative_Text")

# Now I'll read in a csv of suggested stop words (plus some specific to the NTSB data - things I saw popping
# up that wasn't germane to an explanation of the event).
stoplist_ntsb <- read.csv("stoplist_ntsb.csv",header=F)
# check the format
class(stoplist_ntsb)
# need to first convert this as a list
stoplist_ntsb.l <- as.list(stoplist_ntsb)
class(stoplist_ntsb.l)
# unlist it and turn it into a character vector so I can use it with qdap's rm_stopwords function.
stoplist_ntsb.l <- unlist(stoplist_ntsb.l)
stoplist_ntsb.l <- as.character(stoplist_ntsb.l)
# checking that we're in character vector format.
class(stoplist_ntsb.l)

# Just a test to see that we've got a character vector.
stoplist_ntsb.l[633]

# Now I'll remove the digits and apostrophe's, while keeping the periods, because qdap is unfortunately 
# merging the end of the first sentence word with the second sentence word. When we omit the period as part
# of the stoplist however, we retain the separate words without having the punctuation.
Narrative_Text$Narrative_Text <- strip(Narrative_Text$Narrative_Text, digit.remove = TRUE, apostrophe.remove = TRUE,
                                       char.keep = ".")
# using the qdap function rm_stopword to remove the stopwords from my list.
Narrative_Text_stop <- rm_stopwords(Narrative_Text$Narrative_Text, stoplist_ntsb.l, 
                                 separate = FALSE, strip = FALSE, apostrophe.remove = TRUE)
# turning the resulting Large list into a dataframe.
Narrative_Text_stopwords_df <- data.frame(matrix(unlist(Narrative_Text_stop), nrow=77257, byrow=T),stringsAsFactors=FALSE)
# renaming the variable to Narrative_Text_clean
names(Narrative_Text_stopwords_df) <- c("Narrative_Text_clean")
# making sure values that were turned into text as NA are understood by R as being missing.
Narrative_Text_stopwords_df[Narrative_Text_stopwords_df=="NA"] <- NA
# merging with the original dataset.
merged_data <- cbind(merged_data, Narrative_Text_stopwords_df)

##### Probable Cause Variable #####

# Now I'll run the exact same cleaning for the Probable_Cause variable
Probable_Cause <- tolower(merged_data$Probable_Cause)
Probable_Cause <- data.frame(matrix(unlist(Probable_Cause), nrow=77257, byrow=T),stringsAsFactors=FALSE)
names(Probable_Cause) <- c("Probable_Cause")
Probable_Cause$Probable_Cause <- strip(Probable_Cause$Probable_Cause, digit.remove = TRUE, apostrophe.remove = TRUE,
                                       char.keep = ".")
Probable_Cause_stop <- rm_stopwords(Probable_Cause$Probable_Cause, stoplist_ntsb.l, 
                                    separate = FALSE, strip = FALSE, apostrophe.remove = TRUE)
Probable_Cause_stopwords_df <- data.frame(matrix(unlist(Probable_Cause_stop), nrow=77257, byrow=T),stringsAsFactors=FALSE)
names(Probable_Cause_stopwords_df) <- c("Probable_Cause_clean")
Probable_Cause_stopwords_df[Probable_Cause_stopwords_df=="NA"] <- NA
merged_data <- cbind(merged_data, Probable_Cause_stopwords_df)

#######################
### Frequent Terms ####
#######################

### Narrative Text Frequent Terms ###

length(unique(merged_data$Narrative_Text_clean))

# i'm bringing this package in later because it masks some functions I need 
install.packages("quanteda")
library(quanteda)



narrative_text_corpus <- corpus(merged_data$Narrative_Text_clean)
class(narrative_text_corpus)


narrative_text_dfm <- dfm(narrative_text_corpus)

table_nt_topwords <- as.data.frame(topfeatures(narrative_text_dfm, 10), stringsAsFactors=FALSE)

library(data.table)
table_nt_topwords <- setDT(table_nt_topwords, keep.rownames = TRUE)[]

names(table_nt_topwords) <- c("word","freq")

length(narrative_text_dfm)

table_nt_topwords$percent <- 100*(table_nt_topwords$freq/length(narrative_text_dfm))

p <- ggplot(table_nt_topwords, aes(word, percent))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) 
p <- p + ggtitle("Frequency of Words in Narrative Text (top 10) as Percentage")
p 


### Probable Cause Frequent Terms ###

length(unique(merged_data$Probable_Cause_clean))

probable_cause_corpus <- corpus(merged_data$Probable_Cause_clean)
class(probable_cause_corpus)


probable_cause_dfm <- dfm(probable_cause_corpus)

table_pc_topwords <- as.data.frame(topfeatures(probable_cause_dfm, 10), stringsAsFactors=FALSE)

library(data.table)
table_pc_topwords <- setDT(table_pc_topwords, keep.rownames = TRUE)[]

names(table_pc_topwords) <- c("word","freq")

length(probable_cause_dfm)

table_pc_topwords$percent <- 100*(table_pc_topwords$freq/length(probable_cause_dfm))

p <- ggplot(table_pc_topwords, aes(word, percent))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) 
p <- p + ggtitle("Frequency of Words in Probable Cause (top 10) as Percentage")
p 


# Here we can see that the "Pilot's" (apostrophe removed) word is the most common
# with failure following. Landing is also frequent, as is loss and control. The most 
# likely explanation is the common occurrence of the phrase pilot's failure/loss of control
# during landing. 


####### Frequent Term by Year ######

## Narrative Text ##

# Just getting the year from the event date.
merged_data$year <- year(merged_data$EventDate)

class(merged_data$year)
summary(merged_data$year)
# 1948 to 2015 is year range.



# create a list of the available years.
year_length <- as.list(unique(merged_data$year, na.rm = TRUE))
class(year_length)

# remove NA value from years.
year_length <- year_length[!is.na(year_length)]

# create an emtpy dataframe
year_narrative_text <- data.frame()

# Now I'll write a for loop that creates a separate
# corpus based on the data from each year, and extracts
# the top word from that year. Obviously some years will have more
# than others (more recent years with better reporting, or outlier years
# with few incidents).

for(i in 1:length(year_length)){
  print(year_length[[i]])
  val <- year_length[i]
  newdata <- merged_data[which(merged_data$year==val), ]
  narrative_text_corpus_year <- corpus(newdata$Narrative_Text_clean)
  narrative_text_dfm_year <- dfm(narrative_text_corpus_year)
  table_nt_topwords_year <- as.data.frame(topfeatures(narrative_text_dfm_year, 1), stringsAsFactors=FALSE)
  table_nt_topwords_year <- setDT(table_nt_topwords_year, keep.rownames = TRUE)[]
  names(table_nt_topwords_year) <- c("word","freq")
  table_nt_topwords_year$year <- val
  year_narrative_text <-rbind(year_narrative_text, table_nt_topwords_year)
}

# Now I'll sort the dataframe by year.
narrative_text_topword_by_year.t <- data.table(year_narrative_text[with(year_narrative_text, order(year)), ])

# show top narrative text word by year
narrative_text_topword_by_year.t

## here we can see that over time, the top word(s) are either aircraft/pilot/airplane, 
# with a chunk in the 90's of pilot followed by airplane in the 2000's and on.

## Probable Cause ##

# create an emtpy dataframe
year_probable_cause <- data.frame()

# Now I'll write a for loop that creates a separate
# corpus based on the data from each year, and extracts
# the top word from that year. Obviously some years will have more
# than others (more recent years with better reporting, or outlier years
# with few incidents).

for(i in 1:length(year_length)){
  print(year_length[[i]])
  val <- year_length[i]
  newdata <- merged_data[which(merged_data$year==val), ]
  probable_cause_corpus_year <- corpus(newdata$Probable_Cause_clean)
  probable_cause_dfm_year <- dfm(probable_cause_corpus_year)
  table_pc_topwords_year <- as.data.frame(topfeatures(probable_cause_dfm_year, 1), stringsAsFactors=FALSE)
  table_pc_topwords_year <- setDT(table_pc_topwords_year, keep.rownames = TRUE)[]
  names(table_pc_topwords_year) <- c("word","freq")
  table_pc_topwords_year$year <- val
  year_probable_cause <-rbind(year_probable_cause, table_pc_topwords_year)
}

# Now I'll sort the dataframe by year.
probable_cause_topword_by_year.t <- data.table(year_probable_cause[with(year_probable_cause, order(year)), ])

# show top narrative text word by year
probable_cause_topword_by_year.t

# Looking at probable cause, although from 1988 to 1994, failure was the top word
# from 1995 on the top word was pilots (indicating probable cause was "pilot's" something (e.g. loss of control, failure, etc.))

#######################
### Topic Modeling ####
#######################


# I'm going to perform some topic modeling of the narrative text
# and the probable cause variables. To do this, I'll be using the "mallet" package, 
# which has a solid background in text analysis (originally as a Java tool), and is 
# frequently used in the humanities to analyze large corpuses.

library(mallet)

setwd("/Users/christopherinkpen/PSU_Class_Documents/PSU_Class_Documents/Job_Market_Materials/RTI/exercises/exercise02")

## Narrative Text ##

# I'll first set up an instance for the topic model, a pointer for Java.
mallet.instances.nt <- mallet.import(merged_data$EventId,
                                  merged_data$Narrative_Text_clean,
                                  stoplist.file = "stoplist_ntsb.csv")

# Here I'll list the number of topics to use. Understandably this is subjective
# but because I want a broad overview of the corpus, I'm selecting ten, as this is
# also the default setting in Mallet and suggested for exploratory analyses where
# no pre-set notion of topics is available.
topic.model.nt <- MalletLDA(num.topics = 10)

# Now i will load the instances into the topic model place-holder.
topic.model.nt$loadDocuments(mallet.instances.nt)


# This command grabs every word in the topic model
vocabulary.nt <- topic.model.nt$getVocabulary()
head(vocabulary.nt)

# I can now use the topic model to check word frequencies.
# They are the same as the previous word frequencies.
word.freqs.nt <- mallet.word.freqs(topic.model.nt)
head(word.freqs.nt)

# Now I will set a seed so I can replicate this model again.
set.seed(1234)
# This sets the number of iterations of the topic model I want to run.
# Greater numbers usually lead to higher quality models up to a certain point
# and this can be compared based on the log-likelihood figure (lower = better)
# but for the sake of convenience, I'll set the number at 400. The burn-in (amount initially
# discarded) is set at 200.
topic.model.nt$train(400)

# I'll now make a matrix of all the words by topic, and show the proportion 
# in which they appear in each topic.
topic.words.m.nt <- mallet.topic.words(topic.model.nt,
                                    smoothed = TRUE,
                                    normalized = TRUE)

# we can confirm the number of unique words (51,738) assigned
# to ten different topics.
dim(topic.words.m.nt)

# I'll add the word names here to the matrix.
colnames(topic.words.m.nt) <- vocabulary.nt


# Now I can show the top ten words in each topic
for(i in 1:10){
  print(mallet.top.words(topic.model.nt, topic.words.m.nt[i,], 10))
}


path = "/Users/christopherinkpen/PSU_Class_Documents/PSU_Class_Documents/Job_Market_Materials/RTI/exercises/exercise02/"

## we can also visualize the top words with word clouds for the topics.

for(i in 1:10){
  topic.top.words.nt_i <- mallet.top.words(topic.model.nt, topic.words.m.nt[i,], 50)
  title <- paste("Narrative Cause Word Cloud for Topic", i)
  graph_name <- paste("WC_Narrative_Text_Topic_", i, ".png", sep = "")
  #print(graph_name)
  png(file = graph_name)
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, title)
  wordcloud(topic.top.words.nt_i$words,
          topic.top.words.nt_i$weights,
          c(4,.8), rot.per=0, random.order = F,
          main = "Title")
  dev.off()
}

## Now I can create a matrix that has the proportion of each topic
# by observation (aka document).

doc.topics.m.nt <- mallet.doc.topics(topic.model.nt,
                                  smoothed = TRUE,
                                  normalized = TRUE)
# I can create this as a dataframe and rename the variables
doc.topics.m.nt.df <- as.data.frame(doc.topics.m.nt)
names(doc.topics.m.nt.df) <- c("NT_topic 1", "NT_topic 2", "NT_topic 3", "NT_topic 4", "NT_topic 5",
                               "NT_topic 6", "NT_topic 7", "NT_topic 8", "NT_topic 9", "NT_topic 10")


# Finally, I can append the proportions to the original dataframe for future analysis.
doc.topics.df <- cbind(merged_data, doc.topics.m.nt.df)



## Probable Cause ##

# I'll first set up an instance for the topic model for probable cause, a pointer for Java.
mallet.instances.pc <- mallet.import(merged_data$EventId,
                                     merged_data$Probable_Cause_clean,
                                     stoplist.file = "stoplist_ntsb.csv")

#listing 10 topics
topic.model.pc <- MalletLDA(num.topics = 10)

# Now i will load the instances into the topic model place-holder.
topic.model.pc$loadDocuments(mallet.instances.pc)


# This command grabs every word in the topic model
vocabulary.pc <- topic.model.pc$getVocabulary()
head(vocabulary.pc)

# I can now use the topic model to check word frequencies.
# They are the same as the previous word frequencies.
word.freqs.pc <- mallet.word.freqs(topic.model.pc)
head(word.freqs.pc)

# Now I will set a seed so I can replicate this model again.
set.seed(1234)

topic.model.pc$train(400)

# I'll now make a matrix of all the words by topic, and show the proportion 
# in which they appear in each topic.
topic.words.m.pc <- mallet.topic.words(topic.model.pc,
                                       smoothed = TRUE,
                                       normalized = TRUE)

# we can confirm the number of unique words (51,738) assigned
# to ten different topics.
dim(topic.words.m.pc)

# I'll add the word names here to the matrix.
colnames(topic.words.m.pc) <- vocabulary.pc


# Now I can show the top ten words in each topic
for(i in 1:10){
  print(mallet.top.words(topic.model.pc, topic.words.m.pc[i,], 10))
}


path = "/Users/christopherinkpen/PSU_Class_Documents/PSU_Class_Documents/Job_Market_Materials/RTI/exercises/exercise02/"

## we can also visualize the top words with word clouds for the topics.

for(i in 1:10){
  topic.top.words.pc_i <- mallet.top.words(topic.model.pc, topic.words.m.pc[i,], 50)
  title <- paste("Probable Cause Word Cloud for Topic", i)
  graph_name <- paste("WC_Probable_Cause_Topic_", i, ".png", sep = "")
  #print(graph_name)
  png(file = graph_name)
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, title)
  wordcloud(topic.top.words.pc_i$words,
            topic.top.words.pc_i$weights,
            c(4,.8), rot.per=0, random.order = F,
            main = "Title")
  dev.off()
}

## Now I can create a matrix that has the proportion of each topic
# by observation (aka document).

doc.topics.m.pc <- mallet.doc.topics(topic.model.pc,
                                     smoothed = TRUE,
                                     normalized = TRUE)
# I can create this as a dataframe and rename the variables
doc.topics.m.pc.df <- as.data.frame(doc.topics.m.pc)
names(doc.topics.m.pc.df) <- c("PC_topic 1", "PC_topic 2", "PC_topic 3", "PC_topic 4", "PC_topic 5",
                               "PC_topic 6", "PC_topic 7", "PC_topic 8", "PC_topic 9", "PC_topic 10")


# Finally, I can append the proportions to the original dataframe for future analysis.
doc.topics.df <- cbind(doc.topics.df, doc.topics.m.pc.df)








