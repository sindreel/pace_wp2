###########################################################
#Clear memory
rm(list = ls(all = TRUE))
###########################################################

#########################################################################
#download Greenland telemetry-data
#########################################################################

#Last updated telemetry data file - 17.10.2022

#-----------------------------------------------------------------------
# Download data from external source. This is best practice for making 
# your code transportable among different computers.. 
#-----------------------------------------------------------------------

# This script downloads and merges the data needed for further analyses for the Greenland tracking project.


# first create new folder (./data/raw_data) to store the files 
# locally (cache down the data) - remember to update gitignore folder (add 
# the line */data/raw_data to the .gitignore file)
dir.create("data/raw_data", showWarnings = FALSE, recursive = TRUE)
dir.create("data/modified_data", showWarnings = FALSE, recursive = TRUE)

#change timeout option to fix problem with large files
options(timeout = max(999, getOption("timeout")))


URL_tracking_data <- "https://ntnu.box.com/shared/static/05zj75bnm855vvgsq591csm6brnqm7jh.rds" #Uploaded new version 29.11.22
URL_tagging_data <- "https://ntnu.box.com/shared/static/9zjyen83bw7ixgjm19cu6o8sjc259y6o.csv" #Uploaded new version 29.11.22
URL_receiver_deployment <- "https://ntnu.box.com/shared/static/zxzhk0sdb0o7zj2r8sk9lmsrgi8lcab4.rds" #Uploaded new version 29.11.22


#download data
download.file(url=URL_tracking_data,destfile="./data/raw_data/tracking_data.rds")  
download.file(url=URL_tagging_data,destfile="./data/raw_data/tagging_data.csv")
download.file(url=URL_receiver_deployment,destfile="./data/raw_data/receiver_deployment.rds")

#check downloaded data
tracking_data <- readRDS("./data/raw_data/tracking_data.rds")
str(tracking_data)


fishdata <- read.csv("./data/raw_data/tagging_data.csv", sep = ";")
names(fishdata)
str(fishdata)
fishdata <- fishdata[1:152, ]
fishdata$tagging_date <- as.POSIXct(fishdata$tagging_date)
fishdata$tagging_location <- as.factor(fishdata$tagging_location)
fishdata$fishID <- as.factor(fishdata$fishID)
fishdata$species <- as.factor(fishdata$species)
fishdata$transmitterID <- as.factor(fishdata$transmitterid)
fishdata$sex <- as.factor(fishdata$sex_d)
fishdata$capture_method <- as.factor(fishdata$capture_method)
saveRDS(fishdata, "./data/raw_data/tagging_data.RDS")

deployment_data <- readRDS("./data/raw_data/receiver_deployment.rds")
