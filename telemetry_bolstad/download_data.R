###########################################################
#Clear memory
rm(list = ls(all = TRUE))
###########################################################
getwd()
#test

#########################################################################
#download Beiarfjord-data
#########################################################################

#Last updated telemetry data file - 30.09.2020

#-----------------------------------------------------------------------
# Download data from external source. This is best practice for making 
# your code transportable among different computers.. 
#-----------------------------------------------------------------------

# This script downloads and merges the data needed for further analyses for the CHASES WP2 (Marine migration and physiology) paper.
# End product of this script is a .csv file containing fish info and key migratory variables (timing of marine entry, timing of marine exit, marine residence duration and categorized migratory distance)


# first create new folder (./data/raw_data) to store the files 
# locally (cache down the data) - remember to update gitignore folder (add 
# the line */data/raw_data to the .gitignore file)
dir.create("data/raw_data", showWarnings = FALSE, recursive = TRUE)
dir.create("data/modified_data", showWarnings = FALSE, recursive = TRUE)
dir.create("data/shapes", showWarnings = FALSE, recursive = TRUE)

URL_tracking_data <- "https://ntnu.box.com/shared/static/s0nec8uc89lxqrpq2z684ftzzvpq62g8.csv" #09.01.2022
#URL_tagging_data <- "https://ntnu.box.com/shared/static/iqf8qt0fb9tsc3h70y0ichs8yll3fjs7.csv" #
#URL_receiver_deployment <- "https://ntnu.box.com/shared/static/y4c7j3wkmaije6ihup92utzmd3ve1gl7.csv" #
URL_capture_release_sites <- "https://ntnu.box.com/shared/static/wrv636o9rxkt0p06qp0hdauvpnkwok4e.csv" #
URL_shapes <- "https://ntnu.box.com/shared/static/uqo7bzhky3muzbvkoe557x1toxhs3gvx.zip" #
download.file(url=URL_tracking_data,destfile="./data/raw_data/detections_bolstadfjord.csv")  
#download.file(url=URL_tagging_data,destfile="./data/raw_data/tagging_data_A20.csv")
#download.file(url=URL_receiver_deployment,destfile="./data/raw_data/receiver_deployment.csv")
download.file(url=URL_capture_release_sites,destfile="./data/raw_data/capture_release_sites.csv")
download.file(url=URL_shapes,destfile="./data/shapes/Shapes.zip")
###########################################################