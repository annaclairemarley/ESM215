# load libraries
library(tidyverse)
library(entropy)

# set working directory
setwd("~/Documents/github/ESM215/Lab2")
rm(list = ls())

# read in the data
ex2dat <- read_csv("ex2_sample_data2.csv") %>% 
  filter(landcover != -9999) %>% 
  filter(geology != -9999) %>% 
  filter(flowaccum != -9999) %>% 
  filter(winter_rad != -9999)
  
###### make tables of the different combos ############

#landcover and geology
land_geo <- table(ex2dat$landcover, ex2dat$geology)
land_geo_mi <- mi.empirical(land_geo)

#landcover and flow accumulation
land_flow <- table(ex2dat$landcover, ex2dat$flowaccum)
land_flow_mi <-mi.empirical(land_flow)

#landcover and winter radiation
land_rad <- table(ex2dat$landcover, ex2dat$winter_rad)
land_rad_mi <-mi.empirical(land_rad)

### highest mututal information is between landcover and geology

###### make tables of the different combos of subsets ############

# geol 1 and flow
geo1_land_geo1_flow <- table(ex2dat[ex2dat[,6]==1,5], ex2dat[ex2dat[,6]==1,7])
geo1_land_geo1_flow_mi <- mi.empirical(geo1_land_geo1_flow)

# try in a different way 
geo1_land_geo1_flow <- ex2dat %>% 
  filter(geology == 1) 

geo1_land_geo1_flow_tab <- table(geo1_land_geo1_flow$landcover, 
                                 geo1_land_geo1_flow$flowaccum)

geo1_land_geo1_flow_mi <- mi.empirical(geo1_land_geo1_flow_tab)

# empty dataframe
df_rad <- data.frame(Level = integer(),
                     winter_rad_mi = numeric())

# function to make this process a bit faster

  mi_secondlev = function(level = c(1,2,3,4,5)){
    
  for (i in level){
    geo_land_geo_x <- ex2dat %>% 
      filter(geology == level) 
    
    geo_land_geo_x_tab <- table(geo_land_geo_x$landcover, 
                                geo_land_geo_x$winter_rad)
    
    mi <- mi.empirical(geo_land_geo_x_tab)
    
    return(mi)
  }
    
  }

### mi of all combos ###
# FLOW

  # Geology 1 | Land | Flow
  geo1_land_geo1_flow_mi <- mi_secondlev()
  
  # Geology 2 | Land | Flow
  geo2_land_geo2_flow_mi <- mi_secondlev(level = 2)
  
  # Geology 3 | Land | Flow
  geo3_land_geo3_flow_mi <- mi_secondlev(level = 3)
  
  # Geology 4 | Land | Flow
  geo4_land_geo4_flow_mi <- mi_secondlev(level = 4)
  
  # Geology 5 | Land | Flow
  geo5_land_geo5_flow_mi <- mi_secondlev(level = 5)

# winter radiation
  
  # Geology 1 | Land | winter radiation
  geo1_land_geo1_rad_mi <- mi_secondlev(level = 1)
  # Geology 2 | Land | winter radiation
  geo2_land_geo2_rad_mi <- mi_secondlev(level = 2)
  
  # Geology 3 | Land | winter radiation
  geo3_land_geo3_rad_mi <- mi_secondlev(level = 3)
  
  # Geology 4 | Land | winter radiation
  geo4_land_geo4_rad_mi <- mi_secondlev(level = 4)
  
  # Geology 5 | Land | winter radiation
  geo5_land_geo5_rad_mi <- mi_secondlev(level = 5)
  
  
  
  
  
  
  
  
  
  
  
