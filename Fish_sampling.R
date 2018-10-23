##===================Install needed packages

#    install.packages('dplyr')
#    install.packages('ggplot2')
#    install.packages('scales')
#    install.packages('magrittr')
#    install.packages('lemon')
#    install.packages('Hmisc')

library(dplyr)
library(ggplot2)
library(scales)
library(magrittr)
library(lemon)
library(Hmisc)
library(ggpubr)


#-------------read directory contents
dir()

##==================Import working data
Ois_data <- read.csv('data_files/Ois_fish_data.csv')
Ois_Ab_Mass <- read.csv('data_files/Ois_abundance_biomass.csv')

##==========================Import functions and themes
source('R_script/Fish_sampling_themes.R')     # Import ggplot themes
source('R_script/Fish_functions.R')   # Import all the fish length freqency functions

#----------Check the working data
names(Ois_data)    # check variable names
class(Ois_data)    # check data class
str(Ois_data)      # check data structure
glimpse(Ois_data)  # check data structure with dplyr
summary(Ois_data)  # check data summary



#---------Create new variable names
names(Ois_data)[4:5] <- c('Length_mm', 'Weight_g')
names(Ois_data)[10:12] <- c('Strech.length_m',
                            'Stretch.width_m', 'Discharge_m3/s')


#---------Create and rename levels of Lower.Upper.site
  levels(Ois_data$Lower.Upper.site)     # check the name of levels
  
  levels(Ois_data$Lower.Upper.site) <- c('Lower', 'Upper', # add a new level
                                         'Rothschild-Upper')  

# Give the Rothschild upper section its own name
  Ois_data$Lower.Upper.site[Ois_data$Section=='Rothschild'] <- 'Rothschild-Upper'
  
  
  levels(Ois_data$Lower.Upper.site) <- sub('^Lower$', # Rename the lower level
                                           'Hinterleiten-Lower',
                                           levels(Ois_data$Lower.Upper.site))
  
  levels(Ois_data$Lower.Upper.site) <- sub('^Upper$', # Rename the upper level
                                           'Hinterleiten-Upper',
                                           levels(Ois_data$Lower.Upper.site))
  
  levels(Ois_data$Lower.Upper.site) # Recheck the name of levels
  
# Reorder the mesohabitat factors
  Ois_data$Mesohabitat <- factor(Ois_data$Mesohabitat,  # reorder the species
                                levels = c('Pool', 'Run', 'Riffle'))
  
  levels(Ois_data$Mesohabitat)
  str(Ois_data)          # recheck structure


##==========================Set up all subsets of data

#-----------Focus on only Brown trout and Rainbow trout
Ois <- subset(Ois_data, Species == 'Salmo trutta' | Species == 'Onchorhynchus mykiss')

# 
#             #------------Divide the Ois into sections
#             Hinterleiten <- subset(Ois, Section == 'Hinterleiten')
#             Rothschild <- subset(Ois, Section == 'Rothschild')
# 
#             #------------Divide hinterland into upper and lower
#             Hinter.lower <- subset(Hinterleiten, Lower.Upper.site == 'Hinterleiten-Lower')
#             Hinter.upper <- subset(Hinterleiten, Lower.Upper.site == 'Hinterleiten-Upper')
# 
#             #------------Divide entire Ois into mesohabitats
#             Pools <- subset(Ois, Mesohabitat == 'Pool')
#             Riffles <- subset(Ois, Mesohabitat == 'Riffle')
#             Runs <- subset(Ois, Mesohabitat == 'Run')
# 
#             #-----------Divide the sections into meso habitats
#             Hinter.pools <- subset(Hinterleiten, Mesohabitat == 'Pool')
#             Hinter.riffles <- subset(Hinterleiten, Mesohabitat == 'Riffle')
#             Hinter.runs <- subset(Hinterleiten, Mesohabitat == 'Run')
#             Roth.pools <- subset(Rothschild, Mesohabitat == 'Pool')
#             Roth.riffles <- subset(Rothschild, Mesohabitat == 'Riffle')
#             Roth.runs <- subset(Rothschild, Mesohabitat == 'Run')
# 
#             #----------Divide the Hinterleiten upper and lower sites into mesohabitats
#             Upper.hinter.pools <- subset(Hinter.upper, Mesohabitat == 'Pool')
#             Upper.hinter.riffles <- subset(Hinter.upper, Mesohabitat == 'Riffle')
#             Upper.hinter.runs <- subset(Hinter.upper, Mesohabitat == 'Run')
#             Lower.hinter.pools <- subset(Hinter.lower, Mesohabitat == 'Pool')
#             Lower.hinter.riffles <- subset(Hinter.lower, Mesohabitat == 'Riffle')
#             Lower.hinter.runs <- subset(Hinter.lower, Mesohabitat == 'Run')


##=========================Start plotting the data

#----------------Single data plot of the species for the Ois
Brown_trout_single(Ois, "Ois River")
Rainbow_trout_single(Ois, "Ois River")

#---------------- 1 x 2 data plot of the Ois sections
Brown_trout_section(Ois, "Ois sections")
Rainbow_trout_section(Ois, "Ois sections")

#----------------- 1 x 3 data plot of the Ois mesohabitats
Brown_trout_mesohabitat(Ois, "Ois mesohabitats")
Rainbow_trout_mesohabitat(Ois, "Ois mesohabitats")

#----------------- 3 x 3 data plot of the mesohabitats and sites
Brown_trout_mix(Ois, "Ois mesohabitats by section")
Rainbow_trout_mix(Ois, "Ois mesohabitats by section")



##=======================Create plot of average fish lengths
Ois$Species <- factor(Ois$Species,  # reorder the species
                      levels = c('Salmo trutta', 'Onchorhynchus mykiss'))  

Avg_length(Ois,'Test')





##=============Begin data anaylsis of abundance and biomass
names(Ois_Ab_Mass)    # check variable names
colnames(Ois_Ab_Mass)[1] <- 'Species'  # rename variable
Ois_Ab_Mass$Mesohabitat <- factor(Ois_Ab_Mass$Mesohabitat,  # reorder the species
                               levels = c('Pool', 'Run', 'Riffle'))
Ois_Ab_Mass$Species <- factor(Ois_Ab_Mass$Species,  # reorder the species
                              levels = c('All Species','Salmo trutta',
                                         'Onchorhynchus mykiss',
                                         'Thymallus thymallus'))
class(Ois_Ab_Mass)    # check data class
str(Ois_Ab_Mass)      # check data structure
glimpse(Ois_Ab_Mass)  # check data structure with dplyr
summary(Ois_Ab_Mass)  # check data summary



#======================Create abundance and biomass graphs
  All_species(Ois_Ab_Mass, 'All species')
  Brown_trout(Ois_Ab_Mass, 'Brown trout')
  Rainbow_trout(Ois_Ab_Mass, 'Rainbow trout')
  Grayling(Ois_Ab_Mass, 'Grayling')
  Meso_AB(Ois_Ab_Mass, 'Mesohabitat')
  Section_AB(Ois_Ab_Mass, 'Section')

#======================Create abundance graphs
#      All_abundance(Ois_Ab_Mass, 'All Species')
#      Brown_abundance(Ois_Ab_Mass, 'Rainbow trout')
#      Rainbow_abundance(Ois_Ab_Mass, 'Rainbow trout')
#      Grayling_abundance(Ois_Ab_Mass, 'Grayling')
#      Meso_abundance(Ois_Ab_Mass, 'Meso')
#      Sec_abundance(Ois_Ab_Mass, 'Sec')

#======================Create biomass graphs
#      All_biomass(Ois_Ab_Mass, 'All Species')
#      Brown_biomass(Ois_Ab_Mass, 'Rainbow trout')
#      Rainbow_biomass(Ois_Ab_Mass, 'Rainbow trout')
#      Grayling_biomass(Ois_Ab_Mass, 'Grayling')
#      Meso_biomass(Ois_Ab_Mass, 'Meso')
#      Sec_biomass(Ois_Ab_Mass, 'Sec')


