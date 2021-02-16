
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Global configuration script
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# R-packages -----------------------------------------------------------

# Data analysis
library(tidyverse) 
library(magrittr)  
library(lubridate)
library(readxl)
library(arrow) # read-in feather files

# Visualization
library(viridis)   
library(ggridges)  
library(ggfortify) 
library(ggcorrplot)
library(ggpmisc)
library(ggforce)
library(grid)      
library(gridExtra) 
library(scales) 
library(paletteer) # collection of color palettes
library(flextable)

# Parallel computing
library(foreach)   
library(doParallel) 

# Misc statistics
library(foreign)  
library(e1071)
library(Kendall) 
library(qqplotr)  
library(extRemes)
library(trend)




# Personal libraries
#devtools::install_github('tanerumit/hydrosystems')
#devtools::install_github('tanerumit/ggHydro')
library(hydrosystems)
library(ggHydro)


################################################################################

# R/Rstudio configuration ------------------------------------------------------

options(readr.num_columns = 0) #Surpress notifications for readr
num_cores <- detectCores() - 1 # num cores for parallel processing

## ggplot settings
theme_set(theme_light())
################################################################################

# Python configuration ---------------------------------------------------------

#devtools::install_github("rstudio/reticulate")
library(reticulate)

# Specify the path for the conda environment
#pathToEnv <- "C:/Users/taner/Anaconda3/envs/climate-data-analysis/"
#pathToEnv <- "C:/Users/taner/Anaconda3/envs/climate-data-analysis/"
#use_condaenv(pathToEnv, require = TRUE)
#use_python(paste0(pathToEnv, "python.exe"), require = TRUE)



################################################################################


# GGPLOT Options ---------------------------------------------------------------

### Custom themes for ggplot2

theme_custom <- function() { 
  
  font <- "Georgia"   #assign font family up front
  font_size <- 12
    
    
  theme_bw() %+replace%    #replace elements we want to change
    
    theme(
      
      # Legend title and text labels
      #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      # # Title font color size and face
      # legend.title = element_text(color, size, face),
      # # Title alignment. Number from 0 (left) to 1 (right)
      # legend.title.align = NULL,             
      # # Text label font color size and face
      # legend.text = element_text(color, size, face), 
      # # Text label alignment. Number from 0 (left) to 1 (right)
      # legend.text.align = NULL,
      # 
      # # Legend position, margin and background
      # #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      # # Legend position: right, left, bottom, top, none
      # legend.position = "right", 
      # # Margin around each legend
      # legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
      # # Legend background
      # legend.background = element_rect(fill, color, size, linetype),
      # 
      # # Legend direction and justification
      # #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      # # Layout of items in legends ("horizontal" or "vertical")
      # legend.direction = NULL, 
      # # Positioning legend inside or outside plot 
      # # ("center" or two-element numeric vector) 
      # legend.justification = "center", 
      
      # Background underneath legend keys
      #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      legend.key = element_rect(fill, color),  # Key background
      legend.key.size = unit(1.2, "lines"),    # key size (unit)
      # legend.key.height = NULL,                # key height (unit)
      # legend.key.width = NULL,                 # key width (unit)
      
      # Spacing between legends. 
      #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      legend.spacing = unit(0.4, "cm"), 
      # legend.spacing.x = NULL,                 # Horizontal spacing
      # legend.spacing.y = NULL,                 # Vertical spacing
      # 
      # Legend box
      #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      # Arrangement of multiple legends ("horizontal" or "vertical")
      legend.box = NULL, 
      # Margins around the full legend area
      legend.box.margin = margin(0, 0, 0, 0, "cm"), 
      # Background of legend area: element_rect()
      legend.box.background = element_blank(), 
      # The spacing between the plotting area and the legend box
      legend.box.spacing = unit(0.4, "cm")
    
      # Text elements
      #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      # plot.title = element_text(             #title
      #   family = font,            #set font family
      #   size = 20,                #set font size
      #   face = 'bold',            #bold typeface
      #   hjust = 0,                #left align
      #   vjust = 2),               #raise slightly
      # 
      # plot.subtitle = element_text(          #subtitle
      #   family = font,            #font family
      #   size = 14),               #font size
      # 
      # plot.caption = element_text(           #caption
      #   family = font,            #font family
      #   size = 9,                 #font size
      #   hjust = 1),               #right align
      # 
      # axis.title = element_text(             #axis titles
      #   family = font,            #font family
      #   size = 10),               #font size
      # 
      # axis.text = element_text(              #axis text
      #   family = font,            #axis famuly
      #   size = 9),                #font size
      # 
      # axis.text.x = element_text(            #margin for axis text
      #   margin=margin(5, b = 10))
      
    )
}