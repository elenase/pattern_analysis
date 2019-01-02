
#rm(list = ls())
############################################################################
cat("Package installation\n")
### Uncomment to load the packages used in this analysis
#lab.packages <- c("lattice", "gmodels", "car","ggplot2","extrafont","ggthemes","zoo","reshape2",
#"maptools","rgdal","rgeos","ggmap","sp",")
#install.packages(pkgs=lab.packages)

packages <- c(#"ggplot2", # package for elegant data visualization using the Grammar of Graphics
              "Hmisc", # generate a detailled describtion of a given dataset 
              # "AER",  # interesting datasets
              "lattice",
              "tufte",
              "VGAM",
              "aod",
              "fields", 
              "stringr", # manipulation of string data
              "ellipse",
              "pastecs","XML",
              "devtools", # package used to load packages hosted in github -- install CURL before and separately
              "plyr","hexbin",
              "vcd", # Visualisation of categorical data
              "reshape2", # package to easily melt data to long form
              "RColorBrewer", # a package offering color palette from 
              "viridis", # another color palette,
              "extrafont", ##" load additional font
              "sp","maptools","rgdal","rgeos","ggmap","cartography", "SpatialPosition",
              "sqldf", "geojsonio",  #PBSmapping", ## packages used for the maps --
              ## install gdal and geos separately before  http://robinlovelace.net/r/2013/11/26/installing-rgdal-on-ubuntu.html
              "raster","classInt","lubridate","date","gdata","gridExtra","scales",
              "ggthemes", ## load different custmised theme: excel, stata, economist, tufte, wall street journal...
              "xkcd", ## Style from the xkcd comics 
              "Amelia",
              "rattle",
              "gvlma", "glmulti",
              "scatterplot3d", "cluster", 
              "ade4",  "psych", 
              "ada", "ade4", "arules", "arulesViz", "boot",
              "C50", "car", "caret", #"CHAID",
              "combinat","cluster",
              "corrplot", "doSNOW", "e1071", "extraTrees",
              "FactoMineR", "foreach", "foreign", "gbm", 
              "prabclus",
              "glmnet", "gmodels", "grplasso", "ipred",
              "kernlab", "leaps", "LiblineaR",
              "MASS", "missForest", "nnet", "plsRglm", "misc3d",
              "prim", "pROC", "pscl",
              "questionr", "randomForest",
              "randtoolbox", "rgl", #"rgrs",
              "ROCR", 
              "rpart", "rpart.plot", # "sas7bdat", 
              "snow", "speedglm", "tree",
              "animation","spacetime",
              "gridExtra","directlabels",
              "prettyR","xtable","knitr","pander",
              "files", #for creation of new folders
              "formatR", #, "gWidgetsRGtk2" # used to format the code
              #"XLConnect" ## Read and write excel files
              "PCAmixdata", ##for splitting of quantitative and qualitative data
              "ztable",
              "dummies",
              "mltools",
              "data.table",
              "poLCA",
              "qdapTools",
              "doBy",
              "ggpubr",
              "scales"
              
              
)
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

rm(packages)

# devtools::install_github("wilkelab/cowplot", force=TRUE)
# library(cowplot)

# install.packages("devtools")
# library("devtools")
# install_github("kassambara/factoextra")
# install_github('Rapporter/pander')

# loads packages into memory
#library(prettyR)
library(plyr)
library(reshape2) ## Restructure data between wide and long format before plotting them - melt and cast
#gpclibPermit()
library(zoo) ## Manage reformatting of date
library(date)
library(lubridate)

############################################
#### graphic 
#library(ggplot2) ## The grammar of graphics!
library(directlabels)
library(extrafont) ## Additional fonts
#library(ggthemes) ## Additional themes for gplot2
library(gdata)
library(gridExtra)
#library(scales)

#library(lattice)

############################################
#### Spatial Packages
library(maptools) ## Create maps
library(rgdal) ## Open geographic files
library(rgeos)
library(ggmap) ## get background map from google map
library(sp) ## Spatial library
#library(raster) ## Managing raster dataset
library(classInt) ## Classififcation
library(hexbin) ## Hexa binning
library(geojsonio)
library(cartography)
library(curl)

############################################
## Code reformatting
#library(formatR)
#library(RGtk2)
#library(gWidgetsRGtk2)
## tidy.gui('RGtk2')
library(xtable)
library(knitr)
library(pander)  ## library to format regression output in Rmd
library(tufte)
library(sqldf)  ## enables sql queries

############################################
#### Regressions

#library(Amelia) ## For missing data

#library(rattle)
#library(ada)
#library(ade4)
#library(arules)
#library(arulesViz)
#library(boot)
#library(C50)
library(car)
library(caret)
#library(CHAID)
#library(combinat)
library(cluster)
library(corrplot)
#library(doSNOW)
#library(e1071)
#library(extraTrees)
library(FactoMineR) ## Multiple correspondance analysis and classification
#library(factoextra)
#library(foreach)
#library(gbm)
library(glmnet)
#library(glmulti)
library(gmodels)
#library(grplasso)
#library(ipred)
#library(kernlab)
#library(leaps)
#library(LiblineaR)
#library(MASS)
#library(missForest)
#library(nnet)
#library(plsRglm)
#library(prim)
#library(pscl) ## used to verify the prediction power of a logistic model
library(pROC)
#library(questionr)
library(randomForest)
#library(randtoolbox)
#library(rgl)
#library(rgrs)
#library(ROCR)
#library(rpart)
library(rpart.plot)
#library(snow)
#library(speedglm)
#library(tree)

########################################
### Read other format
#library(foreign)
#library(sas7bdat)
library(gridExtra)


############################################
## Color palette
library(RColorBrewer) 
library(viridis) 

############################################
## Folder creation
library(files)

############################################
##splitting data in quanti and quali
library(PCAmixdata)

############################################
## for visualizing numeric data in table
library(ztable)

library(dummies) ## for generating dummy variables

library(mltools) ## create bins (=equal distributed groups)

library(data.table) ## changing multiple column names by name

library(poLCA) ## LCA

library(qdapTools) ## for fast lookup funtion

library(doBy) ## for summarizing based on formula

library(ggpubr) ## to arrange plots on one page

library(scales) ## makes it possible to show percent sign in legends
library(ggthemes)
############################################################################
cat("Styling Themes for Maps\n")

## theme for general text and borders which is the same in all maps
theme.chart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Calibri", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.spacing = unit(c(-.1,1,0.02,1), "cm"),
      plot.caption = element_text(size = 9, hjust = 0, color = "#595851"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      ...
    )
}

## theme for general text and borders which is the same in all maps
theme.base <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Helvetica-Narrow", color = "#22211d"),
      
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      
      panel.spacing = unit(c(-.1,1,0.02,1), "cm"),
      panel.grid.major = element_line(color = "white", size = 0.2),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      
      plot.caption = element_text(size = 9, hjust = 0, color = "#595851"),
      plot.title = element_text(size=12, face="bold", color = "black", hjust = 1), 
      plot.subtitle = element_text(size=10, face="bold", color = "black", hjust = 1),
      plot.background = element_rect(fill = "white", color = NA), 
      
      legend.title = element_text(size=12, face="bold", color = "black", hjust = 1),
      legend.direction = "vertical",
      legend.position = "right", #c(0.5, 0.01),
      legend.background = element_rect(fill = "white", color = NA),
      legend.text = element_text(size = 12, color = "black"),
      legend.margin = unit(c(1,.5,0.2,.5), "cm"),
      ...
    )
}

## theme for main choropleth map
theme.choropleth <- function(...) {
  theme(
    plot.title = element_text(hjust = 0, color = "black", size = 13, face="plain"),
    plot.subtitle = element_text(hjust = 0, color = "black", size = 13, debug = F),
    
    legend.direction = "horizontal",
    legend.position = "bottom", #c(0.5, 0.01),
    legend.text.align = 1,      #postition of label: 0=left, 1=right
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.text = element_text(size = 12, hjust = 0, color = "black"),
    legend.margin = unit(c(1,.5,.2,.5), "cm"),
    legend.key.height = unit(4, units = "mm"),                             #height of legend
    legend.key.width = unit(100/length(labels), units = "mm"),             #width of legend
    legend.title = element_text(size = 0),           #put to 0 to remove title but keep space
    
    panel.grid.major = element_line(color = "#f5f5f2", size = 0.2),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.margin = unit(c(.5,.2,1,.1), "cm"),
    
    ...
  )
}

theme.detail <- function(...) {
  theme(
    plot.subtitle = element_blank(),
    plot.margin = unit(c(0,0,0,0), "cm"),
    panel.spacing = unit(c(.1,.01,.02,.01), "cm"),
    plot.title = element_text(hjust = 0, color = "black", size = 12, debug = F),
    
    ...
  )
}

## theme for choropleth map margin of error
theme.confidence <- function(...) {
  theme(
    legend.key.height = unit(7, units = "mm"),            #height of legend
    legend.key.width = unit(6, units = "mm"),             #width of legend
    legend.key.size = unit(1.5, 'lines'),
    legend.title = element_text(size = 0),  
    
    plot.margin = unit(c(0.3,1.05,0.3,0,3), "cm"),
    
    ...
  )
}

## theme for choropleth map margin of error
theme.zscore <- function(...) {
  theme(
    legend.direction = "horizontal",
    legend.position = "bottom", #c(0.5, 0.01),
    #legend.text.align = 1,      #postition of label: 0=left, 1=right
    legend.key.height = unit(4, units = "mm"),                             #height of legend
    legend.key.width = unit(100/length(labels), units = "mm"),             #width of legend
    legend.title = element_text(size = 0),           #put to 0 to remove title but keep space
    legend.background = element_rect(fill = "white", color = NA),
    
    panel.grid.major = element_line(color = "white", size = 0.2),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    plot.margin = unit(c(0.3,1.05,0.3,0,3), "cm"),
    
    ...
  )
}



# ############################################################################
# cat("Functions\n")
# ## Function to summarize data, source (http://www.cookbook-r.com/Manipulating_data/Summarizing_data/):
# ## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
# ##   data: a data frame.
# ##   measurevar: the name of a column that contains the variable to be summariezed
# ##   groupvars: a vector containing names of columns that contain grouping variables
# ##   na.rm: a boolean that indicates whether to ignore NA's
# ##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


## function to check wheter a variable already exists in workspace
check=function(x) tryCatch(if(class(x) == 'logical') 1 else 1, error=function(e) 0) 

#############################################################################
cat("Creation of Folder Structure\n")


## mainDir: data, in original project folders 'source data' and 'reference_data' is contained
mainDir <- "data"
dir.create(file.path(mainDir, "/data_processed"), showWarnings = FALSE, recursive=TRUE)

dir.create(file.path(mainDir, "/lca"), showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(mainDir, "/lca/lca_temporal"), showWarnings = FALSE, recursive=TRUE)

dir.create(file.path(mainDir, "/mapping"), showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(mainDir, "/mapping/basemaps"), showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(mainDir, "/mapping/geojson"), showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(mainDir, "/mapping/geojson/adm0"), showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(mainDir, "/mapping/geojson/adm1"), showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(mainDir, "/mapping/geojson/adm2"), showWarnings = FALSE, recursive=TRUE)


## mainDir: out, 
mainDir <- "out"
dir.create(file.path(mainDir, "/data_exploration"), showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(mainDir, "/maps"), showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(mainDir, "/maps/adm0"), showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(mainDir, "/maps/adm1"), showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(mainDir, "/maps/risk_change"), showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(mainDir, "/spatial_regression"), showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(mainDir, "/lca"), showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(mainDir, "/lca/class_comparison"), showWarnings = FALSE, recursive=TRUE)

rm(mainDir)


if("dplyr" %in% (.packages())){
  detach("package:dplyr", unload=TRUE) 
  detach("package:plyr", unload=TRUE) 
} 
library(plyr)
library(dplyr)