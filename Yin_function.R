#####################################################################################
# Fit the Yin Beta function (https://doi.org/10.1093/aob/mcg029) to plant height data                    
#####################################################################################

# clear data and plots
graphics.off()
rm(list = ls())
options(error=stop)


#install and load required libraries
if(!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse, mosaic, neldermead, rstudioapi)


# set working directory
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# Reading input data file: temperature
Temp <- read_delim("Temp_2017.txt", delim = "\t") 

# Reading calibration data file: length measurements
Height <- read_delim("Plantheight.txt", delim = "\t")


# initial parameter values before calibration
T_b <- 5     #Base temperature   
t_m <- 200   # Yin parameter   
t_e <- 250  # Yin parameter    
L_0 <- 20     # Yin parameter 
L_max <- 150 # Yin parameter: maximum height in cm   

Yin_f <- function(L_0, L_max, t_m, t_e, TT) {
  H <- ifelse(TT > t_e, L_max, L_0 + (L_max - L_0)*(1 + (t_e - TT)/(t_e-t_m))*(TT/t_e)^(t_e/(t_e-t_m)))
  return(H)
}

# Adding a column in the datafile Temp for growing degree days based on base temperature
Temp <- Temp %>%
  dplyr::mutate(gddDay_yin = pmax(Temp-T_b, 0),
                gddSum_yin = cumsum(gddDay_yin))

# Join the dataframes of Temperature and observed height data 
FitData <- Temp %>% inner_join(Height, by="DOY")

# Pivot the wide dataframe into long format
FitData_Long <- FitData %>% pivot_longer(starts_with("Plant"), names_to = "Plant", names_prefix = "Plant_", values_to = "H")

# Estimate the parameters per plant by nls function
FitList <- FitData_Long %>%
  group_by(Plant) %>%
  group_map(~ nls(formula = H ~ Yin_f(L_0, L_max, t_m, t_e, gddSum_yin), data = .x, 
                start = list(L_0 = min(.x$H, na.rm =T), L_max = max(.x$H, na.rm=T), t_m = 200, t_e = 250 ),
                algorithm = "port",
                lower = c(L_0 = 0, L_max = 20, t_m = 0, t_e = 40),
                upper = c(L_0 = 60, L_max = 180, t_m = 1000, t_e = 1500))$m$getPars())

# Extract the parameter values and organize in dataframe + save
Fits <- bind_rows(FitList)
Fits <- Fits %>%
  dplyr::mutate(Plant = unique(FitData_Long$Plant))

saveRDS(Fits, "Yin_Parameters_Pilot_Height")

####################################
## generate check plots for fits

#Fits <- readRDS("Yin_Parameters_Pilot_Height")

Yin_f <- function(L_0, L_max, t_m, t_e, TT)
  
Plotfun <- function(L_0, L_max, t_m, t_e, Plant){
  PlotPlant <- Plant
  PlotPoints <- FitData_Long %>% 
    filter(Plant==PlotPlant)
  
  PlotData <- Temp %>%
    dplyr::mutate(Yinpred = Yin_f(L_0, L_max, t_m, t_e, gddSum_yin))

  ggplot() + 
    geom_line(data = PlotData, aes(x=DOY, y=Yinpred), color='red') +
    geom_point(data= PlotPoints, aes(x=DOY, y=H))
  
}

Plots <- Fits %>% 
  group_by(Plant) %>%
  group_map(~Plotfun(.x$L_0, .x$L_max, .x$t_m, .x$t_e, .x$Plant), .keep=T)

pdf("allplots.pdf")
Plots
dev.off()

