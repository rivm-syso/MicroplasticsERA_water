
library(dplyr)


# Functions to calculate geometry of MP


## Method CSF


## possible shapes and formula required


calc_volume_CSF <- function(shape,length,diameter,CSF){
  ifelse(shape == "Sphere",
    
         volume<-(4/3 * pi * (0.5 * length)^3),
    ifelse(shape == "Fiber", 
           volume<-(pi * (0.5 * diameter )^2 * length ),
           
           volume<-((pi *(length)^3 * (CSF)^2 )/6 )
           )
  )
  return(volume)
  
  
  
}

calc_volume_CSF2 <- function(shape,length,diameter,CSF){
  
  case_when(shape == "Sphere" ~ (4/3 * pi * (0.5 * length)^3)  ,
            shape == "Fiber" ~ (pi * (0.5 * diameter )^2 * length),
            .default = (pi *(length)^3 * (CSF)^2 )/6)
  
  
  
}

###########
## Volume calculated in the Simon model
###########

Volume.ellipsoid<- function(l,w,h){
  result <- as.numeric(4/3 * pi * l *w * h)
  return(result)
}


#################
# Formulas to calculate volume and surface area based on the different assumptions (Mehinto, Redondo, Peter?, something else?)
##################


#### Surface area of an elipsoid: https://en.wikipedia.org/wiki/Ellipsoid#Approximate_formula

Surface.ellipsoid<-function(a,b,c,p){
  # P is usually 1.6
  # a = length /2 (like radius)
  # b = widht/ 2 (like radius)
  # c = height/ 2 (like radius)
  
  sa<- 4*pi*(((a*b)^p+(a*c)^p+(b*c)^p)/3)^(1/p)
  return(sa)
}


Volume.sphere <- function(r){
  result<- as.numeric(4/3*pi*r^3)
  return(result)
}

Surface.sphere <- function(r){
  result<-as.numeric(4*pi*r^2)
  return(result)
}


Volume.cilinder<- function(shortest,longest){
  result <- as.numeric((shortest/2)^2 * pi * longest)
  return(result)
}

Surface.cilinder<- function(shortest,longest){
  result<- as.numeric(2*pi*shortest/2 * (shortest/2 + longest))
  return(result)
}
