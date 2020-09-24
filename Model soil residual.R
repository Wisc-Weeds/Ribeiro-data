# Victor Hugo Vidal Ribeiro

# Linear Models 

library(emmeans)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(lme4)

Data = read.csv("Soil1.csv")
glimpse(Data)

Data <- Data %>% 
  
  filter(trt != "check")

# Herbicide Pursuit

Pursuit <- Data %>% 
  filter(trt == "purs")

pursrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Pursuit)
summary(pursrad)$r.squared

pursrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Pursuit)
summary(pursrye)$r.squared

purspal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Pursuit)
summary(purspal)$r.squared

pursfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Pursuit)
summary(pursfox)$r.squared


# Herbicide Classic

Classic <- Data %>% 
  filter(trt == "clas")

clasrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Classic)
summary(clasrad)$r.squared

clasrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Classic)
summary(clasrye)$r.squared

claspal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Classic)
summary(claspal)$r.squared

clasfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Classic)
summary(clasfox)$r.squared

# Herbicide FirsRate

FirstRate <- Data %>% 
  filter(trt == "first")

firstrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=FirstRate)
summary(firstrad)$r.squared

firstrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=FirstRate)
summary(firstrye)$r.squared

firstpal <- lm(biopercent ~ GDD, subset = spec=="pal", data=FirstRate)
summary(firstpal)$r.squared

firstfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=FirstRate)
summary(firstfox)$r.squared


# Herbicide Tricor

Tricor <- Data %>% 
  filter(trt == "tric")

tricrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Tricor)
summary(tricrad)$r.squared

tricrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Tricor)
summary(tricrye)$r.squared

tricpal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Tricor)
summary(tricpal)$r.squared

tricfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Tricor)
summary(tricfox)$r.squared


# Herbicide Spartan

Spartan <- Data %>% 
  filter(trt == "spart")

spartrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Spartan)
summary(spartrad)$r.squared

spartrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Spartan)
summary(spartrye)$r.squared

spartpal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Spartan)
summary(spartpal)$r.squared

spartfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Spartan)
summary(spartfox)$r.squared


# Herbicide Valor

Valor <- Data %>% 
  filter(trt == "valor")

valorrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Valor)
summary(valorrad)$r.squared

valorrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Valor)
summary(valorrye)$r.squared

valorpal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Valor)
summary(valorpal)$r.squared

valorfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Valor)
summary(valorfox)$r.squared



# Herbicide Saflufenacil

Saflufenacil <- Data %>% 
  filter(trt == "sharp")

sharprad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Saflufenacil)
summary(sharprad)$r.squared

sharprye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Saflufenacil)
summary(sharprye)$r.squared

sharppal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Saflufenacil)
summary(sharppal)$r.squared

sharpfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Saflufenacil)
summary(sharpfox)$r.squared


# Herbicide Warrant

Warrant <- Data %>% 
  filter(trt == "war")

warrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Warrant)
summary(warrad)$r.squared

warrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Warrant)
summary(warrye)$r.squared

warpal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Warrant)
summary(warpal)$r.squared

warfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Warrant)
summary(warfox)$r.squared


# Herbicide Dual

Dual <- Data %>% 
  filter(trt == "dual")

dualrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Dual)
summary(dualrad)

newdata <- data.frame(GDD = 0) 

predict(dualrad, newdata, interval = "confidence")

dualrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Dual)
summary(dualrye)$r.squared

dualpal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Dual)
summary(dualpal)$r.squared

dualfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Dual)
summary(dualfox)$r.squared


# Herbicide Outlook

Outlook <- Data %>% 
  filter(trt == "out")

outrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Outlook)
summary(outrad)$r.squared

outrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Outlook)
summary(outrye)$r.squared

outpal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Outlook)
summary(outpal)$r.squared

outfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Outlook)
summary(outfox)$r.squared


# Herbicide Zidua

Zidua <- Data %>% 
  filter(trt == "zid")

zidrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Zidua)
summary(zidrad)$r.squared

zidrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Zidua)
summary(zidrye)$r.squared

zidpal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Zidua)
summary(zidpal)$r.squared

zidfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Zidua)
summary(zidfox)$r.squared
