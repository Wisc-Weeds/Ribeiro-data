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

# Predicted values and Confidence Interval
new_gdd <- data.frame( GDD = c(100, 500, 900))

# Herbicide Pursuit

Pursuit <- Data %>% 
  filter(trt == "purs")

pursrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Pursuit)
summary(pursrad)$r.squared
predict(pursrad, newdata = new_gdd, interval = "confidence")


pursrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Pursuit)
summary(pursrye)$r.squared
predict(pursrye, newdata = new_gdd, interval = "confidence")

purspal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Pursuit)
summary(purspal)$r.squared
predict(purspal, newdata = new_gdd, interval = "confidence")

pursfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Pursuit)
summary(pursfox)$r.squared
predict(pursfox, newdata = new_gdd, interval = "confidence")


# Herbicide Classic

Classic <- Data %>% 
  filter(trt == "clas")

clasrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Classic)
summary(clasrad)$r.squared
predict(clasrad, newdata = new_gdd, interval = "confidence")

clasrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Classic)
summary(clasrye)$r.squared
predict(clasrye, newdata = new_gdd, interval = "confidence")

claspal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Classic)
summary(claspal)$r.squared
predict(claspal, newdata = new_gdd, interval = "confidence")

clasfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Classic)
summary(clasfox)$r.squared
predict(clasfox, newdata = new_gdd, interval = "confidence")

# Herbicide FirsRate

FirstRate <- Data %>% 
  filter(trt == "first")

firstrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=FirstRate)
summary(firstrad)$r.squared
predict(firstrad, newdata = new_gdd, interval = "confidence")

firstrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=FirstRate)
summary(firstrye)$r.squared
predict(firstrye, newdata = new_gdd, interval = "confidence")

firstpal <- lm(biopercent ~ GDD, subset = spec=="pal", data=FirstRate)
summary(firstpal)$r.squared
predict(firstpal, newdata = new_gdd, interval = "confidence")

firstfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=FirstRate)
summary(firstfox)$r.squared
predict(firstfox, newdata = new_gdd, interval = "confidence")


# Herbicide Tricor

Tricor <- Data %>% 
  filter(trt == "tric")

tricrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Tricor)
summary(tricrad)$r.squared
predict(tricrad, newdata = new_gdd, interval = "confidence")

tricrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Tricor)
summary(tricrye)$r.squared
predict(tricrye, newdata = new_gdd, interval = "confidence")

tricpal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Tricor)
summary(tricpal)$r.squared
predict(tricpal, newdata = new_gdd, interval = "confidence")

tricfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Tricor)
summary(tricfox)$r.squared
predict(tricfox, newdata = new_gdd, interval = "confidence")


# Herbicide Spartan

Spartan <- Data %>% 
  filter(trt == "spart")

spartrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Spartan)
summary(spartrad)$r.squared
predict(spartrad, newdata = new_gdd, interval = "confidence")

spartrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Spartan)
summary(spartrye)$r.squared
predict(spartrye, newdata = new_gdd, interval = "confidence")

spartpal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Spartan)
summary(spartpal)$r.squared
predict(spartpal, newdata = new_gdd, interval = "confidence")

spartfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Spartan)
summary(spartfox)$r.squared
predict(spartfox, newdata = new_gdd, interval = "confidence")


# Herbicide Valor

Valor <- Data %>% 
  filter(trt == "valor")

valorrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Valor)
summary(valorrad)$r.squared
predict(valorrad, newdata = new_gdd, interval = "confidence")

valorrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Valor)
summary(valorrye)$r.squared
predict(valorrye, newdata = new_gdd, interval = "confidence")

valorpal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Valor)
summary(valorpal)$r.squared
predict(valorpal, newdata = new_gdd, interval = "confidence")

valorfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Valor)
summary(valorfox)$r.squared
predict(valorfox, newdata = new_gdd, interval = "confidence")



# Herbicide Saflufenacil

Saflufenacil <- Data %>% 
  filter(trt == "sharp")

sharprad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Saflufenacil)
summary(sharprad)$r.squared
predict(sharprad, newdata = new_gdd, interval = "confidence")

sharprye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Saflufenacil)
summary(sharprye)$r.squared
predict(sharprye, newdata = new_gdd, interval = "confidence")

sharppal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Saflufenacil)
summary(sharppal)$r.squared
predict(sharppal, newdata = new_gdd, interval = "confidence")

sharpfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Saflufenacil)
summary(sharpfox)$r.squared
predict(sharpfox, newdata = new_gdd, interval = "confidence")


# Herbicide Warrant

Warrant <- Data %>% 
  filter(trt == "war")

warrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Warrant)
summary(warrad)$r.squared
predict(warrad, newdata = new_gdd, interval = "confidence")

warrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Warrant)
summary(warrye)$r.squared
predict(warrye, newdata = new_gdd, interval = "confidence")

warpal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Warrant)
summary(warpal)$r.squared
predict(warpal, newdata = new_gdd, interval = "confidence")

warfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Warrant)
summary(warfox)$r.squared
predict(warfox, newdata = new_gdd, interval = "confidence")


# Herbicide Dual

Dual <- Data %>% 
  filter(trt == "dual")

dualrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Dual)
summary(dualrad)
predict(dualrad, newdata = new_gdd, interval = "confidence")

dualrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Dual)
summary(dualrye)$r.squared
predict(dualrye, newdata = new_gdd, interval = "confidence")

dualpal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Dual)
summary(dualpal)$r.squared
predict(dualpal, newdata = new_gdd, interval = "confidence")

dualfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Dual)
summary(dualfox)$r.squared
predict(dualfox, newdata = new_gdd, interval = "confidence")


# Herbicide Outlook

Outlook <- Data %>% 
  filter(trt == "out")

outrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Outlook)
summary(outrad)$r.squared
predict(outrad, newdata = new_gdd, interval = "confidence")

outrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Outlook)
summary(outrye)$r.squared
predict(outrye, newdata = new_gdd, interval = "confidence")

outpal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Outlook)
summary(outpal)$r.squared
predict(outpal, newdata = new_gdd, interval = "confidence")

outfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Outlook)
summary(outfox)$r.squared
predict(outfox, newdata = new_gdd, interval = "confidence")


# Herbicide Zidua

Zidua <- Data %>% 
  filter(trt == "zid")

zidrad <- lm(biopercent ~ GDD, subset = spec=="rad", data=Zidua)
summary(zidrad)$r.squared
predict(zidrad, newdata = new_gdd, interval = "confidence")

zidrye <- lm(biopercent ~ GDD, subset = spec=="rye", data=Zidua)
summary(zidrye)$r.squared
predict(zidrye, newdata = new_gdd, interval = "confidence")

zidpal <- lm(biopercent ~ GDD, subset = spec=="pal", data=Zidua)
summary(zidpal)$r.squared
predict(zidpal, newdata = new_gdd, interval = "confidence")

zidfox <- lm(biopercent ~ GDD, subset = spec=="fox", data=Zidua)
summary(zidfox)$r.squared
predict(zidfox, newdata = new_gdd, interval = "confidence")
