######## Statistical analyses from Swanson & Dudycha 2025
### "Light color and nutrient availability alter trophic transfer from algae to zooplankton"
library(tidyverse)
library(pixiedust)
library(lme4)
library(lmerTest)
library(performance)
library(rstatix)

jsgr<-read.csv("jsgr_data.csv")
chi<-read.csv("chi_treatment.csv")
######### Survivorship chi-squared, stats presented parenthetically in the 
#results section of the manuscript
chi<-chi[,-1]
chis_table<-chisq_test(chi)

####### JSGR linear models

## Starting with the model with a random effect of microcosm
mod<-lmer(jsgr_neg~Light*Nutrient+(1| Microcosm), data=jsgr)
rand(mod)######### AIC is lower without random effect

###### preliminary table formatting for random effects table, Table S1 in supplement
dust(rand(mod))%>%
  sprinkle(col=2:4,round=3)%>%
  sprinkle(col=5,fn=quote(pvalString(value)))

###### linear model without random effect
mod2<-lm(jsgr_neg~Light*Nutrient, data=jsgr)
summary(mod2)
anova(mod2)
check_model(mod2)

#### linear model table formatting, Table S2 in supplement
dust(mod2)%>%
  sprinkle(col=2:4,round=3)%>%
  sprinkle(col=5,fn=quote(pvalString(value)))%>%
  sprinkle_colnames(term="Term",
                    estimate="Estimate",
                    std.error="SE",
                    statistic="T-statistic",
                    p.value="P-value")

#### ANOVA table formatting, Table 1 in manuscript
dust(anova(mod2))%>%
  sprinkle(col=2:4,round=3)%>%
  sprinkle(col=5,fn=quote(pvalString(value)))

########### path analysis, source an updated version of multigroup for later use
source("multigroup2.R")

### Testing categorical variables with piecewiseSEM package
library(piecewiseSEM)

######## piecewiseSEM
nut_ord_psem<-psem(
  lm(jsgr_neg~nut_ord+non_fil+filamentous+green,data=jsgr),### should this be level or bi?
  #lm(mean_den~non_fil+green+nut_ord,data=path),
  lm(non_fil~nut_ord,data=jsgr),
  lm(filamentous~nut_ord,data=jsgr),
  lm(green~nut_ord+non_fil,data=jsgr)
)
summary(nut_ord_psem) ### Table S3 in the supplement comes from this output

#### multigroup analysis
multigroup2(nut_ord_psem,group = "Light") ######## Tables S4 - S7 in the supplement come from this output