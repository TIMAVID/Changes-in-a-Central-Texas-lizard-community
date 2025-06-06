##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
#                             BODY SIZE ANALYSES                           ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(curl)
library(readr)
library(dplyr) # AUSEFUL PACKAGE FOR CLEAING UP/ MANIPULATING DATA

##                     READ IN MODERN DATA                  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gitbs1<- curl("https://raw.githubusercontent.com/TIMAVID/Changes-in-a-Central-Texas-lizard-community/refs/heads/main/Modern_Lizard%20bone%20measurements.csv")
Lizard_bone_measurements <- read_csv(gitbs1) # READ THIS DATA IN
Lizard_bone_measurements <- Lizard_bone_measurements %>% select(!c(`Occipital complex_WC`, Scapulocoracoid_HG, Femur_GL, Humerus_GL, Pelvis_HA, `Ilium crest_GL`))

## FILTER MODERN DATASET
Anguids <- filter(Lizard_bone_measurements,grepl('Anguidae',Family)) # MAKING SEPARATE DATAFRAME FOR EACH TAXA
Gerrhonotines <- filter(Lizard_bone_measurements,grepl('Elgaria|Gerrhonotus',genus)) # MAKING SEPARATE DATAFRAME FOR EACH TAXA
Ophisaurus <- filter(Lizard_bone_measurements,grepl('Ophisaurus',genus)) # MAKING SEPARATE DATAFRAME FOR EACH TAXA
Scincids <- filter(Lizard_bone_measurements,grepl('Scincidae',Family))
Teiids <- filter(Lizard_bone_measurements,grepl('Teiidae',Family))
Crotaphytids <- filter(Lizard_bone_measurements,grepl('Crotaphytidae',Family))
Phrynosomatids <- filter(Lizard_bone_measurements,grepl('Phrynosomatidae',Family))
Phrynosomatids <- filter(Phrynosomatids,!grepl('Phrynosoma',genus))
Phrynosoma <- filter(Lizard_bone_measurements,grepl('Phrynosoma',genus))

## Bones function TO CREATE LINEAR MODELS FOR PREDICTING SVL-----
varlist <- names(Lizard_bone_measurements)[3:11] #all the different measurements
varlist_Ophisaurus <- names(Lizard_bone_measurements)[c(3:5, 8:11)] #all the different measurements

bones.lm<-function(variables,data) # THIS FUNCTION CREATES LINEAR MODELS FOR EACH MEASUREMENT AND ALSO PLOTS THE MEASUREMENTS WITH SVL
{ 
  require(ggplot2)
  require(gridExtra)
  figs<-lapply(variables, function(x) {
    ggplot(data = data, 
           aes(log(SVL),log(get(x)),)) + geom_point()+ ggtitle(x)+ theme_classic()+ ylab("log(Measurement)")})
  do.call(grid.arrange, c(figs, ncol=3, top = deparse(substitute(data)))) # linear model plots
  
  models <- lapply(variables, function(x) { #function to perform linear regression on all measurements for each dataset
    lm(substitute(log(SVL) ~ log(i), list(i = as.name(x))), data = data)
  })
  names(models) <- variables
  (sum <- (lapply(models, summary)))
  b<-(lapply(sum, function (x)x$coefficients[1]))
  m<-(lapply(sum, function (x)x$coefficients[2]))
  R<-(lapply(sum, function (x)x$adj.r.squared))
  P<-(lapply(sum, function (x)x$coefficients[2,4])) #may need to p.adjust
  MSE <- (lapply(sum, function (x)(mean(x$residuals^2))))
  Con <- (lapply(models, confint))
  C<-(lapply(Con, function (x)x[2,]))
  out<-list(m,b,R,P,MSE,models,C)
  names(out)<-c("slope","Y-intercept","adj.R-squared","P-value","MSE","models","ConfidenceInt")
  out <- do.call(rbind, out)
  out <- t(out)
  out <- as.data.frame(out)
  return(out)
}

###  CREATE LINEAR MODELS FOR EACH TAXON ----
Anguid_lm <-bones.lm(varlist,Anguids) # HERE WE ARE CREATING liner models of all measurements FOR TAXA IN THE MODERN LIZARD DATASETS
Gerrhonotine_lm <-bones.lm(varlist,Gerrhonotines)
Ophisaurus_lm <-bones.lm(varlist_Ophisaurus,Ophisaurus)
Scincids_lm <-bones.lm(varlist,Scincids)
Teiids_lm <-bones.lm(varlist,Teiids)
Crotaphytids_lm <-bones.lm(varlist,Crotaphytids)
Phrynosomatids_lm <-bones.lm(varlist,Phrynosomatids)
Phrynosoma_lm <-bones.lm(varlist,Phrynosoma)


##                     READ IN FOSSIL DATA                  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gitbs2 <-  curl("https://raw.githubusercontent.com/TIMAVID/Changes-in-a-Central-Texas-lizard-community/refs/heads/main/Fossil_Lizard_measurements.csv")
Total_Lizard_fossils <- Total_Lizard_fossils <- read_csv(gitbs2, 
                                                         col_types = cols(Specimen_Letter = col_character())) # READ IN THE DATAFAME FOR ALL OF THE FOSSIL MEASUREMENTS

library(tidyverse)
library(data.table)

classes5 <- seq(from = 1, to = 60, by = 1) ## 5 cm intervals # THIS CODE IS ASSIGNING A 5-CM AGE BIN TO EACH FOSSIL BASED ON THE DEPTH OF THE FOSSIL IN THE CAVE DEPOSIT
upper5 <- seq(from = 5, to = 300, by = 5)
lower5 <- seq(from = 0, to = 295, by = 5)
Total_Lizard_fossils5cmBIN = copy(Total_Lizard_fossils)
setDT(Total_Lizard_fossils5cmBIN)
interval_lookup5 <- data.table(classes5, upper5,lower5)
Total_Lizard_fossils5cmBIN<- Total_Lizard_fossils5cmBIN[interval_lookup5, Bin:=classes5, on=c("Level_min >= lower5","Level_max <= upper5")]

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                AGE-DEPTH MODEL                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(rbacon)
set.seed(123)
Bacon('HallsCave', youngest.age = 31, depths=seq(0, 350,
                                                 length=350), run = FALSE, accept.suggestions=TRUE) # set the youngest age as 31 ybp (aka 1993 CE)
times5cm<-Bacon.hist(seq(from = 2.5, to = 300, by = 5)) # V1 = min, V2 = max, V3 = median, V4 = mean
setDT(Total_Lizard_fossils5cmBIN)
meanage5cm<-(times5cm[,4])
interval_lookup5age <- data.table(meanage5cm, upper5,lower5)
Total_Lizard_fossils5cmBIN<- Total_Lizard_fossils5cmBIN[interval_lookup5age, Age:=meanage5cm, on=c("Level_min >= lower5","Level_max <= upper5")] # THIS CODE IS ASSIGNING AN AGE  TO EACH FOSSIL BASED ON THE ASSIGNED BIN

library(tidyr) #ANOTHER USEFUL PACKAGE FOR MANIPULATING/ CLEANING UP DATA

Total_Lizard_fossils5cmBIN_wide <- Total_Lizard_fossils5cmBIN %>% #NEXT WE GET FOSSIL DATA INTO A FORMAT FOR ESTIMATING SVL USING OUR LINEAR MODELS
  pivot_wider(names_from = Measurement_Type, values_from = c(`Measurement_(mm)`))
Total_Lizard_fossils5cmBIN_wide <- Total_Lizard_fossils5cmBIN_wide %>% relocate(Maxilla_LDR, Dentary_LDR, Frontal_GL, Frontal_SW, Frontal_PW, Parietal_GW, Parietal_GL, Quadrate_GH, Articular_WR)
Total_Lizard_fossils5cmBIN_wide <- Total_Lizard_fossils5cmBIN_wide %>% 
  mutate(Classification = coalesce(MS_classification, Family))

## CREATING DIFFERENT FOSSIL DATASETS BASED ON IDENTIFICATIONS
Anguid_fossils <- filter(Total_Lizard_fossils5cmBIN_wide,grepl('Anguidae',Family)) # THIS IS FILTERING AND SELECTING ONLY FOSSILS THAT BELONG TO THE FAMILY ANGUIDAE
Gerrhonotine_fossils <- filter(Total_Lizard_fossils5cmBIN_wide,grepl('Gerrhonotus',MS_classification))
Ophisaurus_fossils <- filter(Total_Lizard_fossils5cmBIN_wide,grepl('Ophisaurus',MS_classification))
Ophisaurus_fossils <- Ophisaurus_fossils %>% select(!c(Frontal_SW, Frontal_PW))
Scincid_fossils <- filter(Total_Lizard_fossils5cmBIN_wide,grepl('Scincidae',Family))
Scincinae_fossils <- filter(Scincid_fossils,!grepl('Scincella',MS_classification))

Teiid_fossils <- filter(Total_Lizard_fossils5cmBIN_wide,grepl('Teiidae',Family))
Crotaphytid_fossils <- filter(Total_Lizard_fossils5cmBIN_wide,grepl('Crotaphytidae',Family))
Phrynosomatid_fossils <- filter(Total_Lizard_fossils5cmBIN_wide,grepl('Phrynosomatidae',Family))
Phrynosomatid_fossils <- filter(Phrynosomatid_fossils,!grepl('Phrynosoma',MS_classification))
Phrynosoma_fossils <- filter(Total_Lizard_fossils5cmBIN_wide,grepl('Phrynosoma',MS_classification))
Phrynosoma_douglassi_complex_fossils <- filter(Phrynosoma_fossils,grepl('Phrynosoma douglasii species complex|cf. Phrynosoma douglasii species complex',MS_classification))
Phrynosoma_cornutum_fossils <- filter(Phrynosoma_fossils,grepl('Phrynosoma cornutum|Phrynosoma cf. cornutum',MS_classification))


###                FOSSIL BODY-SIZE PREDICTIONS                 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

estimate_SVL <- function(lm, data, x, y # FUNCTION TO PREDICT BODY SIZE # need to check specific columns [,x:y] in data match the order in the lm list and are the ones to estimate from
) #function to estimate SVL FOR FOSSILS
{
  t<- t(lm)
  t <- as.data.frame(t)
  slopes <-as.vector(t[1,])
  intercepts <- as.vector(t[2,])
  estimates <- mapply(function(m,b,data){{(exp(as.numeric(m)*
                                                 log(data) + as.numeric(b)))}}, 
                      data= data[,x:y], m=slopes, b=intercepts)
  data<-list(estimates,data$Age, data$Specimen_Number,data$Specimen_Letter, data$Family, data$Classification, data$Details)
  names(data)<-c("Estimated_SVL","Age", "Specimen", "Letter", "Family", "lower identification", "Details")
  return((data))
}

#### BODY-SIZE PREDICTIONS FOR EACH TAXON ----
Anguid_Fossil_estimates <- estimate_SVL(Anguid_lm, Anguid_fossils, x=1, y=9) # ESTAIMATING SVL FOR FOSSIL SPECIMENS BASED ON THE LINEAR MODEL PREVIOUSLY CREATED BASED ON THE MODERN SPECIMENS
Gerrhonotus_Fossil_estimates <- estimate_SVL(Gerrhonotine_lm, Gerrhonotine_fossils, x=1, y=9)
Ophisaurus_Fossil_estimates <- estimate_SVL(Ophisaurus_lm, Ophisaurus_fossils, x=1, y=7)
Scincidae_Fossil_estimates <- estimate_SVL(Scincids_lm, Scincid_fossils, x=1, y=9)
Scincinae_Fossil_estimates <- estimate_SVL(Scincids_lm, Scincinae_fossils, x=1, y=9)
Teiidae_Fossil_estimates <- estimate_SVL(Teiids_lm, Teiid_fossils, x=1, y=9)
Crotaphytidae_Fossil_estimates <- estimate_SVL(Crotaphytids_lm, Crotaphytid_fossils, x=1, y=9)
Phrynosomatidae_Fossil_estimates <- estimate_SVL(Phrynosomatids_lm, Phrynosomatid_fossils, x=1, y=9)
Phrynosoma_Fossil_estimates <- estimate_SVL(Phrynosoma_lm, Phrynosoma_fossils, x=1, y=9)
Phrynosoma_doug_cplx_Fossil_estimates <- estimate_SVL(Phrynosoma_lm, Phrynosoma_douglassi_complex_fossils, x=1, y=9)
Phrynosoma_cornutum_Fossil_estimates <- estimate_SVL(Phrynosoma_lm, Phrynosoma_cornutum_fossils, x=1, y=9)


data_formatting <-function(data) # HERE WE ARE CONVERTING THE FOSSILING ELEMENTS FROM A LIST INTO A DATAFRAME AND CLEANING UP THE DATA A LITTLE BIT
{
  require(Hmisc)
  Fossil_estimates<-llist(data)
  Fossil_estimates<- unlist(Fossil_estimates,recursive=FALSE)
  Fossil_estimates<- lapply(Fossil_estimates, data.frame, stringsAsFactors = FALSE)
  Fossil_estimates<-bind_cols(Fossil_estimates, .name_repair = c("universal_quiet"))
  Fossil_estimates<- Fossil_estimates %>% 
    rename("Age" = "X..i.....10",
           "Specimen_Number" = "X..i.....11",
           "Letter" = "X..i.....12",
           "Family" = "X..i.....13",
           "lower identification" = "X..i.....14",
           "Details" = "X..i.....15")
  # HERE WE ARE MAKING THE DATA INTO LONG FORMAT WHICH MAKES IT EASIER TO PLOT EVERTHING TOGETHER
  Fossil_estimates <- Fossil_estimates %>%  
    gather(Measurement_type, SVL_estimate, - c(Age:Details))
  Fossil_estimates %>%
    mutate(L_Column = str_extract(Fossil_estimates$Details, "\\d+(?=L)"),
           R_Column = str_extract(Fossil_estimates$Details, "\\d+(?=R)"))
}

##### DATA FORMATTING FOR PREDICTIONS ----
Anguid_Fossil_estimates <- data_formatting(Anguid_Fossil_estimates)
Gerrhonotus_Fossil_estimates <- data_formatting(Gerrhonotus_Fossil_estimates)
Scincidae_Fossil_estimates <- data_formatting(Scincidae_Fossil_estimates)
Scincinae_Fossil_estimates <- data_formatting(Scincinae_Fossil_estimates)
Teiidae_Fossil_estimates <- data_formatting(Teiidae_Fossil_estimates)
Crotaphytidae_Fossil_estimates <- data_formatting(Crotaphytidae_Fossil_estimates)
Phrynosomatidae_Fossil_estimates <- data_formatting(Phrynosomatidae_Fossil_estimates)
Phrynosoma_Fossil_estimates <- data_formatting(Phrynosoma_Fossil_estimates)
Phrynosoma_doug_cplx_Fossil_estimates <- data_formatting(Phrynosoma_doug_cplx_Fossil_estimates)
Phrynosoma_cornutum_Fossil_estimates <- data_formatting(Phrynosoma_cornutum_Fossil_estimates)

require(Hmisc)
Ophisaurus_Fossil_estimates<-llist(Ophisaurus_Fossil_estimates)
Ophisaurus_Fossil_estimates<- unlist(Ophisaurus_Fossil_estimates,recursive=FALSE)
Ophisaurus_Fossil_estimates<- lapply(Ophisaurus_Fossil_estimates, data.frame, stringsAsFactors = FALSE)
Ophisaurus_Fossil_estimates<-bind_cols(Ophisaurus_Fossil_estimates, .name_repair = c("universal_quiet"))

Ophisaurus_Fossil_estimates<- Ophisaurus_Fossil_estimates %>% 
  rename("Age" = "X..i.....8",
         "Specimen_Number" = "X..i.....9",
         "Letter" = "X..i.....10",
         "Family" = "X..i.....11",
         "lower identification" = "X..i.....12",
         "Details" = "X..i.....13")
# HERE WE ARE MAKING THE DATA INTO LONG FORMAT WHICH MAKES IT EASIER TO PLOT EVERTHING TOGETHER
Ophisaurus_Fossil_estimates <- Ophisaurus_Fossil_estimates %>%  
  gather(Measurement_type, SVL_estimate, - c(Age:Details))
Ophisaurus_Fossil_estimates <- Ophisaurus_Fossil_estimates %>%
  mutate(L_Column = str_extract(Ophisaurus_Fossil_estimates$Details, "\\d+(?=L)"),
         R_Column = str_extract(Ophisaurus_Fossil_estimates$Details, "\\d+(?=R)"))


##                PLOTTING BODY-SIZE PREDICTIONS               ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library("ggpubr")
library(mdthemes)
library(ggpmisc)

IDcolors <- c("Anguidae" = "#BDD9BF", "Gerrhonotus" = "#BDD9BF", "cf. Gerrhonotus" = "#CDF051", "Ophisaurus" = "#3C9D68", "cf. Ophisaurus" = "#468A09", "Crotaphytidae" = "#FFC857", 
              "Scincidae" = "#797664","cf. Scincidae" = "#797664", "Scincinae" = "#797664", "cf. Plestiodon" = "#636050", "Scincella" = "#FD7CC2", "Phrynosoma" = "#A25F7B" , "cf. Phrynosoma" = "#923D61",
              "Phrynosoma cornutum" =  "#541EDE"   ,"Phrynosoma cf. cornutum" = "#2E0B8A", "Phrynosoma douglasii species complex" =  "#CB8FE0" ,
              "cf. Phrynosoma douglasii species complex" = "#ECC9F9",
              "Sceloporinae" =   "#412E26", "Morphotype A" =   "#674232", "cf. Sceloporinae" = "#674232","cf. Sceloporus" = "#674232", 
              "Callisaurini" =  "#E4AC81" , "Morphotype B" = "#CA8550",
              "cf. Callisaurini" =  "#CA8550" ,
              "Urosaurus" =  "#A5313B", "Phrynosomatidae" = "#A997DF", "cf. Phrynosomatidae" = "#A997DF",  "Teiidae" = "#2E4052", "Teiinae" =   "#2E4052")



###### Anguidae ######
Anguid_Fossil_estimates <- rbind(Gerrhonotus_Fossil_estimates, Ophisaurus_Fossil_estimates)

## ANGUIDAE
ggplot(Anguid_Fossil_estimates, aes(x = Age, y = SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate),alpha = .2, method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Anguidae") + mdthemes::md_theme_classic(base_size = 16) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 230)+ theme(panel.spacing.x = unit(10, "mm"), legend.position = "bottom")+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 200) + facet_wrap("Measurement_type")

Anguid_Fossil_estimates %>% 
  filter(Details=="1L") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate),alpha = .2, method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Anguidae L") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 230)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 200) + facet_wrap("Measurement_type")

Anguid_Fossil_estimates %>% 
  filter(Details=="1R") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate),alpha = .2, method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Anguidae R") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 230)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 200) + facet_wrap("Measurement_type")

#####.....GERRHONOTUS ----
ggplot(Gerrhonotus_Fossil_estimates, aes(x = Age, y = SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate),alpha = .2, method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "*Gerrhonotus*") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 175)+ theme(panel.spacing.x = unit(10, "mm"), legend.position = "bottom")+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 175) + facet_wrap("Measurement_type")

Gerrhonotus_Fossil_estimates %>% 
  filter(Details=="1L") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate),alpha = .2, method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "*Gerrhonotus* L") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 175)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 175) + facet_wrap("Measurement_type")

Gerrhonotus_Fossil_estimates %>% 
  filter(Details=="1R") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate),alpha = .2, method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "*Gerrhonotus* R") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 175)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 175) + facet_wrap("Measurement_type")

####.....OPHISAURUS----
ggplot(Ophisaurus_Fossil_estimates, aes(x = Age, y = SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`),size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "*Ophisaurus*") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 215)+theme(panel.spacing.x = unit(10, "mm"), legend.position = "bottom")+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 180) + facet_wrap("Measurement_type")

Ophisaurus_Fossil_estimates %>% 
  filter(Details=="1L") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`),size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "*Ophisaurus* L") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 370)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 300) + facet_wrap("Measurement_type")

Ophisaurus_Fossil_estimates %>% 
  filter(Details=="1R") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`),size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "*Ophisaurus* R") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 205)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 180) + facet_wrap("Measurement_type")


###### Scincidae ######
ggplot(Scincidae_Fossil_estimates, aes(x = Age, y = SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Scincidae") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 215)+theme(panel.spacing.x = unit(10, "mm"), legend.position = "bottom")+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 180) + facet_wrap("Measurement_type")

Scincidae_Fossil_estimates %>% 
  filter(Details=="1L") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Scincidae L") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 240)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 180) + facet_wrap("Measurement_type")

Scincidae_Fossil_estimates %>% 
  filter(Details=="1R") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Scincidae R") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 175)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 150) + facet_wrap("Measurement_type")

## SCINCINAE
ggplot(Scincinae_Fossil_estimates, aes(x = Age, y = SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Scincinae") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 210)+theme(panel.spacing.x = unit(10, "mm"), legend.position = "bottom")+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 175) + facet_wrap("Measurement_type")

Scincinae_Fossil_estimates %>% 
  filter(Details=="1L") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Scincinae L") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 230)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 175) + facet_wrap("Measurement_type")

Scincinae_Fossil_estimates %>% 
  filter(Details=="1R") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Scincinae R") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 200)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 175) + facet_wrap("Measurement_type")


###### Teiidae ######
ggplot(Teiidae_Fossil_estimates, aes(x = Age, y = SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Teiidae") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 110)+theme(panel.spacing.x = unit(10, "mm"), legend.position = "bottom")+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 100) + facet_wrap("Measurement_type")

Teiidae_Fossil_estimates %>% 
  filter(Details=="1L") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Teiidae L") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 110)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 100) + facet_wrap("Measurement_type")

Teiidae_Fossil_estimates %>% 
  filter(Details=="1R") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Teiidae R") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 110)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 100) + facet_wrap("Measurement_type")


###### Crotaphytidae ######
ggplot(Crotaphytidae_Fossil_estimates, aes(x = Age, y = SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Crotaphytidae") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 120)+theme(panel.spacing.x = unit(10, "mm"), legend.position = "bottom")+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 110) + facet_wrap("Measurement_type")

Crotaphytidae_Fossil_estimates %>% 
  filter(Details=="1L") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Crotaphytidae L") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 130)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 110) + facet_wrap("Measurement_type")

Crotaphytidae_Fossil_estimates %>% 
  filter(Details=="1R") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`), size = 4, position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Crotaphytidae R") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 225)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 175) + facet_wrap("Measurement_type")

###### Phrynosomatidae ######
ggplot(Phrynosomatidae_Fossil_estimates, aes(x = Age, y = SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`),size = 4, position=position_jitter(width=50,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Phrynosomatidae") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 160)+theme(panel.spacing.x = unit(10, "mm"))+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 143) + facet_wrap("Measurement_type")

Phrynosomatidae_Fossil_estimates %>% 
  filter(Details=="1L") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`),size = 4, position=position_jitter(width=50,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Phrynosomatidae L") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 158)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 145) + facet_wrap("Measurement_type")

Phrynosomatidae_Fossil_estimates %>% 
  filter(Details=="1R") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`),size = 4, position=position_jitter(width=50,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "Phrynosomatidae R") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 158)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 145) + facet_wrap("Measurement_type")

## PHRYNOSOMA
ggplot(Phrynosoma_Fossil_estimates, aes(x = Age, y = SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`),size = 4, position=position_jitter(width=0,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "*Phrynosoma*") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 130)+theme(panel.spacing.x = unit(10, "mm"))+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 120) + facet_wrap("Measurement_type")

Phrynosoma_Fossil_estimates %>% 
  filter(Details=="1L") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`),size = 4, position=position_jitter(width=0,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "*Phrynosoma* L") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 130)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 120) + facet_wrap("Measurement_type")

Phrynosoma_Fossil_estimates %>% 
  filter(Details=="1R") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`),size = 4, position=position_jitter(width=0,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "*Phrynosoma* R") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 130)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 120) + facet_wrap("Measurement_type")

####.....PHRYNOSOMA DOUGLASII SPECIES COMPLEX ----
ggplot(Phrynosoma_doug_cplx_Fossil_estimates, aes(x = Age, y = SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`),size = 4, position=position_jitter(width=0,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "*Phrynosoma douglassi* species complex") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 125)+theme(panel.spacing.x = unit(10, "mm"), legend.position = "bottom")+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 115) + facet_wrap("Measurement_type")

Phrynosoma_doug_cplx_Fossil_estimates %>% 
  filter(Details=="1L") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`),size = 4, position=position_jitter(width=0,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "*Phrynosoma douglassi* species complex L") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 125)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 115) + facet_wrap("Measurement_type")

library(diptest)
bimodal_test <- Phrynosoma_doug_cplx_Fossil_estimates%>% # TEST FOR BIMODAL DISTRIBUTION
  filter(Measurement_type=="Dentary_LDR")
dip.test(bimodal_test$SVL_estimate)

Phrynosoma_doug_cplx_Fossil_estimates %>% 
  filter(Details=="1R") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`),size = 4, position=position_jitter(width=0,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "*Phrynosoma douglassi* species complex R") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 125)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 115) + facet_wrap("Measurement_type")

####.....PHRYNOSOMA CORNUTUM ----
ggplot(Phrynosoma_cornutum_Fossil_estimates, aes(x = Age, y = SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`),size = 4, position=position_jitter(width=0,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "*Phrynosoma cornutum*") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 135)+theme(panel.spacing.x = unit(10, "mm"), legend.position = "bottom")+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 120) + facet_wrap("Measurement_type")

Phrynosoma_cornutum_Fossil_estimates %>% 
  filter(Details=="1L") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`),size = 4, position=position_jitter(width=0,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "*Phrynosoma cornutum* L") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 135)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 120) + facet_wrap("Measurement_type")

Phrynosoma_cornutum_Fossil_estimates %>% 
  filter(Details=="1R") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = `lower identification`)) +
  geom_point(aes(color=`lower identification`),size = 4, position=position_jitter(width=0,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "*Phrynosoma cornutum* R") + mdthemes::md_theme_classic(base_size = 18) +
  stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 135)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 120) + facet_wrap("Measurement_type")


###### All lizards ######
All_fossil_estimates <- rbind(G = Gerrhonotus_Fossil_estimates, O= Ophisaurus_Fossil_estimates, S= Scincidae_Fossil_estimates, T= Teiidae_Fossil_estimates, 
                              C= Crotaphytidae_Fossil_estimates, P = Phrynosomatidae_Fossil_estimates, Phr = Phrynosoma_Fossil_estimates)

All_fossil_estimates$`lower identification` <- ifelse(All_fossil_estimates$`lower identification` == "Morphotype A", # combine common elements
                                                      "cf. Sceloporinae", All_fossil_estimates$`lower identification`)

All_fossil_estimates$`lower identification` <- ifelse(All_fossil_estimates$`lower identification` == "Morphotype B", # combine common elements
                                                      "cf. Callisaurini", All_fossil_estimates$`lower identification`)

All_fossil_estimates$`lower identification` <- ifelse(All_fossil_estimates$`lower identification` == "Morphotype B", # combine common elements
                                                      "cf. Callisaurini", All_fossil_estimates$`lower identification`)

All_fossil_estimates$`lower identification` <- ifelse(All_fossil_estimates$`lower identification` == "cf. Plestiodon", # combine common elements
                                                      "Scincinae", All_fossil_estimates$`lower identification`)

All_fossil_estimates <- All_fossil_estimates %>% 
  mutate(identification = coalesce(`lower identification`,Family)) %>% drop_na(SVL_estimate)

All_fossil_estimates$`lower identification` <- factor(All_fossil_estimates$`lower identification`, levels=c("Anguidae"  ,                               "Gerrhonotus" ,                            
                                                                                                            "cf. Gerrhonotus"    ,                      "Ophisaurus"    ,                          
                                                                                                            "cf. Ophisaurus"  ,                         "Crotaphytidae"  ,                         
                                                                                                            "Scincidae"   ,                             "Scincinae"   ,                            
                                                                                                            "cf. Plestiodon"  ,                         "Scincella"  ,                             
                                                                                                            "Phrynosomatidae"    ,                      "cf. Phrynosomatidae"    ,                 
                                                                                                            "Phrynosoma" ,                              "cf. Phrynosoma" ,                         
                                                                                                            "Phrynosoma cornutum"     ,                 "Phrynosoma cf. cornutum"    ,             
                                                                                                            "Phrynosoma douglasii species complex"  ,   "cf. Phrynosoma douglasii species complex",
                                                                                                            "Sceloporinae"      ,                       "cf. Sceloporinae"  ,                      
                                                                                                            "cf. Sceloporus"   ,                        "Callisaurini"    ,                        
                                                                                                            "cf. Callisaurini"    ,                     "Urosaurus"    ,                           
                                                                                                            "Teiidae"     ,                             "Teiinae" ))


ggplot(All_fossil_estimates, aes(x = Age, y = SVL_estimate, color = identification)) +
  geom_point(aes(color=identification), position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE, color = "black") +
  theme_bw() +
  scale_color_manual(values =IDcolors, na.value = "#000000") +theme(panel.spacing.x = unit(10, "mm"), legend.position = "none")+
  labs(x = "Years BP", y = "SVL estimates", title = "All Fossil Lizards") + facet_wrap("Measurement_type")

ggplot(All_fossil_estimates, aes(x = Age, y = SVL_estimate, color = identification)) +
  geom_point(aes(color=identification), position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() + mdthemes::md_theme_classic(base_size = 18) +
  scale_color_manual(values =IDcolors, na.value = "#000000") +theme(panel.spacing.x = unit(10, "mm"), legend.position = "right")+
  labs(x = "Years BP", y = "SVL estimates", title = "All Fossil Lizards") +stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 200)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 185) + facet_wrap("Measurement_type")

All_fossil_estimates %>% 
  filter(Details=="1L") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = identification)) +        
  geom_point(aes(color=identification), position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() + mdthemes::md_theme_classic(base_size = 18) +
  scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "All Fossil Lizards L") +stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 220)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 185) + facet_wrap("Measurement_type")

All_fossil_estimates %>% 
  filter(Details=="1R") %>% 
  ggplot(., aes(x=Age, y=SVL_estimate, color = identification)) +
  geom_point(aes(color=identification), position=position_jitter(width=30,height=0)) +
  geom_smooth(aes(x = Age, y = SVL_estimate), method = "lm", inherit.aes = FALSE ) +
  theme_bw() +
  scale_color_manual(values =IDcolors, na.value = "#000000") +
  labs(x = "Years BP", y = "SVL estimates", title = "All Fossil Lizards R") +stat_cor(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 180)+
  stat_regline_equation(aes(x = Age, y = SVL_estimate), formula = y ~ x, inherit.aes = FALSE ,label.y = 155) + facet_wrap("Measurement_type")

###.....fossil_estimates_summary ----
fossil_estimates_summary <- All_fossil_estimates %>% 
  group_by(`lower identification`) %>% 
  summarise(
    max = round(max(SVL_estimate),1),
    mean = round(mean(SVL_estimate, na.rm = TRUE),1),
    min = round(min(SVL_estimate),1)
  )