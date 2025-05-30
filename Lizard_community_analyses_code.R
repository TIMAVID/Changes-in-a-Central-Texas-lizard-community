##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                Read in Fossil data                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(readr)
Fossil_lizards <- read_csv("Lizards database12_2_23.csv")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                AGE-DEPTH MODEL                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(rbacon)

# RUN AGE-DEPTH MODEL
set.seed(123)
Bacon('HallsCave', youngest.age = 31, depths=seq(0, 350,
                                                 length=350)) # set the youngest age as 31 ybp (aka 1993 CE)

# assign ages to mid-point of levels at 5-cm intervals
times5cm<-Bacon.hist(seq(from = 2.5, to = 300, by = 5)) # V1 = min, V2 = max, V3 = median, V4 = mean
times5cm_long <- Bacon.hist(seq(from = 2.5, to = 350, by = 5)) # V1 = min, V2 = max, V3 = median, V4 = mean

# obtain age estimates for borders of excavated levels
times5cmborder<-Bacon.hist(seq(from = 0, to = 300, by = 5)) # V1 = min, V2 = max, V3 = median, V4 = mean
times5cmborder[,2] <- c(times5cmborder[,2][-1], NA)

# assign ages to mid-point of levels at 15-cm intervals
times15cm<-Bacon.hist(seq(from = 7.5, to = 300, by = 15))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                BIN LEVELS & ADD AGE                      ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(data.table)

# assign bins for each 5-cm interval
classes5 <- seq(from = 1, to = 60, by = 1)
upper5 <- seq(from = 5, to = 300, by = 5)
lower5 <- seq(from = 0, to = 295, by = 5)
Fossil_lizards5cmBIN = copy(Fossil_lizards)
setDT(Fossil_lizards5cmBIN)
interval_lookup5 <- data.table(classes5, upper5,lower5)
Fossil_lizards5cmBIN<- Fossil_lizards5cmBIN[interval_lookup5, Bin:=classes5, on=c("Level_min >= lower5","Level_max <= upper5")]

# assign min, average, and max ages for each 5-cm interval based on bins
setDT(Fossil_lizards5cmBIN)
meanage5cm<-c(times5cm[,4])
minage5cm<-c(times5cmborder[,1])
minage5cm<-minage5cm[-61]
maxage5cm<-c(times5cmborder[,2])
maxage5cm<-maxage5cm[-61]
interval_lookup5age <- data.table(meanage5cm, upper5,lower5)
interval_lookup5Minage <- data.table(minage5cm, upper5,lower5)
interval_lookup5Maxage <- data.table(maxage5cm, upper5,lower5)
Fossil_lizards5cmBIN<- Fossil_lizards5cmBIN[interval_lookup5age, Age:=meanage5cm, on=c("Level_min >= lower5","Level_max <= upper5")]
Fossil_lizards5cmBIN<- Fossil_lizards5cmBIN[interval_lookup5Minage, MinAge:=minage5cm, on=c("Level_min >= lower5","Level_max <= upper5")]
Fossil_lizards5cmBIN<- Fossil_lizards5cmBIN[interval_lookup5Maxage, MaxAge:=maxage5cm, on=c("Level_min >= lower5","Level_max <= upper5")]


# assign bins for each 15-cm interval
classes15 <- seq(from = 1, to = 20, by = 1)
upper15 <- seq(from = 15, to = 300, by = 15)
lower15 <- seq(from = 0, to = 285, by = 15)
Fossil_lizards15cmBIN = copy(Fossil_lizards)
setDT(Fossil_lizards15cmBIN)
interval_lookup15 <- data.table(classes15, upper15,lower15)
Fossil_lizards15cmBIN<- Fossil_lizards15cmBIN[interval_lookup15, Bin:=classes15, on=c("Level_min >= lower15","Level_max <= upper15")]

# assign ages for each 15-cm interval based on bins
setDT(Fossil_lizards15cmBIN)
meanage15cm<-c(times15cm[,3])
interval_lookup15age <- data.table(meanage15cm, upper15,lower15)
Fossil_lizards15cmBIN<- Fossil_lizards15cmBIN[interval_lookup15age, Age:=meanage15cm, on=c("Level_min >= lower15","Level_max <= upper15")]

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                Visualize NISP BY PIT                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr)
library(ggplot2)
library(cowplot)

#..........................Total number of fossils in data set.........................
LIZTotal <- Fossil_lizards %>% summarise(NISP = sum(NISP))

#..........................Visualize fossils by pit...........................
LIZ_PIT_VIZ <- Fossil_lizards5cmBIN %>% filter(!is.na(Bin)) %>%  group_by(Bin, Pit) %>% summarise(NISP = sum(NISP))

levels(as.factor(Fossil_lizards5cmBIN$Pit))

LIZ_PIT_VIZ$Pit <- ifelse(LIZ_PIT_VIZ$Pit == "1E", # combine common elements
                     "1DE", LIZ_PIT_VIZ$Pit)
LIZ_PIT_VIZ$Pit <- ifelse(LIZ_PIT_VIZ$Pit == "1D", # combine common elements
                     "1DE", LIZ_PIT_VIZ$Pit)
LIZ_PIT_VIZ$Pit <- ifelse(LIZ_PIT_VIZ$Pit == "1D/E", # combine common elements
                     "1DE", LIZ_PIT_VIZ$Pit)

LIZ_PIT_VIZ <- LIZ_PIT_VIZ %>% group_by(Bin, Pit) %>% summarise(NISP = sum(NISP))

LIZ_PIT_VIZ <- filter(LIZ_PIT_VIZ,grepl('1|1A|1B|1C|1DE',Pit)) #filter out uncertain identifications
LIZ_PIT_VIZ <- filter(LIZ_PIT_VIZ,!grepl(',|1/2|East|West|Trench|Possibly',Pit)) #filter out uncertain provenance

LIZ_PIT_VIZ_plot<-ggplot(LIZ_PIT_VIZ, aes(Bin, NISP))+ geom_point()+
  geom_line(colour="grey")+
  scale_x_reverse(breaks =seq(0,100,2))+
  scale_y_continuous(breaks =seq(0,2000,150))+
  xlab("BIN 5 cm")+ylab("NISP")+
  coord_flip() +
  theme_classic(base_size = 17) + facet_grid(~LIZ_PIT_VIZ$Pit,scales = "free", space = "free")
LIZ_PIT_VIZ_plot

#........................Total fossils IDENTIFIED TO FAMILY.......................

LIZFamTotal <- Fossil_lizards %>% filter(!is.na(Family)) %>% summarise(NISP = sum(NISP)) # total number of fossil identified to family

#.......................Fossils identified to family by pit.......................

famcolors <- c("#BDD9BF", "#FFC857", "#A997DF", "#929084", "#2E4052")

LIZNISP_Pit <- Fossil_lizards5cmBIN
LIZNISP_Pit$Pit <- ifelse(LIZNISP_Pit$Pit == "1E", # combine common elements
                          "1DE", LIZNISP_Pit$Pit)
LIZNISP_Pit$Pit <- ifelse(LIZNISP_Pit$Pit == "1D", # combine common elements
                          "1DE", LIZNISP_Pit$Pit)
LIZNISP_Pit$Pit <- ifelse(LIZNISP_Pit$Pit == "1D/E", # combine common elements
                          "1DE", LIZNISP_Pit$Pit)
LIZNISP_Pit$Pit <- ifelse(LIZNISP_Pit$Pit == "W Trench Pit 1", # combine common elements
                          "1", LIZNISP_Pit$Pit)

LIZNISP_Pit <- LIZNISP_Pit %>% 
  #filter(!is.na(Details))  %>%
  filter(!is.na(Family)) %>% 
  group_by(Family,Age,Pit, .drop=FALSE) %>% tally
LIZNISP_Pit <- LIZNISP_Pit %>% group_by(Family, Age, Pit) %>% summarise(NISP = max(n))
LIZNISP_Pit <- na.omit(LIZNISP_Pit)
LIZNISP_Pit <- filter(LIZNISP_Pit,grepl('1|1A|1B|1C|1DE',Pit)) #filter out uncertain identifications
LIZNISP_Pit <- filter(LIZNISP_Pit,!grepl('cf|Pleurodonta',Family)) #filter out uncertain identifications
LIZNISP_Pit_wide <- spread(LIZNISP_Pit, Family, NISP)
LIZNISP_Pit_wide[is.na(LIZNISP_Pit_wide)] <- 0
LIZNISP_Pit_long<- tidyr::gather(LIZNISP_Pit_wide, Family, NISP, Anguidae:Teiidae, factor_key=TRUE)


LIZVIZ2_plot<-ggplot(LIZNISP_Pit_long, aes(Age, NISP, fill= Family))+ # absolute lizard family NISP by pit
  geom_area()+
  #geom_bar(position="fill", stat="identity", width=800)+
  scale_x_reverse(breaks =seq(0,20000,2000))+
  scale_y_continuous(breaks =seq(0,300,30))+
  xlab("Age (cal. BP)")+ylab("NISP")+
  coord_flip() + scale_fill_manual(name = "Family", values=c(famcolors))+
  theme_classic(base_size = 17) +facet_grid(~LIZNISP_Pit_long$Pit,scales = "free", space = "free")
LIZVIZ2_plot # absolute lizard family NISP by pit

LIZVIZ3_plot<-ggplot(LIZNISP_Pit_long, aes(Age, NISP, fill= Family))+ # relative abundance lizard family NISP by pit
  geom_area(position = "fill", aes(fill = Family))+
  #geom_bar(position="fill", stat="identity", width=800)+
  #geom_line(linewidth = 2)+
  scale_x_reverse(breaks =seq(0,20000,2000))+
  scale_y_continuous(breaks =seq(0,300,30))+
  xlab("Age (cal. BP)")+ylab("NISP")+
  coord_flip() +scale_fill_manual(name = "Family", values=c(famcolors))+
  theme_classic(base_size = 17) +facet_grid(~LIZNISP_Pit_long$Pit,scales = "free", space = "free")
LIZVIZ3_plot # relative abundance lizard family NISP by pit

plot_grid(LIZVIZ2_plot, LIZVIZ3_plot, ncol = 1, labels = "auto", align = "hv", # Absolute and relative abundance of lizard families from each pit based on NISP
          axis = "lr")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           ELEMENT REPRESENTATION                        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library("ggpubr")

levels(as.factor(Fossil_lizards5cmBIN$Element))

Fossil_lizards5cmBIN_element <- filter(Fossil_lizards5cmBIN,!grepl('tooth bearing element|jugal?|longbones|roofing bone',Element)) #filter out uncertain identifications
Fossil_lizards5cmBIN_element$Element <- ifelse(Fossil_lizards5cmBIN_element$Element == "ilium", # combine common elements
                                               "pelvis", Fossil_lizards5cmBIN_element$Element)
Fossil_lizards5cmBIN_element$Element <- ifelse(Fossil_lizards5cmBIN_element$Element == "surangular", # combine common elements
                                               "surangular_articular", Fossil_lizards5cmBIN_element$Element)
Fossil_lizards5cmBIN_element$Element <- ifelse(Fossil_lizards5cmBIN_element$Element == "articular", # combine common elements
                                               "surangular_articular", Fossil_lizards5cmBIN_element$Element)
Fossil_lizards5cmBIN_element$Element <- ifelse(Fossil_lizards5cmBIN_element$Element == "dermarticular", # combine common elements
                                               "surangular_articular", Fossil_lizards5cmBIN_element$Element)
Fossil_lizards5cmBIN_element$Element <- ifelse(Fossil_lizards5cmBIN_element$Element == "surangular_articular", # combine common elements
                                               "compound bone", Fossil_lizards5cmBIN_element$Element)
Fossil_lizards5cmBIN_element$Element <- ifelse(Fossil_lizards5cmBIN_element$Element == "sphenoid", # combine common elements
                                               "braincase", Fossil_lizards5cmBIN_element$Element)
Fossil_lizards5cmBIN_element$Element <- ifelse(Fossil_lizards5cmBIN_element$Element == "otooccipital", # combine common elements
                                               "braincase", Fossil_lizards5cmBIN_element$Element)
Fossil_lizards5cmBIN_element$Element <- ifelse(Fossil_lizards5cmBIN_element$Element == "prootic", # combine common elements
                                               "braincase", Fossil_lizards5cmBIN_element$Element)
Fossil_lizards5cmBIN_element$Element <- ifelse(Fossil_lizards5cmBIN_element$Element == "supraoccipital", # combine common elements
                                               "braincase", Fossil_lizards5cmBIN_element$Element)
Fossil_lizards5cmBIN_element$Element <- ifelse(Fossil_lizards5cmBIN_element$Element == "supraoccipital", # combine common elements
                                               "braincase", Fossil_lizards5cmBIN_element$Element)
Fossil_lizards5cmBIN_element$Element <- ifelse(Fossil_lizards5cmBIN_element$Element == "supraoccipital", # combine common elements
                                               "braincase", Fossil_lizards5cmBIN_element$Element)
levels(as.factor(Fossil_lizards5cmBIN_element$Element))


LIZ_element_total <- Fossil_lizards5cmBIN_element %>% group_by(Element) %>% summarise(n = n()) # element count for all levels
LIZ_element_level <- Fossil_lizards5cmBIN_element %>% drop_na(Bin) %>% group_by(Bin) %>% summarise(n = n()) # element count by level

Element_rep <- aggregate(data = Fossil_lizards5cmBIN_element,   # Applying aggregate to count # of unique elements per bin
                         Element ~ Bin,
                         function(x) length(unique(x)))

Element_rep <- data.frame(yr=meanage5cm,# make dataframe of number of elements & total number of identified specimens per interval
                          Bin=as.vector(as.matrix(Element_rep$Bin)),
                          Num=(as.vector(as.matrix(Element_rep$Element))),
                          NISP=(as.vector(as.matrix(LIZ_element_level$n))))

Element_rep_plot<-ggplot(Element_rep )+ #plot
  geom_line(aes(yr, Num), colour="black")+
  geom_bar(aes(x=yr, y=NISP),stat="identity", fill="cyan",colour="#006000")+
  scale_x_reverse(breaks =seq(0,100000,2000))+
  scale_y_continuous(breaks =seq(0,500,20))+
  xlab("Age (cal. BP)")+ylab("# Unique Elements")+
  coord_flip() +
  theme_classic(base_size = 17)
Element_rep_plot

Element_rep_log <- data.frame(yr=meanage5cm,# make dataframe of log-transformed number of elements & total number of identified specimens per interval
                           Bin=as.vector(as.matrix(Element_rep$Bin)),
                           Num=log(as.vector(as.matrix(Element_rep$Num))),
                           NISP=log(as.vector(as.matrix(LIZ_element_level$n))))

ggscatter(Element_rep_log, x = "NISP", y = "Num", # number of unique elements by total number of identified specimens
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "log(NISP)", ylab = "log(Number of unique elements)")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           BEAKAGE CLUSTERING                                        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library("vegan")
library("rioja")
library(riojaPlot)

levels(as.factor(Fossil_lizards5cmBIN$Element))

#filter out elements not scored for completeness
Fossil_lizard_5bin_break <- filter(Fossil_lizards5cmBIN,!grepl('braincase|tooth bearing element|jugal?|longbones|postorbitofrontal?|roofing bone|atlas|axis|vertebra|metacarpals_metatarsals|osteoderm|phalanges|ribs|horn|otooccipital|palatine|prootic|sphenoid|supraoccipital',Element)) #filter out elements not scored for completeness
Fossil_lizard_5bin_break<-droplevels(Fossil_lizard_5bin_break)
levels(as.factor(Fossil_lizard_5bin_break$Element))

LizBreak <- Fossil_lizard_5bin_break %>% 
  filter(!is.na(Quadrant))  %>%
  group_by(Bin,Element,Age, .drop=FALSE) %>% summarise(PctPresent = mean(Quadrant)) # mean percent completeness summary

levels(as.factor(LizBreak$Element))
levels(LizBreak$Element)

# recode elements as cranial (C) or postcranial (P)
LizBreak <- LizBreak %>% 
  mutate(Position = recode(Element, "articular"="C"     ,       "braincase"="C" ,            "coronoid"  ="C" ,          
                           "dentary"="C" ,              "ectopterygoid" ="C" ,       "femur" ="P",               
                           "frontal" ="C" ,             "humerus" ="P",             "ilium"  ="P",             
                           "interclavicle" ="P",       "maxilla" ="C" ,             "nasal" ="C" ,  "compound bone"= "C",            
                           "parietal"   ="C" ,          "pelvis" ="P",              "postorbital"="C" ,         
                           "prefrontal" ="C" ,          "premaxilla"="C" ,           "pterygoid" ="C" ,          
                           "quadrate" ="C" ,            "radius"   ="P",            "scapulocorocoid" ="P",    
                           "splenial" ="C" ,            "squamosal"="C" ,            "surangular" ="C" ,         
                           "surangular_articular"="C" , "tibia" ="P",               "ulna" ="P",   "postfrontal"="C", "dermarticular"="C" ,          
                           "vomer"="C"  ))

BreakSum <- LizBreak %>% 
  filter(!is.na(PctPresent))  %>% filter(Age < 16200) %>% # analyze all fossils younger than 16,200 ybp together
  group_by(Position) %>% summarise(PctCompl = mean(PctPresent))

BreakBinSum <- LizBreak %>% 
  filter(!is.na(PctPresent))  %>% filter(!is.na(Bin)) %>% filter(Age < 16200) %>% # analyze all fossils younger than 16,200 ybp by age
  group_by(Bin,Position, Age) %>% summarise(PctCompl = mean(PctPresent)) %>% na.omit()

BreakBinSum_plot<-ggplot(BreakBinSum, aes(Age, PctCompl))+
  geom_line(aes(color = Position), linewidth=2)+
  scale_x_reverse(breaks =seq(0,100000,2000))+
  xlab("Age (cal. BP)")+ylab("Percent complete")+
  coord_flip() +
  theme_classic(base_size = 17)
BreakBinSum_plot

BreakBinSum_C <- filter(BreakBinSum, Position == "C") # cranial completeness by age
BreakBinSum_P <- filter(BreakBinSum, Position == "P") # postcranial completeness by age


# CRANIAL
break.dist_C <- dist(BreakBinSum_C$PctCompl) # distance matrix
break.chclust_C <- chclust(break.dist_C, method="coniss") # Constrained hierarchical clustering
plot(break.chclust_C)
bstick(break.chclust_C, ng=20, plot=TRUE) # hierarchical clustering comparison with broken stick model

library(dendextend)
# Convert the "hclust" object into a "dendrogram" object
dend_break_C <- as.dendrogram(break.chclust_C)
# Plot the dendrogram with coloured branches
dend_break_C %>% set("branches_k_color", k = 2) %>% plot
# Use standard colors for clusters
clusters <- cutree(dend_break_C, 2)[order.dendrogram(dend_break_C)]
dend_break_C %>%
  set("branches_k_color", k = 2, value = unique(clusters) + 1) %>%
  plot
dend_break_C %>% rect.dendrogram(k=2, border = "black")

# POSTCRANIAL
break.dist_P <- dist(BreakBinSum_P$PctCompl)
break.chclust_P <- chclust(break.dist_P, method="coniss")
plot(break.chclust_P)
bstick(break.chclust_P, ng=20, plot=TRUE)

# Convert the "hclust" object into a "dendrogram" object
dend_break_P <- as.dendrogram(break.chclust_P)
# Plot the dendrogram with coloured branches
dend_break_P %>% set("branches_k_color", k = 7) %>% plot
# Use standard colours for clusters
clusters <- cutree(dend_break_P, 7)[order.dendrogram(dend_break_P)]
dend_break_P %>%
  set("branches_k_color", k = 7, value = unique(clusters) + 1) %>%
  plot
dend_break_P %>% rect.dendrogram(k=7, border = "black")


# CRANIAL AND POSTCRANIAL MERGED
Break_total <- merge(BreakBinSum_C, BreakBinSum_P, by.x = "Age", by.y = "Age", suffixes = c(".C",".P"), no.dups = TRUE, all = TRUE) %>% 
  dplyr::select(!starts_with('Bin.P') & !starts_with("Position")) # remove duplicate columns
Break_total$Bin.C <- as.numeric(Break_total$Bin.C)
depthBreak <- Break_total[,1:2]%>%
  mutate( 
    Bin = Bin.C*5
  )

## RIOJA PLOT
breaks <- data.frame(name = colnames(Break_total[,3:4]), Group = c("Breakage Cranial", "Breakage Postcranial")) # goup factor for riojaplot
breaks$Group <-as.factor(breaks$Group)

lithology <- data.frame( # Hall's Cave lithology for riojaplot
  top=   c(0, 30, 80, 100, 155, 205), 
  bottom=c(30, 80, 100, 155, 205, 250),
  lithology=c("Unit 1","Unit1.2", "Unit 2", "Unit 3", "Unit 4a", "Unit 4b"), #from Toomey (1993)
  colour=c("black","saddlebrown","white",  "brown4","#ff0000", "#d5381a")
)
myfun <- function(x, style) {
  x %>% purrr::pwalk(~rect(0, ..1, 1, ..2, col=..4)) 
}

riojaPlot(Break_total[,3:4], depthBreak, selVars=colnames(Break_total[,3:4]), groups=breaks,lithology=lithology,
          scale.percent=FALSE, 
          plot.groups=TRUE, ymin=0, ymax=250, sec.ymin=0, sec.ymax=16200, sec.yinterval=1000, yvar.name="Bin",
          sec.yvar.name="Age",
          plot.sec.axis=TRUE,
          clust = break.chclust_C,
          do.clust=FALSE,
          plot.clust=TRUE,
          plot.zones="auto",
          fun.lithology=myfun)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                  CORROSION CLUSTERING                               ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(analogue)

# corrosion dataframe by family
Fossil_lizards5cmBIN_Corrosion_fam <- Fossil_lizards5cmBIN %>% # corrosion dataframe by family
  filter(!is.na(TeethCorrosion))  %>% 
  filter(!is.na(Bin)) %>% filter(!grepl('bone', TeethCorrosion)) %>% filter(!grepl('cf|Pleurodonta', Family)) %>% 
  group_by(Family, TeethCorrosion, .drop=FALSE) %>% summarise(n = sum(NISP)) %>% na.omit()
Fossil_lizards5cmBIN_Corrosion_fam2 <- tidyr::pivot_wider(Fossil_lizards5cmBIN_Corrosion_fam, id_cols = "TeethCorrosion", names_from = "Family", values_from = "n") 
Fossil_lizards5cmBIN_Corrosion_fam2[is.na(Fossil_lizards5cmBIN_Corrosion_fam2)] <- 0
Fossil_lizards5cmBIN_Corrosion_fam2$TeethCorrosion <- as.factor(Fossil_lizards5cmBIN_Corrosion_fam2$TeethCorrosion)
# summary of proportion of tooth corrosion type for each family
cbind(Fossil_lizards5cmBIN_Corrosion_fam2$TeethCorrosion,(as.data.frame(lapply(Fossil_lizards5cmBIN_Corrosion_fam2[,2:6], function(x) x / sum(x))))) # proportion of tooth corrosion type for each family

# corrosion dataframe for levels younger than 16,200 ybp
Fossil_lizards5cmBIN_Corrosion <- Fossil_lizards5cmBIN %>% # corrosion dataframe for levels younger than 16,200 ybp
  filter(!is.na(TeethCorrosion))  %>% 
  filter(!is.na(Bin)) %>% filter(!grepl('bone', TeethCorrosion)) %>% filter(Age < 16200) %>%
  group_by(Bin, TeethCorrosion, Age, .drop=FALSE) %>% summarise(n = sum(NISP)) %>% na.omit()
Fossil_lizards5cmBIN_Corrosion_wide <- spread(Fossil_lizards5cmBIN_Corrosion, TeethCorrosion, n) # wide data format

# transform into percentages
Corrosion.pct_0 <- tran(Fossil_lizards5cmBIN_Corrosion_wide[,3:6], method = 'missing', na.rm = FALSE, na.value = 0)
Corrosion.pct <- data.frame(Age = Fossil_lizards5cmBIN_Corrosion_wide$Age, tran(Corrosion.pct_0, method = 'percent')) # CONVERT INTO PERCENTS
Corrosion.pct_0_AGE <- data.frame(Age = Fossil_lizards5cmBIN_Corrosion_wide$Age, Bin = Fossil_lizards5cmBIN_Corrosion_wide$Bin, Corrosion.pct[,2:5])

# PLOT PERCENTAGES
Stratiplot(Age ~ ., Corrosion.pct, sort = 'var', svar = c("none", "crown", "shaft_crown", "shaft"), type = 'poly',xlab = "Percentage",
           ylab ='Years Before Present')

# CORROSION CLUSTER ANALYSIS
library(compositions)
Corrosion.clr <- clr(Corrosion.pct_0) # centered log-ratio transformation
Corrosion.clr.ch <- dist(Corrosion.clr) # distance
Corrosion.clr.chclust <- chclust(Corrosion.clr.ch, method="coniss")  # Constrained hierarchical clustering
plot(Corrosion.clr.chclust)
bstick(Corrosion.clr.chclust, ng=20, plot=TRUE) # hierarchical clustering comparison with broken stick model
# Cut the dendrogram in 4 clusters
(Corrosion_gr4 <- cutree(Corrosion.clr.chclust, k = 4))
# Plot the dendrogram
plot(Corrosion.clr.chclust, hang = -1, main = "CONISS clustering")
rect.hclust(Corrosion.clr.chclust, k = 4)

Corrosion.pct_0_AGE$Bin <- as.numeric(Corrosion.pct_0_AGE$Bin)
depthCorrosion <- Corrosion.pct_0_AGE[,1:2]%>% # convert bin to depth
  mutate( 
    Bin = Bin*5
  )

# corrosion rioja plot with clusters
riojaPlot(Corrosion.pct_0_AGE[,3:6], depthCorrosion, selVars=colnames(Corrosion.pct_0_AGE[,3:6]),lithology=lithology,
          scale.percent=FALSE, 
          plot.groups=TRUE, ymin=0, ymax=250, sec.ymin=0, sec.ymax=20000, sec.yinterval=1000, yvar.name="Bin",
          sec.yvar.name="Age",
          plot.sec.axis=TRUE,
          clust = Corrosion.clr.chclust,
          do.clust=FALSE,
          plot.clust=TRUE,
          plot.zones="auto",
          fun.lithology=myfun)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##     SUMMARIZE BELOW FAMILY ID'S   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr)
library(ggplot2)
library(grid)

# SUMMARIZE BELOW FAMILY ID'S
LIZNISPlowID <- Fossil_lizards5cmBIN %>% 
  #filter(!is.na(Details))  %>%
  filter(!is.na(MS_classification)) %>% 
  group_by(MS_classification,Family,Bin, Age,MinAge,MaxAge, .drop=FALSE) %>% summarise(NISP = sum(NISP), Prescence = any(NISP>0)) %>%  filter(!is.na(Age))
levels(LIZNISPlowID$MS_classification)

LIZNISPlowID$MS_classification <- ifelse(LIZNISPlowID$MS_classification == "Morphotype A", # combine common elements
                                         "cf. Sceloporinae", LIZNISPlowID$MS_classification)
LIZNISPlowID$MS_classification <- ifelse(LIZNISPlowID$MS_classification == "Morphotype B", # combine common elements
                                         "cf. Callisaurini", LIZNISPlowID$MS_classification)
LIZNISPlowID$MS_classification <- ifelse(LIZNISPlowID$MS_classification == "Morphotype B", # combine common elements
                                         "cf. Callisaurini", LIZNISPlowID$MS_classification)
LIZNISPlowID$MS_classification <- ifelse(LIZNISPlowID$MS_classification == "cf. Plestiodon", # combine common elements
                                         "Scincinae", LIZNISPlowID$MS_classification)
# order id levels
LIZNISPlowID$MS_classification <- factor(LIZNISPlowID$MS_classification, levels=c("Gerrhonotus" , "cf. Gerrhonotus" , "Ophisaurus", "cf. Ophisaurus" , 
                                                                                  "Scincinae" , "Scincella" , "Phrynosoma"  , "cf. Phrynosoma" ,
                                                                                  "Phrynosoma cornutum"    ,"Phrynosoma cf. cornutum" , "Phrynosoma douglasii species complex"  , 
                                                                                  "cf. Phrynosoma douglasii species complex",
                                                                                  "Sceloporinae" , "cf. Sceloporinae" ,"cf. Sceloporus", "Callisaurini" ,
                                                                                  "cf. Callisaurini"  ,
                                                                                  "Urosaurus" , "Teiinae" ))


lowIDcolors <- c("Gerrhonotus" = "#BDD9BF", "cf. Gerrhonotus" = "#CDF051", "Ophisaurus" = "#3C9D68", "cf. Ophisaurus" = "#468A09", 
                 "Scincinae" = "#797664", "Scincella" = "#FD7CC2", "Phrynosoma" = "#A25F7B" , "cf. Phrynosoma" = "#923D61",
                 "Phrynosoma cornutum" =  "#541EDE"   ,"Phrynosoma cf. cornutum" = "#481FB1", "Phrynosoma douglasii species complex" =  "#CB8FE0" ,
                 "cf. Phrynosoma douglasii species complex" = "#ECC9F9",
                 "Sceloporinae" =   "#412E26",  "cf. Sceloporinae" = "#674232","cf. Sceloporus" = "#674232", 
                 "Callisaurini" =  "#E4AC81" , 
                 "cf. Callisaurini" =  "#CA8550" ,
                 "Urosaurus" =  "#A5313B", "Teiinae" =   "#2E4052")

theme_new <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # remove grids
                   panel.background = element_blank(), axis.line = element_line(colour = "black"),
                   strip.text.x = element_text(size=10, angle=90, vjust=0), # Taxa names
                   strip.background = element_blank(),
                   strip.text.y = element_text(angle = 0),
                   legend.position="right",panel.border = element_blank(),
                   axis.text.x=element_text(angle=45,hjust=1)) # Axis tick label angle

# plot low ID summary
ggplot(LIZNISPlowID, aes(Age,Prescence, colour=MS_classification))+
  geom_pointrange(aes(xmin=MinAge, xmax=MaxAge, size=(NISP)))+ scale_size(range = c(.5,2))+
  scale_x_reverse(breaks =seq(0,100000,2000))+
  xlab("Years BP")+ylab("%")+
  coord_flip()+
  theme_new+
  scale_colour_manual(values =lowIDcolors, breaks = 
                        c("Gerrhonotus" , "cf. Gerrhonotus" , "Ophisaurus", "cf. Ophisaurus" , 
                          "Scincinae" , "Scincella" , "Phrynosoma"  , "cf. Phrynosoma" ,
                          "Phrynosoma cornutum"    ,"Phrynosoma cf. cornutum" , "Phrynosoma douglasii species complex"  ,
                          "Sceloporinae" , "cf. Sceloporinae" ,"cf. Sceloporus", "Callisaurini"  ,
                          "cf. Callisaurini"  ,
                          "Urosaurus" , "Teiinae" )) + facet_grid(~LIZNISPlowID$MS_classification,scales = "free", space = "free")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##     PREPARING THE FOSSIL DATA   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr)

levels(as.factor(Fossil_lizards5cmBIN$Family)) #find all levels in data
Fossil_lizards5cmBIN <- filter(Fossil_lizards5cmBIN,!grepl('cf|Pleurodonta',Family)) #filter out uncertain identifications
levels(as.factor(Fossil_lizards5cmBIN$Family)) #find all levels in data
levels(as.factor(Fossil_lizards5cmBIN$MS_classification)) #find all levels in data
Fossil_lizards5cmBIN <- Fossil_lizards5cmBIN %>%
  filter(!is.na(Bin)) #filter out uncertain depth

#MAKE VARIABLES FACTORS
Fossil_lizards5cmBIN$Bin <- as.factor(Fossil_lizards5cmBIN$Bin)
Fossil_lizards5cmBIN$Family <- as.factor(Fossil_lizards5cmBIN$Family)
Fossil_lizards5cmBIN$MS_classification <- as.factor(Fossil_lizards5cmBIN$MS_classification)
Fossil_lizards5cmBIN$Details <- as.factor(Fossil_lizards5cmBIN$Details)
Fossil_lizards5cmBIN$Element <- as.factor(Fossil_lizards5cmBIN$Element)

# 15 cm bins
levels(as.factor(Fossil_lizards15cmBIN$Family)) #find all levels in data
Fossil_lizards15cmBIN <- filter(Fossil_lizards15cmBIN,!grepl('cf|Pleurodonta',Family)) #filter out uncertain identifications
levels(as.factor(Fossil_lizards15cmBIN$Family)) #find all levels in data
levels(as.factor(Fossil_lizards15cmBIN$MS_classification)) #find all levels in data
Fossil_lizards15cmBIN <- Fossil_lizards15cmBIN %>%
  filter(!is.na(Bin))

#MAKE VARIABLES FACTORS
Fossil_lizards15cmBIN$Bin <- as.factor(Fossil_lizards15cmBIN$Bin)
Fossil_lizards15cmBIN$Family <- as.factor(Fossil_lizards15cmBIN$Family)
Fossil_lizards15cmBIN$MS_classification <- as.factor(Fossil_lizards15cmBIN$MS_classification)
Fossil_lizards15cmBIN$Details <- as.factor(Fossil_lizards15cmBIN$Details)
Fossil_lizards15cmBIN$Element <- as.factor(Fossil_lizards15cmBIN$Element)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###...     PREPARING THE DATA (NISP BASED ON ALL ELEMENTS)   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SUMMARIZE INTO NISP DATA
LIZNISP5cmBIN <- Fossil_lizards5cmBIN %>% filter(!is.na(Family)) %>% group_by(Family, Bin, Age, .drop=FALSE) %>% summarise(NISP = sum(NISP))
LIZNISP5cmBIN <- na.omit(LIZNISP5cmBIN) 

# SUMMARIZE 15 cm bins
LIZNISP15cmBIN <- Fossil_lizards15cmBIN %>% filter(!is.na(Family)) %>% group_by(Family, Bin, Age, .drop=FALSE) %>% summarise(NISP = sum(NISP))
LIZNISP15cmBIN <- na.omit(LIZNISP15cmBIN) 

# SUMMARIZE COMPLETE PITS
LIZNISP5cmBIN_CompltPits <- Fossil_lizards5cmBIN %>% 
  #filter(!is.na(Details))  %>%
  filter(!grepl('1D|1E|1C|W|Unknown',Pit), fixed = TRUE) %>%  #filter only Completely picked pits
  filter(!is.na(Family)) %>% 
  group_by(Family,Bin, Age, .drop=FALSE) %>% summarise(NISP = sum(NISP))
LIZNISP5cmBIN_CompltPits <- na.omit(LIZNISP5cmBIN_CompltPits) 


# MAKE INTO WIDE FORMAT WITH FAMILIES AS COLUMNS AND BINS AS ROWS
library(tidyverse)
library(tidyr)
LIZNISP5cmBIN_wide <- spread(LIZNISP5cmBIN, Family, NISP)
LIZNISP5cmBIN_wide2 <- spread(LIZNISP5cmBIN, Family, NISP)
LIZNISP5cmBIN_wide <- LIZNISP5cmBIN_wide %>% remove_rownames %>% column_to_rownames(var="Bin")
LIZNISP5cmBIN_wide[is.na(LIZNISP5cmBIN_wide)] <- 0

# 15 cm bins
LIZNISP15cmBIN_wide <- spread(LIZNISP15cmBIN, Family, NISP)

# complete pits
LIZNISP5cmBIN_CompltPits_wide <- spread(LIZNISP5cmBIN_CompltPits, Family, NISP)
LIZNISP5cmBIN_CompltPits_wide <- LIZNISP5cmBIN_CompltPits_wide %>% remove_rownames %>% column_to_rownames(var="Bin")
LIZNISP5cmBIN_CompltPits_wide[is.na(LIZNISP5cmBIN_CompltPits_wide)] <- 0

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###...     PREPARING THE DATA (MNI BASED ON THE DENTARY AND/OR MAXILLA)   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr)

Fossil_lizard_5bin_MNI <- filter(Fossil_lizards5cmBIN, grepl('dentary|\\bmaxilla\\b',Element)) #only include counts of dentary and the maxilla
Fossil_lizard_5bin_MNI<-droplevels(Fossil_lizard_5bin_MNI)

# Separate the Left and Right values into two columns based on the comma
LIZMNI5cm_LR <- Fossil_lizard_5bin_MNI %>%
  mutate(L_Column = str_extract(Fossil_lizard_5bin_MNI$Details, "\\d+(?=L)"),
         R_Column = str_extract(Fossil_lizard_5bin_MNI$Details, "\\d+(?=R)"))

# SUMMARIZE INTO MNI DATA 5-cm
LIZMNI5cm <- LIZMNI5cm_LR %>% 
  filter(!is.na(Details))  %>%
  filter(!is.na(Family)) %>% 
  group_by(Family,Bin,Element,L_Column,R_Column,Age, .drop=FALSE) %>% summarise(MNI = sum(NISP)) 
LIZMNI5cm <- LIZMNI5cm %>% group_by(Family, Bin, Age) %>% summarise(MNI = max(MNI)) %>% drop_na()

# SUMMARIZE INTO MNI DATA 15-cm
LIZMNI5cm_CompltPits <- LIZMNI5cm_LR %>% 
  filter(!grepl('1D|1E|1C|W',Pit), fixed = TRUE) %>%  #filter only Completely picked pits
  filter(!is.na(Details))  %>%
  filter(!is.na(Family)) %>% 
  group_by(Family,Bin,Element,L_Column,R_Column,Age, .drop=FALSE) %>% summarise(MNI = sum(NISP))  
LIZMNI5cm_CompltPits <- LIZMNI5cm_CompltPits %>% group_by(Family, Bin, Age) %>% summarise(MNI = max(MNI)) %>% drop_na()

# MAKE INTO LONG FORMAT WITH FAMILIES AS COLUMNS AND BINS AS ROWS
library(tidyverse)
library(tidyr)
LIZMNI5cm_wide <- spread(LIZMNI5cm, Family, MNI)
LIZMNI5cm_wide[is.na(LIZMNI5cm_wide)] <- 0

# for complete pits, MAKE INTO LONG FORMAT WITH FAMILIES AS COLUMNS AND BINS AS ROWS
LIZMNI5cm_CompltPits_wide <- spread(LIZMNI5cm_CompltPits, Family, MNI)
LIZMNI5cm_CompltPits_wide[is.na(LIZMNI5cm_CompltPits_wide)] <- 0

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                Pollen plots                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(grid)
library(analogue)


###... NISP POLLEN PLOT----
# 5-cm
LIZNISP5cmBIN_wide_0 <- tran(LIZNISP5cmBIN_wide[,2:6], method = 'missing', na.rm = FALSE, na.value = 0) # add 0's
LIZNISP5cm.pct <- data.frame(Age = LIZNISP5cmBIN_wide$Age, tran(LIZNISP5cmBIN_wide_0, method = 'percent')) # CONVERT INTO PERCENTS
NISP5cmdf <- data.frame(yr=LIZNISP5cm.pct$Age, 
                     per=as.vector(as.matrix(LIZNISP5cm.pct[,2:6])), 
                     taxa=as.factor(rep(colnames(LIZNISP5cm.pct[,2:6]), each=nrow(LIZNISP5cm.pct))))

NISPdf_NISP_abs <- data.frame(yr=LIZNISP5cmBIN_wide$Age, 
                      per=as.vector(as.matrix(LIZNISP5cmBIN_wide[,2:6])), 
                      taxa=as.factor(rep(colnames(LIZNISP5cmBIN_wide[,2:6]), each=nrow(LIZNISP5cmBIN_wide))))
# 15-cm
LIZNISP15cmBIN_wide_0 <- tran(LIZNISP15cmBIN_wide[,3:7], method = 'missing', na.rm = FALSE, na.value = 0)
LIZ15cmNISP.pct <- data.frame(Age = LIZNISP15cmBIN_wide$Age, tran(LIZNISP15cmBIN_wide_0, method = 'percent')) # CONVERT NISP INTO PERCENTS
NISP15cmdf <- data.frame(yr=LIZ15cmNISP.pct$Age, 
                         per=as.vector(as.matrix(LIZ15cmNISP.pct[,2:6])), 
                         taxa=as.factor(rep(colnames(LIZ15cmNISP.pct[,2:6]), each=nrow(LIZ15cmNISP.pct))))

# complete pits only
LIZNISP5cmBIN_CompltPits_wide.pct <- data.frame(Age = LIZNISP5cmBIN_CompltPits_wide$Age, tran(LIZNISP5cmBIN_CompltPits_wide[,2:6], method = 'percent')) # CONVERT MNI INTO PERCENTS
NISPdf_compltpits <- data.frame(yr=LIZNISP5cmBIN_CompltPits_wide$Age, 
                      per=as.vector(as.matrix(LIZNISP5cmBIN_CompltPits_wide[,2:6])), 
                      taxa=as.factor(rep(colnames(LIZNISP5cmBIN_CompltPits_wide[,2:6]), each=nrow(LIZNISP5cmBIN_CompltPits_wide))))
NISPdf_compltpits_pct <- data.frame(yr=LIZNISP5cmBIN_CompltPits_wide.pct$Age, 
                      per=as.vector(as.matrix(LIZNISP5cmBIN_CompltPits_wide.pct[,2:6])), 
                      taxa=as.factor(rep(colnames(LIZNISP5cmBIN_CompltPits_wide.pct[,2:6]), each=nrow(LIZNISP5cmBIN_CompltPits_wide.pct))))


# PLOT
famcolors <- c("#BDD9BF", "#FFC857", "#A997DF", "#929084", "#2E4052")

# facet_grid plot of % NISP for each family at 5-cm
NISPplot<-NISP5cmdf %>% 
  filter(yr < 16200) %>% 
  ggplot()+
  geom_line(aes(yr,per))+
  geom_area(aes(yr,per,fill=taxa))+
  scale_x_reverse(breaks =seq(0,100000,2000))+
  scale_y_continuous(breaks =seq(0,100,10))+
  xlab("Age (cal. BP)")+ylab("%")+
  coord_cartesian(xlim = c(0,16200)) +
  coord_flip()+
  theme_new+
  scale_fill_manual(name = "Family", values=c(famcolors))+
  facet_grid(~taxa,scales = "free", space = "free")
NISPplot

# facet_grid plot of % NISP for each family at 15-cm
NISP15cmplot<-NISP15cmdf %>% 
  filter(yr < 16200) %>% 
  ggplot()+
  geom_line(aes(yr,per))+
  geom_area(aes(yr,per,fill=taxa))+
  scale_x_reverse(breaks =seq(0,100000,2000))+
  scale_y_continuous(breaks =seq(0,100,10))+
  xlab("Age (cal. BP)")+ylab("%")+
  coord_cartesian(ylim = c(0,16200)) +
  coord_flip()+
  theme_new+
  scale_fill_manual(name = "Family", values=c(famcolors))+
  facet_grid(~taxa,scales = "free", space = "free")
NISP15cmplot

# combined facet_grid plots of % NISP for each family at 5-cm and 15-cm
plot_grid(NISPplot,NISP15cmplot, ncol = 2, labels = "auto", align = "hv",
          axis = "lr")

# fill plot of % NISP for each family at 5-cm
NISPplot_fill<- NISP5cmdf %>% 
  filter(yr < 16200) %>% 
  ggplot(aes(fill=taxa, y=per, x=yr)) + 
  geom_area()+
  #theme_classic(base_size = 15) +
  theme_classic2() +
  scale_x_continuous(breaks =seq(0,100000,2000))+
  scale_y_continuous(breaks =seq(0,100,20))+
  coord_cartesian(xlim = c(0,16200)) +
  scale_fill_manual(name = "Family", values=c(famcolors)) +
  ylab("Relative Abundace") + xlab("Years BP")
NISPplot_fill

# fill plot of ABSOLUTE NISP for each family at 5-cm
NISPplot_NISP_abs<- NISPdf_NISP_abs %>% 
  filter(yr < 16200) %>%
  ggplot(aes(fill=taxa, y=per, x=yr)) + 
  geom_area()+
  # theme_classic(base_size = 15) +
  theme_classic2() +
  scale_x_continuous(breaks =seq(0,100000,2000))+
  scale_y_continuous(breaks =seq(0,400,40))+
  coord_cartesian(xlim = c(0,16200)) +
  scale_fill_manual(name = "Family", values=c(famcolors)) +
  ylab("Absolute Abundace") + xlab("Years BP") 
NISPplot_NISP_abs

# fill plot of ABSOLUTE NISP for each family at 5-cm for COMPLETE PITS
NISPplot_NISP_abs_complt<- ggplot(NISPdf_compltpits, aes(fill=taxa, y=per, x=yr)) + 
  geom_area()+
  theme_classic() +
  scale_x_continuous(breaks =seq(0,100000,2000))+
  scale_y_continuous(breaks =seq(0,200,10))+
  coord_cartesian(xlim = c(0,16200)) +
  scale_fill_manual(name = "Family", values=c(famcolors)) +
  ylab("Absolute Abundace Complete Pits") + xlab("Years BP")

# fill plot of % NISP for each family at 5-cm for COMPLETE PITS
NISPplotcompltpits_pct<- ggplot(NISPdf_compltpits_pct, aes(fill=taxa, y=per, x=yr)) + 
  geom_area()+
  theme_classic() +
  scale_x_continuous(breaks =seq(0,100000,2000))+
  scale_y_continuous(breaks =seq(0,200,10))+
  coord_cartesian(xlim = c(0,16200)) +
  scale_fill_manual(name = "Family", values=c(famcolors)) +
  ylab("Relative Abundace Complete Pits") + xlab("Years BP")

#~~ LIZARD FAMILY RIOJA CLUSTER PLOT
chron_liz_roja <- data.frame(Age=LIZNISP5cm.pct$Age[1:48])
LIZNISP.pct_roja <- decostand(LIZNISP5cmBIN_wide_0[1:48,], "normalize")
LIZNISP.pct_roja.ch <- vegdist(LIZNISP.pct_roja, "euc") # Chord distance
LIZNISP5cmBIN.h.chclust <- chclust(LIZNISP.pct_roja.ch, method="coniss")
plot(LIZNISP5cmBIN.h.chclust)
bstick(LIZNISP5cmBIN.h.chclust, ng=20, plot=TRUE)

lizard_types <- data.frame(name = colnames(LIZNISP5cm.pct[,2:6]), Group = c("Anguidae", "Crotaphytodae","Phrynosomatidae","Scincidae", "Teiidae"))
lizard_types$Group <-factor(lizard_types$Group, levels = c("Anguidae", "Crotaphytodae" , "Phrynosomatidae" ,"Scincidae", "Teiidae"))
riojaPlot(LIZNISP5cm.pct[1:48,2:6], chron_liz_roja, selVars=colnames(LIZNISP5cm.pct[1:48,2:6]), groups=lizard_types,
          scale.percent=TRUE, 
          plot.groups=TRUE, 
          plot.sec.axis=TRUE, ymin=0, ymax=16200, ytks1=seq(0, 16200, by=500),
          col.group = famcolors,
          clust = LIZNISP5cmBIN.h.chclust,
          do.clust=FALSE,
          plot.clust=TRUE,
          plot.zones="auto")


###... MNI POLLEN PLOT----

LIZMNI5cm_wide_0 <- tran(LIZMNI5cm_wide[,3:7], method = 'missing', na.rm = FALSE, na.value = 0) # add 0's
LIZMNI.pct <- data.frame(Age = LIZMNI5cm_wide$Age, tran(LIZMNI5cm_wide_0, method = 'percent')) # CONVERT MNI INTO PERCENTS
LIZMNI5cm_CompltPits_wide.pct <- data.frame(Age = LIZMNI5cm_CompltPits_wide$Age, tran(LIZMNI5cm_CompltPits_wide[,3:7], method = 'percent')) # CONVERT MNI INTO PERCENTS


MNIdf.pct <- data.frame(yr=LIZMNI.pct$Age, 
                    per=as.vector(as.matrix(LIZMNI.pct[,2:6])), 
                    taxa=as.factor(rep(colnames(LIZMNI.pct[,2:6]), each=nrow(LIZMNI.pct))))

MNIdf <- data.frame(yr=LIZMNI5cm_wide$Age, 
                     per=as.vector(as.matrix(LIZMNI5cm_wide[,3:7])), 
                     taxa=as.factor(rep(colnames(LIZMNI5cm_wide[,3:7]), each=nrow(LIZMNI5cm_wide))))

MNIdfcomplt <- data.frame(yr=LIZMNI5cm_CompltPits_wide$Age, 
                     per=as.vector(as.matrix(LIZMNI5cm_CompltPits_wide[,3:7])), 
                     taxa=as.factor(rep(colnames(LIZMNI5cm_CompltPits_wide[,3:7]), each=nrow(LIZMNI5cm_CompltPits_wide))))

MNIdfcomplt.pct <- data.frame(yr=LIZMNI5cm_CompltPits_wide.pct$Age, 
                     per=as.vector(as.matrix(LIZMNI5cm_CompltPits_wide.pct[,2:6])), 
                     taxa=as.factor(rep(colnames(LIZMNI5cm_CompltPits_wide.pct[,2:6]), each=nrow(LIZMNI5cm_CompltPits_wide.pct))))

# facet_grid plot of % MNI for each family at 5-cm
MNIplot<-ggplot(MNIdf.pct)+
  geom_line(aes(yr,per))+
  geom_area(aes(yr,per,fill=taxa))+
  scale_x_reverse(breaks =seq(0,100000,2000))+
  scale_y_continuous(breaks =seq(0,100,10))+
  xlab("Age (cal. BP)")+ylab("%")+
  coord_flip()+
  theme_new+
  scale_fill_manual(name = "Family", values=c(famcolors))+
  facet_grid(~MNIdf.pct$taxa,scales = "free", space = "free")
MNIplot

# fill plot of % MNI for each family at 5-cm
MNIplot.pct<-ggplot(MNIdf.pct, aes(fill=taxa, y=per, x=yr)) + 
  geom_area()+
  theme_classic(base_size = 15) +
  scale_x_continuous(breaks =seq(0,100000,2000))+
  scale_y_continuous(breaks =seq(0,200,10))+
  coord_cartesian(xlim = c(0,16200)) +
  scale_fill_manual(name = "Family", values=c(famcolors)) +
  ylab("Relative Abundace") + xlab("Years BP")

# fill plot of ABSOLUTE MNI for each family at 5-cm
MNIplot.abs<-ggplot(MNIdf, aes(fill=taxa, y=per, x=yr)) + 
  geom_area()+
  theme_classic(base_size = 15) +
  scale_x_continuous(breaks =seq(0,100000,2000))+
  scale_y_continuous(breaks =seq(0,200,10))+
  coord_cartesian(xlim = c(0,16200)) +
  scale_fill_manual(name = "Family", values=c(famcolors)) +
  ylab("Absolute Abundace") + xlab("Years BP")

# fill plot of ABSOLUTE MNI for each family at 5-cm for COMPLETE PITS
MNIplot.complt<-ggplot(MNIdfcomplt, aes(fill=taxa, y=per, x=yr)) + 
  geom_area()+
  theme_classic(base_size = 15) +
  scale_x_continuous(breaks =seq(0,100000,2000))+
  scale_y_continuous(breaks =seq(0,200,5))+
  coord_cartesian(xlim = c(0,20000)) +
  scale_fill_manual(name = "Family", values=c(famcolors)) +
  ylab("Absolute Abundace Complete Pits") + xlab("Years BP")

# fill plot of % MNI for each family at 5-cm for COMPLETE PITS
MNIplot_pct.complt<-ggplot(MNIdfcomplt.pct, aes(fill=taxa, y=per, x=yr)) + 
  geom_area()+
  theme_classic(base_size = 15) +
  scale_x_continuous(breaks =seq(0,100000,2000))+
  scale_y_continuous(breaks =seq(0,200,10))+
  coord_cartesian(xlim = c(0,20000)) +
  scale_fill_manual(name = "Family", values=c(famcolors)) +
  ylab("Relative Abundace Complete Pits") + xlab("Years BP")

###... NISP AND MNI PLOTS COMBINED ----
library(ggpubr)
# facet_grid plot comparing relative abundance of families based on MNI and NISP
Pollen.plot_MNIvsNISP_facet <- ggarrange(MNIplot, NISPplot,
                          labels = c("MNI", "NISP"),
                          ncol = 1, nrow = 2)
# fill plot comparing relative abundance of families based on MNI and NISP
Pollen.plot_MNIvsNISP_fill <- ggarrange(MNIplot.pct, NISPplot_fill,
                                   labels = c("MNI", "NISP"),
                                   ncol = 1, nrow = 2)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- MODERN HALL'S CAVE TAXA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(readr)

Regional_modern_lizards_50_km <- read_delim("Regional modern lizards 50 km.csv", # modern lizard records from Gbif within 50 square km of Hall's Cave
                                            delim = "\t", escape_double = FALSE, 
                                            trim_ws = TRUE)

# piechart of modern lizard records from Gbif
famcolors2 <- c(Anguidae = "#BDD9BF", Crotaphytidae = "#FFC857", Phrynosomatidae = "#A997DF", Scincidae = "#929084", Teiidae = "#2E4052", Dactyloidae = "#8DFF27", Gekkonidae = "#A7313C")
regionPIE_plot <- Regional_modern_lizards_50_km %>% 
  group_by(family) %>% summarise(NISP = n()) %>% 
  ggplot(aes(x="", y=NISP, fill=family)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  geom_text(aes(label = NISP),
            position = position_stack(vjust = 0.5), color ="white") +
  scale_fill_manual(name = "Family", values=c(famcolors2)) + labs(title = "Modern lizard community within 50 square km")

# pie plot of fossil lizard abundance within upper layers in Hall's Cave (<1,000 ybp)
fossilPIE_plot <- NISPdf_NISP_abs %>% 
  filter(between(yr, 0, 1000)) %>% 
  group_by(taxa) %>% summarise(NISP = sum(per)) %>% 
  ggplot(aes(x="", y=NISP, fill=taxa)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  geom_text(aes(label = NISP),
            position = position_stack(vjust = 0.5), color ="white") +
  scale_fill_manual(name = "Family", values=c(famcolors2)) + labs(title = "Fossil lizard community within last 1000 years")

# combined pie charts
plot_grid(regionPIE_plot, fossilPIE_plot)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                RAREFACTION                               ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# following Chao et al. (2014) 
library(iNEXT)
library(ggplot2)

##   MNI
LIZMNI5cm_wideT<-t(LIZMNI5cm_wide_0) #transpose data

#iNEXT_MNIout<-iNEXT(LIZMNI5cm_wideT, q=c(1), datatype="abundance") # Interpolation and extrapolation of Hill number of order q (1)
#ggiNEXT(out, type=2, facet.var="Assemblage") # visualize coverage-based- rarefaction curves for each 5-cm bin

Shannon_MNI_estimates<- estimateD(LIZMNI5cm_wideT,q=c(1), datatype="abundance", base="coverage", level=NULL, conf=0.95, nboot = 1000) #Estimate Shannon div using coverage based rarefaction
Shannon_MNI_estimates2 <- ChaoShannon(LIZMNI5cm_wideT, transform = TRUE, B= 1000) #Estimate Shannon div to get observed diversity

##   NISP
LIZNISP5cmBIN_wideT<-t(LIZNISP5cmBIN_wide[,2:6]) #transpose data

#iNEXT_NISPout<-iNEXT(LIZNISP5cmBIN_wideT, q=c(1), datatype="abundance")
#iNEXT_NISPout$AsyEst
#ggiNEXT(iNEXT_NISPout, type=2, facet.var="Assemblage") # visualize coverage-based- rarefaction curves for each 5-cm bin

Shannon_NISP_estimates<- estimateD(LIZNISP5cmBIN_wideT,q=c(1), datatype="abundance", base="size", level=100, conf=0.95, nboot = 1000) #Estimate Shannon div using coverage based rarefaction
Shannon_NISP_estimates2 <- ChaoShannon(LIZNISP5cmBIN_wideT, transform = TRUE, B= 1000) #Estimate Shannon div to get observed diversity


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           Shannon diversity plots                        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SHANNON DIVERSITY BASED ON MNI
HMNI <- data.frame(Age = LIZMNI5cm_wide$Age,
                   Est=as.vector(as.matrix(Shannon_MNI_estimates$qD)),
                   Obs=as.vector(as.matrix(Shannon_MNI_estimates2$Observed)),
                   L95=as.vector(as.matrix(Shannon_MNI_estimates$qD.LCL)),
                   U95=as.vector(as.matrix(Shannon_MNI_estimates$qD.UCL)))

HMNI_plot<-ggplot(HMNI, aes(Age, Est))+
  geom_line(colour="blue")+
  scale_x_continuous(breaks =seq(0,100000,2000))+
  scale_y_continuous(breaks =seq(0,10,1))+
  xlab("Age (cal. BP)")+ylab("H")+
  #coord_flip() +
  theme_classic(base_size = 15)+ geom_ribbon(aes(ymin = L95, ymax = U95), alpha = 0.3)+
  geom_line(aes(y = Obs), color = "black") # Black line = observed diversity; blue line = estimated diversity

# SHANNON DIVERSITY BASED ON NISP
HNISP <- data.frame(Age=(LIZNISP5cmBIN_wide$Age),#make dataframe
                    Obs=as.vector(as.matrix(Shannon_NISP_estimates2$Observed)),
                    Est=as.vector(as.matrix(Shannon_NISP_estimates$qD)),
                    L95=as.vector(as.matrix(Shannon_NISP_estimates$qD.LCL)),
                    U95=as.vector(as.matrix(Shannon_NISP_estimates$qD.UCL)))

HNISP_plot<-HNISP %>% 
  filter(Age < 16200) %>% 
  ggplot(aes(Age, Est))+
  geom_line(colour="blue")+
  scale_x_continuous(breaks =seq(0,100000,2000))+
  scale_y_continuous(breaks =seq(0,7,1))+
  xlab("Years BP")+ylab("H (Shannon div.)")+
  #coord_flip() +
  coord_cartesian(xlim = c(0,16200)) +
  theme_classic2() + geom_ribbon(aes(ymin = L95, ymax = U95), alpha = 0.3) +
  geom_line(aes(y = Obs), color = "black") # Black line = observed diversity; blue line = estimated diversity

# PLOT MNI AND NISP TOGETHER
plot_grid(HMNI_plot, HNISP_plot, ncol = 1, labels = c('MNI', 'NISP'), align = "hv",
          axis = "lr")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------- BAYESIAN POSTERIOR CHANGE ANALYSIS-----------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(bcp)

HNISP_bcp.ri <- bcp(as.vector(HNISP$Obs[1:48])) # Bayesian change point analysis
plot(HNISP_bcp.ri)

# create change point dataframe
data_bcp <- data.frame(postmean = HNISP_bcp.ri$posterior.mean, prob = HNISP_bcp.ri$posterior.prob, Age=HNISP$Age[1:48])

# plot change point results
bchange_plot<-ggplot(data=data_bcp, aes(x=Age, y=X1)) +
  geom_line(alpha = 0.8, colour = "black",linewidth =3)+ 
  geom_point(data = HNISP[1:48,],
             mapping = aes(x = Age, y = Obs),
             inherit.aes = FALSE,
             size = 2)+theme_bw() +
  geom_line(data = data_bcp,
            mapping = aes(x = Age, y = prob, alpha = 0.2, colour = "red", linewidth =3),
            inherit.aes = FALSE)+ scale_x_continuous(breaks =seq(0,100000,2000)) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Shannon diversity posterior mean",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1, name="Posterior probability")
  ) + coord_cartesian(xlim = c(0,16200)) + theme(legend.position="none") +xlab("Years BP")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           READ IN AND VIZUALIZE POLLEN DATA           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr)

#~ POLLEN DATA .............................-------
Total_Pollen_data_raw <- read_csv("Cordova_Pollen_data.csv")
Total_Pollen_data <- mutate(Total_Pollen_data_raw, Asteraceae_Tubuliflorae = `Aster type`+`Cirsium type`) # Asteraceae_Tubuliflorae is in publication but is not in the dataset. Aster type and Cirsium type are both a type of Asteraceae Tubuliflorae
## 5 cm intervals
classes5 <- seq(from = 1, to = 60, by = 1)
upper5 <- seq(from = 5, to = 300, by = 5)
lower5 <- seq(from = 0, to = 295, by = 5)
## ASSIGN NEW AGES BASED ON AGE-DEPTH MODEL
setDT(Total_Pollen_data)
interval_lookup5 <- data.table(classes5, upper5,lower5)
Total_Pollen_dataBIN<- Total_Pollen_data[interval_lookup5, Bin:=classes5, on=c("Depth >= lower5","Depth <= upper5")]
Total_Pollen_dataBIN$Bin <- as.factor(Total_Pollen_dataBIN$Bin)

# create lizard and pollen abundance dataframes
Liz_pollen <- merge(LIZNISP5cmBIN_wide2, Total_Pollen_dataBIN, by= "Bin", all.x = TRUE)
Liz_pollen <- Liz_pollen %>% dplyr::select(Anguidae, Crotaphytidae, Phrynosomatidae, Scincidae, Teiidae, Pinus, Picea, Cuppressaceae, Salix, Quercus, Juglans, 
                                           Carya, Ulmus, Celtis, Moraceae, Rhamnaceae, Alnus, Fraxinus, Ephedra, Poaceae,
                                           Chenopodiaceae, `Eriogonum type`, Opuntia, Umbelliferae, Onagraceae, `Salvia type`, `Ambrosia-type`,
                                           Artemisia, Asteraceae_Tubuliflorae, `Asteraceae liguliflorae`)
env_pollen <- merge(LIZNISP5cmBIN_wide2, Total_Pollen_dataBIN, by= "Bin", all.x = TRUE) 
env_pollen <- env_pollen %>% column_to_rownames(var='Bin')
env_pollen <- env_pollen %>% dplyr::select(Pinus, Picea, Cuppressaceae, Salix, Quercus, Juglans, 
                                           Carya, Ulmus, Celtis, Moraceae, Rhamnaceae, Alnus, Fraxinus, Ephedra, Poaceae,
                                           Chenopodiaceae, `Eriogonum type`, Opuntia, Umbelliferae, Onagraceae, `Salvia type`, `Ambrosia-type`,
                                           Artemisia, Asteraceae_Tubuliflorae, `Asteraceae liguliflorae`)

# subset pollen data to match Cordova and Johnson 2019
Total_Pollen_data_sub<- Total_Pollen_data %>% dplyr::select(Pinus, Picea, Cuppressaceae, Salix, Quercus, Juglans, 
                                                            Carya, Ulmus, Celtis, Moraceae, Rhamnaceae, Alnus, Fraxinus, Ephedra, Poaceae,
                                                            Chenopodiaceae, `Eriogonum type`, Opuntia, Umbelliferae, Onagraceae, `Salvia type`, `Ambrosia-type`,
                                                            Artemisia, Asteraceae_Tubuliflorae, `Asteraceae liguliflorae`)
# only trees
Total_Pollen_data_arbor<- Total_Pollen_data %>% dplyr::select(Pinus, Picea, Cuppressaceae, Salix, Populus, Juglans, Carya,Alnus, `Ostrya-Carpinus`, Quercus, Ulmus, Celtis,
                                                              Moraceae, Vitis, Tilia, `Pyrus type`, Liquidambar, Rhus, Acer, Rhamnaceae, Cornus, Fraxinus)


#~ LIZARDS & POLLEN DATA .............................-------
library(vegan)
library(rioja)
library(riojaPlot)


# prepare dataset for rioja plot
Liz_roja <- tran(LIZNISP5cmBIN_wide2, method = 'missing', na.rm = FALSE, na.value = 0)
pollen_roja <- Liz_pollen %>% dplyr::select(!c(Anguidae, Phrynosomatidae, Crotaphytidae, Scincidae, Teiidae))
Liz_pollen_roja <- cbind(Liz_roja, env_pollen) %>% na.omit()
chron_pollen_roja <-  Liz_pollen_roja %>% dplyr::select(c(Age, Bin))
chron_pollen_roja$Bin <- as.numeric(chron_pollen_roja$Bin)*5 # convert bin to depth
Liz_pollen_roja <- Liz_pollen_roja %>% dplyr::select(!c(Bin, Age))
Liz_pollen_roja_pct <- data.frame(tran(Liz_pollen_roja[,1:5], method = 'percent'), tran(Liz_pollen_roja[,6:30], method = 'percent'))
# alphabetical sort of plant taxa names
sortcols <- colnames(Liz_pollen_roja_pct[,6:18])
Liz_pollen_roja_pct <- Liz_pollen_roja_pct %>% 
  select(-sortcols, sort(sortcols)) # 
sortcols2 <- colnames(Liz_pollen_roja_pct[,6:17])
Liz_pollen_roja_pct <- Liz_pollen_roja_pct %>% 
  select(-sortcols2, sort(sortcols2))

Pollen.rioja.norm <- decostand(Liz_pollen_roja[1:42,6:30], "normalize") # normalize pollen abundance
Pollen.ch.rioja <- vegdist(Pollen.rioja.norm, "euc") # Chord distance
Pollen.chclust.rioja <- chclust(Pollen.ch.rioja, method="coniss") # cluster analysis
plot(Pollen.chclust.rioja)
bstick(Pollen.chclust.rioja, ng=20, plot=TRUE) # comparison with broken stick model

Pollen.rioja.norm.h <- decostand(Liz_pollen_roja[1:42,6:30], "hellinger")
Pollen.h.rioja <- vegdist(Pollen.rioja.norm.h, "euc") # Chord distance
Pollen.chclust.h.rioja <- chclust(Pollen.h.rioja, method="coniss") # cluster analysis
plot(Pollen.chclust.h.rioja)
bstick(Pollen.chclust.h.rioja, ng=20, plot=TRUE) # comparison with broken stick model

# rioja plot
types2 <- data.frame(name = colnames(Liz_pollen_roja_pct), Group = c("Anguidae", "Crotaphytodae","Phrynosomatidae","Scincidae", "Teiidae", rep("Tree&Shrub", 13) , rep("Nonarboreal", 12)))
types2$Group <-factor(types2$Group, levels = c("Anguidae", "Crotaphytodae" , "Phrynosomatidae" ,"Scincidae", "Teiidae" ,"Tree&Shrub", "Nonarboreal"))
famPollencolors <- c("#BDD9BF","#FFC857", "#A997DF", "#929084", "#2E4052", "#1D7723", "#E6BD84")

myzones <- c(30, 110, 150, 195, 230); zones <-c("3", "2b", "2a", "1c", "1b", "1a") # pollen zones from Cordova and Johnson 2019
lizard_pollen_rp <- riojaPlot(Liz_pollen_roja_pct[1:42,], chron_pollen_roja[1:42,], selVars=colnames(Liz_pollen_roja_pct), groups=types2,
                scale.percent=TRUE, 
                plot.groups=TRUE, ymin=0, ymax=250, sec.ymin=0, sec.ymax=17000, sec.yinterval=1000, yvar.name="Bin",
                sec.yvar.name="Age",ytks2=seq(0, 16200, by=500),
                plot.sec.axis=TRUE,
                col.group = famPollencolors)
addRPZone(lizard_pollen_rp, myzones, col="red")


# ~ GRASS PHYTOLITH DATA .............................-------
Grass_Phytolith_data <- read_csv("Grass_Phytolith_data.csv")

# summarize C3, C4 percents and relative Panicoid
Grass_Phytolith_data <- mutate(Grass_Phytolith_data, C4_sum = (`Aristida-type bilobates`+Panicoid_total+Chloridoid_total))
Grass_Phytolith_data.pct <- mutate(Grass_Phytolith_data, C3pct = Pooid_total / (`Aristida-type bilobates`+Panicoid_total+Chloridoid_total+Pooid_total))
Grass_Phytolith_data.pct <- mutate(Grass_Phytolith_data.pct, Panipct = Panicoid_total/(Chloridoid_total+Panicoid_total))

# add 5-cm bin
Grass_Phytolith_data_abun <- Grass_Phytolith_data %>% select(Depth, Pooid_total, `Aristida-type bilobates`, Panicoid_total, Chloridoid_total)
setDT(Grass_Phytolith_data_abun)
interval_lookup5 <- data.table(classes5, upper5,lower5)
Grass_Phytolith_data_abunBIN<- Grass_Phytolith_data_abun[interval_lookup5, Bin:=classes5, on=c("Depth >= lower5","Depth <= upper5")]
Grass_Phytolith_data_abunBIN$Bin <- as.factor(Grass_Phytolith_data_abunBIN$Bin)

# plot
GrassPollen_plt_C3_C4 <- ggplot(Grass_Phytolith_data) +
  geom_area(aes(x = `Cal_yr_BP`,Pooid_total, fill = "Pooid"), alpha = 0.6) + 
  scale_x_continuous(breaks =seq(0,20000,1000))+ 
  geom_area(aes(x = `Cal_yr_BP`,C4_sum, fill = "C4 sum"),alpha = 0.5)+ 
  labs(y = "Pooid vs. C4", x = "Years Before Present") +theme_classic2() +
  scale_color_manual(values=c("Pooid" = "#52CBFF", "C4 sum"= "#DC0606")) +
  scale_fill_manual(values=c("Pooid" = "#52CBFF", "C4 sum"= "#DC0606"))

GrassPollen_plt_Chl_Pan <- ggplot(Grass_Phytolith_data) +
  geom_area(aes(x = `Cal_yr_BP`,Panicoid_total, fill = "Panicoid"), alpha = 0.6) + 
  scale_x_continuous(breaks =seq(0,20000,1000))+ 
  geom_area(aes(x = `Cal_yr_BP`,Chloridoid_total, fill = "Chloridoid"),alpha = 0.5)+ 
  labs(y = "Panicoid vs. Chloridoid", x = "Years Before Present") +theme_classic2()+
  scale_color_manual(values=c("Panicoid" = "#568EA7", "Chloridoid"= "#E4AC81")) +
  scale_fill_manual(values=c("Panicoid" = "#568EA7", "Chloridoid"= "#E4AC81"))

plot_grid(GrassPollen_plt_C3_C4, GrassPollen_plt_Chl_Pan, ncol = 1, labels = "auto", align = "hv")

#~ LIZARDS & PHYTOLITH DATA .............................-------
LIZNISP5cmBIN_grass <- data.frame(Bin = LIZNISP5cmBIN_wide2$Bin, analogue::tran(LIZNISP5cmBIN_wide2[,3:7], method = 'missing', na.rm = FALSE, na.value = 0), Age = LIZNISP5cmBIN_wide2$Age)
Liz_grass <- merge(LIZNISP5cmBIN_grass, Grass_Phytolith_data_abunBIN, by= "Bin", all.x = TRUE)
Liz_grass <- Liz_grass[1:48,]  %>% na.omit()
chron_grass <- Liz_grass %>% dplyr::select(c(Age, Bin))
chron_grass$Bin <- as.numeric(chron_grass$Bin)*5
Liz_grass <- Liz_grass %>% dplyr::select(!c(Age, Depth, Bin))

# Prepare data for clustering analysis
LIZ_grass.pct <- analogue::tran(Liz_grass[,1:5], method = 'percent')
env_grass.ch <- vegan::decostand(Liz_grass[,6:9], "normalize")
env_grass.ch <- vegdist(env_grass.ch, "euc") # Chord distance
env_grass.chclust <- chclust(env_grass.ch, method="coniss")
plot(env_grass.chclust)
bstick(env_grass.chclust, ng=20, plot=TRUE)
# Cut the dendrogram in 2 clusters
(gr4 <- cutree(env_grass.chclust, k = 2))
# Plot the dendrogram
plot(env_grass.chclust, hang = -1, main = "CONISS clustering")
rect.hclust(env_grass.chclust, k = 2)

# roja plot
Liz_grass_roja_pct <- data.frame(tran(Liz_grass[,1:5], method = 'percent'), tran(Liz_grass[,6:9], method = 'percent'))

types_grass <- data.frame(name = colnames(Liz_grass_roja_pct), Group = c("Anguidae", "Crotaphytodae","Phrynosomatidae","Scincidae", "Teiidae", rep("C3", 1) , rep("C4", 3)))
types_grass$Group <-factor(types_grass$Group, levels = c("Anguidae", "Crotaphytodae" , "Phrynosomatidae" ,"Scincidae", "Teiidae" ,"C3", "C4"))
famPollencolors <- c("#BDD9BF","#FFC857", "#A997DF", "#929084", "#2E4052", "#1D7723", "#E6BD84")

riojaPlot(Liz_grass_roja_pct, chron_grass, selVars=colnames(Liz_grass_roja_pct), groups=types_grass,
          scale.percent=TRUE, 
          plot.groups=TRUE, ymin=0, ymax=250, sec.ymin=0, sec.ymax=17000, sec.yinterval=1000, yvar.name="Bin",
          sec.yvar.name="Age",
          plot.sec.axis=TRUE,
          col.group = famPollencolors,
          clust = env_grass.chclust,
          do.clust=FALSE,
          plot.clust=TRUE,
          plot.zones="auto")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           READ IN AND VIZUALIZE PALEOCLIM DATA           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(readr)
library(tidyverse)
library(data.table)
library(cowplot)
# proxy labels
d13c_label <- expression(delta^{13}*C)
dD_label <- expression(delta*D[wax])
MS_label <- expression(Magnetic~susptability~(m^3/kg))
Sr_label <- expression(Sr^{87}/Sr^{86})

#~ Hall's Cave local paleoclimate----

# carbon and duterium values from Sun et al. 2021
Paleoclim_C_D <- read_csv("Paleoclim data.csv") # carbon and duterium values from Sun et al. 2021
# add new ages from age-depth model
times_C_D<-Bacon.hist(seq(from = .5, to = 285.5, by = 1)) # V1 = min, V2 = max, V3 = median, V4 = mean
times_C_D<- data.frame(times_C_D, Depth = seq(from = .5, to = 285.5, by = 1))
Paleoclim_C_D <- merge(Paleoclim_C_D, times_C_D, by.x = "AvgDepth", by.y = "Depth")
Paleoclim_C_D <- Paleoclim_C_D %>% 
  rename(
    Age_old = Age,
    Age = X4
  )

# magnetic suseptability from Ellwood and Gose 2006
Elwood_Gose_maneticsusep <- read_csv("Elwood_Gose_maneticsusep.csv")
Elwood_Gose_maneticsusep <- Elwood_Gose_maneticsusep %>% filter(!Depth<0) # exclude values above the Zero datum of Toomey (1993)
# add new ages from age-depth model
times_maneticsusep<-Bacon.hist(seq(from = 0, to = 300, by = 1)) # V1 = min, V2 = max, V3 = median, V4 = mean
times_maneticsusep <- tibble::rownames_to_column(as.data.frame(times_maneticsusep), "Depth")
Elwood_Gose_maneticsusep <- merge(Elwood_Gose_maneticsusep, times_maneticsusep, by = "Depth")
Elwood_Gose_maneticsusep <- Elwood_Gose_maneticsusep %>% 
  rename(
    Age = V4
  )

# titanium values from Sun et al. 2021
Paleoclim_data_Ti <- read_csv("Paleoclim_data_Ti.csv")
# add new ages from age-depth model
times_Ti <-Bacon.hist(Paleoclim_data_Ti$AvgDepth) # V1 = min, V2 = max, V3 = median, V4 = mean
times_Ti <- data.frame(times_Ti, Depth = Paleoclim_data_Ti$AvgDepth)
Paleoclim_data_Ti <- merge(Paleoclim_data_Ti, times_Ti, by.x = "AvgDepth", by.y = "Depth")
Paleoclim_data_Ti <- Paleoclim_data_Ti %>% 
  rename(
    Age_old = Age,
    Age = X4
  )

# fossil mammal abundances from Toomey 1993
Mammal_Toomey_NISP <- read_csv("Mammal_Toomey_NISP.csv")

# strontium values from Cook et al. 2003
Cooke_strontium <- read_csv("Cooke_strontium.csv")
Cooke_strontium$Material <- ifelse(Cooke_strontium$Material == "vole_enamel", # combine common elements
                                   "rodent_enamel", Cooke_strontium$Material)
Cooke_strontium$Material <- ifelse(Cooke_strontium$Material == "pocket_gopher_enamel", # combine common elements
                                   "rodent_enamel", Cooke_strontium$Material)

# PLOT DIFFERENT PROXIES
d13c_plt <- ggplot(Paleoclim_C_D, aes(x = Age, y = d13C)) +
  geom_point() + geom_line(color = "blue")+ coord_cartesian(xlim = c(0,20000))+scale_y_reverse()+
  labs(y = d13c_label, x = "Years Before Present") +theme_classic2()

dD_plt <- ggplot(Paleoclim_C_D, aes(x = Age, y = dD_corr)) +
  geom_point() + geom_line(color = "forestgreen")+ coord_cartesian(xlim = c(0,20000))+ scale_y_reverse()+
  labs(y = dD_label, x = "Years Before Present") +theme_classic2()

Ti_plt <- ggplot(Paleoclim_data_Ti, aes(x = Age, y = Ti_pct)) +
  geom_point() + geom_line(color = "red")+
  labs(y = "Ti %", x = "Years Before Present") +theme_classic2()

MagSusept_plt <- ggplot(Elwood_Gose_maneticsusep, aes(x = Age, y = MS)) +
  geom_point() + geom_line(color = "orange")+ coord_cartesian(xlim = c(0,20000))+
  labs(y = MS_label, x = "Years Before Present") + geom_ribbon(aes(ymin = MS-SD, ymax = MS+SD), alpha = 0.3)+
  theme_classic2()

dSr_plt <- ggplot(Cooke_strontium, aes(x = YBP, y = Sr87_Sr86, shape = Material)) +
  geom_point(size = 2) + stat_smooth(method="lm", se=TRUE,
                                     formula=y ~ poly(x, 2, raw=TRUE),colour="white")+ coord_cartesian(xlim = c(0,20000))+
  labs(y = Sr_label, x = "Years Before Present") +theme_classic2() +theme(legend.position="bottom")+
  geom_hline(yintercept=0.708408, linetype="dashed", color = "red")+annotate("text", x=0, y=0.70835, label="Shallow")+
  geom_hline(yintercept=0.709968, linetype="dashed", color = "blue")+annotate("text", x=0, y=0.710026, label="Deep")

# combine plots
plot_grid(dSr_plt, Ti_plt, ncol = 1, labels = "auto", align = "hv",
          axis = "lr")
plot_grid(d13c_plt, dD_plt, MagSusept_plt, ncol = 1, labels = "auto", align = "hv",
          axis = "lr")

#~ Hall's Cave regional paleoclimate----
library("pastclim")
#label
degreeC <- expression(degree~C)

# coordinates of Hall's Cave
coords<-data.frame(longitude=-99.533333,latitude=c(30.133333))
# map of Hall's Cave
tx_counties <- map_data("county", "texas") %>% 
  dplyr::select(lon = long, lat, group, id = subregion)
head(tx_counties)
ggplot(tx_counties, aes(lon, lat, group = group)) +
  geom_polygon(fill = "white", colour = "grey50") + theme_classic()+
  coord_quickmap() + geom_point(data = coords, mapping = aes(x = longitude, y = latitude), colour = "royalblue", inherit.aes = FALSE)

# download with pastclim the climatic variables
#set_data_path(".")
#download_dataset("Beyer2020")

# get paleoclim time series for location
time_series <-location_series(x=coords,
                              bio_variables=c("bio01","bio04", "bio07", "bio08","bio09", "bio10","bio11", "bio12", "bio15", "bio16","bio17", "bio18", "bio19"),
                              dataset="Beyer2020")

# subset times to study interval
time_series$time_bp <- -(time_series$time_bp)
time_series_sub <- time_series[time_series$time %in% seq(from = 0, to = 20000, by = 1000),]

# plot paleoclimate variables
bio01<-ggplot()+
  geom_line(data = time_series_sub, aes(time_bp, bio01)) +theme_classic() + labs(
    title = "Annual Mean Temperature", x = "YPB",
    y = degreeC)
bio04<-ggplot()+
  geom_line(data = time_series_sub, aes(time_bp, bio04)) +theme_classic() + labs(
    title = "Temperature seasonality", x = "YPB",
    y = degreeC)
bio07<-ggplot()+
  geom_line(data = time_series_sub, aes(time_bp, bio07)) +theme_classic() + labs(
    title = "Temperature Annual Range", x = "YPB",
    y = degreeC)
bio08<-ggplot()+
  geom_line(data = time_series_sub, aes(time_bp, bio08)) +theme_classic() + labs(
    title = "Mean temperature of the wettest quarter", x = "YPB",
    y = degreeC)
bio09<-ggplot()+
  geom_line(data = time_series_sub, aes(time_bp, bio09)) +theme_classic() + labs(
    title = "Mean temperature of driest quarter", x = "YPB",
    y = degreeC)
bio10 <- ggplot()+
  geom_line(data = time_series_sub, aes(time_bp, bio10)) +theme_classic() + labs(
    title = "Mean temperature of hottest quarter", x = "YPB",
    y = degreeC)
bio11<-ggplot()+
  geom_line(data = time_series_sub, aes(time_bp, bio11)) +theme_classic() + labs(
    title = "Mean Temperature of Coldest Quarter", x = "YPB",
    y = degreeC)
bio12 <- ggplot()+
  geom_line(data = time_series_sub, aes(time_bp, bio12)) +theme_classic() + labs(
    title = "Annual precipitation", x = "YPB",
    y = "mm per year")
bio15 <- ggplot()+
  geom_line(data = time_series_sub, aes(time_bp, bio15)) +theme_classic() + labs(
    title = "Precipitation seasonality", x = "YPB",
    y = "")
bio16 <- ggplot()+
  geom_line(data = time_series_sub, aes(time_bp, bio16)) +theme_classic() + labs(
    title = "Precipitation of wettest quarter", x = "YPB",
    y = "mm per quarter")
bio17 <- ggplot()+
  geom_line(data = time_series_sub, aes(time_bp, bio17)) +theme_classic() + labs(
    title = "Precipitation of driest quarter", x = "YPB",
    y = "mm per quarter")
bio18<-ggplot()+
  geom_line(data = time_series_sub, aes(time_bp, bio18)) +theme_classic() + labs(
    title = "Precipitation of hottest Quarter", x = "YPB",
    y = "mm per quarter")
bio19<-ggplot()+
  geom_line(data = time_series_sub, aes(time_bp, bio19)) +theme_classic() + labs(
    title = "Precipitation of Coldest Quarter", x = "YPB",
    y = "mm per quarter")
dev.new()
cowplot::plot_grid(bio01, bio04, bio07, bio08, bio09, bio10, bio11, bio12, bio15, bio16, bio17, bio18, bio19, ncol = 3, labels = "auto", align = "hv")

library("PerformanceAnalytics")
# correlation between variables
chart.Correlation(time_series_sub[,c("bio01", "bio04","bio07", "bio08", "bio09", "bio10","bio11", "bio12", "bio15", "bio16", "bio17", "bio18", "bio19")], histogram=FALSE)

chart.Correlation(time_series_sub[,c("bio07","bio10","bio11", "bio18", "bio19")], histogram=FALSE)

# plot variables with lower correlation
plot_grid(bio07, bio10,bio18,bio11,bio19, ncol = 2, labels = "auto", align = "hv")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                        MODEL PALEOCLIM DATA                       ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library("mgcv")
library("scam")
library("ggplot2")
library("cowplot")
library("tidyr")
# packageurl <- "https://github.com/gavinsimpson/gratia/archive/refs/tags/v0.8.1.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")
#devtools::install_github("gavinsimpson/gratia")
library("gratia")


## fit Paleoclim data with GAM using gamm() with a CAR(1)

#.............................Carbon.............................----

d13C_mod <- gamm(d13C ~ s(Age, k = 45), data = Paleoclim_C_D,
                 correlation = corCAR1(form = ~ Age), method = "REML")
summary(d13C_mod$gam)
gam.check(d13C_mod$gam)

## MODEL WITH AGES CORRESPONDING TO FOSSIL LIZARD DATA
meanage5cm_df <- data.frame(Age= meanage5cm)
fitd13C <- predict(d13C_mod$gam, meanage5cm_df, se.fit = TRUE)
d13Ccrit.t <- qt(0.975, df = df.residual(d13C_mod$gam))
d13C_newGCV <- data.frame(Age = meanage5cm,
                          d13C_fit = fitd13C$fit,
                          se.fit = fitd13C$se.fit)
d13C_newGCV <- transform(d13C_newGCV,
                         upper = d13C_fit + (d13Ccrit.t * se.fit),
                         lower = d13C_fit - (d13Ccrit.t * se.fit))

ggplot(d13C_newGCV, aes(Age, d13C_fit))+geom_point()+geom_line() +
  geom_point(data = Paleoclim_C_D, aes(x = Age, y = d13C), color = "blue")


#.............................dD wax.............................----

dD_mod <- gamm(dD_corr ~ s(Age, k = 40), data = Paleoclim_C_D,
               correlation = corCAR1(form = ~ Age), method = "REML")
summary(dD_mod$gam)
gam.check(dD_mod$gam)

## MODEL WITH AGES CORRESPONDING TO FOSSIL LIZARD DATA
meanage5cm_df <- data.frame(Age= meanage5cm)
fitdD <- predict(dD_mod$gam, meanage5cm_df, se.fit = TRUE)
dDcrit.t <- qt(0.975, df = df.residual(dD_mod$gam))
dD_newGCV <- data.frame(Age = meanage5cm,
                        dD_fit = fitdD$fit,
                        se.fit = fitdD$se.fit)
dD_newGCV <- transform(dD_newGCV,
                       upper = dD_fit + (dDcrit.t * se.fit),
                       lower = dD_fit - (dDcrit.t * se.fit))

ggplot(dD_newGCV, aes(Age, dD_fit))+geom_point()+geom_line() +
  geom_point(data = Paleoclim_C_D, aes(x = Age, y = dD_corr), color = "blue")


#..............................Ti %..............................----
Ti_pct_mod <- gamm(Ti_pct ~ s(Age, bs="cr" , k = 45), data = Paleoclim_data_Ti,
                   correlation = corCAR1(form = ~ Age), method = "REML")
summary(Ti_pct_mod$gam)
gam.check(Ti_pct_mod$gam)
k.check(Ti_pct_mod$gam)

## MODEL WITH AGES CORRESPONDING TO FOSSIL LIZARD DATA
meanage5cm_df <- data.frame(Age= meanage5cm)
fitTi_pct <- predict(Ti_pct_mod$gam, meanage5cm_df, se.fit = TRUE)
Ti_pctcrit.t <- qt(0.975, df = df.residual(Ti_pct_mod$gam))
Ti_pct_newGCV <- data.frame(Age = meanage5cm,
                            Ti_pct_fit = fitTi_pct$fit,
                            se.fit = fitTi_pct$se.fit)
Ti_pct_newGCV <- transform(Ti_pct_newGCV,
                           upper = Ti_pct_fit + (Ti_pctcrit.t * se.fit),
                           lower = Ti_pct_fit - (Ti_pctcrit.t * se.fit))

ggplot(Ti_pct_newGCV, aes(Age, Ti_pct_fit))+geom_point()+geom_line() +
  geom_point(data = Paleoclim_data_Ti, aes(x = Age, y = Ti_pct), color = "blue")


#...............................MS...............................----

MS_mod <- gamm(MS ~ s(Age, bs="tp" , k = 65), data = Elwood_Gose_maneticsusep,
               correlation = corCAR1(form = ~ YBP), method = "REML")
summary(MS_mod$gam)
gam.check(MS_mod$gam)
k.check(MS_mod$gam)

## MODEL WITH AGES CORRESPONDING TO FOSSIL LIZARD DATA
meanage5cm_df <- data.frame(Age= meanage5cm)
fitMS <- predict(MS_mod$gam, meanage5cm_df, se.fit = TRUE)
MScrit.t <- qt(0.975, df = df.residual(MS_mod$gam))
MS_newGCV <- data.frame(Age = meanage5cm,
                        MS_fit = fitMS$fit,
                        se.fit = fitMS$se.fit)
MS_newGCV <- transform(MS_newGCV,
                       upper = MS_fit + (MScrit.t * se.fit),
                       lower = MS_fit - (MScrit.t * se.fit))

ggplot(MS_newGCV, aes(Age, MS_fit))+geom_point()+geom_line() +
  geom_point(data = Elwood_Gose_maneticsusep, aes(x = Age, y = MS), color = "blue")


#.............................regional paleoclim VARIABLES.............................----

bio01_mod <- gamm(bio01 ~ s(time_bp, k = 15), gamma = .5, data = time_series_sub,
                  correlation = corCAR1(form = ~ time_bp), method = "REML")
summary(bio01_mod$gam)
par(mfrow = c(2, 2))
gam.check(bio01_mod$gam)
par(mfrow = c(1, 2))
acf(resid(bio01_mod$gam), lag.max = 36, main = "ACF")
pacf(resid(bio01_mod$gam), lag.max = 36, main = "pACF")

bio04_mod <- gamm(bio04 ~ s(time_bp, k = 15), data = time_series_sub,
                  correlation = corCAR1(form = ~ time_bp), method = "REML")
summary(bio04_mod$gam)
gam.check(bio04_mod$gam)
par(mfrow = c(1, 2))
acf(resid(bio04_mod$gam), lag.max = 36, main = "ACF")
pacf(resid(bio04_mod$gam), lag.max = 36, main = "pACF")

bio07_mod <- gamm(bio07 ~ s(time_bp, k = 15), data = time_series_sub,
                  correlation = corCAR1(form = ~ time_bp), method = "REML")
summary(bio07_mod$gam)
gam.check(bio07_mod$gam)
par(mfrow = c(1, 2))
acf(resid(bio07_mod$gam), lag.max = 36, main = "ACF")
pacf(resid(bio07_mod$gam), lag.max = 36, main = "pACF")

bio08_mod <- gamm(bio08 ~ s(time_bp, k = 15), data = time_series_sub,
                  correlation = corCAR1(form = ~ time_bp), method = "REML")
summary(bio08_mod$gam)
gam.check(bio08_mod$gam)
par(mfrow = c(1, 2))
acf(resid(bio08_mod$gam), lag.max = 36, main = "ACF")
pacf(resid(bio08_mod$gam), lag.max = 36, main = "pACF")

bio09_mod <- gamm(bio09 ~ s(time_bp, k = 15), data = time_series_sub,
                  correlation = corCAR1(form = ~ time_bp), method = "REML")
summary(bio09_mod$gam)
gam.check(bio09_mod$gam)

bio10_mod <- gamm(bio10 ~ s(time_bp, k = 15), data = time_series_sub,
                  correlation = corCAR1(form = ~ time_bp), method = "REML")
summary(bio10_mod$gam)
gam.check(bio10_mod$gam)

bio11_mod <- gamm(bio11 ~ s(time_bp, k = 15), data = time_series_sub,
                  correlation = corCAR1(form = ~ time_bp), method = "REML")
summary(bio11_mod$gam)
gam.check(bio11_mod$gam)

bio12_mod <- gamm(bio12 ~ s(time_bp, k = 15), data = time_series_sub,
                  correlation = corCAR1(form = ~ time_bp), method = "REML")
summary(bio12_mod$gam)
k.check(bio12_mod$gam)

bio15_mod <- gamm(bio15 ~ s(time_bp, k = 15), data = time_series_sub,
                  correlation = corCAR1(form = ~ time_bp), method = "REML")
summary(bio15_mod$gam)
par(mfrow = c(2, 2))
gam.check(bio15_mod$gam)
k.check(bio15_mod$gam)
par(mfrow = c(1, 2))
acf(resid(bio15_mod$gam), lag.max = 36, main = "ACF")
pacf(resid(bio15_mod$gam), lag.max = 36, main = "pACF")

bio16_mod <- gamm(bio16 ~ s(time_bp, k = 15), data = time_series_sub,
                  correlation = corCAR1(form = ~ time_bp), method = "REML")
summary(bio16_mod$gam)
k.check(bio16_mod$gam)
gam.check(bio16_mod$gam)

bio17_mod <- gamm(bio17 ~ s(time_bp, k = 15), data = time_series_sub,
                  correlation = corCAR1(form = ~ time_bp), method = "REML")
summary(bio17_mod$gam)
k.check(bio17_mod)
gam.check(bio17_mod$gam)

bio18_mod <- gamm(bio18 ~ s(time_bp, k = 15), data = time_series_sub,
                  correlation = corCAR1(form = ~ time_bp), method = "REML")
summary(bio18_mod$gam)

bio19_mod <- gamm(bio19 ~ s(time_bp, k = 15), data = time_series_sub,
                  correlation = corCAR1(form = ~ time_bp), method = "REML")
summary(bio19_mod$gam)

## MODEL WITH AGES CORRESPONDING TO FOSSIL LIZARD DATA
meanage5cm_df <- data.frame(time_bp= meanage5cm)

#.............................bio01.............................
fitbio01 <- predict(bio01_mod$gam, meanage5cm_df, se.fit = TRUE)
bio01crit.t <- qt(0.975, df = df.residual(bio01_mod$gam))
bio01_newGCV <- data.frame(Age = meanage5cm,
                           bio01_fit = fitbio01$fit,
                           se.fit = fitbio01$se.fit)
bio01_newGCV <- transform(bio01_newGCV,
                          upper = bio01_fit + (bio01crit.t * se.fit),
                          lower = bio01_fit - (bio01crit.t * se.fit))
ggplot(bio01_newGCV, aes(Age, bio01_fit))+geom_point()+geom_line() +
  geom_point(data = time_series_sub, aes(x = time_bp, y = bio01), color = "blue")

#.............................bio04.............................
fitbio04 <- predict(bio04_mod$gam, meanage5cm_df, se.fit = TRUE)
bio04crit.t <- qt(0.975, df = df.residual(bio04_mod$gam))
bio04_newGCV <- data.frame(Age = meanage5cm,
                           bio04_fit = fitbio04$fit,
                           se.fit = fitbio04$se.fit)
bio04_newGCV <- transform(bio04_newGCV,
                          upper = bio04_fit + (bio04crit.t * se.fit),
                          lower = bio04_fit - (bio04crit.t * se.fit))
ggplot(bio04_newGCV, aes(Age, bio04_fit))+geom_point()+geom_line() +
  geom_point(data = time_series_sub, aes(x = time_bp, y = bio04), color = "blue")

#.............................bio07.............................
fitbio07 <- predict(bio07_mod$gam, meanage5cm_df, se.fit = TRUE)
bio07crit.t <- qt(0.975, df = df.residual(bio07_mod$gam))
bio07_newGCV <- data.frame(Age = meanage5cm,
                           bio07_fit = fitbio07$fit,
                           se.fit = fitbio07$se.fit)
bio07_newGCV <- transform(bio07_newGCV,
                          upper = bio07_fit + (bio07crit.t * se.fit),
                          lower = bio07_fit - (bio07crit.t * se.fit))
ggplot(bio07_newGCV, aes(Age, bio07_fit))+geom_point()+geom_line() +
  geom_point(data = time_series_sub, aes(x = time_bp, y = bio07), color = "blue")

#.............................bio08.............................
fitbio08 <- predict(bio08_mod$gam, meanage5cm_df, se.fit = TRUE)
bio08crit.t <- qt(0.975, df = df.residual(bio08_mod$gam))
bio08_newGCV <- data.frame(Age = meanage5cm,
                           bio08_fit = fitbio08$fit,
                           se.fit = fitbio08$se.fit)
bio08_newGCV <- transform(bio08_newGCV,
                          upper = bio08_fit + (bio08crit.t * se.fit),
                          lower = bio08_fit - (bio08crit.t * se.fit))
ggplot(bio08_newGCV, aes(Age, bio08_fit))+geom_point()+geom_line() +
  geom_point(data = time_series_sub, aes(x = time_bp, y = bio08), color = "blue")

#.............................bio09.............................
fitbio09 <- predict(bio09_mod$gam, meanage5cm_df, se.fit = TRUE)
bio09crit.t <- qt(0.975, df = df.residual(bio09_mod$gam))
bio09_newGCV <- data.frame(Age = meanage5cm,
                           bio09_fit = fitbio09$fit,
                           se.fit = fitbio09$se.fit)
bio09_newGCV <- transform(bio09_newGCV,
                          upper = bio09_fit + (bio09crit.t * se.fit),
                          lower = bio09_fit - (bio09crit.t * se.fit))
ggplot(bio09_newGCV, aes(Age, bio09_fit))+geom_point()+geom_line() +
  geom_point(data = time_series_sub, aes(x = time_bp, y = bio09), color = "blue")

#.............................bio10.............................
fitbio10 <- predict(bio10_mod$gam, meanage5cm_df, se.fit = TRUE)
bio10crit.t <- qt(0.975, df = df.residual(bio10_mod$gam))
bio10_newGCV <- data.frame(Age = meanage5cm,
                           bio10_fit = fitbio10$fit,
                           se.fit = fitbio10$se.fit)
bio10_newGCV <- transform(bio10_newGCV,
                          upper = bio10_fit + (bio10crit.t * se.fit),
                          lower = bio10_fit - (bio10crit.t * se.fit))
ggplot(bio10_newGCV, aes(Age, bio10_fit))+geom_point()+geom_line() +
  geom_point(data = time_series_sub, aes(x = time_bp, y = bio10), color = "blue")

#.............................bio11.............................
fitbio11 <- predict(bio11_mod$gam, meanage5cm_df, se.fit = TRUE)
bio11crit.t <- qt(0.975, df = df.residual(bio11_mod$gam))
bio11_newGCV <- data.frame(Age = meanage5cm,
                           bio11_fit = fitbio11$fit,
                           se.fit = fitbio11$se.fit)
bio11_newGCV <- transform(bio11_newGCV,
                          upper = bio11_fit + (bio11crit.t * se.fit),
                          lower = bio11_fit - (bio11crit.t * se.fit))
ggplot(bio11_newGCV, aes(Age, bio11_fit))+geom_point()+geom_line() +
  geom_point(data = time_series_sub, aes(x = time_bp, y = bio11), color = "blue")

#.............................bio12.............................
fitbio12 <- predict(bio12_mod$gam, meanage5cm_df, se.fit = TRUE)
bio12crit.t <- qt(0.975, df = df.residual(bio12_mod$gam))
bio12_newGCV <- data.frame(Age = meanage5cm,
                           bio12_fit = fitbio12$fit,
                           se.fit = fitbio12$se.fit)
bio12_newGCV <- transform(bio12_newGCV,
                          upper = bio12_fit + (bio12crit.t * se.fit),
                          lower = bio12_fit - (bio12crit.t * se.fit))
ggplot(bio12_newGCV, aes(Age, bio12_fit))+geom_point()+geom_line() +
  geom_point(data = time_series_sub, aes(x = time_bp, y = bio12), color = "blue")

#.............................bio15.............................
fitbio15 <- predict(bio15_mod$gam, meanage5cm_df, se.fit = TRUE)
bio15crit.t <- qt(0.975, df = df.residual(bio15_mod$gam))
bio15_newGCV <- data.frame(Age = meanage5cm,
                           bio15_fit = fitbio15$fit,
                           se.fit = fitbio15$se.fit)
bio15_newGCV <- transform(bio15_newGCV,
                          upper = bio15_fit + (bio15crit.t * se.fit),
                          lower = bio15_fit - (bio15crit.t * se.fit))
ggplot(bio15_newGCV, aes(Age, bio15_fit))+geom_point()+geom_line() +
  geom_point(data = time_series_sub, aes(x = time_bp, y = bio15), color = "blue")

#.............................bio16.............................
fitbio16 <- predict(bio16_mod$gam, meanage5cm_df, se.fit = TRUE)
bio16crit.t <- qt(0.975, df = df.residual(bio16_mod$gam))
bio16_newGCV <- data.frame(Age = meanage5cm,
                           bio16_fit = fitbio16$fit,
                           se.fit = fitbio16$se.fit)
bio16_newGCV <- transform(bio16_newGCV,
                          upper = bio16_fit + (bio16crit.t * se.fit),
                          lower = bio16_fit - (bio16crit.t * se.fit))
ggplot(bio16_newGCV, aes(Age, bio16_fit))+geom_point()+geom_line() +
  geom_point(data = time_series_sub, aes(x = time_bp, y = bio16), color = "blue")

#.............................bio17.............................
fitbio17 <- predict(bio17_mod$gam, meanage5cm_df, se.fit = TRUE)
bio17crit.t <- qt(0.975, df = df.residual(bio17_mod$gam))
bio17_newGCV <- data.frame(Age = meanage5cm,
                           bio17_fit = fitbio17$fit,
                           se.fit = fitbio17$se.fit)
bio17_newGCV <- transform(bio17_newGCV,
                          upper = bio17_fit + (bio17crit.t * se.fit),
                          lower = bio17_fit - (bio17crit.t * se.fit))
ggplot(bio17_newGCV, aes(Age, bio17_fit))+geom_point()+geom_line() +
  geom_point(data = time_series_sub, aes(x = time_bp, y = bio17), color = "blue")

#.............................bio18.............................
fitbio18 <- predict(bio18_mod$gam, meanage5cm_df, se.fit = TRUE)
bio18crit.t <- qt(0.975, df = df.residual(bio18_mod$gam))
bio18_newGCV <- data.frame(Age = meanage5cm,
                           bio18_fit = fitbio18$fit,
                           se.fit = fitbio18$se.fit)
bio18_newGCV <- transform(bio18_newGCV,
                          upper = bio18_fit + (bio18crit.t * se.fit),
                          lower = bio18_fit - (bio18crit.t * se.fit))
ggplot(bio18_newGCV, aes(Age, bio18_fit))+geom_point()+geom_line() +
  geom_point(data = time_series_sub, aes(x = time_bp, y = bio18), color = "blue")

#.............................bio19.............................
fitbio19 <- predict(bio19_mod$gam, meanage5cm_df, se.fit = TRUE)
bio19crit.t <- qt(0.975, df = df.residual(bio19_mod$gam))
bio19_newGCV <- data.frame(Age = meanage5cm,
                           bio19_fit = fitbio19$fit,
                           se.fit.bio19 = fitbio19$se.fit)
bio19_newGCV <- transform(bio19_newGCV,
                          upper.bio19 = bio19_fit + (bio19crit.t * se.fit.bio19),
                          lower.bio19 = bio19_fit - (bio19crit.t * se.fit.bio19))
ggplot(bio19_newGCV, aes(Age, bio19_fit))+geom_point()+geom_line() +
  geom_point(data = time_series_sub, aes(x = time_bp, y = bio19), color = "blue")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------- CREATE DATASET WITH ALL PALEOCLIM VARIABLES-------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LIZNISP5cmBIN_wide_raw <- LIZNISP5cmBIN_wide
GAM_data_raw <- merge(d13C_newGCV, LIZNISP5cmBIN_wide_raw, by.x = "Age", by.y = "Age", suffixes = c(".d13C",".d13C"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, HNISP, by.x = "Age", by.y = "Age", suffixes = c(".d13C",".H"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, MS_newGCV, by.x = "Age", by.y = "Age", suffixes = c(".d13C",".MS"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, Ti_pct_newGCV, by.x = "Age", by.y = "Age", suffixes = c(".MS",".Ti"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, dD_newGCV, by.x = "Age", by.y = "Age", suffixes = c(".Ti",".dD"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, bio01_newGCV, by.x = "Age", by.y = "Age", suffixes = c(".dD",".bio01"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, bio04_newGCV, by.x = "Age", by.y = "Age", suffixes = c(".bio01",".bio04"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, bio07_newGCV, by.x = "Age", by.y = "Age", suffixes = c(".bio04",".bio07"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, bio08_newGCV, by.x = "Age", by.y = "Age", suffixes = c(".bio07",".bio08"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, bio09_newGCV, by.x = "Age", by.y = "Age", suffixes = c(".bio08",".bio09"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, bio10_newGCV, by.x = "Age", by.y = "Age", suffixes = c(".bio09",".bio10"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, bio11_newGCV, by.x = "Age", by.y = "Age", suffixes = c(".bio10",".bio11"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, bio12_newGCV, by.x = "Age", by.y = "Age", suffixes = c(".bio11",".bio12"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, bio15_newGCV, by.x = "Age", by.y = "Age", suffixes = c(".bio12",".bio15"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, bio16_newGCV, by.x = "Age", by.y = "Age", suffixes = c(".bio15",".bio16"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, bio17_newGCV, by.x = "Age", by.y = "Age", suffixes = c(".bio16",".bio17"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, bio18_newGCV, by.x = "Age", by.y = "Age", suffixes = c(".bio17",".bio18"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, bio19_newGCV, by.x = "Age", by.y = "Age", suffixes = c(".bio18",".bio19"), no.dups = TRUE)
GAM_data_raw <- merge(GAM_data_raw, BreakBinSum_C, by.x = "Age", by.y = "Age", suffixes = c(".bio18",".PctCranial"), no.dups = TRUE)
GAM_data_raw_long<- tidyr::gather(GAM_data_raw[1:48,], Family, NISP, Anguidae:Teiidae, factor_key=TRUE)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------- LIZARDS & PALEOCLIMATE DATA RDA---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggvegan)
library(knitr)
# Set knitr options to remove NA values
options(knitr.kable.NA = '')

# Hellinger transformation of the lizard community data
LIZNISP5cmBIN.h <- decostand(LIZNISP5cmBIN_wide_0[1:48,], "hellinger")
head(LIZNISP5cmBIN.h) # Hellinger transformed liz data
env_pollen.h <- decostand(env_pollen[1:48,], "hellinger") # Hellinger transformed POLLEN data

#~ Hall's Cave local paleoclimate----
# select local paleoclim variables
local_env.pastclim <- GAM_data_raw[1:48,] %>% dplyr::select(starts_with(c("MS","Ti","d13","dD")))
local_env.pastclim.z <- decostand(local_env.pastclim, method = "standardize") # standardize paleoclim variables
# perform RDA
Liz_Local.RDA <- rda(LIZNISP5cmBIN.h ~ .,
                     data = local_env.pastclim.z)
summary(Liz_Local.RDA)
# plot RDA
autoplot(Liz_Local.RDA, arrows = TRUE) 
# forward selection
set.seed(123)
fwd.sel_local <- ordiR2step(rda(LIZNISP5cmBIN.h ~ 1, data = local_env.pastclim.z), # lower model limit (simple!)
                            scope = formula(Liz_Local.RDA), # upper model limit (the "full" model)
                            direction = "forward",
                            R2scope = TRUE, # can't surpass the "full" model's R2
                            pstep = 1000,
                            trace = FALSE) # change to TRUE to see the selection process!
Liz_Local.RDA$call # all variables
fwd.sel_local$call # selected variables
# RDA with selected variables
Liz.rda.signif_local <- rda(LIZNISP5cmBIN.h ~ MS_fit + Ti_pct_fit + dD_fit, data = local_env.pastclim.z)
RsquareAdj(Liz.rda.signif_local)
summary(Liz.rda.signif_local)
# plot RDA of selected variables
autoplot(Liz.rda.signif_local, arrows = TRUE)
# RDA model results
lm.anova.cca_local_g <-anova.cca(Liz.rda.signif_local, step = 1000) ## Global test of the RDA result
kable(lm.anova.cca_local_g, digits = 3)
# RDA scores
rda_fort_local <- fortify(Liz.rda.signif_local, display = "sites")
# RDA plot
Liz.rda.local.signif_Plot <- autoplot(Liz.rda.signif_local, arrows = TRUE,data = rda_fort , fill = factor(label), label = TRUE, label.size = 3, shape = FALSE, loadings = TRUE, loadings.colour = 'black', loadings.label = TRUE, loadings.label.size = 3, legend.position = "none", scale= 0) +
  geom_point(data = rda_fort_local,aes(RDA1,RDA2),size=2,alpha=1)+
  ylim(-.6,.6)+ xlim(-.6,.6)+
  geom_abline(intercept = 0,slope = 0,linetype="dashed", size=0.8)+ geom_vline(aes(xintercept=0), linetype="dashed", size=0.8) + labs(x = "Axis 1 (64.7%)", y = "Axis 2 (34.5%)") + theme_bw(base_size = 20) +
  theme(legend.justification = c(0,0), axis.text = element_text(family = "Arial",  colour = "black", size = 20),
        axis.title = element_text(family = "Arial", size = 25),
        text = element_text(family = "Arial", size = 20))


#~ Hall's Cave regional paleoclimate----
# select local regional variables
regional.env.pastclim <- GAM_data_raw[1:48,] %>% dplyr::select(starts_with("bio"))
regional.env.pastclim.z <- decostand(regional.env.pastclim, method = "standardize") # standardize paleoclim variables
# RDA
Liz.regional.RDA <- rda(LIZNISP5cmBIN.h ~ .,
               data = regional.env.pastclim.z)
summary(Liz.regional.RDA)
vif.cca(Liz.regional.RDA)
# Global adjusted R^2
RsquareAdj(Liz.regional.RDA)$adj.r.squared
# forward selection
set.seed(123)
fwd.regional.sel <- ordiR2step(rda(LIZNISP5cmBIN.h ~ 1, data = regional.env.pastclim.z), # lower model limit (simple!)
                      scope = formula(Liz.regional.RDA), # upper model limit (the "full" model)
                      direction = "forward",
                      R2scope = TRUE, # can't surpass the "full" model's R2
                      pstep = 1000,
                      trace = FALSE) # change to TRUE to see the selection process!
Liz.regional.RDA$call # all variables
fwd.regional.sel$call # selected variables
# RDA with selected variables
Liz.regional.rda.signif <- rda(LIZNISP5cmBIN.h ~ bio18_fit + bio17_fit + bio10_fit + 
                        bio19_fit + bio12_fit, data = regional.env.pastclim.z)
RsquareAdj(Liz.regional.rda.signif)
summary(Liz.regional.rda.signif)
## Global test of the RDA result
lm.anova.regional.cca_g <-anova.cca(Liz.regional.rda.signif, step = 1000) ## Global test of the RDA result
kable(lm.anova.regional.cca_g, digits = 3)
## Tests of all explanatory variables
lm.anova.regional.cca_t <-anova.cca(Liz.regional.rda.signif, step = 1000, by = "term") ## Tests of all explanatory variables
kable(lm.anova.regional.cca_t, digits = 3)
## Tests of all canonical axes
lm.anova.regional.cca_a <-anova.cca(Liz.regional.rda.signif, step = 1000, by = "axis") ## Tests of all canonical axes
kable(lm.anova.regional.cca_a, digits = 3)
# plot RDA
dev.new()
par(mfrow = c(1, 1))
# Type 1 scaling
ordiplot(Liz.regional.rda.signif, scaling = 1, type = "text")
# Type 2 scaling
ordiplot(Liz.regional.rda.signif, scaling = 2, type = "text")
# RDA scores
rda_fort_regional <- fortify(Liz.regional.rda.signif, display = "sites")
# PLOT RDA
Liz.regional.rda.signif_Plot <- autoplot(Liz.regional.rda.signif, arrows = TRUE,data = rda_fort , fill = factor(label), label = TRUE, label.size = 3, shape = FALSE, loadings = TRUE, loadings.colour = 'black', loadings.label = TRUE, loadings.label.size = 3, legend.position = "none", scale= 0) +
  geom_point(data = rda_fort_regional,aes(RDA1,RDA2),size=2,alpha=1)+
  ylim(-.75,.75)+ xlim(-.75,.75)+
  geom_abline(intercept = 0,slope = 0,linetype="dashed", size=0.8)+ geom_vline(aes(xintercept=0), linetype="dashed", size=0.8) + labs(x = "Axis 1 (51.6%)", y="Axis 2 (38.8%)") + theme_bw(base_size = 20) +
  theme(legend.justification = c(0,0), axis.text = element_text(family = "Arial",  colour = "black", size = 20),
        axis.title = element_text(family = "Arial", size = 25),
        text = element_text(family = "Arial", size = 20))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------- LIZARDS, POLLEN & PALEOCLIM VARIANCE PARTITIONING (PARTIAL RDA'S)-------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(compositions)

# create dataframe of all variables used for testing
Liz_Poll_Clim.h <- data.frame(LIZNISP5cmBIN.h, env_pollen.h, regional.env.pastclim.z, BreakBinSum_C[1:48,], Corrosion.pct[1:48,]) %>% na.omit()

# PCA of pollen data
Poll_PCA_simp <- prcomp(Liz_Poll_Clim.h[,6:30], center = TRUE, scale = TRUE)
Poll_PCA <- as.data.frame(Poll_PCA_simp$x) # PCA dataframe of pollen data
Liz_Poll_Clim.h <- cbind(Liz_Poll_Clim.h, Poll_PCA)

# Testing significance of Pollen PC axes using RDA
LizPoll.RDA <- rda(Liz_Poll_Clim.h[,1:5] ~ .,
                   data = Poll_PCA[,1:7])
anova.cca(LizPoll.RDA, step = 1000) ## Global test of the RDA result
anova.cca(LizPoll.RDA, step = 1000, by = "term") ## Tests of all explanatory variables
anova.cca(LizPoll.RDA, step = 1000, by = "axis") ## Tests of all canonical axes
# forward selection
set.seed(123)
fwd.sel_Poll <- ordiR2step(rda(Liz_Poll_Clim.h[,1:5] ~ 1, data = Poll_PCA[,1:7]), # lower model limit (simple!)
                           scope = formula(LizPoll.RDA), # upper model limit (the "full" model)
                           direction = "foward",
                           R2scope = TRUE, # can't surpass the "full" model's R2
                           pstep = 1000,
                           trace = FALSE) # change to TRUE to see the selection process!
LizPoll.RDA$call # all terms
fwd.sel_Poll$call # selected terms
## Global test of the RDA result
set.seed(123)
anova.cca(fwd.sel_Poll, step = 1000) ## Global test of the RDA result


# Parsimonious paleoclim variables from RDA selection
VAR_clim.pars <-Liz_Poll_Clim.h %>% select("bio18_fit",  "bio17_fit",  "bio10_fit", "bio19_fit",  "bio12_fit")

# Partition the variation in the community composition
Var.part.all <- varpart(Liz_Poll_Clim.h[,1:5], VAR_clim.pars, Poll_PCA[,1:2])
Var.part.all$part  # access results!
# variance partitioning plot
dev.new()
plot(Var.part.all,
     Xnames = c("Paleoclim", "Pollen"), # name the partitions
     bg = c("mediumpurple", "seagreen3"), alpha = 80, # colour the circles
     digits = 2, # only show 2 digits
     cex = 1.5,
     xlim = c(-100,100),
     ylim = c(-100,100))


# TESTING RDA SIGNIFICANCE THROUGH PERMUTATION

#[a+b]
lm_a_b<- anova.cca(rda(Liz_Poll_Clim.h[,1:5], VAR_clim.pars))
kable(lm_a_b, digits = 3)

#[b+c]
lm_b_c <- anova.cca(rda(Liz_Poll_Clim.h[,1:5], Poll_PCA[,1]))
kable(lm_b_c, digits = 3)

#[Taphonomy]
BreakBinSum_C.z <- decostand(Liz_Poll_Clim.h$PctCompl, method = "standardize")
Corrosion.z <- decostand(Liz_Poll_Clim.h[,49:52], method = "standardize")
Corrosion.ilr <- ilr(Liz_Poll_Clim.h[,49:52]) # isometric log-ration transformation
pairs(Corrosion.ilr)
plot(Corrosion.z)

lm_break <- anova.cca(rda(Liz_Poll_Clim.h[,1:5], BreakBinSum_C.z))

lm_corrosion <- anova.cca(rda(Liz_Poll_Clim.h[,1:5], Corrosion.z))
set.seed(123)
lm_corrosion_ilr <- anova.cca(rda(Liz_Poll_Clim.h[,1:5], Corrosion.ilr))


# Partition the variation in the community composition
Var.part.tapho <- varpart(Liz_Poll_Clim.h[,1:5], VAR_clim.pars, BreakBinSum_C.z)
Var.part.tapho$part  # access results!

dev.new()
plot(Var.part.tapho,
     Xnames = c("Paleoclim", "Breakage"), # name the partitions
     bg = c("mediumpurple", "seagreen3"), alpha = 80, # color the circles
     digits = 2, # only show 2 digits
     cex = 1.5,
     xlim = c(-100,100),
     ylim = c(-100,100))

#~ PARTIAL RDA'S----

#[a] X1|X2, PARTIAL RDA
X1_X2 <- anova.cca(rda(Liz_Poll_Clim.h[,1:5], VAR_clim.pars, Poll_PCA[,1]))
kable(X1_X2, digits = 3)

#[c] X2|X1, PARTIAL RDA
X2_X1 <- anova.cca(rda(Liz_Poll_Clim.h[,1:5], Poll_PCA[,1], VAR_clim.pars))
kable(X2_X1, digits = 3)

#[a&b] X1|Breakage, PARTIAL RDA
X1_Break <- anova.cca(rda(Liz_Poll_Clim.h[,1:5], VAR_clim.pars, BreakBinSum_C.z))
kable(X1_Break, digits = 3)

#[a&b] X2|Breakage, PARTIAL RDA
X2_Break <- anova.cca(rda(Liz_Poll_Clim.h[,1:5], Poll_PCA[,1], BreakBinSum_C.z))
kable(X2_Break, digits = 3)

#[a&b] X1|Corrosion, PARTIAL RDA
X1_Corrosion <- anova.cca(rda(Liz_Poll_Clim.h[,1:5], VAR_clim.pars, Corrosion.z))
kable(X1_Corrosion, digits = 3)
X1_Corrosion_ilr <- anova.cca(rda(Liz_Poll_Clim.h[,1:5], VAR_clim.pars, Corrosion.ilr))
kable(X1_Corrosion_ilr, digits = 3)

#[a&b] Xr|Corrosion, PARTIAL RDA
X2_Corrosion <- anova.cca(rda(Liz_Poll_Clim.h[,1:5], Poll_PCA[,1], Corrosion.z))
kable(X2_Corrosion, digits = 3)
X2_Corrosion_ilr <- anova.cca(rda(Liz_Poll_Clim.h[,1:5], Poll_PCA[,1], Corrosion.ilr))
kable(X2_Corrosion_ilr, digits = 3)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##---------------- LIZARDS POLLEN, & PASTCLIM DATA CCA & COCA-------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##~ CCA ----
LIZ.cca <- cca(LIZNISP5cmBIN_wide_0[1:48,] ~ ., regional.env.pastclim.z)
summary(LIZ.cca) # Scaling 2 (default)
# Unadjusted and adjusted R^2 - like statistics
RsquareAdj(LIZ.cca)

# Permutation test of the overall analysis
anova(LIZ.cca, permutations = how(nperm = 999))
# Permutation test of each axis
anova(LIZ.cca, by = "axis", permutations = how(nperm = 999))
# CCA-based forward selection using vegan's ordistep()
set.seed(123)
cca.step.forward <- ordistep(cca(LIZNISP5cmBIN_wide_0[1:48,] ~ 1, data = regional.env.pastclim.z),
                             scope = formula(LIZ.cca),
                             direction = "forward",
                             permutations = how(nperm = 199))

cca.step.forward$call
anova.cca(cca.step.forward)
# CCA of selected variables
Liz.cca.pars <- cca(LIZNISP5cmBIN_wide_0[1:48,] ~ bio09_fit + bio17_fit + 
                      bio12_fit + bio16_fit + bio10_fit + bio15_fit + bio19_fit, data = regional.env.pastclim.z)
RsquareAdj(Liz.cca.pars)
anova(Liz.cca.pars, permutations = how(nperm = 999)) # global significance test
anova(Liz.cca.pars, permutations = how(nperm = 999), by = "axis") # significance test by axis
anova(Liz.cca.pars, permutations = how(nperm = 999), by = "term") # significance test by term
summary(Liz.cca.pars)

# R-square – like statistics
RsquareAdj(Liz.cca.pars)
# Compare variance inflation factors
vif.cca(LIZ.cca)
vif.cca(Liz.cca.pars)

# CCA scaling 2 biplot with species but without sites
dev.new()
plot(Liz.cca.pars,
     scaling = 2,
     display = c("sp", "cn"),
     main = "Biplot CCA Liz ~ env - scaling 2"
)
plot(Liz.cca.pars, type="n", scaling="species", xlim = c(-.75,.75), ylim = c(-.75,.75))
text(Liz.cca.pars, dis="cn", scaling="species", col = "red", arrow.mul = .75)
text(Liz.cca.pars, "species", col="blue", cex=0.8, scaling="species")


##~ Co-correspondence analysis ----
library(cocorresp)

# create new dataframe
Liz_pollen2 <- merge(analogue::tran(LIZNISP5cmBIN_wide2, method = 'missing', na.rm = FALSE, na.value = 0), 
                     Total_Pollen_dataBIN, by= "Bin", all.x = TRUE)
Liz_pollen2 <- Liz_pollen2[1:48, ]
Liz_pollen2 <- Liz_pollen2 %>% tibble::column_to_rownames(var='Bin')
Liz_pollen2 <- Liz_pollen2 %>% dplyr::select(Anguidae, Crotaphytidae, Phrynosomatidae, Scincidae, Teiidae, Pinus, Picea, Cuppressaceae, Salix, Quercus, Juglans, 
                                             Carya, Ulmus, Celtis, Moraceae, Rhamnaceae, Alnus, Fraxinus, Ephedra, Poaceae,
                                             Chenopodiaceae, `Eriogonum type`, Opuntia, Umbelliferae, Onagraceae, `Salvia type`, `Ambrosia-type`,
                                             Artemisia, Asteraceae_Tubuliflorae, `Asteraceae liguliflorae`)
Liz_pollen2_no_na <-na.omit(Liz_pollen2) # remove NA's
Liz_coca <- Liz_pollen2_no_na %>% dplyr::select(Anguidae, Crotaphytidae, Phrynosomatidae, Scincidae, Teiidae)
Pollen_coca <- Liz_pollen2_no_na %>% dplyr::select(!c(Anguidae, Crotaphytidae, Phrynosomatidae, Scincidae, Teiidae))

# Co-correspondence analysis
Liz.pred <- coca(Liz_coca ~ ., data = Pollen_coca)
Liz.pred
summary(Liz.pred)

# Leave-one-out cross-validation
cross_coca<-crossval(Liz_coca, Pollen_coca)
cross_coca$CVfit
summary(cross_coca)
# Permutation test
set.seed(123)
Liz.pred.perm <- permutest(Liz.pred, permutations = 99)
summary(Liz.pred.perm)

# Extract the site scores and the species loadings used in the biplots
Liz.scores <- scores(Liz.pred)
load.Liz <- Liz.pred$loadings$Y
load.plant <- Liz.pred$loadings$X

# plot
dev.new(); par(mfrow = c(1, 2))
plot(Liz.pred,
     type = "none",
     main = "Lizards",
     xlim = c(-2.5, 4),
     ylim = c(-2.5, 2.5))
ordipointlabel(load.Liz, display = "species",add = TRUE, col = c("red")) 
points(load.Liz,cex =.8, pch= 5, col = c("red"))
points(Liz.scores$sites$X, pch = 16, cex = 0.5, col = c("#999999"))

plot(Liz.pred,
     type = "none",
     main = "Pollen",
     xlim = c(-2.5, 4),
     ylim = c(-2.5, 2.5)
)
ordipointlabel(load.plant, display = "species",add = TRUE) 
points(load.plant,cex =.8, pch= 19)
points(Liz.scores$sites$X, pch = 16, cex = 0.5, col = c("#999999"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------- POLLEN AND PHYTOLITH CORRELATIONS WITH LIZARDS ABUNDANCE------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(corrplot)

# ....CORRELATION PLOT LIZ POLLEN----
# Hellinger transformed abun data
head(LIZNISP5cmBIN.h)
head(env_pollen.h)

Liz_Poll.h <- data.frame(LIZNISP5cmBIN.h, env_pollen.h) %>% na.omit()
# sort column names
sortcols <- colnames(Liz_Poll.h[,6:18])
library(dplyr)
Liz_Poll.h <- Liz_Poll.h %>% 
  select(-sortcols, sort(sortcols))
sortcols2 <- colnames(Liz_Poll.h[,6:17])
Liz_Poll.h <- Liz_Poll.h %>% 
  select(-sortcols2, sort(sortcols2))
# plot
dev.new()
Lis_poll_rcorr <-  Hmisc::rcorr(as.matrix(Liz_Poll.h), type='spearman')
Lis_poll_rcorr_p <-  Hmisc::rcorr(as.matrix(Liz_Poll.h), type='pearson')
par(mfrow = c(1, 2))
pollen_spearman <- corrplot(Lis_poll_rcorr$r[,1:5], method = 'square',  type = 'lower', col = COL2('PRGn'), tl.col = "black", cl.length = 5, p.mat = Lis_poll_rcorr$P[,1:5], insig = 'label_sig', pch.cex = 1, diag = FALSE)
pollen_pearson <- corrplot(Lis_poll_rcorr_p$r[,1:5], method = 'square',  type = 'lower', col = COL2('PRGn'), tl.col = "black", cl.length = 5, p.mat = Lis_poll_rcorr_p$P[,1:5], insig = 'label_sig', pch.cex = 1, diag = FALSE)


# ....CORRELATION PLOT LIZ GRASS----
# Hellinger transformed abun data
LIZ_grass.h <- vegan::decostand(Liz_grass[,1:5], "hellinger")
env_grass.h <- vegan::decostand(Liz_grass[,6:9], "hellinger")

Liz_grass.df.h <- data.frame(LIZ_grass.h, env_grass.h) %>% na.omit()
# sort column names
sortcols3 <- colnames(Liz_grass.df.h[,6:9])
Liz_grass.df.h <- Liz_grass.df.h %>% 
  select(-sortcols3, sort(sortcols3))
# plot
dev.new()
Liz_grass_rcorr <-  Hmisc::rcorr(as.matrix(Liz_grass.df.h), type='spearman')
Liz_grass_rcorr_p <-  Hmisc::rcorr(as.matrix(Liz_grass.df.h), type='pearson')
par(mfrow = c(1, 2))
grass_spearman <- corrplot(Liz_grass_rcorr$r[, 1:5], method = 'square', type = 'lower', col = COL2('PRGn'), tl.col = "black", cl.length = 5, p.mat = Liz_grass_rcorr$P[, 1:5], insig = 'label_sig', pch.cex = 1, diag = FALSE)
grass_pearson <-corrplot(Liz_grass_rcorr_p$r[, 1:5], method = 'square', type = 'lower', col = COL2('PRGn'), tl.col = "black", cl.length = 5, p.mat = Liz_grass_rcorr_p$P[, 1:5], insig = 'label_sig', pch.cex = 1, diag = FALSE)

#....MANTEL TEST LIZ POLLEN ---- 
# Following https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/mantel-test/
Liz_coca.b <- vegdist(Liz_coca, method="bray", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = TRUE) 
Pollen_coca.b <- vegdist(Pollen_coca, method="bray", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = TRUE) 

set.seed(123)
LizPollen_m <- mantel(Liz_coca.b, Pollen_coca.b, method = "spearman", permutations = 9999, na.rm = TRUE)
LizPollen_m


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------- MODEL LIZARD DIVERSITY THROUGH TIME WITH GAMS-----------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(itsadug)
library(gratia)

### ~~ FIT model with observed Shannon diversity  -------
HNISP.mod <- gam(Obs ~ s(Age, k = 15), method = "REML", data = GAM_data_raw[1:48,])
# model chacks
k.check(HNISP.mod)
gam.check(HNISP.mod)
acf(resid(HNISP.mod), lag.max = 36, main = "ACF")
pacf(resid(HNISP.mod), lag.max = 36, main = "pACF")

# setup prediction data
HNISP.mod_pred <- with(GAM_data_raw[1:48,],
                       data.frame(Age=seq(min(Age), max(Age), length=48),
                                  Obs=seq(min(Obs), max(Obs), length=48)))

# make the prediction, add this and a column of standard errors to the prediction data.frame. Predictions are on the log scale.
HNISP.mod_pred <- cbind(GAM_data_raw[1:48,],
                        HNISP.mod=predict(HNISP.mod, 
                                          GAM_data_raw[1:48,], 
                                          se.fit=TRUE, 
                                          type="response"))
HNISP.mod_crit.t = qt(0.975, df.residual(HNISP.mod))
HNISP.mod_pred <- cbind(HNISP.mod_pred,
                        HNISP_upper = HNISP.mod_pred$HNISP.mod.fit + (HNISP.mod_crit.t * HNISP.mod_pred$HNISP.mod.se.fit),
                        HNISP_lower = HNISP.mod_pred$HNISP.mod.fit - (HNISP.mod_crit.t * HNISP.mod_pred$HNISP.mod.se.fit))

## plot mod fits
HNISP_fitted <- ggplot(GAM_data_raw[1:48,], aes(y = Obs, x = Age)) +
  geom_point() +
  geom_ribbon(data = HNISP.mod_pred,
              mapping = aes(x = Age, ymax = HNISP_upper, ymin = HNISP_lower),
              alpha = 0.3, inherit.aes = FALSE) +
  geom_line(data = HNISP.mod_pred,
            mapping = aes(y = HNISP.mod.fit, x = Age)) +
  labs(y = "H (Shannon div.)", x = "Cal Year BP") +
  theme(legend.position = "right") +theme_bw()
HNISP_fitted


### ~~ Accounting for heteroscedasticity due to time averaging -------
# age interval for model
sampleInterval <- maxage5cm[1:48] - minage5cm[1:48]

# run GAM weighted by time spanned by interval
HNISP.mod_hetero <- gam(Obs ~ s(Age, k = 15), method = "REML", data = GAM_data_raw[1:48,],
                        weights = sampleInterval / mean(sampleInterval))
# model checks
k.check(HNISP.mod_hetero)
summary(HNISP.mod_hetero)
gam.check(HNISP.mod_hetero)

# setup prediction data
newHNISP <- with(GAM_data_raw[1:48,],
                 data.frame(Age=seq(min(Age), max(Age), length=48),
                            Obs=seq(min(Obs), max(Obs), length=48)))
# calculate confidence interval
HNISP.mod_hetero.cint <- confint(HNISP.mod_hetero, parm = "s(Age)",  data = newHNISP,
                                 type = "confidence", shift = TRUE)

## simulations

## ages to simulate at
newHNISP_sim <- with(GAM_data_raw[1:48,],
                     data.frame(Age = seq(min(Age), max(Age),
                                          length.out = 48)))
# make predictions
HNISP_pred_sim <- cbind(newHNISP_sim,
                        data.frame(predict(HNISP.mod_hetero, newHNISP_sim,
                                           se.fit = TRUE)))
# confidence intervals
HNISP.mod_sim_crit.t = qt(0.975, df.residual(HNISP.mod_hetero))
HNISP_pred_sim <- transform(HNISP_pred_sim,
                            upper = fit + (HNISP.mod_sim_crit.t * se.fit),
                            lower = fit - (HNISP.mod_sim_crit.t * se.fit))

set.seed(123)
nsim = 20
sims2 <- simulate(HNISP.mod_hetero, nsim = nsim, data = HNISP_pred_sim,
                  unconditional = TRUE)
## rearrange the output into a long/tidy format
colnames(sims2) <- paste0("sim", seq_len(nsim))
sims2 <- setNames(stack(as.data.frame(sims2)),
                  c("simulated", "run"))
sims2 <- transform(sims2, yr = rep(HNISP_pred_sim$Age, nsim),
                   simulated = simulated)
# plot simulations
HNISPSim.plt <- ggplot(HNISP_pred_sim, aes(x = Age, y = fit)) +
  geom_line(data = sims2,
            mapping = aes(y = simulated, x = yr, group = run),
            colour = "grey80") +
  geom_line(lwd = 2) +
  labs(y = "H (Shannon div.)", x = "Cal. Year BP")+
  geom_point(data = GAM_data_raw[1:48,],
             mapping = aes(x = Age, y = Obs),
             inherit.aes = FALSE,
             size = 0.7)
HNISPSim.plt
# confidence interval
HNISP.sint <- confint(HNISP.mod_hetero, parm = "s(Age)", data = HNISP_pred_sim,
                      type = "simultaneous", shift = TRUE)
# plot
HNISP.sint_plt <- ggplot(HNISP.sint, aes(x = Age, y = est)) +geom_ribbon(mapping = aes(ymin = lower, ymax = upper, x = Age),
                                                                         fill = "grey80", inherit.aes = TRUE)+ theme_bw()+labs(y = "H (Shannon div.)", x = "Year BP")+
  geom_point(data = GAM_data_raw[1:48,],
             mapping = aes(x = Age, y = Obs),
             inherit.aes = FALSE,
             size = 0.7) + theme_bw() + coord_cartesian(xlim = c(0,16200)) +scale_x_continuous(breaks =seq(0,100000,2000))

HNISPInt.plt <- ggplot(HNISP.mod_hetero.cint, aes(x = Age, y = est)) +
  geom_line(data = sims2,
            mapping = aes(y = simulated, x = yr, group = run),
            colour = "grey80") +
  geom_ribbon(data = HNISP.sint,
              mapping = aes(ymin = lower, ymax = upper, x = Age),
              fill = "grey80", inherit.aes = FALSE) +
  geom_ribbon(mapping = aes(ymin = lower, ymax = upper, x = Age),
              fill = "grey60", inherit.aes = FALSE) +
  geom_line(lwd = 1) +
  labs(y = "H (Shannon div.)", x = "Years BP")+
  geom_point(data = GAM_data_raw[1:48,],
             mapping = aes(x = Age, y = Obs),
             inherit.aes = FALSE,
             size = 0.7) + theme_bw()
HNISPInt.plt

###~~ First derivative ----------
# take the derivative
HNISP.mod_hetero.d <- derivatives(HNISP.mod_hetero)
# plot
HNISP_deriv_plt <- ggplot(HNISP.mod_hetero.d, aes(x = data, y = derivative)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2, fill = "black") +
  geom_line() + theme_bw() + coord_cartesian(xlim = c(0,16200)) + scale_x_continuous(breaks =seq(0,100000,2000)) +
  labs(x = "Years BP", y = "First derivative")
HNISP_deriv_plt

deriv_plots <- plot_grid(HNISP.sint_plt, HNISP_deriv_plt, bchange_plot, ncol = 1, labels = "auto", align = "hv",
                         axis = "lr")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------- USING GAMS TO EXPLAIN LIZARD DIVERSITY WITH PALEOCLIMATE PREDICTORS---------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
theme_set(theme_bw())
library(dplyr)
library(mgcv)
library(tidygam)

# ~~~ Local Paleoproxies from Hall's Cave----
M_HC_proxies <- gam(formula = Obs ~ 
                      s(Age, bs="tp", k = 15) + 
                      s(MS_fit, bs="tp", k = 15) + 
                      s(d13C_fit, bs="tp", k = 15) + 
                      s(dD_fit, bs="tp", k = 15)+
                      s(Ti_pct_fit,bs="tp", k = 15)
                    + ti(MS_fit, d13C_fit) 
                    + ti(d13C_fit, dD_fit)
                    + ti(dD_fit, Ti_pct_fit)
                    + ti(MS_fit, dD_fit)
                    + ti(MS_fit, Ti_pct_fit)
                    + ti(d13C_fit, Ti_pct_fit)
                    ,
                    select = TRUE,
                    data = GAM_data_raw[1:48,], method="ML",
                    family = "gaussian" )
# model checks
k.check(M_HC_proxies)
par(mfrow = c(2, 2))
gam.check(M_HC_proxies)
qq_plot(M_HC_proxies, method = "simulate") +
  labs(title =NULL, subtitle =NULL)
par(mfrow = c(1, 2))
acf(resid(M_HC_proxies), lag.max = 36, main = "ACF")
pacf(resid(M_HC_proxies), lag.max = 36, main = "pACF")
gratia::appraise(M_HC_proxies)
# model summary
summary(M_HC_proxies)
# plot partial effects
gratia::draw(M_HC_proxies)
M_HC_proxies_sig_effts.plot <- gratia::draw(M_HC_proxies, dist = .1, n = 70, select = c("s(MS_fit)", "s(Ti_pct_fit)", "ti(d13C_fit,dD_fit)", "ti(MS_fit,Ti_pct_fit)"), ncol = 4, residuals = TRUE)
# observed vs predicted lizard diversity based on model
M_HC_proxies_pred <- cbind(GAM_data_raw[1:48,],
                           gam_raw = predict.gam(M_HC_proxies, GAM_data_raw[1:48,], se.fit = TRUE, type = "response"))
# confidence intervals
M_HC_proxies_pred <- M_HC_proxies_pred %>% 
  dplyr::mutate(gam_raw_pred_lcl = gam_raw.fit - 1.96 * gam_raw.se.fit,
                gam_raw_pred_ucl = gam_raw.fit + 1.96 * gam_raw.se.fit)
# plot observed vs predicted lizard diversity
Pred_colors <- c("Observed" = "#1E88E5", "Fitted" = "#D81B60")
ggplot(M_HC_proxies_pred, aes(Age)) + 
  geom_line(aes(y=Obs, colour="Observed"), linewidth=1)  + 
  geom_line(aes(y=gam_raw.fit, colour="Fitted"), linewidth=1)  + 
  geom_ribbon(aes(ymin = gam_raw_pred_ucl, ymax = gam_raw_pred_lcl), alpha = 0.3) +
  theme_bw() + scale_color_manual(values = Pred_colors) +labs(
    title = "Gam raw HC local proxies",
    subtitle = "",
    x = "Age",
    y = "H (Shannon diversity)")

# setup model predictions with explanatory variables
M_HCpaleoclim_proxies_pred <- with(GAM_data_raw[1:48,],
                                   data.frame(Age=seq(min(Age), max(Age), length=100),
                                              Obs=seq(min(Obs), max(Obs), length=100)))
# make the prediction, add this and a column of standard errors to the prediction data.frame.
M_HCpaleoclim_proxies_pred <- cbind(GAM_data_raw[1:48,],
                                    M_HC_proxies=predict(M_HC_proxies, 
                                                         GAM_data_raw[1:48,], 
                                                         se.fit=TRUE, 
                                                         type="response"))
# plot predictions with each variable
MS_mplot <- ggplot(data=M_HCpaleoclim_proxies_pred, aes(x=Age, y=MS_fit, color = M_HC_proxies.fit)) +
  geom_point(size = 15) + labs(y = MS_label, x = "Years BP", color = "H (Shannon diversity)", subtitle = "")+
  viridis::scale_color_viridis(option = "D") +theme_classic2(base_size = 16)

Ti_mplot <- ggplot(data=M_HCpaleoclim_proxies_pred, aes(x=Age, y=Ti_pct_fit, color = M_HC_proxies.fit)) +
  geom_point(size = 15) + labs(y = "Titanuim percentage", x = "Years BP", color = "H (Shannon diversity)", subtitle = "")+
  viridis::scale_color_viridis(option = "D") +theme_classic2(base_size = 16)

d13_dD_mplot <-ggplot(data=M_HCpaleoclim_proxies_pred, aes(x=d13C_fit, y=dD_fit, color = M_HC_proxies.fit, label = round(Age, digits = -1))) +
  geom_point(size = 15) + labs(y = dD_label, x = d13c_label, color = "H (Shannon diversity)", subtitle = "")+
  viridis::scale_color_viridis(option = "D") +theme_classic2(base_size = 16) +
  geom_text(data=slice(M_HCpaleoclim_proxies_pred, seq(1, nrow(M_HCpaleoclim_proxies_pred), 4)), color = "white")

MS_Ti_mplot <-ggplot(data=M_HCpaleoclim_proxies_pred, aes(x=MS_fit, y=Ti_pct_fit, color = M_HC_proxies.fit, label = round(Age, digits = -1))) +
  geom_point(size = 15) + labs(y = "Titanuim percentage", x = MS_label, color = "H (Shannon diversity)", subtitle = "")+
  viridis::scale_color_viridis(option = "D") +theme_classic2(base_size = 16) +
  geom_text(data=slice(M_HCpaleoclim_proxies_pred, seq(1, nrow(M_HCpaleoclim_proxies_pred), 4)), color = "white")
# plot together
plot_grid(MS_mplot, Ti_mplot, d13_dD_mplot,MS_Ti_mplot, ncol = 2, labels = "auto", align = "hv",
          axis = "lr")

# ~~~ Regional Paleoproxies----
M_regional.paleoclim_proxies <- gam( formula = Obs ~ 
                              s(Age, bs="tp", k = 10) + 
                              s(bio09_fit, bs="tp", k = 10) + 
                              s(bio10_fit, bs="tp", k = 10) + 
                              s(bio12_fit, bs="tp", k = 10) +
                              s(bio15_fit, bs="tp", k = 10) +
                              s(bio16_fit, bs="tp", k = 10) +
                              s(bio17_fit, bs="tp", k = 10) +
                              s(bio18_fit, bs="tp", k = 10) +
                              s(bio19_fit, bs="tp", k = 10)
                            + ti(bio10_fit, bio12_fit)
                            + ti(bio10_fit, bio15_fit) 
                            + ti(bio10_fit, bio16_fit) 
                            ,
                            data = GAM_data_raw[1:48,], method="ML", 
                            select = TRUE,
                            family= "gaussian")
# model checks
k.check(M_regional.paleoclim_proxies)
par(mfrow = c(2, 2))
gam.check(M_regional.paleoclim_proxies)
qq_plot(M_regional.paleoclim_proxies, method = "simulate") +
  labs(title =NULL, subtitle =NULL)

# model with log link distribution
set.seed(123)
M_regional.paleoclim_proxies_log <- gam( formula = Obs ~ 
                                  s(Age, bs="tp", k = 10) + 
                                  s(bio09_fit, bs="tp", k = 10) + 
                                  s(bio10_fit, bs="tp", k = 10) + 
                                  s(bio12_fit, bs="tp", k = 10) +
                                  s(bio15_fit, bs="tp", k = 10) +
                                  s(bio16_fit, bs="tp", k = 10) +
                                  s(bio17_fit, bs="tp", k = 10) +
                                  s(bio18_fit, bs="tp", k = 10) +
                                  s(bio19_fit, bs="tp", k = 10)
                                + ti(bio10_fit, bio12_fit)
                                + ti(bio10_fit, bio15_fit) 
                                + ti(bio10_fit, bio16_fit) 
                                ,
                                data = GAM_data_raw[1:48,], method="ML", 
                                select = TRUE,
                                family= gaussian(link = "log"))
# model checks
k.check(M_regional.paleoclim_proxies_log)
par(mfrow = c(2, 2))
gam.check(M_regional.paleoclim_proxies_log)
qq_plot(M_regional.paleoclim_proxies_log, method = "simulate") +
  labs(title =NULL, subtitle =NULL)
par(mfrow = c(1, 2))
acf(resid(M_regional.paleoclim_proxies_log), lag.max = 36, main = "ACF")
pacf(resid(M_regional.paleoclim_proxies_log), lag.max = 36, main = "pACF")

# model comparison
AIC(M_regional.paleoclim_proxies, M_regional.paleoclim_proxies_log)
# best model summary
summary(M_regional.paleoclim_proxies_log)
# plot partial effects of significant terms
M_regional.proxies_sig_effts.plot <- gratia::draw(M_regional.paleoclim_proxies_log, select = c("s(bio10_fit)","s(bio17_fit)", "ti(bio10_fit,bio15_fit)"), ncol = 3, residuals = TRUE)

# setup model predictions with explanatory variables
M_regional.paleoclim_proxies_pred <- with(GAM_data_raw[1:48,],
                                 data.frame(Age=seq(min(Age), max(Age), length=100),
                                            Obs=seq(min(Obs), max(Obs), length=100)))

# make the prediction, add this and a column of standard errors to the prediction data.frame.
M_regional.paleoclim_proxies_pred <- cbind(GAM_data_raw[1:48,],
                                  M_regional.paleoclim_proxies_log=predict(M_regional.paleoclim_proxies_log, 
                                                                  GAM_data_raw[1:48,], 
                                                                  se.fit=TRUE, 
                                                                  type="response"))
# plot predictions with each variable
bio10_mplot <- ggplot(data=M_regional.paleoclim_proxies_pred, aes(x=Age, y=bio10_fit, color = M_regional.paleoclim_proxies_log.fit)) +
  geom_point(size = 15) + labs(y = "Mean temperature of hottest quarter (°C)", x = "Years BP", color = "H (Shannon diversity)", subtitle = "")+
  viridis::scale_color_viridis(option = "D") +theme_classic2(base_size = 16)


bio17_mplot <- ggplot(data=M_regional.paleoclim_proxies_pred, aes(x=Age, y=bio17_fit, color = M_regional.paleoclim_proxies_log.fit)) +
  geom_point(size = 15) + labs(y = " Precipitation of driest quarter (mm)", x = "Years BP", color = "H (Shannon diversity)", subtitle = "")+
  viridis::scale_color_viridis(option = "D") +theme_classic2(base_size = 16)

bio10_15_mplot <-ggplot(data=M_regional.paleoclim_proxies_pred, aes(x=bio10_fit, y=bio15_fit, color = M_regional.paleoclim_proxies_log.fit, label = round(Age, digits = -1))) +
  geom_point(size = 15) + labs(x = "Mean temperature of hottest quarter (°C)", y = "Precipitation seasonality", color = "H (Shannon diversity)", subtitle = "")+
  viridis::scale_color_viridis(option = "D") +theme_classic2(base_size = 16) +
  geom_text(data=slice(M_regional.paleoclim_proxies_pred, seq(1, nrow(M_regional.paleoclim_proxies_pred), 5)), color = "white")

# plot together with local and regional proxies
plot_grid(d13_dD_mplot, Ti_mplot,MS_mplot, MS_Ti_mplot, bio10_mplot, bio17_mplot, bio10_15_mplot, ncol = 3, labels = "auto", align = "hv",
          axis = "lr")
