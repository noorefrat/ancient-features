#inset maps -- enclaves paper
#Script to generate the data and create the maps for Enclaves paper
#some pacakages might be older versions 
library(dplyr)
library(tibble) 
library(tidyverse) 
library(lingtypology)
library(ggplot2)
library(gridExtra)
library(cowplot)

###################################generate data###############################################
#get anea data
anea<- read.csv("~/Downloads/anea_4.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n")) #import anea dataset
colnames(anea)[8:11] <- c("Optative", "Gender Distinctions in Independent Personal Pronouns", "Ergative", "Synthesis")

#get WALS data
f_names <- c("73a", "44a")
wals <- wals.feature(f_names)

colnames(wals)[4:5] <- c("Optative", "Gender Distinctions in Independent Personal Pronouns")
wals <- wals %>% distinct(language, .keep_all = T)

# load the dataset
load('~/Downloads/autotyp-data-master/data/autotyp.RData')
al<- Alignment %>% filter(CombinedPredicateClassID %in% c("1.2")) %>% group_by(Glottocode) %>% summarize(ergative = any(Alignment2 == c('S=P≠Atr', 'S≠Atr≠P', 'S≠Atr=P')))
vs<- select(VerbSynthesis, Glottocode, VerbInflectionMaxCategoryCount)
autotyp<- full_join(al, vs, by='Glottocode')
autotyp <- autotyp %>% distinct(Glottocode, .keep_all = T)
colnames(autotyp)[which(colnames(autotyp) == 'Glottocode')] <- 'glottocode'
colnames(autotyp)[2:3] <- c("Ergative", "Synthesis")

#get metadata
autotypmeta<- select(Register, Glottocode, Macrocontinent, MajorBranch, LocalRegion)
colnames(autotypmeta)[which(colnames(autotypmeta) == 'Glottocode')] <- 'glottocode'
colnames(autotypmeta)[which(colnames(autotypmeta) == 'Macrocontinent')] <- 'area'
colnames(autotypmeta)[which(colnames(autotypmeta) == 'MajorBranch')] <- 'family'
colnames(autotypmeta)[which(colnames(autotypmeta) == 'LocalRegion')] <- 'AncientArea'

#combine datasets
autotypm <- merge(autotyp, autotypmeta, by= "glottocode", all.x = T)
walsm <- merge(wals, autotypmeta, by= "glottocode", all.x = T)
reference <- merge(walsm, autotypm, all = T, by = c('glottocode', 'area', 'family', 'AncientArea'))
reference <- relocate(reference, language)
reference<- select(reference, -wals.code)

#keep only Old World data
ow<- reference %>% filter(area %in% c("Africa", "Eurasia"))


#combine anea and Old World
ow.anea<- full_join(ow, anea)
ow.anea<- ow.anea %>% distinct(language, .keep_all = T)


#load isolates languages (list from Glottolog)
isolates<- read.csv("data/isolates.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n"))

iso.glot <- as.vector(isolates$glottocode) #extract glottocodes of isolates
iso_data<-ow.anea[ ow.anea$glottocode %in% iso.glot, ] #subset to include only isolate languages. #option 2
#create world data
isolate_data<- ow.anea

isolate_data$AncientArea[isolate_data$glottocode %in% iso.glot] <- "isolate" #assign isolate to isolates


#####################################################################################################3
enclave_data<-isolate_data
enclave_data$AncientArea[enclave_data$AncientArea == "Caucasus"] <- "enclave" #assign Caucasus to enclave
enclave_data$AncientArea[enclave_data$AncientArea == "Himalaya"] <- "enclave" #assign Himalaya to enclave

#change NAs to 'other'
enclave_data$AncientArea[enclave_data$AncientArea %in% NA] <- "modern" # Use that new level to change NA to this level #with this method 'other' will be added as the last item (also in plots)

enclave_data<- enclave_data %>% distinct(language, .keep_all = T)
enclave_data<- enclave_data %>% drop_na(longitude)
#write.csv(enclave_data, file = "complete_data_4.csv")

#make long format
enclave_data$Synthesis<-as.character(enclave_data$Synthesis)
enclave_data$Ergative<-as.character(enclave_data$Ergative)
best<- enclave_data %>% pivot_longer(8:ncol(enclave_data), names_to = "feature")
#write.csv(best, file = "enclave_map_data.csv")

##########################################################################################################################################
##########################################################################################################################################
#################################################### PLOT ################################################################################
##########################################################################################################################################

#best<- read.csv("~/Downloads/enclave_map_data.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n")) 

#load projection script
source('~/Downloads/ggworld_sf_v1.R') # Script for pac-centered equal-earth map

#color pallette
cl <- c("#3d3de3", "#FF5E5B")

#Optative
##########################################################################################################################################
optative<- best %>% filter(feature %in% c("Optative")) 
gg.opt <- project_data(df = optative %>% filter(area %in% c("Africa", "Eurasia", "ANEA")) ,
                       xmin = -20,
                       xmax = 200,
                       ymin = -35,
                       ymax = 80)

opt<- gg.opt$base_plot +
  geom_sf(data = gg.opt$data, aes(color = value, shape= AncientArea),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(3, 17, 15, 16, 8), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL, labels=c("absent", "present")
  ) +
  theme(panel.grid = element_blank(), legend.direction = "vertical", legend.position = "right", legend.box = "vertical", legend.text = element_text(size = 10, family= "serif"), plot.title = element_text(family = "Helvetica", face = "bold", size = (10), hjust = 1)
  ) +
  #  labs(title = "Optative")+
  guides(color= guide_legend(keywidth = 1, keyheight = 1, override.aes = list(size = 1)),
         shape = guide_legend(order = 1, keywidth = 1, keyheight = 1, override.aes = list(size = 1))) 
  #geom_rect(aes(xmin = 20, xmax = 60, ymin = 25, ymax = 50), color = "red", fill = NA) 
  #coord_sf(crs = "+proj=8859 +xmin = -180, +xmax = 180, +ymin = -60 +ymax = 85 +ellps=WGS84 +units=m +no_defs") 


#####################inset###########################
optative<- best %>% filter(feature %in% c("Optative")) 
gg.opt.inset <- project_data(df = optative %>% filter(area %in% c("Africa", "Eurasia", "ANEA")) ,
                       xmin = 20,
                       xmax = 60,
                       ymin = 25,
                       ymax = 50)

opt.inset<- gg.opt.inset$base_plot +
  geom_sf(data = gg.opt.inset$data, aes(color = value, shape= AncientArea),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(3, 17, 15, 16, 8), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL
  ) +
  theme(panel.grid = element_blank(), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5), panel.border = element_rect(colour = "gray", fill=NA, size=1)
  ) +
  guides(shape = "none", color = "none") 


##################################################

mapoptin<- ggdraw() +
  draw_plot(opt) +
  draw_plot(opt.inset, x = 0.50, y = 0.01, width = 0.35, height = 0.35)


ggsave("mapoptin.pdf", width = 15, height = 10, units = c("cm"), dpi = 600)



##########################################################################################################################################

#Ergative
##########################################################################################################################################
ergative<- best %>% filter(feature %in% c("Ergative")) 
gg.erg <- project_data(df = ergative %>% filter(area %in% c("Africa", "Eurasia", "ANEA")) ,
                       xmin = -20,
                       xmax = 200,
                       ymin = -35,
                       ymax = 80)

erg<- gg.erg$base_plot +
  geom_sf(data = gg.erg$data, aes(color = value, shape= AncientArea),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(3, 17, 15, 16, 8), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL, labels=c("absent", "present")
  ) +
  theme(panel.grid = element_blank(), legend.direction = "vertical", legend.position = "right", legend.box = "vertical", legend.text = element_text(size = 10, family= "serif"), plot.title = element_text(family = "Helvetica", face = "bold", size = (10), hjust = 0.5)
  ) +
  #  labs(title = "Ergative Alignment")+
  guides(color= guide_legend(keywidth = 1, keyheight = 1, override.aes = list(size = 1)),
         shape = guide_legend(order = 1, keywidth = 1, keyheight = 1, override.aes = list(size = 1)))


#####################inset###########################
ergative<- best %>% filter(feature %in% c("Ergative")) 
gg.erg.inset <- project_data(df = ergative %>% filter(area %in% c("Africa", "Eurasia", "ANEA")) ,
                             xmin = 20,
                             xmax = 60,
                             ymin = 25,
                             ymax = 50)

erg.inset<- gg.erg.inset$base_plot +
  geom_sf(data = gg.erg.inset$data, aes(color = value, shape= AncientArea),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(3, 17, 15, 16, 8), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL
  ) +
  theme(panel.grid = element_blank(), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5), panel.border = element_rect(colour = "gray", fill=NA, size=1)
  ) +
  guides(shape = "none", color = "none") 


##################################################

mapergin<- ggdraw() +
  draw_plot(erg) +
  draw_plot(erg.inset, x = 0.50, y = 0.01, width = 0.35, height = 0.35)


ggsave("mapergin.pdf", width = 15, height = 10, units = c("cm"), dpi = 600)


##########################################################################################################################################
#Synthesis
##########################################################################################################################################
cl <- c("#3d3de3", "#FF5E5B")
synthesis<- best %>% filter(feature %in% c("Synthesis")) 
synthesis$value<- as.numeric(synthesis$value)
gg.syn <- project_data(df = synthesis %>% filter(area %in% c("Africa", "Eurasia", "ANEA")) ,
                       xmin = -20,
                       xmax = 200,
                       ymin = -35,
                       ymax = 80)

syn <- gg.syn$base_plot +
  geom_sf(data = gg.syn$data, aes(color = value, shape= AncientArea),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(3, 17, 15, 16, 8), name= NULL) +
  scale_color_gradient(name = NULL,  low = "#3d3de3",
                       high = "#FF5E5B",
                       space = "Lab",
                       na.value = "white",
                       guide = "colourbar",
                       aesthetics = "colour"
  ) +
  theme(panel.grid = element_blank(), legend.direction = "vertical", legend.position = "right", legend.box = "vertical", legend.text = element_text(size = 10, family= "serif"), plot.title = element_text(family = "Helvetica", face = "bold", size = (10), hjust = 0.5)
  ) +
  #  labs(title = "Synthesis")+
  guides(color= guide_colorbar(barwidth = 1, barheight = 3, override.aes = list(size = 1), order = 1),
         shape = guide_legend(order = 1, keywidth = 1, keyheight = 1, override.aes = list(size = 1)))

##########################################################################################################################################

#####################inset###########################
synthesis<- best %>% filter(feature %in% c("Synthesis")) 
synthesis$value<- as.numeric(synthesis$value)
gg.syn.inset <- project_data(df = synthesis %>% filter(area %in% c("Africa", "Eurasia", "ANEA")) ,
                             xmin = 20,
                             xmax = 60,
                             ymin = 25,
                             ymax = 50)

syn.inset <- gg.syn.inset$base_plot +
  geom_sf(data = gg.syn.inset$data, aes(color = value, shape= AncientArea),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(3, 17, 15, 16, 8), name= NULL) +
  scale_color_gradient(name = NULL,  low = "#3d3de3",
                       high = "#FF5E5B",
                       space = "Lab",
                       na.value = "white",
                       guide = "colourbar",
                       aesthetics = "colour"
  ) +
  theme(panel.grid = element_blank(), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5), panel.border = element_rect(colour = "gray", fill=NA, size=1)
  ) +
  guides(shape = "none", color = "none") 



##################################################

mapsynin<- ggdraw() +
  draw_plot(syn) +
  draw_plot(syn.inset, x = 0.50, y = 0.01, width = 0.35, height = 0.35)


ggsave("mapsynin.pdf", width = 15, height = 10, units = c("cm"), dpi = 600)



##########################################################################################################################################
#Gender
##########################################################################################################################################
gender<- best %>% filter(feature %in% c("Gender Distinctions in Independent Personal Pronouns")) 
gender$value[gender$value == "No gender distinctions"] <- "No"
gender$value[gender$value == "3rd person singular only"] <- "Yes"
gender$value[gender$value == "3rd person only, but also non-singular"] <- "Yes"
gender$value[gender$value == "In 3rd person + 1st and/or 2nd person"] <- "Yes"
gender$value[gender$value == "1st or 2nd person but not 3rd"] <- "Yes"
gender$value[gender$value == "3rd person non-singular only"] <- "Yes"

gg.gen <- project_data(df = gender %>% filter(area %in% c("Africa", "Eurasia", "ANEA")) ,
                       xmin = -20,
                       xmax = 200,
                       ymin = -35,
                       ymax = 80)

gen<- gg.gen$base_plot +
  geom_sf(data = gg.gen$data, aes(color = value, shape= AncientArea),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(3, 17, 15, 16, 8), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL, labels=c("absent", "present")
  ) +
  theme(panel.grid = element_blank(), legend.direction = "vertical", legend.position = "right", legend.box = "vertical", legend.text = element_text(size = 10, family = "serif"), plot.title = element_text(family = "Helvetica", face = "bold", size = (10), hjust = 0.5)
  ) +
  #  labs(title = "Gender Distinctions in \nIndependent Personal Pronouns")+
  guides(color= guide_legend(keywidth = 1, keyheight = 1, override.aes = list(size = 1)),
         shape = guide_legend(order = 1, keywidth = 1, keyheight = 1, override.aes = list(size = 1)))


##########################################################################################################################################

#####################inset###########################
gender<- best %>% filter(feature %in% c("Gender Distinctions in Independent Personal Pronouns")) 
gender$value[gender$value == "No gender distinctions"] <- "No"
gender$value[gender$value == "3rd person singular only"] <- "Yes"
gender$value[gender$value == "3rd person only, but also non-singular"] <- "Yes"
gender$value[gender$value == "In 3rd person + 1st and/or 2nd person"] <- "Yes"
gender$value[gender$value == "1st or 2nd person but not 3rd"] <- "Yes"
gender$value[gender$value == "3rd person non-singular only"] <- "Yes"

gg.gen.inset <- project_data(df = gender %>% filter(area %in% c("Africa", "Eurasia", "ANEA")) ,
                       xmin = 20,
                       xmax = 60,
                       ymin = 25,
                       ymax = 50)


gen.inset<- gg.gen.inset$base_plot +
  geom_sf(data = gg.gen.inset$data, aes(color = value, shape= AncientArea),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(3, 17, 15, 16, 8), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL
  ) +
  theme(panel.grid = element_blank(), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5), panel.border = element_rect(colour = "gray", fill=NA, size=1)
  ) +
  guides(shape = "none", color = "none") 


##################################################

mapgenin<- ggdraw() +
  draw_plot(gen) +
  draw_plot(gen.inset, x = 0.50, y = 0.01, width = 0.35, height = 0.35)


ggsave("mapgenin.pdf", width = 15, height = 10, units = c("cm"), dpi = 600)











##########################################################################################################################################
#########  CP MAPS  ############
##########################################################################################################################################
######### prepare data ############

#combine anea and World
w.anea<- full_join(reference, anea)
w.anea<- w.anea %>% distinct(language, .keep_all = T)

#load Circum Pacific languages
cp<- Register %>% distinct(Glottocode, .keep_all = T) %>%
  mutate(AncientArea = ifelse(Continent %in% c('Australia',
                                               'NG and Oceania',
                                               'W N America',
                                               'S America',
                                               'C America',
                                               'E N America') |
                                Area %in% 'N Coast Asia',
                              "Inside Circum-Pacific", "Outside Circum-Pacific"),
         AN = ifelse(Stock %in% 'Austronesian', 'Austronesian', 'Other')
  )
cp<- select(cp, Glottocode, Language, AncientArea)
colnames(cp)[which(colnames(cp) == 'Glottocode')] <- 'glottocode'
colnames(cp)[which(colnames(cp) == 'Language')] <- 'language'
#colnames(cp)[which(colnames(cp) == 'AncientArea')] <- 'CP'

#load Circum Pacific languages (from 'Large and Ancient Linguistic Areas' https://github.com/balthasarbickel/ancient_areas. autotyp.cp$CP:
#cpn<- read.csv("data/circum_pacific_languages.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n"))
#cp<- select(cp, -glottolog_LID.2014)
cp<- cp %>% distinct(language, .keep_all = T)

cp_data<- w.anea
cp_data$AncientArea[isolate_data$glottocode %in% iso.glot] <- "isolate" #assign isolate to isolates

#add Circum Pacific data to world data
cp_data<- full_join(cp_data, cp, by= c('language', 'glottocode', 'AncientArea'))
cp_data$AncientArea[cp_data$CP == "Inside Circum-Pacific"] <- "CP" #assign CP to enclave

cp_data$AncientArea[cp_data$AncientArea == "Caucasus"] <- "enclave" #assign Caucasus to enclave
cp_data$AncientArea[cp_data$AncientArea == "Himalaya"] <- "enclave" #assign Himalaya to enclave

#change NAs to 'other'
cp_data$AncientArea[cp_data$AncientArea %in% NA] <- "modern" # Use that new level to change NA to this level #with this method 'other' will be added as the last item (also in plots)

cp_data<- cp_data %>% distinct(language, .keep_all = T)
cp_data<- cp_data %>% drop_na(longitude)
#write.csv(cp_data, file = "cp_data_4.csv")

#make long format
cp_data$Synthesis<-as.character(cp_data$Synthesis)
cp_data$Ergative<-as.character(cp_data$Ergative)
cp.data<- cp_data %>% pivot_longer(8:ncol(cp_data), names_to = "feature")
#write.csv(cp.data, file = "cp_map_data.csv")



##########################################################################################################################################
##########################################################################################################################################
#################################################### PLOT ################################################################################
##########################################################################################################################################

#cp.data<- read.csv("~/Downloads/cp_map_data.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n")) 


#Optative
##########################################################################################################################################
optative<- cp.data %>% filter(feature %in% c("Optative")) 
gg.opt.d <- project_data(df = optative)

opt.d<- gg.opt.d$base_plot +
  geom_sf(data = gg.opt.d$data, aes(color = value, shape= AncientArea),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(3, 17, 15, 16, 8), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL, labels=c("absent", "present")
  ) +
  theme(panel.grid = element_blank(), legend.direction = "horizontal", legend.position = "bottom", legend.text = element_text(size = 8), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5)
  ) +
  labs(title = "Optative")+
  guides(color= guide_legend(keywidth = 0.2, keyheight = 0.5, override.aes = list(size = 4)),
         shape = "none")

##########################################################################################################################################
#Ergative
##########################################################################################################################################
ergative<- cp.data %>% filter(feature %in% c("Ergative")) 
gg.erg.d <- project_data(df = ergative)

erg.d<- gg.erg.d$base_plot +
  geom_sf(data = gg.erg.d$data, aes(color = value, shape= AncientArea),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(3, 17, 15, 16, 8), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL, labels=c("absent", "present")
  ) +
  theme(panel.grid = element_blank(), legend.direction = "horizontal", legend.position = "bottom", legend.text = element_text(size = 8), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5)
  ) +
  labs(title = "Ergative")+
  guides(color= guide_legend(keywidth = 0.2, keyheight = 0.5, override.aes = list(size = 4)),
         shape = "none")

##########################################################################################################################################

##########################################################################################################################################
#Synthesis
##########################################################################################################################################
synthesis<- cp.data %>% filter(feature %in% c("Synthesis")) 
synthesis$value<- as.numeric(synthesis$value)
gg.syn.d <- project_data(df = synthesis)

syn.d <- gg.syn.d$base_plot +
  geom_sf(data = gg.syn.d$data, aes(color = value, shape= AncientArea),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(3, 17, 15, 16, 8), name= NULL) +
  scale_color_gradient(name = NULL,  low = "#3d3de3",
                       high = "#FF5E5B",
                       space = "Lab",
                       na.value = "white",
                       guide = "colourbar",
                       aesthetics = "colour"
  ) +
  theme(panel.grid = element_blank(), legend.direction = "horizontal", legend.position = "bottom", legend.text = element_text(size = 8), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5)
  ) +
  labs(title = "Synthesis")+
  guides(color= guide_colorbar(barwidth = 4, barheight = 0.5, override.aes = list(size = 4)),
         shape = "none")


##########################################################################################################################################

##########################################################################################################################################
#Gender
##########################################################################################################################################
gender<- cp.data %>% filter(feature %in% c("Gender Distinctions in Independent Personal Pronouns")) 
gender$value[gender$value == "No gender distinctions"] <- "No"
gender$value[gender$value == "3rd person singular only"] <- "Yes"
gender$value[gender$value == "3rd person only, but also non-singular"] <- "Yes"
gender$value[gender$value == "In 3rd person + 1st and/or 2nd person"] <- "Yes"
gender$value[gender$value == "1st or 2nd person but not 3rd"] <- "Yes"
gender$value[gender$value == "3rd person non-singular only"] <- "Yes"

gg.gen.d <- project_data(df = gender)

gen.d<- gg.gen.d$base_plot +
  geom_sf(data = gg.gen.d$data, aes(color = value, shape= AncientArea),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(3, 17, 15, 16, 8), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL, labels=c("absent", "present")
  ) +
  theme(panel.grid = element_blank(), legend.direction = "horizontal", legend.position = "bottom", legend.text = element_text(size = 8), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5)
  ) +
  labs(title = "Gender Distinction")+
  guides(color= guide_legend(keywidth = 0.2, keyheight = 0.5, override.aes = list(size = 4)),
         shape = "none")



##########################################################################################################################################

pdf("mapcp.pdf", height = 6, width = 8)  
grid.arrange(opt.d, erg.d, syn.d, gen.d)
dev.off()


mapcpp<- grid.arrange(opt.d, erg.d, syn.d, gen.d)
ggsave("mapcpp.pdf", width = 15, height = 10, units = c("cm"), dpi = 600)

