### r script to get the data and create the plots

library(tidyverse)
library(devtools)
if(!require(familybias)) install_github('IVS-UZH/familybias')
library(familybias)
library(parallel)
library(ggdist)
library(cowplot)
library(gridExtra)



### WALS v.2020.2 from https://github.com/cldf-datasets/wals/releases ###
#########################################################################

wals.df <- read.csv('~/Downloads/wals-2020.2/cldf/values.csv') %>%
  select(Language_ID, Code_ID) %>%
  inner_join(read.csv('~/Downloads/wals-2020.2/cldf/languages.csv'),
             by = c('Language_ID' = 'ID')) %>%
  rename(Language = Name) %>%
  inner_join(read.csv('~/Downloads/wals-2020.2/cldf/codes.csv'),
             by = c('Code_ID' = 'ID')) %>%
  rename(Value = Name)


### Autotyp v.1.1.0 from https://github.com/autotyp/autotyp-data ###
####################################################################

load('~/Downloads/autotyp-data-master/data/autotyp.RData')

##############################################################
#read anea data
anea<- read.csv("~/Downloads/anea_enclaves.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n")) #import anea dataset
isolates<- read.csv("~/Downloads/isolates_enc.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n"))
iso.glot <- as.vector(isolates$Glottocode) #extract glottocodes of isolates


##############################################################
#load projection for maps
source('https://gitlab.uzh.ch/-/snippets/45/raw/master/ggworld2.R')

###################
#Genderless pronouns
##################
pronouns.df <- filter(wals.df, Parameter_ID %in% '44A') %>%
    # match different glottocode choices between WALS and Autotyp, identified by
    # pre-mutated
    # subset(pronouns.df, !Glottocode %in% Register$Glottocode, c('Language', 'Glottocode'))
    mutate(Glottocode = case_when(Language %in% 'Albanian' ~ 'tosk1239',
                                  Language %in% 'Azerbaijani' ~ 'azer1255',
                                  Language %in% 'Gwari' ~ 'gbar1246',
                                  Language %in% 'Jakaltek' ~ 'popt1235',
                                  Language %in% 'Tukang Besi' ~ 'tuka1249',
                                  Language %in% 'Tubu' ~ 'teda1241',
                                  Language %in% 'Slave' ~ 'nort2942',
                                  Language %in% 'Nubian (Dongolese)' ~ 'nubi1251',
                                  Language %in% 'Mixtec (Chalcatongo)' ~ 'sanm1295',
                                  Language %in% 'Malagasy' ~ 'mala1537',
                                  Language %in% 'Kpelle' ~ 'kpel1252',
                                  Language %in% 'Kongo' ~ 'kong1295',
                                  Language %in% 'Kalmyk' ~ 'kalm1243',
                                  Language %in% 'Kurmanji' ~ 'khor1267',
                                  Language %in% 'Kiwai (Southern)' ~ 'nort2930',
                                  Language %in% 'Kisi' ~ 'sout2778',
                                  Language %in% 'Kewa' ~ 'west2599',
                                  Language %in% 'Indonesian' ~ 'indo1316',
                                  Language %in% 'Haida' ~ 'sout2956',
                                  Language %in% 'Grebo' ~ 'greb1256',
                                  Language %in% 'Cakchiquel' ~ 'cakc1235',
                                  Language %in% 'Buriat' ~ 'buri1258',
                                  Language %in% 'Asmat' ~ 'casu1237',
                                  Language %in% 'Canela' ~ 'cane1242',
                                  Language %in% 'Uradhi' ~ 'urad1238',
                                  Language %in% 'Komo' ~ 'komo1260',
                                  Language %in% 'Hua' ~ 'yaga1260',
                                  Language %in% 'Dong (Southern)' ~ 'nort2735',
                                  Language %in% 'Cora' ~ 'cora1260',
                                  TRUE ~ Glottocode)) %>%
    # add areas:
    rename(WALS_Language = Language) %>% # avoid conflict with Autotyp
    left_join(Register, by = c('Glottocode', 'Longitude', 'Latitude')) %>%
    distinct(Glottocode, .keep_all = T) %>% # matching too many (ok after checking group_by(pronouns.df, Glottocode) %>% summarize(x=n_distinct(Value)>1))
    mutate(AncientArea = ifelse(is.na(Continent) | is.na(Area), NA,
                                ifelse(Continent %in% c('Australia',
                                                        'NG and Oceania',
                                                        'W N America',
                                                        'S America',
                                                        'C America',
                                                        'E N America') |
                                         Area %in% 'N Coast Asia',
                                       "Inside Circum-Pacific", "Outside Circum-Pacific")
    ),
    Genderless = ifelse(is.na(Value), NA,
                        Value %in% 'No gender distinctions')
    ) %>%
  filter(Macrocontinent %in% c("Africa", "Eurasia"))

genderless<- select(pronouns.df, Language, Glottocode, Macrocontinent, MajorBranch, LocalRegion, Latitude, Longitude, Genderless)



###################
#Optative
##################

optatives.df <- filter(wals.df, Parameter_ID %in% '73A') %>%
  # match different glottocode choices between WALS and Autotyp, identified by
  # pre-mutated
  # subset(optatives.df, !Glottocode %in% Register$Glottocode, c('Language', 'Glottocode'))
  mutate(Glottocode = case_when(Language %in% 'Albanian' ~ 'tosk1239',
                                Language %in% 'Azerbaijani' ~ 'azer1255',
                                Language %in% 'Gwari' ~ 'gbar1246',
                                Language %in% 'Jakaltek' ~ 'popt1235',
                                Language %in% 'Tukang Besi' ~ 'tuka1249',
                                Language %in% 'Tubu' ~ 'teda1241',
                                Language %in% 'Slave' ~ 'nort2942',
                                Language %in% 'Nubian (Dongolese)' ~ 'nubi1251',
                                Language %in% 'Mixtec (Chalcatongo)' ~ 'sanm1295',
                                Language %in% 'Malagasy' ~ 'mala1537',
                                Language %in% 'Kpelle' ~ 'kpel1252',
                                Language %in% 'Kongo' ~ 'kong1295',
                                Language %in% 'Kalmyk' ~ 'kalm1243',
                                Language %in% 'Kurmanji' ~ 'khor1267',
                                Language %in% 'Kiwai (Southern)' ~ 'nort2930',
                                Language %in% 'Kisi' ~ 'sout2778',
                                Language %in% 'Kewa' ~ 'west2599',
                                Language %in% 'Indonesian' ~ 'indo1316',
                                Language %in% 'Haida' ~ 'sout2956',
                                Language %in% 'Grebo' ~ 'greb1256',
                                Language %in% 'Cakchiquel' ~ 'cakc1235',
                                Language %in% 'Buriat' ~ 'buri1258',
                                Language %in% 'Asmat' ~ 'casu1237',
                                TRUE ~ Glottocode)) %>%
  # add areas:
  rename(WALS_Language = Language) %>% # avoid conflict with Autotyp
  left_join(Register, by = c('Glottocode', 'Longitude', 'Latitude')) %>%
  distinct(Glottocode, .keep_all = T) %>% # matching too many (ok after checking group_by(optatives.df, Glottocode) %>% summarize(x=n_distinct(Value)>1))
  mutate(AncientArea = ifelse(is.na(Continent) | is.na(Area), NA,
                              ifelse(Continent %in% c('Australia',
                                                      'NG and Oceania',
                                                      'W N America',
                                                      'S America',
                                                      'C America',
                                                      'E N America') |
                                       Area %in% 'N Coast Asia',
                                     "Inside Circum-Pacific", "Outside Circum-Pacific")
  ),
  Optative = ifelse(is.na(Value), NA,
                    Value %in% 'Inflectional optative present')
  ) %>%
  filter(Macrocontinent %in% c("Africa", "Eurasia")) 

optativ <- select(optatives.df, Language, Glottocode, Macrocontinent, MajorBranch, LocalRegion, Latitude, Longitude, Optative)


###################
#Ergative
##################

ergatives.df <- Alignment %>%
  filter(CombinedPredicateClassID %in% c("1.2") &
           SelectorType %in% c('case marker or adposition',
                               'agreement (marker)')) %>%
  group_by(Glottocode) %>%
  summarize(Ergative = any(Alignment2  %in%  c('S=P≠Atr',
                                               'S≠Atr=P',
                                               'S≠Atr≠P')),
  ) %>%
  inner_join(Register) %>%
  mutate(AncientArea = ifelse(Continent %in% c('Australia',
                                               'NG and Oceania',
                                               'W N America',
                                               'S America',
                                               'C America',
                                               'E N America') |
                                Area %in% 'N Coast Asia',
                              "Inside Circum-Pacific", "Outside Circum-Pacific")
  ) %>%
  filter(Macrocontinent %in% c("Africa", "Eurasia")) 



ergativ <- select(ergatives.df, Language, Glottocode, Macrocontinent, MajorBranch, LocalRegion, Latitude, Longitude, Ergative)



##########################################################################################################################################
##########################################################################################################################################

hhh<- purrr::reduce(list(genderless, optativ, ergativ), dplyr::full_join)
ane<- select(anea, -Synthesis)
enclave_data <- bind_rows(ane, hhh) %>% distinct(Language, .keep_all = T)

enclave_data$LocalRegion[enclave_data$Glottocode %in% iso.glot] <- "isolate" #assign isolate to isolates
enclave_data$LocalRegion[enclave_data$LocalRegion == "Caucasus"] <- "enclave" #assign Caucasus to enclave
enclave_data$LocalRegion[enclave_data$LocalRegion == "Himalaya"] <- "enclave" #assign Himalaya to enclave
enclave_data$LocalRegion[enclave_data$LocalRegion %in% NA] <- "modern" # Use that new level to change NA to this level #with this method 'other' will be added as the last item (also in plots)
colnames(enclave_data)[which(colnames(enclave_data) == 'Macrocontinent')] <- 'area'
colnames(enclave_data)[which(colnames(enclave_data) == 'MajorBranch')] <- 'family'
colnames(enclave_data)[which(colnames(enclave_data) == 'LocalRegion')] <- 'AncientArea'
enclave_data<- enclave_data %>% drop_na(Longitude)

#make long format
map_data <- enclave_data %>% pivot_longer(8:ncol(enclave_data), names_to = "feature")


###################
#Synthesis
##################
synthesis.df <- MaximallyInflectedVerbSynthesis %>%
  filter(!is.na(VerbInflectionMaxCategoryCount)) %>%
  mutate(Synthesis_Binary = cut(VerbInflectionMaxCategoryCount,
                                quantile(VerbInflectionMaxCategoryCount,
                                         probs = c(0, 1/2, 1)),
                                right = F, labels = c('low', 'high')),
         Synthesis_Ternary = cut(VerbInflectionMaxCategoryCount,
                                 quantile(VerbInflectionMaxCategoryCount,
                                          probs = c(0, 1/3, 2/3, 1)),
                                 right = F, labels = c('low', 'medium', 'high'))
  ) %>%
  mutate(Synthesis_Binary = ifelse(
    VerbInflectionMaxCategoryCount == max(VerbInflectionMaxCategoryCount), 'high',
    paste(Synthesis_Binary)),
    Synthesis_Ternary = ifelse(
      VerbInflectionMaxCategoryCount == max(VerbInflectionMaxCategoryCount), 'high',           paste(Synthesis_Ternary))
  ) %>%
  mutate(Synthesis_Binary = Synthesis_Binary %in% 'high') %>%
  inner_join(Register) %>%
  mutate(AncientArea = ifelse(Continent %in% c('Australia',
                                               'NG and Oceania',
                                               'W N America',
                                               'S America',
                                               'C America',
                                               'E N America') |
                                Area %in% 'N Coast Asia',
                              "Inside Circum-Pacific", "Outside Circum-Pacific")
  ) %>% select(!where(is.list)) %>%
  filter(Macrocontinent %in% c("Africa", "Eurasia")) 


synth <- select(synthesis.df, Language, Glottocode, Macrocontinent, MajorBranch, LocalRegion, Latitude, Longitude, VerbInflectionMaxCategoryCount)
colnames(synth)[which(colnames(synth) == 'VerbInflectionMaxCategoryCount')] <- 'Synthesis'

aneasyn <- select(anea, Language, Glottocode, Macrocontinent, MajorBranch, LocalRegion, Latitude, Longitude, Synthesis)

syn_data <- bind_rows(aneasyn, synth) %>% distinct(Language, .keep_all = T)

syn_data$LocalRegion[syn_data$Glottocode %in% iso.glot] <- "isolate" #assign isolate to isolates
syn_data$LocalRegion[syn_data$LocalRegion == "Caucasus"] <- "enclave" #assign Caucasus to enclave
syn_data$LocalRegion[syn_data$LocalRegion == "Himalaya"] <- "enclave" #assign Himalaya to enclave
syn_data$LocalRegion[syn_data$LocalRegion %in% NA] <- "modern" # Use that new level to change NA to this level #with this method 'other' will be added as the last item (also in plots)
colnames(syn_data)[which(colnames(syn_data) == 'Macrocontinent')] <- 'area'
colnames(syn_data)[which(colnames(syn_data) == 'MajorBranch')] <- 'family'
colnames(syn_data)[which(colnames(syn_data) == 'LocalRegion')] <- 'AncientArea'
syn_data<- syn_data %>% drop_na(Longitude)

#make long format

synthesis <- syn_data %>% pivot_longer(8:ncol(syn_data), names_to = "feature")



##########################################################################################################################################
#################################################### PLOT ################################################################################
##########################################################################################################################################

#load projection script
source('~/Downloads/ggworld_sf_v1.R') # Script for pac-centered equal-earth map

#color pallette
cl <- c("#3d3de3", "#FF5E5B")

#Optative
##########################################################################################################################################
optative<- map_data %>% filter(feature %in% c("Optative")) 
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


#ggsave("mapoptative.pdf", width = 15, height = 10, units = c("cm"), dpi = 600)



##########################################################################################################################################

#Ergative
##########################################################################################################################################
ergative<- map_data %>% filter(feature %in% c("Ergative")) 
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


ggsave("mapergative.pdf", width = 15, height = 10, units = c("cm"), dpi = 600)




##########################################################################################################################################
#Gender
##########################################################################################################################################
gender<- map_data %>% filter(feature %in% c("Genderless"))

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
         shape = guide_legend(order = 2, keywidth = 1, keyheight = 1, override.aes = list(size = 1)))


##########################################################################################################################################

#####################inset###########################

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
  draw_plot(gen.inset, x = 0.45, y = 0.01, width = 0.35, height = 0.35)


ggsave("mapgenderless.pdf", width = 15, height = 10, units = c("cm"), dpi = 600)



##########################################################################################################################################
#Synthesis
##########################################################################################################################################
syn.g <- project_data(df = synthesis %>% filter(area %in% c("Africa", "Eurasia", "ANEA")) ,
                       xmin = -20,
                       xmax = 200,
                       ymin = -35,
                       ymax = 80)

syn<- syn.g$base_plot + 
  geom_sf(data = syn.g$data, aes(color = value, shape= AncientArea),
          alpha = .5,
          size = 1
  ) +
  scale_colour_gradient2(breaks = c(0,6,16), labels = c("0", "median", "16"),
                         midpoint = median(syn.g$data$value),
                         low = 'blue', mid = 'grey', high = 'red', name = NULL) + 
  scale_shape_manual(values=c(3, 17, 15, 16, 8), name= NULL) +
  theme(panel.grid = element_blank(), legend.direction = "vertical", legend.position = "right", legend.box = "vertical", legend.text = element_text(size = 10, family= "serif"), plot.title = element_text(family = "Helvetica", face = "bold", size = (10), hjust = 0.5)
  ) +
  #  labs(title = "Synthesis")+
  guides(color= guide_colorbar(barwidth = 1, barheight = 3, override.aes = list(size = 1), order = 1),
         shape = guide_legend(order = 1, keywidth = 1, keyheight = 1, override.aes = list(size = 1)))

##########################################################################################################################################

#####################inset###########################
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
  scale_colour_gradient2(breaks = c(0,6,16), labels = c("0", "median", "16"),
                         midpoint = median(syn.g$data$value),
                         low = 'blue', mid = 'grey' , high = 'red', name = NULL) +
  theme(panel.grid = element_blank(), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5), panel.border = element_rect(colour = "gray", fill=NA, size=1)
  ) +
  guides(shape = "none", color = "none") 



##################################################

mapsynin<- ggdraw() +
  draw_plot(syn) +
  draw_plot(syn.inset, x = 0.45, y = 0.01, width = 0.35, height = 0.35)


ggsave("mapsynthesisg.pdf", width = 15, height = 10, units = c("cm"), dpi = 600)








##########################################################################################################################################
##########################################################################################################################################
####################################################  CP  ################################################################################
##########################################################################################################################################

### Optative ###
################

optatives.df.cp <- filter(wals.df, Parameter_ID %in% '73A') %>%
  # match different glottocode choices between WALS and Autotyp, identified by
  # pre-mutated
  # subset(optatives.df, !Glottocode %in% Register$Glottocode, c('Language', 'Glottocode'))
  mutate(Glottocode = case_when(Language %in% 'Albanian' ~ 'tosk1239',
                                Language %in% 'Azerbaijani' ~ 'azer1255',
                                Language %in% 'Gwari' ~ 'gbar1246',
                                Language %in% 'Jakaltek' ~ 'popt1235',
                                Language %in% 'Tukang Besi' ~ 'tuka1249',
                                Language %in% 'Tubu' ~ 'teda1241',
                                Language %in% 'Slave' ~ 'nort2942',
                                Language %in% 'Nubian (Dongolese)' ~ 'nubi1251',
                                Language %in% 'Mixtec (Chalcatongo)' ~ 'sanm1295',
                                Language %in% 'Malagasy' ~ 'mala1537',
                                Language %in% 'Kpelle' ~ 'kpel1252',
                                Language %in% 'Kongo' ~ 'kong1295',
                                Language %in% 'Kalmyk' ~ 'kalm1243',
                                Language %in% 'Kurmanji' ~ 'khor1267',
                                Language %in% 'Kiwai (Southern)' ~ 'nort2930',
                                Language %in% 'Kisi' ~ 'sout2778',
                                Language %in% 'Kewa' ~ 'west2599',
                                Language %in% 'Indonesian' ~ 'indo1316',
                                Language %in% 'Haida' ~ 'sout2956',
                                Language %in% 'Grebo' ~ 'greb1256',
                                Language %in% 'Cakchiquel' ~ 'cakc1235',
                                Language %in% 'Buriat' ~ 'buri1258',
                                Language %in% 'Asmat' ~ 'casu1237',
                                TRUE ~ Glottocode)) %>%
  # add areas:
  rename(WALS_Language = Language) %>% # avoid conflict with Autotyp
  left_join(Register, by = c('Glottocode', 'Longitude', 'Latitude')) %>%
  distinct(Glottocode, .keep_all = T) %>% # matching too many (ok after checking group_by(optatives.df, Glottocode) %>% summarize(x=n_distinct(Value)>1))
  mutate(AncientArea = ifelse(is.na(Continent) | is.na(Area), NA,
                              ifelse(Continent %in% c('Australia',
                                                      'NG and Oceania',
                                                      'W N America',
                                                      'S America',
                                                      'C America',
                                                      'E N America') |
                                       Area %in% 'N Coast Asia',
                                     "Inside Circum-Pacific", "Outside Circum-Pacific")
  ),
  Optative = ifelse(is.na(Value), NA,
                    Value %in% 'Inflectional optative present')
  )


optatives.cp <- select(optatives.df.cp, Language, Glottocode, Macrocontinent, MajorBranch, LocalRegion, Latitude, Longitude, Optative)

optatives.cp$LocalRegion[optatives.cp$Glottocode %in% iso.glot] <- "isolate" #assign isolate to isolates
optatives.cp$LocalRegion[optatives.cp$LocalRegion == "Caucasus"] <- "enclave" #assign Caucasus to enclave
optatives.cp$LocalRegion[optatives.cp$LocalRegion == "Himalaya"] <- "enclave" #assign Himalaya to enclave
optatives.cp$LocalRegion[optatives.cp$LocalRegion %in% NA] <- "modern" # Use that new level to change NA to this level #with this method 'other' will be added as the last item (also in plots)
colnames(optatives.cp)[which(colnames(optatives.cp) == 'Macrocontinent')] <- 'area'
colnames(optatives.cp)[which(colnames(optatives.cp) == 'MajorBranch')] <- 'family'
colnames(optatives.cp)[which(colnames(optatives.cp) == 'LocalRegion')] <- 'AncientArea'
optatives.cp<- optatives.cp %>% drop_na(Longitude)


### Ergative ####
#################

ergatives.df.cp <- Alignment %>%
  filter(CombinedPredicateClassID %in% c("1.2") &
           SelectorType %in% c('case marker or adposition',
                               'agreement (marker)')) %>%
  group_by(Glottocode) %>%
  summarize(Ergative = any(Alignment2  %in%  c('S=P≠Atr',
                                               'S≠Atr=P',
                                               'S≠Atr≠P')),
  ) %>%
  inner_join(Register) %>%
  mutate(AncientArea = ifelse(Continent %in% c('Australia',
                                               'NG and Oceania',
                                               'W N America',
                                               'S America',
                                               'C America',
                                               'E N America') |
                                Area %in% 'N Coast Asia',
                              "Inside Circum-Pacific", "Outside Circum-Pacific")
  )

ergative.cp <- select(ergatives.df.cp, Language, Glottocode, Macrocontinent, MajorBranch, LocalRegion, Latitude, Longitude, Ergative)


ergative.cp$LocalRegion[ergative.cp$Glottocode %in% iso.glot] <- "isolate" #assign isolate to isolates
ergative.cp$LocalRegion[ergative.cp$LocalRegion == "Caucasus"] <- "enclave" #assign Caucasus to enclave
ergative.cp$LocalRegion[ergative.cp$LocalRegion == "Himalaya"] <- "enclave" #assign Himalaya to enclave
ergative.cp$LocalRegion[ergative.cp$LocalRegion %in% NA] <- "modern" # Use that new level to change NA to this level #with this method 'other' will be added as the last item (also in plots)
colnames(ergative.cp)[which(colnames(ergative.cp) == 'Macrocontinent')] <- 'area'
colnames(ergative.cp)[which(colnames(ergative.cp) == 'MajorBranch')] <- 'family'
colnames(ergative.cp)[which(colnames(ergative.cp) == 'LocalRegion')] <- 'AncientArea'
ergative.cp<- ergative.cp %>% drop_na(Longitude)


### Genderless Pronouns ###
###########################

pronouns.df.cp <- filter(wals.df, Parameter_ID %in% '44A') %>%
  # match different glottocode choices between WALS and Autotyp, identified by
  # pre-mutated
  # subset(pronouns.df, !Glottocode %in% Register$Glottocode, c('Language', 'Glottocode'))
  mutate(Glottocode = case_when(Language %in% 'Albanian' ~ 'tosk1239',
                                Language %in% 'Azerbaijani' ~ 'azer1255',
                                Language %in% 'Gwari' ~ 'gbar1246',
                                Language %in% 'Jakaltek' ~ 'popt1235',
                                Language %in% 'Tukang Besi' ~ 'tuka1249',
                                Language %in% 'Tubu' ~ 'teda1241',
                                Language %in% 'Slave' ~ 'nort2942',
                                Language %in% 'Nubian (Dongolese)' ~ 'nubi1251',
                                Language %in% 'Mixtec (Chalcatongo)' ~ 'sanm1295',
                                Language %in% 'Malagasy' ~ 'mala1537',
                                Language %in% 'Kpelle' ~ 'kpel1252',
                                Language %in% 'Kongo' ~ 'kong1295',
                                Language %in% 'Kalmyk' ~ 'kalm1243',
                                Language %in% 'Kurmanji' ~ 'khor1267',
                                Language %in% 'Kiwai (Southern)' ~ 'nort2930',
                                Language %in% 'Kisi' ~ 'sout2778',
                                Language %in% 'Kewa' ~ 'west2599',
                                Language %in% 'Indonesian' ~ 'indo1316',
                                Language %in% 'Haida' ~ 'sout2956',
                                Language %in% 'Grebo' ~ 'greb1256',
                                Language %in% 'Cakchiquel' ~ 'cakc1235',
                                Language %in% 'Buriat' ~ 'buri1258',
                                Language %in% 'Asmat' ~ 'casu1237',
                                Language %in% 'Canela' ~ 'cane1242',
                                Language %in% 'Uradhi' ~ 'urad1238',
                                Language %in% 'Komo' ~ 'komo1260',
                                Language %in% 'Hua' ~ 'yaga1260',
                                Language %in% 'Dong (Southern)' ~ 'nort2735',
                                Language %in% 'Cora' ~ 'cora1260',
                                TRUE ~ Glottocode)) %>%
  # add areas:
  rename(WALS_Language = Language) %>% # avoid conflict with Autotyp
  left_join(Register, by = c('Glottocode', 'Longitude', 'Latitude')) %>%
  distinct(Glottocode, .keep_all = T) %>% # matching too many (ok after checking group_by(pronouns.df, Glottocode) %>% summarize(x=n_distinct(Value)>1))
  mutate(AncientArea = ifelse(is.na(Continent) | is.na(Area), NA,
                              ifelse(Continent %in% c('Australia',
                                                      'NG and Oceania',
                                                      'W N America',
                                                      'S America',
                                                      'C America',
                                                      'E N America') |
                                       Area %in% 'N Coast Asia',
                                     "Inside Circum-Pacific", "Outside Circum-Pacific")
  ),
  Genderless = ifelse(is.na(Value), NA,
                      Value %in% 'No gender distinctions')
  )

pronouns.cp <- select(pronouns.df.cp, Language, Glottocode, Macrocontinent, MajorBranch, LocalRegion, Latitude, Longitude, Genderless)

pronouns.cp$LocalRegion[pronouns.cp$Glottocode %in% iso.glot] <- "isolate" #assign isolate to isolates
pronouns.cp$LocalRegion[pronouns.cp$LocalRegion == "Caucasus"] <- "enclave" #assign Caucasus to enclave
pronouns.cp$LocalRegion[pronouns.cp$LocalRegion == "Himalaya"] <- "enclave" #assign Himalaya to enclave
pronouns.cp$LocalRegion[pronouns.cp$LocalRegion %in% NA] <- "modern" # Use that new level to change NA to this level #with this method 'other' will be added as the last item (also in plots)
colnames(pronouns.cp)[which(colnames(pronouns.cp) == 'Macrocontinent')] <- 'area'
colnames(pronouns.cp)[which(colnames(pronouns.cp) == 'MajorBranch')] <- 'family'
colnames(pronouns.cp)[which(colnames(pronouns.cp) == 'LocalRegion')] <- 'AncientArea'
pronouns.cp<- pronouns.cp %>% drop_na(Longitude)


#### Synthesis ###
##################

synthesis.df.cp <- MaximallyInflectedVerbSynthesis %>%
  filter(!is.na(VerbInflectionMaxCategoryCount)) %>%
  mutate(Synthesis_Binary = cut(VerbInflectionMaxCategoryCount,
                                quantile(VerbInflectionMaxCategoryCount,
                                         probs = c(0, 1/2, 1)),
                                right = F, labels = c('low', 'high')),
         Synthesis_Ternary = cut(VerbInflectionMaxCategoryCount,
                                 quantile(VerbInflectionMaxCategoryCount,
                                          probs = c(0, 1/3, 2/3, 1)),
                                 right = F, labels = c('low', 'medium', 'high'))
  ) %>%
  mutate(Synthesis_Binary = ifelse(
    VerbInflectionMaxCategoryCount == max(VerbInflectionMaxCategoryCount), 'high',
    paste(Synthesis_Binary)),
    Synthesis_Ternary = ifelse(
      VerbInflectionMaxCategoryCount == max(VerbInflectionMaxCategoryCount), 'high',           paste(Synthesis_Ternary))
  ) %>%
  mutate(Synthesis_Binary = Synthesis_Binary %in% 'high') %>%
  inner_join(Register) %>%
  mutate(AncientArea = ifelse(Continent %in% c('Australia',
                                               'NG and Oceania',
                                               'W N America',
                                               'S America',
                                               'C America',
                                               'E N America') |
                                Area %in% 'N Coast Asia',
                              "Inside Circum-Pacific", "Outside Circum-Pacific")
  ) %>% select(!where(is.list))


synthesis.cp <- select(synthesis.df.cp, Language, Glottocode, Macrocontinent, MajorBranch, LocalRegion, Latitude, Longitude, VerbInflectionMaxCategoryCount)
colnames(synthesis.cp)[which(colnames(synthesis.cp) == 'VerbInflectionMaxCategoryCount')] <- 'Synthesis'

synthesis.cp$LocalRegion[synthesis.cp$Glottocode %in% iso.glot] <- "isolate" #assign isolate to isolates
synthesis.cp$LocalRegion[synthesis.cp$LocalRegion == "Caucasus"] <- "enclave" #assign Caucasus to enclave
synthesis.cp$LocalRegion[synthesis.cp$LocalRegion == "Himalaya"] <- "enclave" #assign Himalaya to enclave
synthesis.cp$LocalRegion[synthesis.cp$LocalRegion %in% NA] <- "modern" # Use that new level to change NA to this level #with this method 'other' will be added as the last item (also in plots)
colnames(synthesis.cp)[which(colnames(synthesis.cp) == 'Macrocontinent')] <- 'area'
colnames(synthesis.cp)[which(colnames(synthesis.cp) == 'MajorBranch')] <- 'family'
colnames(synthesis.cp)[which(colnames(synthesis.cp) == 'LocalRegion')] <- 'AncientArea'
synthesis.cp<- synthesis.cp %>% drop_na(Longitude)






##########################################################################################################################################
#################################################### PLOT ################################################################################
##########################################################################################################################################


#Optative
##########################################################################################################################################
gg.opt.d <- project_data(df = optatives.cp)

opt.d<- gg.opt.d$base_plot +
  geom_sf(data = gg.opt.d$data, aes(color = Optative, shape= AncientArea),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(17, 15, 16), name= NULL) +
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
gg.erg.d <- project_data(df = ergative.cp)

erg.d<- gg.erg.d$base_plot +
  geom_sf(data = gg.erg.d$data, aes(color = Ergative, shape= AncientArea),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(17, 15, 16), name= NULL) +
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
gg.syn.d <- project_data(df = synthesis.cp)

syn.d <- gg.syn.d$base_plot +
  geom_sf(data = gg.syn.d$data, aes(color = Synthesis, shape= AncientArea),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(17, 15, 16), name= NULL) +
  scale_colour_gradient2(breaks = c(0,6,16), labels = c("0", "median", "16"),
                         midpoint = median(syn.g$data$value),
                         low = 'blue', mid = 'lightgrey', high = 'red', name = NULL) + 
  theme(panel.grid = element_blank(), legend.direction = "horizontal", legend.position = "bottom", legend.text = element_text(size = 8), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5)
  ) +
  labs(title = "Synthesis")+
  guides(color= guide_colorbar(barwidth = 4, barheight = 0.2, override.aes = list(size = 4)),
         shape = "none")


##########################################################################################################################################

##########################################################################################################################################
#Gender
##########################################################################################################################################
gg.gen.d <- project_data(df = pronouns.cp)

gen.d<- gg.gen.d$base_plot +
  geom_sf(data = gg.gen.d$data, aes(color = Genderless, shape= AncientArea),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(17, 15, 16), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL, labels=c("absent", "present")
  ) +
  theme(panel.grid = element_blank(), legend.direction = "horizontal", legend.position = "bottom", legend.text = element_text(size = 8), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5)
  ) +
  labs(title = "Genderless Pronouns")+
  guides(color= guide_legend(keywidth = 0.2, keyheight = 0.5, override.aes = list(size = 4)),
         shape = "none")



##########################################################################################################################################

#pdf("mapcp.pdf", height = 6, width = 8)  
#grid.arrange(opt.d, erg.d, syn.d, gen.d)
#dev.off()

mapcp<- grid.arrange(opt.d, erg.d, syn.d, gen.d)
ggsave("mapcp.pdf", mapcp, width = 15, height = 10, units = c("cm"), dpi = 600)


