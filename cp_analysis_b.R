# Testing family bias inside vs outside the Circum-Pacific using the method described in Bickel 2011 and Bugaeva et al 2021 (both in the journal Linguistic Typology)
#
# Written by Balthasar Bickel, September 2022

library(tidyverse)
library(devtools)
if(!require(familybias)) install_github('IVS-UZH/familybias')
library(familybias)
library(parallel)
library(ggdist)

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


### Helper function to summarize findings, assuming TRUE/FALSE data ####
#########################################################################

summarize_familybias <- function(fam) {
  df <- do.call(rbind, mclapply(fam$extrapolations, function(e) {
    as.data.frame(xtabs( ~ majority.response + AncientArea,
                         subset = distribution  %in% 'biased', data = e,
                         drop.unused.levels = T))}))
  tabs <- group_by(df, AncientArea, majority.response) %>%
    summarize(medians = median(Freq),
              # 95% quantile interval
              lower = qi(Freq, .width = 0.95)[1],
              upper = qi(Freq, .width = 0.95)[2], .groups = 'drop'
    )
  In_T <- subset(tabs, AncientArea %in% 'Inside Circum-Pacific' &
                   majority.response %in% 'TRUE')
  In_F <- subset(tabs, AncientArea %in% 'Inside Circum-Pacific' &
                   majority.response %in% 'FALSE')
  Out_T <- subset(tabs, AncientArea %in% 'Outside Circum-Pacific' &
                    majority.response %in% 'TRUE')
  Out_F <- subset(tabs, AncientArea %in% 'Outside Circum-Pacific' &
                    majority.response %in% 'FALSE')
  OR_median <- (In_T$medians/In_F$medians)/(Out_T$medians/Out_F$medians)
  OR_upper <- (In_T$upper/In_F$upper)/(Out_T$upper/Out_F$upper)
  OR_lower <- (In_T$lower/In_F$lower)/(Out_T$lower/Out_F$lower)
  return(
    list(OR = list(median = OR_median,
                   from_lower = OR_lower,
                   from_upper = OR_upper),
         tabs = list(median = xtabs(medians~AncientArea+majority.response, tabs),
                     from_lower = xtabs(lower~AncientArea+majority.response, tabs),
                     from_upper = xtabs(upper~AncientArea+majority.response, tabs))
    ))
}


### Optative ###
################

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
  left_join(Register, by = 'Glottocode') %>%
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

opt.fam <- familybias(optatives.df,
                      family.names = c('Stock',
                                       'MajorBranch',
                                       'SubBranch',
                                       'SubSubBranch',
                                       'Language'),
                      r.name = 'Optative',
                      p.names = 'AncientArea',
                      lapplyfunc = mclapply, B = 4000)



summarize_familybias(opt.fam)


### Ergative ####
#################

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
)

erg.fam <- familybias(ergatives.df,
                       family.names = c('Stock',
                                        'MajorBranch',
                                        'SubBranch',
                                        'SubSubBranch',
                                        'Language'),
                       r.name = 'Ergative', p.names = 'AncientArea',
                       lapplyfunc = mclapply, B = 4000)

summarize_familybias(erg.fam)


#### Synthesis ###
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
  ) %>% select(!where(is.list))

syn_binary.fam <- familybias(synthesis.df,
                      family.names = c('Stock',
                                       'MajorBranch',
                                       'SubBranch',
                                       'SubSubBranch',
                                       'Language'),
                      r.name = 'Synthesis_Binary', p.names = 'AncientArea',
                      lapplyfunc = mclapply, B = 4000)

summarize_familybias(syn_binary.fam)

# Sensitivity check shows the same trend although family counts get obviously low:
syn_ternary.fam <- familybias(synthesis.df,
                             family.names = c('Stock',
                                              'MajorBranch',
                                              'SubBranch',
                                              'SubSubBranch',
                                              'Language'),
                             r.name = 'Synthesis_Ternary', p.names = 'AncientArea',
                             lapplyfunc = mclapply, B = 4000)

xtabs(Freq~., mean(syn_ternary.fam)) # see ?familybias documentation

### Genderless Pronouns ###
###########################

# subset(pronouns.df, !Glottocode %in% Register$Glottocode, c('Language', 'Glottocode'))

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
  left_join(Register, by = 'Glottocode') %>%
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

pro.fam <- familybias(pronouns.df,
                      family.names = c('Stock',
                                       'MajorBranch',
                                       'SubBranch',
                                       'SubSubBranch',
                                       'Language'),
                      r.name = 'Genderless', p.names = 'AncientArea',
                      lapplyfunc = mclapply, B = 4000)

summarize_familybias(pro.fam)

