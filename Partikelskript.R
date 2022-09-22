#Script for particle project
# Creating dataframes -----------------------------------------------------
install.packages("tidyverse")
library(utils)
library(ggplot2)

DF_3particles <- read.csv("DataFrame_3particles.csv")
SV_3particles <- subset(DF_3particles, text_srclang %in% "SV")
CS_3particles <- subset(DF_3particles, text_srclang %in% "CS")

#Tidying the data to avoid problems
DF_3particles$text_srclang <- DF_3particles$text.srclang
SV_3particles$text_srclang <-SV_3particles$text.srclang
CS_3particles$text_srclang <- CS_3particles$text.srclang

DF_3particles$text_group <- DF_3particles$text.group
SV_3particles$text_group <-SV_3particles$text.group
CS_3particles$text_group <- CS_3particles$text.group

DF_3particles$ipm <- DF_3particles$i.p.m.
SV_3particles$ipm <-SV_3particles$i.p.m.
CS_3particles$ipm <- CS_3particles$i.p.m.

DF_3particles$text.srclang <- NULL
DF_3particles$text.group <- NULL
DF_3particles$i.p.m. <- NULL

SV_3particles$text.srclang <- NULL
SV_3particles$text.group <- NULL
SV_3particles$i.p.m. <- NULL

CS_3particles$text.srclang <- NULL
CS_3particles$text.group <- NULL
CS_3particles$i.p.m. <- NULL

# Plotting particles ------------------------------------------------------
#Swedish 3 particles
ggplot(SV_3particles, aes(x = particle, y = ipm, color = text_group)) + geom_point()
ggplot(CS_3particles, aes(x = particle, y = ipm, color = text_group)) + geom_point()

# Preparing and plotting "asi" --------------------------------------------
asi <- read_delim("Asi_samlat.csv")
#Tidy the dataframe:
asi_short<-  asi %>% subset(!Translation_to_SV %in% c("CONSTRUCTION: Asi jako", "CONSTRUCTION: Asi tak"))


asi_short$Translation_to_SV [asi_short$Translation_category == "possibility verbs"] <- "tror/antar/undrar"
asi_short$Translation_category [asi_short$Translation_to_SV == "skulle"] <- "modal verbs"
asi_short$Translation_category [asi_short$Translation_to_SV == "förstås"] <- "approval"
asi_short$Translation_to_SV[asi_short$Translation_category == "modal verbs" & asi_short$Abs_fq_Core_CS == 24]<-"other modal verbs"
asi_short$Translation_category [asi_short$Translation_to_SV == "ERROR"] <- "error"
asi_short$Translation_category [asi_short$Translation_to_SV == "OTHER"] <- "other"
asi_short$Translation_category [asi_short$Translation_to_SV == "ZERO"] <- "zero"
asi_short$Translation_to_SV <- tolower(asi_short$Translation_to_SV)

asi_short_Core_CS_top_ten <- asi_short %>% slice_max(ipm_Core_CS, n=10) %>% select(Translation_to_SV, Translation_category, Abs_fq_Core_CS, ipm_Core_CS) %>% group_by(Translation_to_SV)

asi_short_Core_SV_top_ten <- asi_short %>% slice_max(ipm_Core_SV, n=10) %>% select(Translation_to_SV, Translation_category, Abs_fq_Core_SV, ipm_Core_SV) %>% group_by(Translation_to_SV)

asi_short_Europarl_CS_top_ten <- asi_short %>% slice_max(ipm_Europarl_CS, n=10) %>% select(Translation_to_SV, Translation_category, Abs_fq_Europarl_CS, ipm_Europarl_CS) %>% group_by(Translation_to_SV)

asi_short_Europarl_SV_top_ten <- asi_short %>% slice_max(ipm_Europarl_SV, n=10) %>% select(Translation_to_SV, Translation_category, Abs_fq_Europarl_SV, ipm_Europarl_SV) %>% group_by(Translation_to_SV)

asi_short_Acquis_top_ten <- asi_short %>% slice_max(ipm_Acquis, n=10) %>% select(Translation_to_SV, Translation_category, Acquis, ipm_Acquis) %>% group_by(Translation_to_SV)

#Joining registers no matter the Source Language
asi_short_Core <- full_join(asi_short_Core_CS_top_ten, asi_short_Core_SV_top_ten)
asi_short_Europarl <-  full_join(asi_short_Europarl_CS_top_ten, asi_short_Europarl_SV_top_ten)

#Plotting each register and source language:
#Check how many colours are needed:
asi_short %>% count(Translation_category, sort = T)
#Check suitable palette colours:
brewer.pal(n=9, "OrRd") #n has to be the amount you need, taken from the previous count.
#Create colours
AsiCategoryColours <- c("#F0F0F0", "#9E0142", "#1A1A1A", "#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "#B30000","#7F0000")
names(AsiCategoryColours) = c("zero", "other", "error", "disapproval", "very low probability", "low probability", "medium probability", "high probability", "approval", "modal verbs", "possibility verbs", "with numbers")


#Fill = Translation to SV or Translation_category?

ggplot(asi_short_Core_CS_top_ten, aes(x="", y=ipm_Core_CS, fill=Translation_category))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  ggtitle("Translations into Swedish of 'asi' in Core (fiction), source language Czech.")  +
  theme_void() +
  theme(legend.position="right") +
  geom_text(aes(x = 1.6, label = Translation_to_SV), position = position_stack(vjust = .5)) +
  scale_fill_manual('Translation category', values = AsiCategoryColours)
#scale_fill_brewer('Translation category', palette = 'OrRd')


ggplot(asi_short_Europarl_CS_top_ten, aes(x="", y=ipm_Europarl_CS, fill=Translation_category))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  ggtitle("Translations into Swedish of 'asi' in Europarl (edited spoken), source language Czech.")  +
  theme_void() +
  theme(legend.position="right") +
  geom_text(aes(x = 1.6, label = Translation_to_SV), position = position_stack(vjust = .5)) +
  scale_fill_manual('Translation category', values = AsiCategoryColours)

#NB problems with the below plot. Some Translations_to_SV do not exist. Replace 0 with NA to create plot:
asi_short_Europarl_SV_top_ten [asi_short_Europarl_SV_top_ten == 0] <- NA
ggplot(asi_short_Europarl_SV_top_ten, aes(x="", y=ipm_Europarl_SV, fill=Translation_category, na.rm = TRUE))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  ggtitle("Translations from Swedish of 'asi' in Europarl (edited spoken), target language Czech.")  +
  theme_void() +
  theme(legend.position="right") +
  geom_text(aes(x = 1.6, label = Translation_to_SV), position = position_stack(vjust = .5)) +
  scale_fill_manual('Translation category', values = AsiCategoryColours)

asi_short_Acquis_top_ten [asi_short_Acquis_top_ten == 0] <- NA
ggplot(asi_short_Acquis_top_ten, aes(x="", y=ipm_Acquis, fill=Translation_category))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  ggtitle("Translations in Swedish when CS has 'asi' in Acquis (legal)")  +
  theme_void() +
  theme(legend.position="right") +
  geom_text(aes(x = 1.6, label = Translation_to_SV), position = position_stack(vjust = .5)) +
  scale_fill_manual('Translation category', values = AsiCategoryColours)


# Asi even shorter --------------------------------------------------------
asi_shorter <-  asi %>% subset(!Translation_to_SV %in% c("CONSTRUCTION: Asi jako", "CONSTRUCTION: Asi tak", "ERROR", "OTHER", "ZERO"))
asi_shorter$Translation_to_SV [asi_shorter$Translation_category == "possibility verbs"] <- "tror/antar/undrar"
asi_shorter$Translation_category [asi_shorter$Translation_to_SV == "skulle"] <- "modal verbs"
asi_shorter$Translation_category [asi_shorter$Translation_to_SV == "förstås"] <- "approval"
asi_shorter$Translation_to_SV[asi_shorter$Translation_category == "modal verbs" & asi_shorter$Abs_fq_Core_CS == 24]<-"other modal verbs"

asi_Core_CS_top_ten <- asi_shorter %>% slice_max(ipm_Core_CS, n=10) %>% select(Translation_to_SV, Translation_category, Abs_fq_Core_CS, ipm_Core_CS) %>% group_by(Translation_to_SV)

asi_Core_SV_top_ten <- asi_shorter %>% slice_max(ipm_Core_SV, n=10) %>% select(Translation_to_SV, Translation_category, Abs_fq_Core_SV, ipm_Core_SV) %>% group_by(Translation_to_SV)

asi_Europarl_CS_top_ten <- asi_shorter %>% slice_max(ipm_Europarl_CS, n=10) %>% select(Translation_to_SV, Translation_category, Abs_fq_Europarl_CS, ipm_Europarl_CS) %>% group_by(Translation_to_SV)

#NB: The below does not have ten different translations, only seven!
asi_Europarl_SV_top_ten <- asi_shorter %>% slice_max(ipm_Europarl_SV, n=7) %>% select(Translation_to_SV, Translation_category, Abs_fq_Europarl_SV, ipm_Europarl_SV) %>% group_by(Translation_to_SV)

#NB: The below does not have ten different translations, only nine!
asi_Acquis_top_ten <- asi_shorter %>% slice_max(ipm_Acquis, n=9) %>% select(Translation_to_SV, Translation_category, Acquis, ipm_Acquis) %>% group_by(Translation_to_SV)

#Joining registers no matter the Source Language
asi_Core <- full_join(asi_Core_CS_top_ten, asi_Core_SV_top_ten)
asi_Europarl <-  full_join(asi_Europarl_CS_top_ten, asi_Europarl_SV_top_ten)

#Plotting each register and source language:
#NOT DONE YET
#Check how many colours are needed:
asi_shorter %>% count(Translation_category, sort = T)
#Check suitable palette colours:
brewer.pal(n=9, "OrRd") #n has to be the amount you need, taken from the previous count.
#Create colours
AsiCategoryColours <- c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "#B30000","#7F0000")
names(AsiCategoryColours) = c("disapproval", "very low probability", "low probability", "medium probability", "high probability", "approval", "modal verbs", "possibility verbs", "with numbers")
AsiTranslationColours <-
names(AsiTranslationColours) = c("antagligen", "construction: asi jako", "construction: asi tak", "då", "egentligen ", "error", "förmodligen", "förstås", "ju", "modal verbs", "kanhända", "kanske", "knappast", "månntro", "måhända", "möjligen", "naturligtvis", "nog", "numbers ≈ ungefär", "other", "sannolikt", "skulle", "snarare", "säkert", "possibility verbs", "troligen", "troligtvis", "tydligen", "typ", "tämligen säkert", "uppenbarligen", "utan tvivel", "mycket sannolikt", "visst", "väl", "zero")

#Fill = Translation to SV or Translation_category?

ggplot(asi_Core_CS_top_ten, aes(x="", y=ipm_Core_CS, fill=Translation_category))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  ggtitle("Translations into Swedish of 'asi' in Core (fiction), source language Czech.")  +
  theme_void() +
  theme(legend.position="right") +
  geom_text(aes(x = 1.6, label = Translation_to_SV), position = position_stack(vjust = .5)) +
  scale_fill_manual('Translation category', values = AsiCategoryColours)
  #scale_fill_brewer('Translation category', palette = 'OrRd')

ggplot(asi_Core_SV_top_ten, aes(x="", y=ipm_Core_SV, fill=Translation_category))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  ggtitle("Translations from Swedish of 'asi' in Core (fiction), target language Czech.")  +
  theme_void() +
  theme(legend.position="right") +
  geom_text(aes(x = 1.6, label = Translation_to_SV), angle = 300, position = position_stack(vjust = .5)) +
  scale_fill_manual('Translation category', values = AsiCategoryColours)


ggplot(asi_Europarl_CS_top_ten, aes(x="", y=ipm_Europarl_CS, fill=Translation_category))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  ggtitle("Translations into Swedish of 'asi' in Europarl (edited spoken), source language Czech.")  +
  theme_void() +
  theme(legend.position="right") +
  geom_text(aes(x = 1.6, label = Translation_to_SV), position = position_stack(vjust = .5)) +
  scale_fill_manual('Translation category', values = AsiCategoryColours)

#NB problems with the below plot. Some Translations_to_SV do not exist. Replace 0 with NA to create plot:
asi_Europarl_SV_top_ten [asi_Europarl_SV_top_ten == 0] <- NA
ggplot(asi_Europarl_SV_top_ten, aes(x="", y=ipm_Europarl_SV, fill=Translation_category, na.rm = TRUE))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  ggtitle("Translations from Swedish of 'asi' in Europarl (edited spoken), target language Czech.")  +
  theme_void() +
  theme(legend.position="right") +
  geom_text(aes(x = 1.6, label = Translation_to_SV), position = position_stack(vjust = .5)) +
  scale_fill_manual('Translation category', values = AsiCategoryColours)

asi_Acquis_top_ten [asi_Acquis_top_ten == 0] <- NA
ggplot(asi_Acquis_top_ten, aes(x="", y=ipm_Acquis, fill=Translation_category))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  ggtitle("Translations in Swedish when CS has 'asi' in Acquis (legal)")  +
  theme_void() +
  theme(legend.position="right") +
  geom_text(aes(x = 1.6, label = Translation_to_SV), position = position_stack(vjust = .5)) +
  scale_fill_manual('Translation category', values = AsiCategoryColours)
#Or rather
Acquis_no_numbers <- read_delim("Acquis_no_numbers.csv")
ggplot(Acquis_no_numbers, aes(x="", y=Acquis, fill=Translation_category))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  ggtitle("Translations in Swedish when CS has 'asi' in Acquis (legal), but without combination with numbers")  +
  theme_void() +
  theme(legend.position="right") +
  geom_text(aes(x = 1.6, label = Translation_to_SV), position = position_stack(vjust = .5)) +
  scale_fill_manual('Translation category', values = AsiCategoryColours)


# Právě -------------------------------------------------------------------
prave <- read_delim("Prave_samlad.csv")
prave <-  prave %>% full_join(prave_cat, by = c("Translation_to_SV" = "Translation_SV") )

prave$perc_Core_CS <- prave$Abs_fq_Core_CS/sum(prave$Abs_fq_Core_CS)
#Below, 823 is taken from the i.p.m. mentioned in KonText according to the search link.
prave$ipm_Europarl_CS <- 823.4/sum(prave$Abs_fq_Europarl_CS)*prave$Abs_fq_Europarl_CS
prave$perc_Core_SV <- prave$Abs_fq_Core_SV/sum(prave$Abs_fq_Core_SV)
prave$ipm_Europarl_SV <- 563.23/sum(prave$Abs_fq_Europarl_SV)*prave$Abs_fq_Europarl_SV
prave$perc_Acquis <- prave$Acquis/sum(prave$Acquis)

#Removing zeros and others:
prave_shorter <-  prave %>% subset(!Translation_to_SV %in% c("zero", "other"))


prave_Core_CS_top_ten <- prave %>% slice_max(perc_Core_CS, n=10) %>% select(Translation_to_SV, Translation_category, Abs_fq_Core_CS, perc_Core_CS) %>% group_by(Translation_to_SV)

prave_Core_SV_top_ten <- prave %>% slice_max(perc_Core_SV, n=10) %>% select(Translation_to_SV, Translation_category, Abs_fq_Core_SV, perc_Core_SV) %>% group_by(Translation_to_SV)

prave_Europarl_CS_top_ten <- prave %>% slice_max(ipm_Europarl_CS, n=10) %>% select(Translation_to_SV, Translation_category, Abs_fq_Europarl_CS, ipm_Europarl_CS) %>% group_by(Translation_to_SV)

prave_Europarl_SV_top_ten <- prave %>% slice_max(ipm_Europarl_SV, n=10) %>% select(Translation_to_SV, Translation_category, Abs_fq_Europarl_SV, ipm_Europarl_SV) %>% group_by(Translation_to_SV)

prave_Acquis_top_ten <- prave %>% slice_max(perc_Acquis, n=10) %>% select(Translation_to_SV, Translation_category, Acquis, perc_Acquis) %>% group_by(Translation_to_SV)

#Joining registers no matter the Source Language
prave_Core <- full_join(prave_Core_CS_top_ten, prave_Core_SV_top_ten)
prave_Europarl <-  full_join(prave_Europarl_CS_top_ten, prave_Europarl_SV_top_ten)

prave %>% count(Translation_category, sort = T)
#Result: 13.
#Check if a palette is suitable:
display.brewer.pal(n=11, "Spectral")
brewer.pal(n=11, "Spectral") #n has to be the amount you need, taken from the previous count. If you have more in the count, see of you can add another colour for those above.
#Create colours
praveCategoryColours <- c("#F0F0F0", "#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2", "#1A1A1A")
names(praveCategoryColours) = c("zero", "other", "focus part pos", "present tense adv", "recent past adv", "exactly", "determinative (species)", "species", "just (därför)", "just (tense)", "(tense prog+)", "focus part neg", "error")
prave_shorterCategoryColours <- c("#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#1A1A1A")
names(prave_shorterCategoryColours) = c("focus part pos", "recent past adv", "present tense adv", "determinative", "opposition", "species", "exactly", "tense", "focus part neg", "error")


ggplot(prave_Core_CS_top_ten, aes(x="", y=perc_Core_CS, fill=Translation_category))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  ggtitle("Translations into Swedish of 'právě' in Core (fiction), source language Czech.")  +
  theme_void() +
  theme(legend.position="right") +
  geom_text(aes(x = 1.6, label = Translation_to_SV), position = position_stack(vjust = .5)) +
  scale_fill_manual('Translation category', values = praveCategoryColours)
ggsave("praveCoreCS.png", width = 8, height = 6.4, dpi = 600)

ggplot(prave_Europarl_CS_top_ten, aes(x="", y=ipm_Europarl_CS, fill=Translation_category))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  ggtitle("Translations into Swedish of 'právě' in Europarl (edited spoken), source language Czech.")  +
  theme_void() +
  theme(legend.position="right") +
  geom_text(aes(x = 1.6, label = Translation_to_SV), position = position_stack(vjust = .5)) +
  scale_fill_manual('Translation category', values = praveCategoryColours)
ggsave("praveEuroparlCS.png", width = 8, height = 6.4, dpi = 600)

ggplot(prave_Acquis_top_ten, aes(x="", y=perc_Acquis, fill=Translation_category))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  ggtitle("Translations into Swedish when Czech has 'právě' in Acquis (legal)")  +
  theme_void() +
  theme(legend.position="right") +
  geom_text(aes(x = 1.6, label = Translation_to_SV), position = position_stack(vjust = .5)) +
  scale_fill_manual('Translation category', values = praveCategoryColours)
ggsave("praveAcquis.png", width = 8, height = 6.4, dpi = 600)


# Třeba -------------------------------------------------------------------
#This file is tidied beforehand. Here is a sample of approx. 300 entries per register and SL.
treba <- read_delim("Treba.csv")

treba_Core_CS_top_ten <- treba %>% slice_max(perc_Core_CS, n=10) %>% select(Translation_SV, Translation_category, Abs_fq_Core_CS, perc_Core_CS) %>% group_by(Translation_SV)

treba_Core_SV_top_ten <- treba %>% slice_max(perc_Core_SV, n=10) %>% select(Translation_SV, Translation_category, Abs_fq_Core_SV, perc_Core_SV) %>% group_by(Translation_SV)

treba_Europarl_CS_top_ten <- treba %>% slice_max(perc_Europarl_CS, n=10) %>% select(Translation_SV, Translation_category, Abs_fq_Europarl_CS, perc_Europarl_CS) %>% group_by(Translation_SV)

treba_Europarl_SV_top_ten <- treba %>% slice_max(perc_Europarl_SV, n=10) %>% select(Translation_SV, Translation_category, Abs_fq_Europarl_SV, perc_Europarl_SV) %>% group_by(Translation_SV)

treba_Acquis_top_ten <- treba %>% slice_max(perc_Acquis, n=10) %>% select(Translation_SV, Translation_category, Acquis, perc_Acquis) %>% group_by(Translation_SV)

#Joining registers no matter the Source Language
treba_Core <- full_join(treba_Core_CS_top_ten, treba_Core_SV_top_ten)
treba_Europarl <-  full_join(treba_Europarl_CS_top_ten, treba_Europarl_SV_top_ten)

treba %>% count(Translation_category, sort = T)
#Check suitable palette colours:
brewer.pal(n=11, "RdBu") #n has to be the amount you need, taken from the previous count. If you have more in the count, see of you can add another colour for those above.
#Create colours
TrebaCategoryColours <- c("#F0F0F0", "#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061", "#1A1A1A")
names(TrebaCategoryColours) = c("zero", "(numbers)", "high possibility", "medium possibility", "low possibility", "modality", "necessity", "indifference", "eventuality", "degree", "example", "other", "error")

ggplot(treba_Core_CS_top_ten, aes(x="", y=perc_Core_CS, fill=Translation_category))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  ggtitle("Translations into Swedish of 'třeba' in Core (fiction), source language Czech.")  +
  theme_void() +
  theme(legend.position="right") +
  geom_text(aes(x = 1.6, label = Translation_SV), position = position_stack(vjust = .5)) +
  scale_fill_manual('Translation category', values = TrebaCategoryColours)
#scale_fill_brewer('Translation category', palette = 'RdBu')

ggplot(treba_Europarl_CS_top_ten, aes(x="", y=perc_Europarl_CS, fill=Translation_category))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  ggtitle("Translations into Swedish of 'třeba' in Europarl (edited spoken), source language Czech.")  +
  theme_void() +
  theme(legend.position="right") +
  geom_text(aes(x = 1.6, label = Translation_SV), position = position_stack(vjust = .5)) +
  scale_fill_manual('Translation category', values = TrebaCategoryColours)

ggplot(treba_Acquis_top_ten, aes(x="", y=perc_Acquis, fill=Translation_category))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  ggtitle("Translations into Swedish when Czech has 'třeba' in Acquis (legal)")  +
  theme_void() +
  theme(legend.position="right") +
  geom_text(aes(x = 1.6, label = Translation_SV), position = position_stack(vjust = .5)) +
  scale_fill_manual('Translation category', values = TrebaCategoryColours)
ggsave("TrebaAcquis.png", width = 8, height = 6.4, dpi = 600)


# Notes to continue -------------------------------------------------------
#Omformulera "numbers"?

