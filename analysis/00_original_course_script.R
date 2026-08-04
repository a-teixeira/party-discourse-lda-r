library(ggplot2)
library(dplyr, warn.conflicts=FALSE)
library(reshape,warn.conflicts=FALSE)
library(lubridate,warn.conflicts=FALSE)
library(quanteda.textmodels)
library(quanteda) 
library(readtext)
library(quanteda.textstats)
library(quanteda.textplots)
library(udpipe)
x = c(5,6,9,erre)
x = 2
########################
#### Pre-Processing ####
########################

path_data_Linke= system.file("~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/Linke", package= "readtext")
Linke_gesamt= readtext(paste0(path_data_Linke,"~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/Linke"), encoding= "UTF-8")
Linke_gesamt[2:5]= NULL
Linke_gesamt["Time"]= NULL
Linke_gesamt[4:16]= NULL
Linke_gesamt[,4]=c("Linke")
colnames(Linke_gesamt)=c("doc_id","Date","Text","Partei")
Linke_gesamt[,2]= as.Date(Linke_gesamt[,2])
rownames(Linke_gesamt)= c(1:nrow(Linke_gesamt))
Linke_gesamt[1]= NULL

corpus_Linke= corpus(Linke_gesamt, text_field = "Text")

tokens_Linke= tokens(corpus_Linke, remove_punct = T, remove_numbers = T,remove_symbols = T, remove_url = T)
tokens_Linke= tokens_remove(tokens_Linke, pattern= "@*")
tokens_Linke= tokens_remove(tokens_Linke, pattern= stopwords("de", source ="stopwords-iso"))
tokens_Linke=tokens_remove(tokens_Linke, pattern = stopwords("en", source = "stopwords-iso"))
tokens_Linke=tokens_remove(tokens_Linke, pattern = "#*")
dictionary_words= dictionary(list(dict=c("rt", "https" , "htt" , "ht"
                                         ,"http", "danke" , "bitte" , "dr", 
                                         "amp" , "via", "brauchen", "braucht",
                                         "liebe", "schön", "leider", "gerne",
                                         "guten", "eigentlich", "sehen", "sagen",
                                         "wünsche", "Wünschen", "frage", "lesen",
                                         "t.c", "mio", "tolle", "herzlichen", 
                                         "glückwunsch","freue", "diskussion", 
                                         "besuch", "thema", "gestern", "tolle",
                                         "neu", "fragen", "einfach", "herr", "fall",
                                         "antowrt", "genau", "fodert", "abend", 
                                         "u.a", "einfach", "neues", "weiß", "nix",
                                         "läuft","leute", "nacht", "zug", "lassen"
                                         ,"finde", "geht's", "berliner", "neuer",
                                         "letzten", "paar", "fast", "super", "raus",
                                         "stimmt","eher", "klar", "sicher", "halt",
                                         "falsch", "halt", "echt", "leben", "fordert",
                                         "antwort", "reden", "hoffe", "schnell", 
                                         "scheint", "sogar", "schade", "hoffentlich",
                                         "irgendwie", "voll", "vorbei","gesehen","stunden",
                                         "minuten", "wichtig", "schönen", "nächste", 
                                         "schönes", "beste", "völlig", "gibt's", "grüne",
                                         "offenbar", "tut", "schönen", "day",
                                         "spiel", "tor", "schöne", "min", "glaube", 
                                         "schauen", "finden", "sehe", "bleiben", 
                                         "nochmal", "frau", "deutsche", "freuen",
                                         "spricht", "prof", "geburtstag", "rt", "https" , "htt" , "ht","http", "danke","bitte" , "dr","amp" , "via","dass","rt","sehen","https","http","mal","wer","fast",
                                         "haben","gehen","in","es","er","sie","ob","die","der","das","dann","doch","welche","welcher","welches","im",
                                         "ein","eine","des","zu","zum","zur","htt", "als","Ihr","Ihres","Ihren","Ihrer","Ihre", "und", "aber",
                                         "zwar","Wenn","Wann","den","sein","seine","seinen","weil","wo","Wo","bis","via","Bis","seit","Seit","nach","Nach","RT","Den","the","Ich","ich","um","UM","sich","Sich","noch","Noch",
                                         "Da","Da","So","so","wenn","wann", "ist","Ist","von","nicht","bei","auch", "für", "mit", "wie", "auf","man",
                                         "mir","danke","mich", "sind", "hat","nur","ja","nein","lesen","mittlerweile","bloß","umso","weihnachten", "kennen",
                                         "gehören","vorbei","treten","weihnachtlich", "vorweihnachtlich", "januar","februar","märz","april","mai","juni","juli","agost","september","oktober","november",
                                         "dezember","ansteigen","lassen","eigentlich","bekommen", "treffen", "angela", "sieht","bleibt", "treffen")))

tokens_Linke=tokens_select(tokens_Linke, pattern= dictionary_words, selection = "remove")

#AfD
path_data_AFD= system.file("~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/AfD", package= "readtext")
AFD_gesamt= readtext(paste0(path_data_AFD,"~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/AfD"), encoding= "UTF-8")
AFD_gesamt[2:5]= NULL
AFD_gesamt["Time"]= NULL
AFD_gesamt[4:16]= NULL
AFD_gesamt[,4]=c("AFD")
colnames(AFD_gesamt)=c("doc_id","Date","Text","Partei")
AFD_gesamt[,2]= as.Date(AFD_gesamt[,2])
rownames(AFD_gesamt)= c(1:nrow(AFD_gesamt))
AFD_gesamt[1]= NULL

corpus_AFD= corpus(AFD_gesamt, text_field = "Text")

tokens_AFD= tokens(corpus_AFD, remove_punct = T, remove_numbers = T,remove_symbols = T, remove_url = T)
tokens_AFD= tokens_remove(tokens_AFD, pattern= "@*")
tokens_AFD= tokens_remove(tokens_AFD, pattern= stopwords("de", source ="stopwords-iso"))
tokens_AFD=tokens_remove(tokens_AFD, pattern = stopwords("en", source = "stopwords-iso"))
tokens_AFD=tokens_remove(tokens_AFD, pattern = "#*")
tokens_AFD=tokens_select(tokens_AFD,pattern= dictionary_words, selection = "remove")

#CDU 
path_data_CDU= system.file("~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/CDU", package= "readtext")
CDU_gesamt= readtext(paste0(path_data_CDU,"~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/CDU"), encoding= "UTF-8")
CDU_gesamt[2:5]= NULL
CDU_gesamt["Time"]= NULL
CDU_gesamt[4:16]= NULL
CDU_gesamt[,4]=c("CDU")
colnames(CDU_gesamt)=c("doc_id","Date","Text","Partei")
CDU_gesamt[,2]= as.Date(CDU_gesamt[,2])
rownames(CDU_gesamt)= c(1:nrow(CDU_gesamt))
CDU_gesamt[1]= NULL

corpus_CDU= corpus(CDU_gesamt, text_field = "Text")

tokens_CDU= tokens(corpus_CDU, remove_punct = T, remove_numbers = T,remove_symbols = T, remove_url = T)
tokens_CDU= tokens_remove(tokens_CDU, pattern= "@*")
tokens_CDU= tokens_remove(tokens_CDU, pattern= stopwords("de", source ="stopwords-iso"))
tokens_CDU=tokens_remove(tokens_CDU, pattern = stopwords("en", source = "stopwords-iso"))
tokens_CDU=tokens_remove(tokens_CDU, pattern = "#*")
tokens_CDU=tokens_select(tokens_CDU, pattern= dictionary_words, selection = "remove")

#CSU einlesen
path_data_CSU= system.file("~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/CSU", package= "readtext")
CSU_gesamt= readtext(paste0(path_data_CSU,"~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/CSU"), encoding= "UTF-8")
CSU_gesamt[2:5]= NULL
CSU_gesamt["Time"]= NULL
CSU_gesamt[4:16]= NULL
CSU_gesamt[,4]=c("CSU")
colnames(CSU_gesamt)=c("doc_id","Date","Text","Partei")
CSU_gesamt[,2]= as.Date(CSU_gesamt[,2])
rownames(CSU_gesamt)= c(1:nrow(CSU_gesamt))
CSU_gesamt[1]= NULL

corpus_CSU= corpus(CSU_gesamt, text_field = "Text")

tokens_CSU= tokens(corpus_CSU, remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T)
tokens_CSU= tokens_remove(tokens_CSU, pattern= "@*")
tokens_CSU= tokens_remove(tokens_CSU, pattern= stopwords("de", source ="stopwords-iso"))
tokens_CSU=tokens_remove(tokens_CSU, pattern = stopwords("en", source = "stopwords-iso"))
tokens_CSU=tokens_remove(tokens_CSU, pattern = "#*")
tokens_CSU=tokens_select(tokens_CSU,pattern= dictionary_words, selection = "remove")

#Grüne einlesen
path_data_Grüne= system.file("~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/Grüne", package= "readtext")
Grüne_gesamt= readtext(paste0(path_data_Grüne,"~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/Grüne"), encoding= "UTF-8")
Grüne_gesamt[2:5]= NULL
Grüne_gesamt["Time"]= NULL
Grüne_gesamt[4:16]= NULL
Grüne_gesamt[,4]=c("Grüne")
colnames(Grüne_gesamt)=c("doc_id","Date","Text","Partei")
Grüne_gesamt[,2]= as.Date(Grüne_gesamt[,2])
rownames(Grüne_gesamt)= c(1:nrow(Grüne_gesamt))
Grüne_gesamt[1]= NULL

corpus_Grüne= corpus(Grüne_gesamt, text_field = "Text")

tokens_Grüne= tokens(corpus_Grüne, remove_punct = T, remove_numbers = T,remove_symbols = T, remove_url = T)
tokens_Grüne= tokens_remove(tokens_Grüne, pattern= "@*")
tokens_Grüne= tokens_remove(tokens_Grüne, pattern= stopwords("de", source ="stopwords-iso"))
tokens_Grüne=tokens_remove(tokens_Grüne, pattern = stopwords("en", source = "stopwords-iso"))
tokens_Grüne=tokens_remove(tokens_Grüne, pattern = "#*")
tokens_Grüne=tokens_select(tokens_Grüne,pattern= dictionary_words, selection = "remove")

#SPD einlesen 
path_data_SPD= system.file("~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/SPD", package= "readtext")
SPD_gesamt= readtext(paste0(path_data_SPD,"~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/SPD"), encoding= "UTF-8")
SPD_gesamt[2:5]= NULL
SPD_gesamt["Time"]= NULL
SPD_gesamt[4:16]= NULL
SPD_gesamt[,4]=c("SPD")
colnames(SPD_gesamt)=c("doc_id","Date","Text","Partei")
SPD_gesamt[,2]= as.Date(SPD_gesamt[,2])
rownames(SPD_gesamt)= c(1:nrow(SPD_gesamt))
SPD_gesamt[1]= NULL

corpus_SPD= corpus(SPD_gesamt, text_field = "Text")

tokens_SPD= tokens(corpus_SPD, remove_punct = T, remove_numbers = T,remove_symbols = T, remove_url = T)
tokens_SPD= tokens_remove(tokens_SPD, pattern= "@*")
tokens_SPD= tokens_remove(tokens_SPD, pattern= stopwords("de", source ="stopwords-iso"))
tokens_SPD=tokens_remove(tokens_SPD, pattern = stopwords("en", source = "stopwords-iso"))
tokens_SPD=tokens_remove(tokens_SPD, pattern = "#*")
tokens_SPD=tokens_select(tokens_SPD,pattern= dictionary_words, selection = "remove")
















#########
#######topic 1
###############

topic1<-c("twitter", "berlin", "eu", "german", "trump", "germany", "video", "live", "spaß", "minister")

#Topic 1 auf Linke
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
library(dplyr)
library(tidyr)
library(stringr)


tokens_Linke_topic1<-tokens_keep(tokens_Linke, pattern = topic1)
dfm_Linke=dfm(tokens_Linke_topic1)
dfm_Linke_topic1<-dfm_group(dfm_Linke, groups = Date)
Tabelle_Linke_topic1<-convert(dfm_Linke_topic1, to="data.frame")

#Topic Spalten zusammen
Tabelle_Linke_topic1<- Tabelle_Linke_topic1 %>% mutate(sumrow= twitter+berlin+eu+german+trump+germany+video+live+spaß+minister)
Tabelle_Linke_topic1$Linke<-Tabelle_Linke_topic1$sumrow
Tabelle_Linke_topic1[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_Linke_topic1)
Tabelle_Linke_topic1$doc_id<-ymd(Tabelle_Linke_topic1$doc_id)

Tabelle_Linke_topic1<-Tabelle_Linke_topic1 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(Linke = sum(Linke))

#topic 1 auf AFD
tokens_AFD_topic1<-tokens_keep(tokens_AFD, pattern = topic1)
dfm_AFD=dfm(tokens_AFD_topic1)
dfm_AFD_topic1<-dfm_group(dfm_AFD, groups = Date)
Tabelle_AFD_topic1<-convert(dfm_AFD_topic1, to="data.frame")

#Topic Spalten zusammen
Tabelle_AFD_topic1<- Tabelle_AFD_topic1 %>% mutate(sumrow= twitter+berlin+eu+german+trump+germany+video+live+spaß+minister)
Tabelle_AFD_topic1$AFD<-Tabelle_AFD_topic1$sumrow
Tabelle_AFD_topic1[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_AFD_topic1)
Tabelle_AFD_topic1$doc_id<-ymd(Tabelle_AFD_topic1$doc_id)

Tabelle_AFD_topic1<-Tabelle_AFD_topic1 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(AFD = sum(AFD))

#topic1 auf CDU
tokens_CDU_topic1<-tokens_keep(tokens_CDU, pattern = topic1)
dfm_CDU=dfm(tokens_CDU_topic1)
dfm_CDU_topic1<-dfm_group(dfm_CDU, groups = Date)
Tabelle_CDU_topic1<-convert(dfm_CDU_topic1, to="data.frame")

#Topic Spalten zusammen
Tabelle_CDU_topic1<- Tabelle_CDU_topic1 %>% mutate(sumrow= twitter+berlin+eu+german+trump+germany+video+live+spaß+minister)
Tabelle_CDU_topic1$CDU<-Tabelle_CDU_topic1$sumrow
Tabelle_CDU_topic1[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_CDU_topic1)
Tabelle_CDU_topic1$doc_id<-ymd(Tabelle_CDU_topic1$doc_id)

Tabelle_CDU_topic1<-Tabelle_CDU_topic1 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CDU = sum(CDU))

#topic1 auf CSU
tokens_CSU_topic1<-tokens_keep(tokens_CSU, pattern = topic1)
dfm_CSU=dfm(tokens_CSU_topic1)
dfm_CSU_topic1<-dfm_group(dfm_CSU, groups = Date)
Tabelle_CSU_topic1<-convert(dfm_CSU_topic1, to="data.frame")

#Topic Spalten zusammen
Tabelle_CSU_topic1<- Tabelle_CSU_topic1 %>% mutate(sumrow= twitter+berlin+eu+german+trump+germany+video+live+spaß+minister)
Tabelle_CSU_topic1$CSU<-Tabelle_CSU_topic1$sumrow
Tabelle_CSU_topic1[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_CSU_topic1)
Tabelle_CSU_topic1$doc_id<-ymd(Tabelle_CSU_topic1$doc_id)

Tabelle_CSU_topic1<-Tabelle_CSU_topic1 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CSU = sum(CSU))

#topic 1 auf Grüne
tokens_Grüne_topic1<-tokens_keep(tokens_Grüne, pattern = topic1)
dfm_Grüne=dfm(tokens_Grüne_topic1)
dfm_Grüne_topic1<-dfm_group(dfm_Grüne, groups = Date)
Tabelle_Grüne_topic1<-convert(dfm_Grüne_topic1, to="data.frame")

#Topic Spalten zusammen
Tabelle_Grüne_topic1<- Tabelle_Grüne_topic1 %>% mutate(sumrow= twitter+berlin+eu+german+trump+germany+video+live+spaß+minister)
Tabelle_Grüne_topic1$Grüne<-Tabelle_Grüne_topic1$sumrow
Tabelle_Grüne_topic1[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_Grüne_topic1)
Tabelle_Grüne_topic1$doc_id<-ymd(Tabelle_Grüne_topic1$doc_id)

Tabelle_Grüne_topic1<-Tabelle_Grüne_topic1 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(Grüne = sum(Grüne))

#topic1 auf spd 
tokens_SPD_topic1<-tokens_keep(tokens_SPD, pattern = topic1)
dfm_SPD=dfm(tokens_SPD_topic1)
dfm_SPD_topic1<-dfm_group(dfm_SPD, groups = Date)
Tabelle_SPD_topic1<-convert(dfm_SPD_topic1, to="data.frame")

#Topic Spalten zusammen
Tabelle_SPD_topic1<- Tabelle_SPD_topic1 %>% mutate(sumrow= twitter+berlin+eu+german+trump+germany+video+live+spaß+minister)
Tabelle_SPD_topic1$SPD<-Tabelle_SPD_topic1$sumrow
Tabelle_SPD_topic1[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_SPD_topic1)
Tabelle_SPD_topic1$doc_id<-ymd(Tabelle_SPD_topic1$doc_id)

Tabelle_SPD_topic1<-Tabelle_SPD_topic1 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(SPD = sum(SPD))
#topic 1 in eine Tabelle alle Parteien 
alle_Parteien_topic1<-merge(Tabelle_AFD_topic1, 
                            Tabelle_Linke_topic1,
                            by="month")
alle_Parteien_topic1<-merge(alle_Parteien_topic1, Tabelle_CDU_topic1,
                            by="month")
alle_Parteien_topic1<-merge(alle_Parteien_topic1, Tabelle_CSU_topic1,
                            by="month")
alle_Parteien_topic1<-merge(alle_Parteien_topic1, Tabelle_Grüne_topic1,
                            by="month")
alle_Parteien_topic1<-merge(alle_Parteien_topic1, Tabelle_SPD_topic1,
                            by="month")
#parteienprozent
alle_Parteien_topic1$row_sum <- rowSums(alle_Parteien_topic1[ , c(2,3,4,5,6,7)], na.rm=TRUE)
alle_Parteien_topic1$row_sum2<-alle_Parteien_topic1$AFD/alle_Parteien_topic1$row_sum*100
Prozent_AFD1<-sum(alle_Parteien_topic1$AFD)/sum(alle_Parteien_topic1$row_sum)*100
Prozent_CSU1<-sum(alle_Parteien_topic1$CSU)/sum(alle_Parteien_topic1$row_sum)*100
Prozent_CDU1<-sum(alle_Parteien_topic1$CDU)/sum(alle_Parteien_topic1$row_sum)*100
Prozent_SPD1<-sum(alle_Parteien_topic1$SPD)/sum(alle_Parteien_topic1$row_sum)*100
Prozent_Grüne1<-sum(alle_Parteien_topic1$Grüne)/sum(alle_Parteien_topic1$row_sum)*100
Prozent_Linke1<-sum(alle_Parteien_topic1$Linke)/sum(alle_Parteien_topic1$row_sum)*100
Prozent_alle1<-c(Prozent_AFD1, Prozent_CDU1, Prozent_CSU1, Prozent_Grüne1, Prozent_Linke1, Prozent_SPD1)
barplot(Prozent_alle1)

######Visualisierung
##########
library(ggplot2)

matplot(alle_Parteien_topic1$month, alle_Parteien_topic1, type = "l", lty = 1, col = 2:7,
        ylab = "Frequency", xlab = "topic1")
grid()
legend("topleft", col = 2:7, legend = c("Linke", "AFD", "CDU", "CSU", "Grüne", "SPD"), lty = 1, bg = "white")

plot(alle_Parteien_topic1$month, alle_Parteien_topic1$AFD, 
     type = "l", col="#069AF3", ylab="Häufigkeit", 
     xlab="Topic1")
lines(alle_Parteien_topic1$month, alle_Parteien_topic1$Linke
      ,col="#FF00FF")
lines(alle_Parteien_topic1$month, alle_Parteien_topic1$CDU
      ,col="#000000")
lines(alle_Parteien_topic1$month, alle_Parteien_topic1$CSU,
      col="#000080")
lines(alle_Parteien_topic1$month, alle_Parteien_topic1$Grüne,
      col="#15B01A")
lines(alle_Parteien_topic1$month, alle_Parteien_topic1$SPD,
      col="#FF0000")
legend("topleft", legend = c("CDU", "SPD", "Grüne", "CSU", "AFD", "Linke")
       ,cex=1.0, ncol = 2,lty=1, pt.cex=0.6,y.intersp = 0.3,
       text.font = 2.9,bty = "n",
       col=c("#000000","#FF0000","#15B01A","#000080","#069AF3", "#FF00FF"))

###################################
##### GGPLOT Graphic Topic 1 #####
###################################

colors = c("AFD" = "red", "Linke" = "yellow", "CDU" = "blue",
           "CSU" = "steelblue", "Grüne" = "green", "SPD" = "pink")
ggp_topic1= ggplot(as.data.frame(alle_Parteien_topic1), aes(x=month)) +
  ggtitle("Topic 1")+
  geom_line(aes(y = AFD, color = "AFD"), size = 0.4) + 
  geom_line(aes(y = Linke, color = "Linke"), size = 0.4) + 
  geom_line(aes(y = CDU, color = "CDU"), size = 0.4) + 
  geom_line(aes(y = CSU, color = "CSU"), size = 0.4) + 
  geom_line(aes(y = Grüne, color = "Grüne"), size = 0.4) + 
  geom_line(aes(y = SPD, color = "SPD"), size = 0.4) +
  labs(x = "Year",y = "Frequency", color = "Legend") +
  scale_color_manual(values = colors)+
  theme(legend.position = c(0.1,0.8))+
  theme(legend.background = element_rect(fill="grey",size=0.5, linetype="solid",colour ="darkgrey"))
plot(ggp_topic1)


#topic2

topic2<-c("berlin", "gespräch","deutschen","bundestag", "rede", "woche", "cdu", "unterwegs", "mdb", "veranstaltung")


#Topic 2 auf Linke

tokens_Linke_topic2<-tokens_keep(tokens_Linke, pattern = topic2)
dfm_Linke=dfm(tokens_Linke_topic2)
dfm_Linke_topic2<-dfm_group(dfm_Linke, groups = Date)
Tabelle_Linke_topic2<-convert(dfm_Linke_topic2, to="data.frame")

#Topic Spalten zusammen
Tabelle_Linke_topic2<- Tabelle_Linke_topic2 %>% mutate(sumrow= berlin+gespräch+deutschen+bundestag+rede+woche+cdu+unterwegs+mdb+veranstaltung)
Tabelle_Linke_topic2$Linke<-Tabelle_Linke_topic2$sumrow
Tabelle_Linke_topic2[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_Linke_topic2)
Tabelle_Linke_topic2$doc_id<-ymd(Tabelle_Linke_topic2$doc_id)

Tabelle_Linke_topic2<-Tabelle_Linke_topic2 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(Linke = sum(Linke))

#topic 2 auf AFD
tokens_AFD_topic2<-tokens_keep(tokens_AFD, pattern = topic2)
dfm_AFD=dfm(tokens_AFD_topic2)
dfm_AFD_topic2<-dfm_group(dfm_AFD, groups = Date)
Tabelle_AFD_topic2<-convert(dfm_AFD_topic2, to="data.frame")

#Topic Spalten zusammen
Tabelle_AFD_topic2<- Tabelle_AFD_topic2 %>% mutate(sumrow= berlin+gespräch+deutschen+bundestag+rede+woche+cdu+unterwegs+mdb+veranstaltung)
Tabelle_AFD_topic2$AFD<-Tabelle_AFD_topic2$sumrow
Tabelle_AFD_topic2[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_AFD_topic2)
Tabelle_AFD_topic2$doc_id<-ymd(Tabelle_AFD_topic2$doc_id)

Tabelle_AFD_topic2<-Tabelle_AFD_topic2 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(AFD = sum(AFD))

#topic2 auf CDU
tokens_CDU_topic2<-tokens_keep(tokens_CDU, pattern = topic2)
dfm_CDU=dfm(tokens_CDU_topic2)
dfm_CDU_topic2<-dfm_group(dfm_CDU, groups = Date)
Tabelle_CDU_topic2<-convert(dfm_CDU_topic2, to="data.frame")

#Topic Spalten zusammen
Tabelle_CDU_topic2<- Tabelle_CDU_topic2 %>% mutate(sumrow= berlin+gespräch+deutschen+bundestag+rede+woche+cdu+unterwegs+mdb+veranstaltung)
Tabelle_CDU_topic2$CDU<-Tabelle_CDU_topic2$sumrow
Tabelle_CDU_topic2[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_CDU_topic2)
Tabelle_CDU_topic2$doc_id<-ymd(Tabelle_CDU_topic2$doc_id)

Tabelle_CDU_topic2<-Tabelle_CDU_topic2 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CDU = sum(CDU))

#topic2 auf CSU
tokens_CSU_topic2<-tokens_keep(tokens_CSU, pattern = topic2)
dfm_CSU=dfm(tokens_CSU_topic2)
dfm_CSU_topic2<-dfm_group(dfm_CSU, groups = Date)
Tabelle_CSU_topic2<-convert(dfm_CSU_topic2, to="data.frame")

#Topic Spalten zusammen
Tabelle_CSU_topic2<- Tabelle_CSU_topic2 %>% mutate(sumrow= berlin+gespräch+deutschen+bundestag+rede+woche+cdu+unterwegs+mdb+veranstaltung)
Tabelle_CSU_topic2$CSU<-Tabelle_CSU_topic2$sumrow
Tabelle_CSU_topic2[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_CSU_topic2)
Tabelle_CSU_topic2$doc_id<-ymd(Tabelle_CSU_topic2$doc_id)

Tabelle_CSU_topic2<-Tabelle_CSU_topic2 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CSU = sum(CSU))

#topic 2 auf Grüne
tokens_Grüne_topic2<-tokens_keep(tokens_Grüne, pattern = topic2)
dfm_Grüne=dfm(tokens_Grüne_topic2)
dfm_Grüne_topic2<-dfm_group(dfm_Grüne, groups = Date)
Tabelle_Grüne_topic2<-convert(dfm_Grüne_topic2, to="data.frame")

#Topic Spalten zusammen
Tabelle_Grüne_topic2<- Tabelle_Grüne_topic2 %>% mutate(sumrow= berlin+gespräch+deutschen+bundestag+rede+woche+cdu+unterwegs+mdb+veranstaltung)
Tabelle_Grüne_topic2$Grüne<-Tabelle_Grüne_topic2$sumrow
Tabelle_Grüne_topic2[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_Grüne_topic2)
Tabelle_Grüne_topic2$doc_id<-ymd(Tabelle_Grüne_topic2$doc_id)

Tabelle_Grüne_topic2<-Tabelle_Grüne_topic2 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(Grüne = sum(Grüne))

#topic2 auf spd 
tokens_SPD_topic2<-tokens_keep(tokens_SPD, pattern = topic2)
dfm_SPD=dfm(tokens_SPD_topic2)
dfm_SPD_topic2<-dfm_group(dfm_SPD, groups = Date)
Tabelle_SPD_topic2<-convert(dfm_SPD_topic2, to="data.frame")

#Topic Spalten zusammen
Tabelle_SPD_topic2<- Tabelle_SPD_topic2 %>% mutate(sumrow= berlin+gespräch+deutschen+bundestag+rede+woche+cdu+unterwegs+mdb+veranstaltung)
Tabelle_SPD_topic2$SPD<-Tabelle_SPD_topic2$sumrow
Tabelle_SPD_topic2[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_SPD_topic2)
Tabelle_SPD_topic2$doc_id<-ymd(Tabelle_SPD_topic2$doc_id)

Tabelle_SPD_topic2<-Tabelle_SPD_topic2 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(SPD = sum(SPD))


#topic 2 in eine Tabelle alle Parteien 
alle_Parteien_topic2<-merge(Tabelle_AFD_topic2, 
                            Tabelle_Linke_topic2,
                            by="month")
alle_Parteien_topic2<-merge(alle_Parteien_topic2, Tabelle_CDU_topic2,
                            by="month")
alle_Parteien_topic2<-merge(alle_Parteien_topic2, Tabelle_CSU_topic2,
                            by="month")
alle_Parteien_topic2<-merge(alle_Parteien_topic2, Tabelle_Grüne_topic2,
                            by="month")
alle_Parteien_topic2<-merge(alle_Parteien_topic2, Tabelle_SPD_topic2,
                            by="month")

#parteienprozent
alle_Parteien_topic2$row_sum <- rowSums(alle_Parteien_topic2[ , c(2,3,4,5,6,7)], na.rm=TRUE)
alle_Parteien_topic2$row_sum2<-alle_Parteien_topic2$AFD/alle_Parteien_topic2$row_sum*100
Prozent_AFD2<-sum(alle_Parteien_topic2$AFD)/sum(alle_Parteien_topic2$row_sum)*100
Prozent_CSU2<-sum(alle_Parteien_topic2$CSU)/sum(alle_Parteien_topic2$row_sum)*100
Prozent_CDU2<-sum(alle_Parteien_topic2$CDU)/sum(alle_Parteien_topic2$row_sum)*100
Prozent_SPD2<-sum(alle_Parteien_topic2$SPD)/sum(alle_Parteien_topic2$row_sum)*100
Prozent_Grüne2<-sum(alle_Parteien_topic2$Grüne)/sum(alle_Parteien_topic2$row_sum)*100
Prozent_Linke2<-sum(alle_Parteien_topic2$Linke)/sum(alle_Parteien_topic2$row_sum)*100
Prozent_alle2<-c(Prozent_AFD2, Prozent_CDU2, Prozent_CSU2, Prozent_Grüne2, Prozent_Linke2, Prozent_SPD2)


as.matrix(Prozent_alle2,Prozent_alle1)
Prozente_alle12<-rbind(Prozent_alle1, Prozent_alle2)
colnames(Prozente_alle12) <- c("AFD", "CDU", "CSU", "Grüne", "Linke", "SPD")
barplot(Prozente_alle12, main = "Parteien und ihre Themen", xlab = "Parteien", ylab = "Prozentanteil", col = c("red","green"))


r#############
######Visualisierung
##########
library(ggplot2)

matplot(alle_Parteien_topic1$month, alle_Parteien_topic2, type = "l", lty = 1, col = 2:7,
        ylab = "Frequency", xlab = "topic2")
grid()
legend("topleft", col = 2:7, legend = c("Linke", "AFD", "CDU", "CSU", "Grüne", "SPD"), lty = 1, bg = "white")


plot(alle_Parteien_topic2$month, alle_Parteien_topic2$AFD, 
     type = "l", col="#069AF3", ylab="Häufigkeit", 
     xlab="Topic2")
lines(alle_Parteien_topic2$month, alle_Parteien_topic2$Linke
      ,col="#FF00FF")
lines(alle_Parteien_topic2$month, alle_Parteien_topic2$CDU
      ,col="#000000")
lines(alle_Parteien_topic2$month, alle_Parteien_topic2$CSU,
      col="#000080")
lines(alle_Parteien_topic2$month, alle_Parteien_topic2$Grüne,
      col="#15B01A")
lines(alle_Parteien_topic2$month, alle_Parteien_topic2$SPD,
      col="#FF0000")
legend("topleft", legend = c("CDU", "SPD", "Grüne", "CSU", "AFD", "Linke")
       ,cex=1.0, ncol = 2,lty=1, pt.cex=0.6,y.intersp = 0.3,
       text.font = 2.9,bty = "n",
       col=c("#000000","#FF0000","#15B01A","#000080","#069AF3", "#FF00FF"))

#topic3

topic3<-c("deutschland", "euro","debatte","bundestag", "bundesregierung", "geld", "zukunft", "arbeit", "bildung", "antrag")


#Topic 3 auf Linke

tokens_Linke_topic3<-tokens_keep(tokens_Linke, pattern = topic3)
dfm_Linke=dfm(tokens_Linke_topic3)
dfm_Linke_topic3<-dfm_group(dfm_Linke, groups = Date)
Tabelle_Linke_topic3<-convert(dfm_Linke_topic3, to="data.frame")

#Topic Spalten zusammen
Tabelle_Linke_topic3<- Tabelle_Linke_topic3 %>% mutate(sumrow= deutschland+euro+debatte+bundestag+bundesregierung+geld+zukunft+arbeit+bildung+antrag)
Tabelle_Linke_topic3$Linke<-Tabelle_Linke_topic3$sumrow
Tabelle_Linke_topic3[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_Linke_topic3)
Tabelle_Linke_topic3$doc_id<-ymd(Tabelle_Linke_topic3$doc_id)

Tabelle_Linke_topic3<-Tabelle_Linke_topic3 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(Linke = sum(Linke))

#topic 3 auf AFD
tokens_AFD_topic3<-tokens_keep(tokens_AFD, pattern = topic3)
dfm_AFD=dfm(tokens_AFD_topic3)
dfm_AFD_topic3<-dfm_group(dfm_AFD, groups = Date)
Tabelle_AFD_topic3<-convert(dfm_AFD_topic3, to="data.frame")

#Topic Spalten zusammen
Tabelle_AFD_topic3<- Tabelle_AFD_topic3 %>% mutate(sumrow= deutschland+euro+debatte+bundestag+bundesregierung+geld+zukunft+arbeit+bildung+antrag)
Tabelle_AFD_topic3$AFD<-Tabelle_AFD_topic3$sumrow
Tabelle_AFD_topic3[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_AFD_topic3)
Tabelle_AFD_topic3$doc_id<-ymd(Tabelle_AFD_topic3$doc_id)

Tabelle_AFD_topic3<-Tabelle_AFD_topic3 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(AFD = sum(AFD))

#topic3 auf CDU
tokens_CDU_topic3<-tokens_keep(tokens_CDU, pattern = topic3)
dfm_CDU=dfm(tokens_CDU_topic3)
dfm_CDU_topic3<-dfm_group(dfm_CDU, groups = Date)
Tabelle_CDU_topic3<-convert(dfm_CDU_topic3, to="data.frame")

#Topic Spalten zusammen
Tabelle_CDU_topic3<- Tabelle_CDU_topic3 %>% mutate(sumrow= deutschland+euro+debatte+bundestag+bundesregierung+geld+zukunft+arbeit+bildung+antrag)
Tabelle_CDU_topic3$CDU<-Tabelle_CDU_topic3$sumrow
Tabelle_CDU_topic3[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_CDU_topic3)
Tabelle_CDU_topic3$doc_id<-ymd(Tabelle_CDU_topic3$doc_id)

Tabelle_CDU_topic3<-Tabelle_CDU_topic3 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CDU = sum(CDU))

#topic3 auf CSU
tokens_CSU_topic3<-tokens_keep(tokens_CSU, pattern = topic3)
dfm_CSU=dfm(tokens_CSU_topic3)
dfm_CSU_topic3<-dfm_group(dfm_CSU, groups = Date)
Tabelle_CSU_topic3<-convert(dfm_CSU_topic3, to="data.frame")

#Topic Spalten zusammen
Tabelle_CSU_topic3<- Tabelle_CSU_topic3 %>% mutate(sumrow= deutschland+euro+debatte+bundestag+bundesregierung+geld+zukunft+arbeit+bildung+antrag)
Tabelle_CSU_topic3$CSU<-Tabelle_CSU_topic3$sumrow
Tabelle_CSU_topic3[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_CSU_topic3)
Tabelle_CSU_topic3$doc_id<-ymd(Tabelle_CSU_topic3$doc_id)

Tabelle_CSU_topic3<-Tabelle_CSU_topic3 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CSU = sum(CSU))

#topic 3 auf Grüne
tokens_Grüne_topic3<-tokens_keep(tokens_Grüne, pattern = topic3)
dfm_Grüne=dfm(tokens_Grüne_topic3)
dfm_Grüne_topic3<-dfm_group(dfm_Grüne, groups = Date)
Tabelle_Grüne_topic3<-convert(dfm_Grüne_topic3, to="data.frame")

#Topic Spalten zusammen
Tabelle_Grüne_topic3<- Tabelle_Grüne_topic3 %>% mutate(sumrow= deutschland+euro+debatte+bundestag+bundesregierung+geld+zukunft+arbeit+bildung+antrag)
Tabelle_Grüne_topic3$Grüne<-Tabelle_Grüne_topic3$sumrow
Tabelle_Grüne_topic3[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_Grüne_topic3)
Tabelle_Grüne_topic3$doc_id<-ymd(Tabelle_Grüne_topic3$doc_id)

Tabelle_Grüne_topic3<-Tabelle_Grüne_topic3 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(Grüne = sum(Grüne))

#topic3 auf spd 
tokens_SPD_topic3<-tokens_keep(tokens_SPD, pattern = topic3)
dfm_SPD=dfm(tokens_SPD_topic3)
dfm_SPD_topic3<-dfm_group(dfm_SPD, groups = Date)
Tabelle_SPD_topic3<-convert(dfm_SPD_topic3, to="data.frame")

#Topic Spalten zusammen
Tabelle_SPD_topic3<- Tabelle_SPD_topic3 %>% mutate(sumrow= deutschland+euro+debatte+bundestag+bundesregierung+geld+zukunft+arbeit+bildung+antrag)
Tabelle_SPD_topic3$SPD<-Tabelle_SPD_topic3$sumrow
Tabelle_SPD_topic3[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_SPD_topic3)
Tabelle_SPD_topic3$doc_id<-ymd(Tabelle_SPD_topic3$doc_id)

Tabelle_SPD_topic3<-Tabelle_SPD_topic3 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(SPD = sum(SPD))


#topic 3 in eine Tabelle alle Parteien 
alle_Parteien_topic3<-merge(Tabelle_AFD_topic3, 
                            Tabelle_Linke_topic3,
                            by="month")
alle_Parteien_topic3<-merge(alle_Parteien_topic3, Tabelle_CDU_topic3,
                            by="month")
alle_Parteien_topic3<-merge(alle_Parteien_topic3, Tabelle_CSU_topic3,
                            by="month")
alle_Parteien_topic3<-merge(alle_Parteien_topic3, Tabelle_Grüne_topic3,
                            by="month")
alle_Parteien_topic3<-merge(alle_Parteien_topic3, Tabelle_SPD_topic3,
                            by="month")
#parteienprozent
alle_Parteien_topic3$row_sum <- rowSums(alle_Parteien_topic3[ , c(2,3,4,5,6,7)], na.rm=TRUE)
alle_Parteien_topic3$row_sum3<-alle_Parteien_topic3$AFD/alle_Parteien_topic3$row_sum*100
Prozent_AFD3<-sum(alle_Parteien_topic3$AFD)/sum(alle_Parteien_topic3$row_sum)*100
Prozent_CSU3<-sum(alle_Parteien_topic3$CSU)/sum(alle_Parteien_topic3$row_sum)*100
Prozent_CDU3<-sum(alle_Parteien_topic3$CDU)/sum(alle_Parteien_topic3$row_sum)*100
Prozent_SPD3<-sum(alle_Parteien_topic3$SPD)/sum(alle_Parteien_topic3$row_sum)*100
Prozent_Grüne3<-sum(alle_Parteien_topic3$Grüne)/sum(alle_Parteien_topic3$row_sum)*100
Prozent_Linke3<-sum(alle_Parteien_topic3$Linke)/sum(alle_Parteien_topic3$row_sum)*100
Prozent_alle3<-c(Prozent_AFD3, Prozent_CDU3, Prozent_CSU3, Prozent_Grüne3, Prozent_Linke3, Prozent_SPD3)

as.matrix(Prozent_alle2,Prozent_alle1, Prozent_alle3)
Prozente_alle123<-rbind(Prozent_alle1, Prozent_alle2, Prozent_alle3)
colnames(Prozente_alle123) <- c("AFD", "CDU", "CSU", "Grüne", "Linke", "SPD")
barplot(Prozente_alle123, main = "Parteien und ihre Themen", xlab = "Parteien", ylab = "Prozentanteil", col = c("red","green", "blue"))


######Visualisierung
##########

matplot(alle_Parteien_topic3$month, alle_Parteien_topic3, type = "l", lty = 1, col = 2:7,
        ylab = "Frequency", xlab = "topic3")
grid()
legend("topleft", col = 2:7, legend = c("Linke", "AFD", "CDU", "CSU", "Grüne", "SPD"), lty = 1, bg = "white")


plot(alle_Parteien_topic3$month, alle_Parteien_topic3$AFD, 
     type = "l", col="#069AF3", ylab="Häufigkeit", 
     xlab="Topic3")
lines(alle_Parteien_topic3$month, alle_Parteien_topic3$Linke
      ,col="#FF00FF")
lines(alle_Parteien_topic3$month, alle_Parteien_topic3$CDU
      ,col="#000000")
lines(alle_Parteien_topic3$month, alle_Parteien_topic3$CSU,
      col="#000080")
lines(alle_Parteien_topic3$month, alle_Parteien_topic3$Grüne,
      col="#15B01A")
lines(alle_Parteien_topic3$month, alle_Parteien_topic3$SPD,
      col="#FF0000")
legend("topleft", legend = c("CDU", "SPD", "Grüne", "CSU", "AFD", "Linke")
       ,cex=1.0, ncol = 2,lty=1, pt.cex=0.6,y.intersp = 0.3,
       text.font = 2.9,bty = "n",
       col=c("#000000","#FF0000","#15B01A","#000080","#069AF3", "#FF00FF"))

#topic4

topic4<-c("deutschland", "demokratie","europa","polizei", "flüchtlinge", "welt", "land", "gewalt", "deutschen", "bundesregierung")


#Topic 4 auf Linke

tokens_Linke_topic4<-tokens_keep(tokens_Linke, pattern = topic4)
dfm_Linke=dfm(tokens_Linke_topic4)
dfm_Linke_topic4<-dfm_group(dfm_Linke, groups = Date)
Tabelle_Linke_topic4<-convert(dfm_Linke_topic4, to="data.frame")

#Topic Spalten zusammen
Tabelle_Linke_topic4<- Tabelle_Linke_topic4 %>% mutate(sumrow= deutschland+demokratie+europa+polizei+flüchtlinge+welt+land+gewalt+deutschen+bundesregierung)
Tabelle_Linke_topic4$Linke<-Tabelle_Linke_topic4$sumrow
Tabelle_Linke_topic4[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_Linke_topic4)
Tabelle_Linke_topic4$doc_id<-ymd(Tabelle_Linke_topic4$doc_id)

Tabelle_Linke_topic4<-Tabelle_Linke_topic4 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(Linke = sum(Linke))

#topic 4 auf AFD
tokens_AFD_topic4<-tokens_keep(tokens_AFD, pattern = topic4)
dfm_AFD=dfm(tokens_AFD_topic4)
dfm_AFD_topic4<-dfm_group(dfm_AFD, groups = Date)
Tabelle_AFD_topic4<-convert(dfm_AFD_topic4, to="data.frame")

#Topic Spalten zusammen
Tabelle_AFD_topic4<- Tabelle_AFD_topic4 %>% mutate(sumrow= deutschland+demokratie+europa+polizei+flüchtlinge+welt+land+gewalt+deutschen+bundesregierung)
Tabelle_AFD_topic4$AFD<-Tabelle_AFD_topic4$sumrow
Tabelle_AFD_topic4[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_AFD_topic4)
Tabelle_AFD_topic4$doc_id<-ymd(Tabelle_AFD_topic4$doc_id)

Tabelle_AFD_topic4<-Tabelle_AFD_topic4 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(AFD = sum(AFD))

#topic4 auf CDU
tokens_CDU_topic4<-tokens_keep(tokens_CDU, pattern = topic4)
dfm_CDU=dfm(tokens_CDU_topic4)
dfm_CDU_topic4<-dfm_group(dfm_CDU, groups = Date)
Tabelle_CDU_topic4<-convert(dfm_CDU_topic4, to="data.frame")

#Topic Spalten zusammen
Tabelle_CDU_topic4<- Tabelle_CDU_topic4 %>% mutate(sumrow= deutschland+demokratie+europa+polizei+flüchtlinge+welt+land+gewalt+deutschen+bundesregierung)
Tabelle_CDU_topic4$CDU<-Tabelle_CDU_topic4$sumrow
Tabelle_CDU_topic4[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_CDU_topic4)
Tabelle_CDU_topic4$doc_id<-ymd(Tabelle_CDU_topic4$doc_id)

Tabelle_CDU_topic4<-Tabelle_CDU_topic4 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CDU = sum(CDU))

#topic4 auf CSU
tokens_CSU_topic4<-tokens_keep(tokens_CSU, pattern = topic4)
dfm_CSU=dfm(tokens_CSU_topic4)
dfm_CSU_topic4<-dfm_group(dfm_CSU, groups = Date)
Tabelle_CSU_topic4<-convert(dfm_CSU_topic4, to="data.frame")

#Topic Spalten zusammen
Tabelle_CSU_topic4<- Tabelle_CSU_topic4 %>% mutate(sumrow= deutschland+demokratie+europa+polizei+flüchtlinge+welt+land+gewalt+deutschen+bundesregierung)
Tabelle_CSU_topic4$CSU<-Tabelle_CSU_topic4$sumrow
Tabelle_CSU_topic4[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_CSU_topic4)
Tabelle_CSU_topic4$doc_id<-ymd(Tabelle_CSU_topic4$doc_id)

Tabelle_CSU_topic4<-Tabelle_CSU_topic4 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CSU = sum(CSU))

#topic 4 auf Grüne
tokens_Grüne_topic4<-tokens_keep(tokens_Grüne, pattern = topic4)
dfm_Grüne=dfm(tokens_Grüne_topic4)
dfm_Grüne_topic4<-dfm_group(dfm_Grüne, groups = Date)
Tabelle_Grüne_topic4<-convert(dfm_Grüne_topic4, to="data.frame")

#Topic Spalten zusammen
Tabelle_Grüne_topic4<- Tabelle_Grüne_topic4 %>% mutate(sumrow= deutschland+demokratie+europa+polizei+flüchtlinge+welt+land+gewalt+deutschen+bundesregierung)
Tabelle_Grüne_topic4$Grüne<-Tabelle_Grüne_topic4$sumrow
Tabelle_Grüne_topic4[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_Grüne_topic4)
Tabelle_Grüne_topic4$doc_id<-ymd(Tabelle_Grüne_topic4$doc_id)

Tabelle_Grüne_topic4<-Tabelle_Grüne_topic4 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(Grüne = sum(Grüne))

#topic4 auf spd 
tokens_SPD_topic4<-tokens_keep(tokens_SPD, pattern = topic4)
dfm_SPD=dfm(tokens_SPD_topic4)
dfm_SPD_topic4<-dfm_group(dfm_SPD, groups = Date)
Tabelle_SPD_topic4<-convert(dfm_SPD_topic4, to="data.frame")

#Topic Spalten zusammen
Tabelle_SPD_topic4<- Tabelle_SPD_topic4 %>% mutate(sumrow= deutschland+demokratie+europa+polizei+flüchtlinge+welt+land+gewalt+deutschen+bundesregierung)
Tabelle_SPD_topic4$SPD<-Tabelle_SPD_topic4$sumrow
Tabelle_SPD_topic4[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_SPD_topic4)
Tabelle_SPD_topic4$doc_id<-ymd(Tabelle_SPD_topic4$doc_id)

Tabelle_SPD_topic4<-Tabelle_SPD_topic4 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(SPD = sum(SPD))


#topic 4 in eine Tabelle alle Parteien 
alle_Parteien_topic4<-merge(Tabelle_AFD_topic4, 
                            Tabelle_Linke_topic4,
                            by="month")
alle_Parteien_topic4<-merge(alle_Parteien_topic4, Tabelle_CDU_topic4,
                            by="month")
alle_Parteien_topic4<-merge(alle_Parteien_topic4, Tabelle_CSU_topic4,
                            by="month")
alle_Parteien_topic4<-merge(alle_Parteien_topic4, Tabelle_Grüne_topic4,
                            by="month")
alle_Parteien_topic4<-merge(alle_Parteien_topic4, Tabelle_SPD_topic4,
                            by="month")
#parteienprozent
alle_Parteien_topic4$row_sum <- rowSums(alle_Parteien_topic4[ , c(2,3,4,5,6,7)], na.rm=TRUE)
alle_Parteien_topic4$row_sum4<-alle_Parteien_topic4$AFD/alle_Parteien_topic4$row_sum*100
Prozent_AFD4<-sum(alle_Parteien_topic4$AFD)/sum(alle_Parteien_topic4$row_sum)*100
Prozent_CSU4<-sum(alle_Parteien_topic4$CSU)/sum(alle_Parteien_topic4$row_sum)*100
Prozent_CDU4<-sum(alle_Parteien_topic4$CDU)/sum(alle_Parteien_topic4$row_sum)*100
Prozent_SPD4<-sum(alle_Parteien_topic4$SPD)/sum(alle_Parteien_topic4$row_sum)*100
Prozent_Grüne4<-sum(alle_Parteien_topic4$Grüne)/sum(alle_Parteien_topic4$row_sum)*100
Prozent_Linke4<-sum(alle_Parteien_topic4$Linke)/sum(alle_Parteien_topic4$row_sum)*100
Prozent_alle4<-c(Prozent_AFD4, Prozent_CDU4, Prozent_CSU4, Prozent_Grüne4, Prozent_Linke4, Prozent_SPD4)

Prozente_alle1234<-rbind(Prozent_alle1, Prozent_alle2, Prozent_alle3, Prozent_alle4)
as.matrix(Prozent_alle2,Prozent_alle1, Prozent_alle3, Prozent_alle4)
colnames(Prozente_alle1234) <- c("AFD", "CDU", "CSU", "Grüne", "Linke", "SPD")
barplot(Prozente_alle1234, main = "Parteien und ihre Themen", xlab = "Parteien", ylab = "Prozentanteil", col = c("red","green", "blue", "yellow"))

######Visualisierung
##########

matplot(alle_Parteien_topic4$month, alle_Parteien_topic4, type = "l", lty = 1, col = 2:7,
        ylab = "Frequency", xlab = "topic4")
grid()
legend("topleft", col = 2:7, legend = c("Linke", "AFD", "CDU", "CSU", "Grüne", "SPD"), lty = 1, bg = "white")


plot(alle_Parteien_topic4$month, alle_Parteien_topic4$AFD, 
     type = "l", col="#069AF3", ylab="Häufigkeit", 
     xlab="Topic4")
lines(alle_Parteien_topic4$month, alle_Parteien_topic4$Linke
      ,col="#FF00FF")
lines(alle_Parteien_topic4$month, alle_Parteien_topic4$CDU
      ,col="#000000")
lines(alle_Parteien_topic4$month, alle_Parteien_topic4$CSU,
      col="#000080")
lines(alle_Parteien_topic4$month, alle_Parteien_topic4$Grüne,
      col="#15B01A")
lines(alle_Parteien_topic4$month, alle_Parteien_topic4$SPD,
      col="#FF0000")
legend("topleft", legend = c("CDU", "SPD", "Grüne", "CSU", "AFD", "Linke")
       ,cex=1.0, ncol = 2,lty=1, pt.cex=0.6,y.intersp = 0.3,
       text.font = 2.9,bty = "n",
       col=c("#000000","#FF0000","#15B01A","#000080","#069AF3", "#FF00FF"))

#plot neu
colors = c("AFD" = "red", "Linke" = "yellow", "CDU" = "blue",
           "CSU" = "steelblue", "Grüne" = "green", "SPD" = "pink")
ggp_topic4=ggplot(as.data.frame(alle_Parteien_topic4), aes(x=month)) +
  ggtitle("Topic4")+
  geom_line(aes(y = AFD, color = "AFD"), size = 0.4) + 
  geom_line(aes(y = Linke, color = "Linke"), size = 0.4) + 
  geom_line(aes(y = CDU, color = "CDU"), size = 0.4) + 
  geom_line(aes(y = CSU, color = "CSU"), size = 0.4) + 
  geom_line(aes(y = Grüne, color = "Grüne"), size = 0.4) + 
  geom_line(aes(y = SPD, color = "SPD"), size = 0.4) +
  labs(x = "Year",y = "Frequency", color = "Legend") +
  scale_color_manual(values = colors)+
  theme(legend.position = c(0.1,0.8))+
  theme(legend.background = element_rect(fill="grey",size=0.5, linetype="solid",colour ="darkgrey"))
plot(ggp_topic4)
#topic5

topic5<-c("spd", "cdu","merkel","afd", "csu", "fdp", "partei", "linke", "grünen", "politik")


#Topic 5 auf Linke

tokens_Linke_topic5<-tokens_keep(tokens_Linke, pattern = topic5)
dfm_Linke=dfm(tokens_Linke_topic5)
dfm_Linke_topic5<-dfm_group(dfm_Linke, groups = Date)
Tabelle_Linke_topic5<-convert(dfm_Linke_topic5, to="data.frame")

#Topic Spalten zusammen
Tabelle_Linke_topic5<- Tabelle_Linke_topic5 %>% mutate(sumrow= spd+cdu+merkel+afd+csu+fdp+partei+linke+grünen+politik)
Tabelle_Linke_topic5$Linke<-Tabelle_Linke_topic5$sumrow
Tabelle_Linke_topic5[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_Linke_topic5)
Tabelle_Linke_topic5$doc_id<-ymd(Tabelle_Linke_topic5$doc_id)

Tabelle_Linke_topic5<-Tabelle_Linke_topic5 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(Linke = sum(Linke))

#topic 5 auf AFD
tokens_AFD_topic5<-tokens_keep(tokens_AFD, pattern = topic5)
dfm_AFD=dfm(tokens_AFD_topic5)
dfm_AFD_topic5<-dfm_group(dfm_AFD, groups = Date)
Tabelle_AFD_topic5<-convert(dfm_AFD_topic5, to="data.frame")

#Topic Spalten zusammen
Tabelle_AFD_topic5<- Tabelle_AFD_topic5 %>% mutate(sumrow= spd+cdu+merkel+afd+csu+fdp+partei+linke+grünen+politik)
Tabelle_AFD_topic5$AFD<-Tabelle_AFD_topic5$sumrow
Tabelle_AFD_topic5[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_AFD_topic5)
Tabelle_AFD_topic5$doc_id<-ymd(Tabelle_AFD_topic5$doc_id)

Tabelle_AFD_topic5<-Tabelle_AFD_topic5 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(AFD = sum(AFD))

#topic5 auf CDU
tokens_CDU_topic5<-tokens_keep(tokens_CDU, pattern = topic5)
dfm_CDU=dfm(tokens_CDU_topic5)
dfm_CDU_topic5<-dfm_group(dfm_CDU, groups = Date)
Tabelle_CDU_topic5<-convert(dfm_CDU_topic5, to="data.frame")

#Topic Spalten zusammen
Tabelle_CDU_topic5<- Tabelle_CDU_topic5 %>% mutate(sumrow= spd+cdu+merkel+afd+csu+fdp+partei+linke+grünen+politik)
Tabelle_CDU_topic5$CDU<-Tabelle_CDU_topic5$sumrow
Tabelle_CDU_topic5[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_CDU_topic5)
Tabelle_CDU_topic5$doc_id<-ymd(Tabelle_CDU_topic5$doc_id)

Tabelle_CDU_topic5<-Tabelle_CDU_topic5 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CDU = sum(CDU))

#topic5 auf CSU
tokens_CSU_topic5<-tokens_keep(tokens_CSU, pattern = topic5)
dfm_CSU=dfm(tokens_CSU_topic5)
dfm_CSU_topic5<-dfm_group(dfm_CSU, groups = Date)
Tabelle_CSU_topic5<-convert(dfm_CSU_topic5, to="data.frame")

#Topic Spalten zusammen
Tabelle_CSU_topic5<- Tabelle_CSU_topic5 %>% mutate(sumrow= spd+cdu+merkel+afd+csu+fdp+partei+linke+grünen+politik)
Tabelle_CSU_topic5$CSU<-Tabelle_CSU_topic5$sumrow
Tabelle_CSU_topic5[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_CSU_topic5)
Tabelle_CSU_topic5$doc_id<-ymd(Tabelle_CSU_topic5$doc_id)

Tabelle_CSU_topic5<-Tabelle_CSU_topic5 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CSU = sum(CSU))

#topic 5 auf Grüne
tokens_Grüne_topic5<-tokens_keep(tokens_Grüne, pattern = topic5)
dfm_Grüne=dfm(tokens_Grüne_topic5)
dfm_Grüne_topic5<-dfm_group(dfm_Grüne, groups = Date)
Tabelle_Grüne_topic5<-convert(dfm_Grüne_topic5, to="data.frame")

#Topic Spalten zusammen
Tabelle_Grüne_topic5<- Tabelle_Grüne_topic5 %>% mutate(sumrow= spd+cdu+merkel+afd+csu+fdp+partei+linke+grünen+politik)
Tabelle_Grüne_topic5$Grüne<-Tabelle_Grüne_topic5$sumrow
Tabelle_Grüne_topic5[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_Grüne_topic5)
Tabelle_Grüne_topic5$doc_id<-ymd(Tabelle_Grüne_topic5$doc_id)

Tabelle_Grüne_topic5<-Tabelle_Grüne_topic5 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(Grüne = sum(Grüne))

#topic5 auf spd 
tokens_SPD_topic5<-tokens_keep(tokens_SPD, pattern = topic5)
dfm_SPD=dfm(tokens_SPD_topic5)
dfm_SPD_topic5<-dfm_group(dfm_SPD, groups = Date)
Tabelle_SPD_topic5<-convert(dfm_SPD_topic5, to="data.frame")

#Topic Spalten zusammen
Tabelle_SPD_topic5<- Tabelle_SPD_topic5 %>% mutate(sumrow= spd+cdu+merkel+afd+csu+fdp+partei+linke+grünen+politik)
Tabelle_SPD_topic5$SPD<-Tabelle_SPD_topic5$sumrow
Tabelle_SPD_topic5[2:4]=NULL

#von Tagen zu Monaten 
str(Tabelle_SPD_topic5)
Tabelle_SPD_topic5$doc_id<-ymd(Tabelle_SPD_topic5$doc_id)

Tabelle_SPD_topic5<-Tabelle_SPD_topic5 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(SPD = sum(SPD))


#topic 5 in eine Tabelle alle Parteien 
alle_Parteien_topic5<-merge(Tabelle_AFD_topic5, 
                            Tabelle_Linke_topic5,
                            by="month")
alle_Parteien_topic5<-merge(alle_Parteien_topic5, Tabelle_CDU_topic5,
                            by="month")
alle_Parteien_topic5<-merge(alle_Parteien_topic5, Tabelle_CSU_topic5,
                            by="month")
alle_Parteien_topic5<-merge(alle_Parteien_topic5, Tabelle_Grüne_topic5,
                            by="month")
alle_Parteien_topic5<-merge(alle_Parteien_topic5, Tabelle_SPD_topic5,
                            by="month")

#parteienprozent
alle_Parteien_topic5$row_sum <- rowSums(alle_Parteien_topic5[ , c(2,3,4,5,6,7)], na.rm=TRUE)
alle_Parteien_topic5$row_sum5<-alle_Parteien_topic5$AFD/alle_Parteien_topic5$row_sum*100
Prozent_AFD5<-sum(alle_Parteien_topic5$AFD)/sum(alle_Parteien_topic5$row_sum)*100
Prozent_CSU5<-sum(alle_Parteien_topic5$CSU)/sum(alle_Parteien_topic5$row_sum)*100
Prozent_CDU5<-sum(alle_Parteien_topic5$CDU)/sum(alle_Parteien_topic5$row_sum)*100
Prozent_SPD5<-sum(alle_Parteien_topic5$SPD)/sum(alle_Parteien_topic5$row_sum)*100
Prozent_Grüne5<-sum(alle_Parteien_topic5$Grüne)/sum(alle_Parteien_topic5$row_sum)*100
Prozent_Linke5<-sum(alle_Parteien_topic5$Linke)/sum(alle_Parteien_topic5$row_sum)*100
Prozent_alle5<-c(Prozent_AFD5, Prozent_CDU5, Prozent_CSU5, Prozent_Grüne5, Prozent_Linke5, Prozent_SPD5)

as.matrix(Prozent_alle2,Prozent_alle1, Prozent_alle3, Prozent_alle4,Prozent_alle5)
Prozente_alle12345<-rbind(Prozent_alle1, Prozent_alle2, Prozent_alle3, Prozent_alle4,Prozent_alle5)
colnames(Prozente_alle12345) <- c("AFD", "CDU", "CSU", "Grüne", "Linke", "SPD")
barplot(Prozente_allerParteien, main = "Parteien und ihre Themen", xlab = "Parteien", ylab = "Prozentanteil", col = c("red","green", "blue", "yellow"))


Prozente_alleAFD<-c(Prozent_AFD1, Prozent_AFD2,Prozent_AFD3,Prozent_AFD4,Prozent_AFD5)
Prozente_alleCSU<-c(Prozent_CSU1, Prozent_CSU2,Prozent_CSU3,Prozent_CSU4,Prozent_CSU5)
Prozente_allerCDU<-c(Prozent_CDU1,Prozent_CDU2, Prozent_CDU3, Prozent_CDU4,Prozent_CDU5)
Prozent_allerSPD<-c(Prozent_SPD1, Prozent_SPD2, Prozent_SPD3, Prozent_SPD4,Prozent_SPD5 )
Prozent_allerGrüne<-c(Prozent_Grüne1,Prozent_Grüne2,Prozent_Grüne3,Prozent_Grüne4, Prozent_Grüne5)
Prozent_allerLinke<-c(Prozent_Linke1,Prozent_Linke2,Prozent_Linke3,Prozent_Linke4, Prozent_Linke5)
as.matrix(Prozente_alleAFD, Prozente_alleCSU,Prozente_allerCDU,Prozent_allerSPD, Prozent_allerGrüne, Prozent_allerLinke)
Prozente_allerParteien<-rbind(Prozente_alleAFD, Prozente_alleCSU,Prozente_allerCDU,Prozent_allerSPD, Prozent_allerGrüne, Prozent_allerLinke)
colnames(Prozente_allerParteien) <- c("topic1", "topic2", "topic3", "topic4", "topic5")
barplot(Prozente_allerParteien, main = "Parteien und ihre Themen", col = c("#40C9FF","#4d3c33","#32302E","#E3000F","#46962B", "#8B0000"))


######Visualisierung
##########

matplot(alle_Parteien_topic5$month, alle_Parteien_topic5, type = "l", lty = 1, col = 2:7,
        ylab = "Frequency", xlab = "topic5")
grid()
legend("topleft", col = 2:7, legend = c("Linke", "AFD", "CDU", "CSU", "Grüne", "SPD"), lty = 1, bg = "white")


plot(alle_Parteien_topic5$month, alle_Parteien_topic5$AFD, 
     type = "l", col="#069AF3", ylab="Häufigkeit", 
     xlab="Topic5")
lines(alle_Parteien_topic5$month, alle_Parteien_topic5$Linke
      ,col="#FF00FF")
lines(alle_Parteien_topic5$month, alle_Parteien_topic5$CDU
      ,col="#000000")
lines(alle_Parteien_topic5$month, alle_Parteien_topic5$CSU,
      col="#000080")
lines(alle_Parteien_topic5$month, alle_Parteien_topic5$Grüne,
      col="#15B01A")
lines(alle_Parteien_topic5$month, alle_Parteien_topic5$SPD,
      col="#FF0000")
legend("topleft", legend = c("CDU", "SPD", "Grüne", "CSU", "AFD", "Linke")
       ,cex=1.0, ncol = 2,lty=1, pt.cex=0.6,y.intersp = 0.3,
       text.font = 2.9,bty = "n",
       col=c("#000000","#FF0000","#15B01A","#000080","#069A
