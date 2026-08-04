# LDA Topic Modeling #

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

    ###############
### Pre-Processing ####
    ###############
#LINKE
path_data_Linke= system.file("~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/Linke", package= "readtext")
Linke_gesamt= readtext(paste0(path_data_Linke,"~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/Linke"), encoding= "UTF-8")
Linke_gesamt[2:5]= NULL
Linke_gesamt["Time"]= NULL
Linke_gesamt[4:16]= NULL
Linke_gesamt[,4]=c("Linke")
colnames(Linke_gesamt)=c("doc_id","Date","Text","Partei")
Linke_gesamt[,2]= as.Date(Linke_gesamt[,2])

corpus_Linke= corpus(Linke_gesamt, text_field = "Text")
tokens_Linke= tokens(corpus_Linke, remove_punct = T, remove_numbers = T,remove_symbols = T, remove_url = T)
tokens_Linke= tokens_remove(tokens_Linke, pattern= "@*")
tokens_Linke= tokens_remove(tokens_Linke, pattern= stopwords("de", source ="stopwords-iso"))
tokens_Linke= tokens_remove(tokens_Linke, pattern = stopwords("en", source = "stopwords-iso"))
tokens_Linke= tokens_remove(tokens_Linke, pattern = "#*")
dictionary_words= dictionary(list(dict=c("rt", "https" , "htt" , "ht", "http", "danke" , "bitte" , "dr", "amp" , "via")))
tokens_Linke= tokens_select(tokens_Linke,pattern= dictionary_words, selection = "remove")
tokens_Linke= tokens_wordstem(tokens_Linke, language="de")

#AFD
path_data_AFD = system.file("~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/AfD", package = "readtext")
AFD_gesamt= readtext(paste0(path_data_AFD, "~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/AfD"), encoding="UTF-8")
AFD_gesamt[2:5]= NULL
AFD_gesamt["Time"]= NULL
AFD_gesamt[4:16]= NULL
AFD_gesamt[,4]=c("AFD")
colnames(AFD_gesamt)=c("doc_id","Date","Text","Partei")
AFD_gesamt[,2]= as.Date(AFD_gesamt[,2])
rownames(AFD_gesamt)= c(1:nrow(AFD_gesamt))

corpus_AFD= corpus(AFD_gesamt, text_field = "Text")
tokens_AFD= tokens(corpus_AFD, remove_punct = T, remove_numbers = T,remove_symbols = T, remove_url = T)
tokens_AFD= tokens_remove(tokens_AFD, pattern= "@*")
tokens_AFD= tokens_remove(tokens_AFD, pattern= stopwords("de", source ="stopwords-iso"))
tokens_AFD= tokens_remove(tokens_AFD, pattern = stopwords("en", source = "stopwords-iso"))
tokens_AFD= tokens_remove(tokens_AFD, pattern = "#*")
dictionary_words= dictionary(list(dict=c("rt", "https" , "htt" , "ht", "http", "danke" , "bitte" , "dr","amp" , "via")))
tokens_AFD= tokens_select(tokens_AFD, pattern= dictionary_words, selection = "remove")

#CDU
path_data_CDU= system.file("~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/CDU", package = "readtext")
CDU_gesamt= readtext(paste0(path_data_CDU, "~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/CDU"), encoding="UTF-8")
CDU_gesamt[2:5]= NULL
CDU_gesamt["Time"]= NULL
CDU_gesamt[4:16]= NULL
CDU_gesamt[,4]=c("CDU")
colnames(CDU_gesamt)=c("doc_id","Date","Text","Partei")
CDU_gesamt[,2]= as.Date(CDU_gesamt[,2])
rownames(CDU_gesamt)= c(1:nrow(CDU_gesamt))

corpus_CDU= corpus(CDU_gesamt, text_field = "Text")
tokens_CDU= tokens(corpus_CDU, remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T)
tokens_CDU= tokens_remove(tokens_CDU, pattern= "@*")
tokens_CDU= tokens_remove(tokens_CDU, pattern= stopwords("de", source ="stopwords-iso"))
tokens_CDU= tokens_remove(tokens_CDU, pattern = stopwords("en", source = "stopwords-iso"))
tokens_CDU= tokens_remove(tokens_CDU, pattern = "#*")
dictionary_words= dictionary(list(dict=c("rt", "https" , "htt" , "ht","http", "danke" , "bitte" , "dr","amp" , "via")))
tokens_CDU= tokens_select(tokens_CDU,pattern= dictionary_words, selection ="remove")

#CSU
path_data_CSU= system.file("~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/CSU", package = "readtext")
CSU_gesamt= readtext(paste0(path_data_CSU, "~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/CSU"), encoding="UTF-8")
CSU_gesamt[2:5]= NULL
CSU_gesamt["Time"]= NULL
CSU_gesamt[4:16]= NULL
CSU_gesamt[,4]=c("CSU")
CSU_gesamt[5]= NULL
colnames(CSU_gesamt)=c("doc_id","Date","Text","Partei")
CSU_gesamt[,2]= as.Date(CSU_gesamt[,2])
rownames(CSU_gesamt)= c(1:nrow(CSU_gesamt))

corpus_CSU= corpus(CSU_gesamt, text_field = "Text")
tokens_CSU= tokens(corpus_CSU, remove_punct = T, remove_numbers = T,remove_symbols = T, remove_url = T)
tokens_CSU= tokens_remove(tokens_CSU, pattern= "@*")
tokens_CSU= tokens_remove(tokens_CSU, pattern= stopwords("de", source ="stopwords-iso"))
tokens_CSU= tokens_remove(tokens_CSU, pattern = stopwords("en", source = "stopwords-iso"))
tokens_CSU= tokens_remove(tokens_CSU, pattern = "#*")
dictionary_words= dictionary(list(dict=c("rt", "https" , "htt" , "ht","http", "danke" , "bitte" , "dr","amp" , "via")))
tokens_CSU= tokens_select(tokens_CSU,pattern= dictionary_words, selection = "remove")

#Grüne
path_data_Grüne= system.file("~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/Grüne", package = "readtext")
Grüne_gesamt= readtext(paste0(path_data_Grüne, "~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/Grüne"), encoding="UTF-8")
Grüne_gesamt[2:5]= NULL
Grüne_gesamt["Time"]= NULL
Grüne_gesamt[4:16]= NULL
Grüne_gesamt[,4]=c("Grüne")
colnames(Grüne_gesamt)=c("doc_id","Date","Text","Partei")
Grüne_gesamt[,2]= as.Date(Grüne_gesamt[,2])
rownames(Grüne_gesamt)= c(1:nrow(Grüne_gesamt))

corpus_Grüne= corpus(Grüne_gesamt, text_field = "Text")
tokens_Grüne= tokens(corpus_Grüne, remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T)
tokens_Grüne= tokens_remove(tokens_Grüne, pattern= "@*")
tokens_Grüne= tokens_remove(tokens_Grüne, pattern= stopwords("de", source ="stopwords-iso"))
tokens_Grüne= tokens_remove(tokens_Grüne, pattern = stopwords("en", source = "stopwords-iso"))
tokens_Grüne= tokens_remove(tokens_Grüne, pattern = "#*")
dictionary_words= dictionary(list(dict=c("rt", "https" , "htt" , "ht","http", "danke" , "bitte" , "dr", "amp" , "via")))
tokens_Grüne= tokens_select(tokens_Grüne, pattern= dictionary_words, selection = "remove")


#SPD
path_data_SPD= system.file("~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/SPD", package = "readtext")
SPD_gesamt= readtext(paste0(path_data_SPD, "~/Documents/AugustoTeixeira/TUM/3. Semester/CSS/R/csv/SPD"), encoding="UTF-8")
SPD_gesamt[2:5]= NULL
SPD_gesamt["Time"]= NULL
SPD_gesamt[4:16]= NULL
SPD_gesamt[,4]=c("SPD")
colnames(SPD_gesamt)=c("doc_id","Date","Text","Partei")
SPD_gesamt[,2]= as.Date(SPD_gesamt[,2])
rownames(SPD_gesamt)= c(1:nrow(SPD_gesamt))

corpus_SPD= corpus(SPD_gesamt, text_field = "Text")
tokens_SPD= tokens(corpus_SPD, remove_punct = T, remove_numbers = T,remove_symbols = T, remove_url = T)
tokens_SPD= tokens_remove(tokens_SPD, pattern= "@*")
tokens_SPD= tokens_remove(tokens_SPD, pattern= stopwords("de", source ="stopwords-iso"))
tokens_SPD= tokens_remove(tokens_SPD, pattern = stopwords("en", source = "stopwords-iso"))
tokens_SPD= tokens_remove(tokens_SPD, pattern = "#*")
dictionary_words= dictionary(list(dict=c("rt", "https" , "htt" , "ht","http", "danke" , "bitte" , "dr","amp" , "via")))
tokens_SPD= tokens_select(tokens_SPD, pattern= dictionary_words, selection = "remove")


    #######
### TOPIC 1 ### 
    #######
topic1 = c("Polizei", "Deutschland")

#LINKE
tokens_Linke_topic1 = tokens_keep(tokens_Linke, pattern = topic1)
dfm_Linke = dfm(tokens_Linke_topic1)
dfm_Linke_topic1 = dfm_group(dfm_Linke, groups = Date)
Tabelle_Linke_topic1 = convert(dfm_Linke_topic1, to="data.frame")
#Topic Spalten zusammen
Tabelle_Linke_topic1= Tabelle_Linke_topic1 %>% mutate(sumrow= polizei+deutschland)
Tabelle_Linke_topic1$Linke= Tabelle_Linke_topic1$sumrow
Tabelle_Linke_topic1[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_Linke_topic1)
Tabelle_Linke_topic1$doc_id= ymd(Tabelle_Linke_topic1$doc_id)
Tabelle_Linke_topic1= Tabelle_Linke_topic1 %>% 
  group_by(month= lubridate::floor_date(doc_id, "month")) %>%
  summarize(Linke= sum(Linke))

#AFD
tokens_AFD_topic1= tokens_keep(tokens_AFD, pattern = topic1)
dfm_AFD= dfm(tokens_AFD_topic1)
dfm_AFD_topic1= dfm_group(dfm_AFD, groups = Date)
Tabelle_AFD_topic1= convert(dfm_AFD_topic1, to="data.frame")
#Topic Spalten zusammen
Tabelle_AFD_topic1= Tabelle_AFD_topic1 %>% mutate(sumrow= polizei+deutschland)
Tabelle_AFD_topic1$AFD= Tabelle_AFD_topic1$sumrow
Tabelle_AFD_topic1[2:4]=NULL
#von Tagen zu Monaten 
str(Tabelle_AFD_topic1)
Tabelle_AFD_topic1$doc_id= ymd(Tabelle_AFD_topic1$doc_id)
Tabelle_AFD_topic1= Tabelle_AFD_topic1 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(AFD = sum(AFD))

#CDU
tokens_CDU_topic1= tokens_keep(tokens_CDU, pattern = topic1)
dfm_CDU=dfm(tokens_CDU_topic1)
dfm_CDU_topic1= dfm_group(dfm_CDU, groups = Date)
Tabelle_CDU_topic1= convert(dfm_CDU_topic1, to="data.frame")
#Topic Spalten zusammen
Tabelle_CDU_topic1= Tabelle_CDU_topic1 %>% mutate(sumrow= polizei+deutschland)
Tabelle_CDU_topic1$CDU= Tabelle_CDU_topic1$sumrow
Tabelle_CDU_topic1[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_CDU_topic1)
Tabelle_CDU_topic1$doc_id= ymd(Tabelle_CDU_topic1$doc_id)
Tabelle_CDU_topic1= Tabelle_CDU_topic1 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CDU = sum(CDU))

#CSU
tokens_CSU_topic1= tokens_keep(tokens_CSU, pattern = topic1)
dfm_CSU= dfm(tokens_CSU_topic1)
dfm_CSU_topic1= dfm_group(dfm_CSU, groups = Date)
Tabelle_CSU_topic1= convert(dfm_CSU_topic1, to="data.frame")
#Topic Spalten zusammen
Tabelle_CSU_topic1= Tabelle_CSU_topic1 %>% mutate(sumrow= polizei+deutschland)
Tabelle_CSU_topic1$CSU= Tabelle_CSU_topic1$sumrow
Tabelle_CSU_topic1[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_CSU_topic1)
Tabelle_CSU_topic1$doc_id<-ymd(Tabelle_CSU_topic1$doc_id)
Tabelle_CSU_topic1= Tabelle_CSU_topic1 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CSU = sum(CSU))

#Grüne
tokens_Grüne_topic1= tokens_keep(tokens_Grüne, pattern = topic1)
dfm_Grüne= dfm(tokens_Grüne_topic1)
dfm_Grüne_topic1= dfm_group(dfm_Grüne, groups = Date)
Tabelle_Grüne_topic1= convert(dfm_Grüne_topic1, to="data.frame")
#Topic Spalten zusammen
Tabelle_Grüne_topic1= Tabelle_Grüne_topic1 %>% mutate(sumrow= polizei+deutschland)
Tabelle_Grüne_topic1$Grüne= Tabelle_Grüne_topic1$sumrow
Tabelle_Grüne_topic1[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_Grüne_topic1)
Tabelle_Grüne_topic1$doc_id= ymd(Tabelle_Grüne_topic1$doc_id)
Tabelle_Grüne_topic1= Tabelle_Grüne_topic1 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(Grüne = sum(Grüne))

#SPD
tokens_SPD_topic1= tokens_keep(tokens_SPD, pattern = topic1)
dfm_SPD= dfm(tokens_SPD_topic1)
dfm_SPD_topic1= dfm_group(dfm_SPD, groups = Date)
Tabelle_SPD_topic1= convert(dfm_SPD_topic1, to="data.frame")
#Topic Spalten zusammen
Tabelle_SPD_topic1= Tabelle_SPD_topic1 %>% mutate(sumrow= polizei+deutschland)
Tabelle_SPD_topic1$SPD= Tabelle_SPD_topic1$sumrow
Tabelle_SPD_topic1[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_SPD_topic1)
Tabelle_SPD_topic1$doc_id= ymd(Tabelle_SPD_topic1$doc_id)
Tabelle_SPD_topic1= Tabelle_SPD_topic1 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(SPD = sum(SPD))

#Alle Parteien
alle_Parteien_topic1= merge(Tabelle_AFD_topic1, 
                            Tabelle_Linke_topic1,
                            by="month")
alle_Parteien_topic1= merge(alle_Parteien_topic1, Tabelle_CDU_topic1,
                            by="month")
alle_Parteien_topic1= merge(alle_Parteien_topic1, Tabelle_CSU_topic1,
                            by="month")
alle_Parteien_topic1= merge(alle_Parteien_topic1, Tabelle_Grüne_topic1,
                            by="month")
alle_Parteien_topic1= merge(alle_Parteien_topic1, Tabelle_SPD_topic1,
                            by="month")

   #####################
### All Parties Graphic ###
   #####################
matplot(alle_Parteien_topic1$month, alle_Parteien_topic1, type = "l", lty = 1, col = 2:7,
        ylab = "Frequency", xlab = "topic1")
grid()
legend("topleft", col = 2:7, legend = c("Linke", "AFD", "CDU", "CSU", "Grüne", "SPD"), lty = 1, bg = "white")

    #######
### TOPIC 2 ### 
    #######

topic2 = c("berlin", "glückwunsch")