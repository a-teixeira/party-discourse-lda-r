# LDA Text-Topic Modeling
# Data: Tweets from political actors
# Date: 2012/2018
# Total: 1,588,399

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
dictionary_words= dictionary(list(dict=c("rt", "https" , "htt" , "ht","http", "danke" , "bitte" , "dr","amp" , "via")))
dictionary_words_1= dictionary(list(dict=c("rt", "https" , "htt" , "ht"
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

dictionary_words_2= dictionary(list(dict=c("glückwunsch","tolle","guten","einfach","schön", "u.a.", "prof","finde","gestern", "abend", "fragen","frage","genau", "sagen","klar","eher","leider", "gern","gerne")))

tokens_Linke= tokens_select(tokens_Linke,pattern= dictionary_words, selection = "remove")
tokens_Linke= tokens_select(tokens_Linke,pattern= dictionary_words_1, selection = "remove")
tokens_Linke= tokens_select(tokens_Linke,pattern= dictionary_words_2, selection = "remove")

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
tokens_AFD= tokens_select(tokens_AFD, pattern= dictionary_words_1, selection = "remove")
tokens_AFD= tokens_select(tokens_AFD, pattern= dictionary_words_2, selection = "remove")


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
tokens_CDU= tokens_select(tokens_CDU,pattern= dictionary_words_1, selection ="remove")
tokens_CDU= tokens_select(tokens_CDU,pattern= dictionary_words_2, selection ="remove")


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
tokens_CSU= tokens_select(tokens_CSU,pattern= dictionary_words_1, selection ="remove")
tokens_CSU= tokens_select(tokens_CSU,pattern= dictionary_words_2, selection ="remove")



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
tokens_Grüne= tokens_select(tokens_Grüne,pattern= dictionary_words_1, selection ="remove")
tokens_Grüne= tokens_select(tokens_Grüne,pattern= dictionary_words_2, selection ="remove")

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
tokens_SPD= tokens_select(tokens_SPD, pattern= dictionary_words, selection = "remove")
tokens_SPD= tokens_select(tokens_SPD, pattern= dictionary_words_1, selection = "remove")
tokens_SPD= tokens_select(tokens_SPD, pattern= dictionary_words_2, selection = "remove")


#######################
#### Alle Parteien ####
#######################

Alle_Parteien_gesamt= rbind(AFD_gesamt, Linke_gesamt, CSU_gesamt, SPD_gesamt, Grüne_gesamt, CDU_gesamt)
Alle_Parteien_gesamt[,2]= as.Date(Alle_Parteien_gesamt[,2])
rownames(Alle_Parteien_gesamt)= c(1:nrow(Alle_Parteien_gesamt))
Alle_Parteien_gesamt[1]= NULL

#Corpus & tokens
corpus_bind_Alle= corpus(Alle_Parteien_gesamt, text_field = "Text")
tokens_bind_Alle= tokens(corpus_bind_Alle, remove_punct = T, remove_numbers = T,remove_symbols = T, remove_url = T)
tokens_bind_Alle= tokens_remove(tokens_bind_Alle, pattern= "@*")
tokens_bind_Alle= tokens_remove(tokens_bind_Alle, pattern= stopwords("de", source ="stopwords-iso"))
tokens_bind_Alle= tokens_remove(tokens_bind_Alle, pattern = stopwords("en", source = "stopwords-iso"))
tokens_bind_Alle= tokens_remove(tokens_bind_Alle, pattern = "#*")
tokens_bind_Alle= tokens_tolower(tokens_bind_Alle)
tokens_bind_Alle= tokens_remove(tokens_bind_Alle,pattern= dictionary_words)
tokens_bind_Alle= tokens_remove(tokens_bind_Alle,pattern= dictionary_words_1)
tokens_bind_Alle= tokens_remove(tokens_bind_Alle,pattern= dictionary_words_2)


#DFM
Parteien_seq<-dfm(tokens_bind_Alle)
topfeatures(Parteien_seq)

Parteien_seq_e1<-dfm(tokens_bind_Alle) %>% 
  dfm_trim(min_termfreq = 0.075, termfreq_type = "quantile",
           max_docfreq = 0.9, docfreq_type = "prop")

Parteien_seq_e2<-dfm(tokens_bind_Alle) %>% 
  dfm_trim(min_termfreq = 0.075, termfreq_type = "quantile",
           max_docfreq = 0.9, docfreq_type = "prop")

Parteien_seq_e3<-dfm(tokens_bind_Alle) %>% 
  dfm_trim(min_termfreq = 0.075, termfreq_type = "quantile",
           max_docfreq = 0.9, docfreq_type = "prop")


########################## LDA ###############################
library(seededlda, warn.conflicts = FALSE)

tmod_lda_e1 = textmodel_lda(Parteien_seq_e1, k = 10)
themen_Parteien_e1 = terms(tmod_lda_e1, 10)
View(themen_Parteien_e1)

tmod_lda_e2 = textmodel_lda(Parteien_seq_e1, k = 5)
themen_Parteien_e2 = terms(tmod_lda_e2, 10)
View(themen_Parteien_e2)

###############################################################


###############################
### Posterior Probabilities ###
###############################

# The topic model inference results in two (approximate) posterior probability distributions: 
# a distribution `theta` over K topics within each document and 
# a distribution `beta` over V terms within each topic. (V represents the length of the vocabulary of the collection (V = `r ncol(DTM)`).
# Let's take a closer look at these results:

# Beta Probabilities #

corpus_bind_Alle

DTM_1 <- tokens_bind_Alle %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = 0.01, max_docfreq = NULL, docfreq_type = "prop")
dim(DTM_1)

DTM_1 = DTM_1[,!(colnames(DTM_1) %in% dictionary_words)]


sel_idx <- rowSums(DTM_1) > 0
DTM_1 <- DTM_1[sel_idx, ]
#textdata <- textdata[sel_idx, ]
K= 5

tmod_lda= LDA(DTM_1, K, method = "Gibbs", control = list(iter = 500, seed = 1, verbose = 25))

tmResult= posterior(tmod_lda)
attributes(tmResult)


ncol(DTM_1)
# "Topics" = probability distributions over the entire vocabulary
beta= tmResult$terms
dim(beta)
rowSums(beta)

nrow(DTM_1)
#for every document we have a probability distribution of its contained topics
theta= tmResult$topics
dim(theta)
rowSums(theta)[1:10]


terms(tmod_lda,15)
ex_data_terms=terms(tmod_lda,10)

### Alpha ###
attr(tmod_lda, "alpha") 

tmod_lda_1= LDA(DTM_1, K, method="Gibbs", control=list(iter = 500, verbose = 25, seed = 1, alpha = 0.2))
tmResult= posterior(tmod_lda_1)
theta=tmResult$topics
beta= tmResult$terms

# get topic proportions form example documents
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  


#######################
#######################
#######################




###########################################################################
################################# TOPIC 1 #################################
###########################################################################
topic1 = c( "gesetz","fordert","fall","entscheidung",
           "kritik","antwort")

#LINKE
tokens_Linke_topic1 = tokens_keep(tokens_Linke, pattern = topic1)
dfm_Linke = dfm(tokens_Linke_topic1)
dfm_Linke_topic1 = dfm_group(dfm_Linke, groups = Date)
Tabelle_Linke_topic1 = convert(dfm_Linke_topic1, to="data.frame")
#Topic Spalten zusammen
Tabelle_Linke_topic1= Tabelle_Linke_topic1 %>% mutate(sumrow= gesetz+fordert+fall+entscheidung+
                                                        kritik+antwort+bleibt)
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
Tabelle_AFD_topic1= Tabelle_AFD_topic1 %>% mutate(sumrow= gesetz+fordert+fall+entscheidung+
                                                    kritik+antwort+bleibt)
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
Tabelle_CDU_topic1= Tabelle_CDU_topic1 %>% mutate(sumrow= gesetz+fordert+fall+entscheidung+
                                                    kritik+antwort+bleibt)
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
Tabelle_CSU_topic1= Tabelle_CSU_topic1 %>% mutate(sumrow= gesetz+fordert+fall+entscheidung+
                                                    kritik+antwort+bleibt)
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
Tabelle_Grüne_topic1= Tabelle_Grüne_topic1 %>% mutate(sumrow= gesetz+fordert+fall+entscheidung+
                                                        kritik+antwort+bleibt)
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
Tabelle_SPD_topic1= Tabelle_SPD_topic1 %>% mutate(sumrow= gesetz+fordert+fall+entscheidung+
                                                    kritik+antwort+bleibt)
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

###################################
##### GGPLOT Graphic Topic 1 #####
###################################

colors = c("AFD" = "red", "Linke" = "yellow", "CDU" = "blue",
           "CSU" = "steelblue", "Grüne" = "green", "SPD" = "pink")
ggp_topic1= ggplot(as.data.frame(alle_Parteien_topic1), aes(x=month)) +
  ggtitle("Topic 1: Gesetz/Fordert/Fall/Entscheidung/Kritik/Antwort/Bleibt")+
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

###########################################################################
################################# TOPIC 2 #################################
###########################################################################
topic2 = c("deutschland", "euro")

#LINKE
tokens_Linke_topic2 = tokens_keep(tokens_Linke, pattern = topic2)
dfm_Linke_2 = dfm(tokens_Linke_topic2)
dfm_Linke_topic2 = dfm_group(dfm_Linke_2, groups = Date)
Tabelle_Linke_topic2 = convert(dfm_Linke_topic2, to="data.frame")
#Topic Spalten zusammen
Tabelle_Linke_topic2= Tabelle_Linke_topic2 %>% mutate(sumrow= deutschland+euro)
Tabelle_Linke_topic2$Linke= Tabelle_Linke_topic2$sumrow
Tabelle_Linke_topic2[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_Linke_topic2)
Tabelle_Linke_topic2$doc_id= ymd(Tabelle_Linke_topic2$doc_id)
Tabelle_Linke_topic2= Tabelle_Linke_topic2 %>% 
  group_by(month= lubridate::floor_date(doc_id, "month")) %>%
  summarize(Linke= sum(Linke))

#AFD
tokens_AFD_topic2= tokens_keep(tokens_AFD, pattern = topic2)
dfm_AFD_2= dfm(tokens_AFD_topic2)
dfm_AFD_topic2= dfm_group(dfm_AFD_2, groups = Date)
Tabelle_AFD_topic2= convert(dfm_AFD_topic2, to="data.frame")
#Topic Spalten zusammen
Tabelle_AFD_topic2= Tabelle_AFD_topic2 %>% mutate(sumrow= deutschland+euro)
Tabelle_AFD_topic2$AFD= Tabelle_AFD_topic2$sumrow
Tabelle_AFD_topic2[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_AFD_topic2)
Tabelle_AFD_topic2$doc_id= ymd(Tabelle_AFD_topic2$doc_id)
Tabelle_AFD_topic2= Tabelle_AFD_topic2 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(AFD = sum(AFD))

#CDU
tokens_CDU_topic2= tokens_keep(tokens_CDU, pattern = topic2)
dfm_CDU_2=dfm(tokens_CDU_topic2)
dfm_CDU_topic2= dfm_group(dfm_CDU_2, groups = Date)
Tabelle_CDU_topic2= convert(dfm_CDU_topic2, to="data.frame")
#Topic Spalten zusammen
Tabelle_CDU_topic2= Tabelle_CDU_topic2 %>% mutate(sumrow= deutschland+euro)
Tabelle_CDU_topic2$CDU= Tabelle_CDU_topic2$sumrow
Tabelle_CDU_topic2[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_CDU_topic2)
Tabelle_CDU_topic2$doc_id= ymd(Tabelle_CDU_topic2$doc_id)
Tabelle_CDU_topic2= Tabelle_CDU_topic2 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CDU = sum(CDU))

#CSU
tokens_CSU_topic2= tokens_keep(tokens_CSU, pattern = topic2)
dfm_CSU_2= dfm(tokens_CSU_topic2)
dfm_CSU_topic2= dfm_group(dfm_CSU_2, groups = Date)
Tabelle_CSU_topic2= convert(dfm_CSU_topic2, to="data.frame")
#Topic Spalten zusammen
Tabelle_CSU_topic2= Tabelle_CSU_topic2 %>% mutate(sumrow= deutschland+euro)
Tabelle_CSU_topic2$CSU= Tabelle_CSU_topic2$sumrow
Tabelle_CSU_topic2[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_CSU_topic2)
Tabelle_CSU_topic2$doc_id= ymd(Tabelle_CSU_topic2$doc_id)
Tabelle_CSU_topic2= Tabelle_CSU_topic2 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CSU = sum(CSU))

#Grüne
tokens_Grüne_topic2= tokens_keep(tokens_Grüne, pattern = topic2)
dfm_Grüne_2= dfm(tokens_Grüne_topic2)
dfm_Grüne_topic2= dfm_group(dfm_Grüne_2, groups = Date)
Tabelle_Grüne_topic2= convert(dfm_Grüne_topic2, to="data.frame")
#Topic Spalten zusammen
Tabelle_Grüne_topic2= Tabelle_Grüne_topic2 %>% mutate(sumrow= deutschland+euro)
Tabelle_Grüne_topic2$Grüne= Tabelle_Grüne_topic2$sumrow
Tabelle_Grüne_topic2[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_Grüne_topic2)
Tabelle_Grüne_topic2$doc_id= ymd(Tabelle_Grüne_topic2$doc_id)
Tabelle_Grüne_topic2= Tabelle_Grüne_topic2 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(Grüne = sum(Grüne))

#SPD
tokens_SPD_topic2= tokens_keep(tokens_SPD, pattern = topic2)
dfm_SPD_2= dfm(tokens_SPD_topic2)
dfm_SPD_topic2= dfm_group(dfm_SPD_2, groups = Date)
Tabelle_SPD_topic2= convert(dfm_SPD_topic2, to="data.frame")
#Topic Spalten zusammen
Tabelle_SPD_topic2= Tabelle_SPD_topic2 %>% mutate(sumrow= deutschland+euro)
Tabelle_SPD_topic2$SPD= Tabelle_SPD_topic2$sumrow
Tabelle_SPD_topic2[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_SPD_topic2)
Tabelle_SPD_topic2$doc_id= ymd(Tabelle_SPD_topic2$doc_id)
Tabelle_SPD_topic2= Tabelle_SPD_topic2 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(SPD = sum(SPD))

#Alle Parteien
alle_Parteien_topic2= merge(Tabelle_AFD_topic2, Tabelle_Linke_topic2,
                            by="month")
alle_Parteien_topic2= merge(alle_Parteien_topic2, Tabelle_CDU_topic2,
                            by="month")
alle_Parteien_topic2= merge(alle_Parteien_topic2, Tabelle_CSU_topic2,
                            by="month")
alle_Parteien_topic2= merge(alle_Parteien_topic2, Tabelle_Grüne_topic2,
                            by="month")
alle_Parteien_topic2= merge(alle_Parteien_topic2, Tabelle_SPD_topic2,
                            by="month")

###################################
##### GGPLOT Graphic Topic 2 #####
###################################

colors = c("AFD" = "red", "Linke" = "yellow", "CDU" = "blue",
           "CSU" = "steelblue", "Grüne" = "green", "SPD" = "pink")
ggp_topic2= ggplot(as.data.frame(alle_Parteien_topic2), aes(x=month)) +
  ggtitle("Topic 2: Deutschland und Euro")+
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
  
plot(ggp_topic2)


###########################################################################
################################# TOPIC 3 #################################
###########################################################################

topic3 = c("bundesregierung", "fordert")

#LINKE
tokens_Linke_topic3 = tokens_keep(tokens_Linke, pattern = topic3)
dfm_Linke_3 = dfm(tokens_Linke_topic3)
dfm_Linke_topic3 = dfm_group(dfm_Linke_3, groups = Date)
Tabelle_Linke_topic3 = convert(dfm_Linke_topic3, to="data.frame")
#Topic Spalten zusammen
Tabelle_Linke_topic3= Tabelle_Linke_topic3 %>% mutate(sumrow= bundesregierung + fordert)
Tabelle_Linke_topic3$Linke= Tabelle_Linke_topic3$sumrow
 Tabelle_Linke_topic3[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_Linke_topic3)
Tabelle_Linke_topic3$doc_id= ymd(Tabelle_Linke_topic3$doc_id)
Tabelle_Linke_topic3= Tabelle_Linke_topic3 %>% 
  group_by(month= lubridate::floor_date(doc_id, "month")) %>%
  summarize(Linke= sum(Linke))

#AFD
tokens_AFD_topic3= tokens_keep(tokens_AFD, pattern = topic3)
dfm_AFD_3= dfm(tokens_AFD_topic3)
dfm_AFD_topic3= dfm_group(dfm_AFD_3, groups = Date)
Tabelle_AFD_topic3= convert(dfm_AFD_topic3, to="data.frame")
#Topic Spalten zusammen
Tabelle_AFD_topic3= Tabelle_AFD_topic3 %>% mutate(sumrow= bundesregierung+fordert)
Tabelle_AFD_topic3$AFD= Tabelle_AFD_topic3$sumrow
Tabelle_AFD_topic3[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_AFD_topic3)
Tabelle_AFD_topic3$doc_id= ymd(Tabelle_AFD_topic3$doc_id)
Tabelle_AFD_topic3= Tabelle_AFD_topic3 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(AFD = sum(AFD))

#CDU
tokens_CDU_topic3= tokens_keep(tokens_CDU, pattern = topic3)
dfm_CDU_3=dfm(tokens_CDU_topic3)
dfm_CDU_topic3= dfm_group(dfm_CDU_3, groups = Date)
Tabelle_CDU_topic3= convert(dfm_CDU_topic3, to="data.frame")
#Topic Spalten zusammen
Tabelle_CDU_topic3= Tabelle_CDU_topic3 %>% mutate(sumrow= bundesregierung+fordert)
Tabelle_CDU_topic3$CDU= Tabelle_CDU_topic3$sumrow
Tabelle_CDU_topic3[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_CDU_topic3)
Tabelle_CDU_topic3$doc_id= ymd(Tabelle_CDU_topic3$doc_id)
Tabelle_CDU_topic3= Tabelle_CDU_topic3 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CDU = sum(CDU))

#CSU
tokens_CSU_topic3= tokens_keep(tokens_CSU, pattern = topic3)
dfm_CSU_3= dfm(tokens_CSU_topic3)
dfm_CSU_topic3= dfm_group(dfm_CSU_3, groups = Date)
Tabelle_CSU_topic3= convert(dfm_CSU_topic3, to="data.frame")
#Topic Spalten zusammen
Tabelle_CSU_topic3= Tabelle_CSU_topic3 %>% mutate(sumrow= bundesregierung+fordert)
Tabelle_CSU_topic3$CSU= Tabelle_CSU_topic3$sumrow
Tabelle_CSU_topic3[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_CSU_topic3)
Tabelle_CSU_topic3$doc_id= ymd(Tabelle_CSU_topic3$doc_id)
Tabelle_CSU_topic3= Tabelle_CSU_topic3 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CSU = sum(CSU))

#Grüne
tokens_Grüne_topic3= tokens_keep(tokens_Grüne, pattern = topic3)
dfm_Grüne_3= dfm(tokens_Grüne_topic3)
dfm_Grüne_topic3= dfm_group(dfm_Grüne_3, groups = Date)
Tabelle_Grüne_topic3= convert(dfm_Grüne_topic3, to="data.frame")
#Topic Spalten zusammen
Tabelle_Grüne_topic3= Tabelle_Grüne_topic3 %>% mutate(sumrow= bundesregierung+fordert)
Tabelle_Grüne_topic3$Grüne= Tabelle_Grüne_topic3$sumrow
Tabelle_Grüne_topic3[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_Grüne_topic3)
Tabelle_Grüne_topic3$doc_id= ymd(Tabelle_Grüne_topic3$doc_id)
Tabelle_Grüne_topic3= Tabelle_Grüne_topic3 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(Grüne = sum(Grüne))

#SPD
tokens_SPD_topic3= tokens_keep(tokens_SPD, pattern = topic3)
dfm_SPD_3= dfm(tokens_SPD_topic3)
dfm_SPD_topic3= dfm_group(dfm_SPD_3, groups = Date)
Tabelle_SPD_topic3= convert(dfm_SPD_topic3, to="data.frame")
#Topic Spalten zusammen
Tabelle_SPD_topic3= Tabelle_SPD_topic3 %>% mutate(sumrow= bundesregierung+fordert)
Tabelle_SPD_topic3$SPD= Tabelle_SPD_topic3$sumrow
Tabelle_SPD_topic3[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_SPD_topic3)
Tabelle_SPD_topic3$doc_id= ymd(Tabelle_SPD_topic3$doc_id)
Tabelle_SPD_topic3= Tabelle_SPD_topic3 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(SPD = sum(SPD))

#Alle Parteien
alle_Parteien_topic3= merge(Tabelle_AFD_topic3, Tabelle_Linke_topic3,
                            by="month")
alle_Parteien_topic3= merge(alle_Parteien_topic3, Tabelle_CDU_topic3,
                            by="month")
alle_Parteien_topic3= merge(alle_Parteien_topic3, Tabelle_CSU_topic3,
                            by="month")
alle_Parteien_topic3= merge(alle_Parteien_topic3, Tabelle_Grüne_topic3,
                            by="month")
alle_Parteien_topic3= merge(alle_Parteien_topic3, Tabelle_SPD_topic3,
                            by="month")

###################################
##### GGPLOT Graphic Topic 3 #####
###################################

colors = c("AFD" = "red", "Linke" = "yellow", "CDU" = "blue",
           "CSU" = "steelblue", "Grüne" = "green", "SPD" = "pink")
ggp_topic3= ggplot(as.data.frame(alle_Parteien_topic3), aes(x=month)) +
  ggtitle("Topic 3: Bundesregierung und Fordert")+
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

plot(ggp_topic3)


###########################################################################
################################# TOPIC 4 #################################
###########################################################################

topic4 = c("deutschland", "europa","deutsche","eu","welt","flüchtlinge","land","deutschen","türkei")

#LINKE
tokens_Linke_topic4 = tokens_keep(tokens_Linke, pattern = topic4)
dfm_Linke_4 = dfm(tokens_Linke_topic4)
dfm_Linke_topic4 = dfm_group(dfm_Linke_4, groups = Date)
Tabelle_Linke_topic4 = convert(dfm_Linke_topic4, to="data.frame")
#Topic Spalten zusammen
Tabelle_Linke_topic4= Tabelle_Linke_topic4 %>% mutate(sumrow= deutschland+eu+europa+deutsche+welt+
                                                        flüchtlinge+land+deutschen+türkei)
Tabelle_Linke_topic4$Linke= Tabelle_Linke_topic4$sumrow
Tabelle_Linke_topic4[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_Linke_topic4)
Tabelle_Linke_topic4$doc_id= ymd(Tabelle_Linke_topic4$doc_id)
Tabelle_Linke_topic4= Tabelle_Linke_topic4 %>% 
  group_by(month= lubridate::floor_date(doc_id, "month")) %>%
  summarize(Linke= sum(Linke))

#AFD
tokens_AFD_topic4= tokens_keep(tokens_AFD, pattern = topic4)
dfm_AFD_4= dfm(tokens_AFD_topic4)
dfm_AFD_topic4= dfm_group(dfm_AFD_4, groups = Date)
Tabelle_AFD_topic4= convert(dfm_AFD_topic4, to="data.frame")
#Topic Spalten zusammen
Tabelle_AFD_topic4= Tabelle_AFD_topic4 %>% mutate(sumrow= deutschland+eu+europa+deutsche+welt+
                                                    flüchtlinge+land+deutschen+türkei)
Tabelle_AFD_topic4$AFD= Tabelle_AFD_topic4$sumrow
Tabelle_AFD_topic4[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_AFD_topic4)
Tabelle_AFD_topic4$doc_id= ymd(Tabelle_AFD_topic4$doc_id)
Tabelle_AFD_topic4= Tabelle_AFD_topic4 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(AFD = sum(AFD))

#CDU
tokens_CDU_topic4= tokens_keep(tokens_CDU, pattern = topic4)
dfm_CDU_4=dfm(tokens_CDU_topic4)
dfm_CDU_topic4= dfm_group(dfm_CDU_4, groups = Date)
Tabelle_CDU_topic4= convert(dfm_CDU_topic4, to="data.frame")
#Topic Spalten zusammen
Tabelle_CDU_topic4= Tabelle_CDU_topic4 %>% mutate(sumrow= deutschland+eu+europa+deutsche+welt+
                                                    flüchtlinge+land+deutschen+türkei)
Tabelle_CDU_topic4$CDU= Tabelle_CDU_topic4$sumrow
Tabelle_CDU_topic4[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_CDU_topic4)
Tabelle_CDU_topic4$doc_id= ymd(Tabelle_CDU_topic4$doc_id)
Tabelle_CDU_topic4= Tabelle_CDU_topic4 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CDU = sum(CDU))

#CSU
tokens_CSU_topic4= tokens_keep(tokens_CSU, pattern = topic4)
dfm_CSU_4= dfm(tokens_CSU_topic4)
dfm_CSU_topic4= dfm_group(dfm_CSU_4, groups = Date)
Tabelle_CSU_topic4= convert(dfm_CSU_topic4, to="data.frame")
#Topic Spalten zusammen
Tabelle_CSU_topic4= Tabelle_CSU_topic4 %>% mutate(sumrow= deutschland+eu+europa+deutsche+welt+
                                                    flüchtlinge+land+deutschen+türkei)
Tabelle_CSU_topic4$CSU= Tabelle_CSU_topic4$sumrow
Tabelle_CSU_topic4[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_CSU_topic4)
Tabelle_CSU_topic4$doc_id= ymd(Tabelle_CSU_topic4$doc_id)
Tabelle_CSU_topic4= Tabelle_CSU_topic4 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CSU = sum(CSU))

#Grüne
tokens_Grüne_topic4= tokens_keep(tokens_Grüne, pattern = topic4)
dfm_Grüne_4= dfm(tokens_Grüne_topic4)
dfm_Grüne_topic4= dfm_group(dfm_Grüne_4, groups = Date)
Tabelle_Grüne_topic4= convert(dfm_Grüne_topic4, to="data.frame")
#Topic Spalten zusammen
Tabelle_Grüne_topic4= Tabelle_Grüne_topic4 %>% mutate(sumrow= deutschland+eu+europa+deutsche+welt+
                                                        flüchtlinge+land+deutschen+türkei)
Tabelle_Grüne_topic4$Grüne= Tabelle_Grüne_topic4$sumrow
Tabelle_Grüne_topic4[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_Grüne_topic4)
Tabelle_Grüne_topic4$doc_id= ymd(Tabelle_Grüne_topic4$doc_id)
Tabelle_Grüne_topic4= Tabelle_Grüne_topic4 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(Grüne = sum(Grüne))

#SPD
tokens_SPD_topic4= tokens_keep(tokens_SPD, pattern = topic4)
dfm_SPD_4= dfm(tokens_SPD_topic4)
dfm_SPD_topic4= dfm_group(dfm_SPD_4, groups = Date)
Tabelle_SPD_topic4= convert(dfm_SPD_topic4, to="data.frame")
#Topic Spalten zusammen
Tabelle_SPD_topic4= Tabelle_SPD_topic4 %>% mutate(sumrow= deutschland+eu+europa+deutsche+welt+
                                                    flüchtlinge+land+deutschen+türkei)
Tabelle_SPD_topic4$SPD= Tabelle_SPD_topic4$sumrow
Tabelle_SPD_topic4[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_SPD_topic4)
Tabelle_SPD_topic4$doc_id= ymd(Tabelle_SPD_topic4$doc_id)
Tabelle_SPD_topic4= Tabelle_SPD_topic4 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(SPD = sum(SPD))

#Alle Parteien
alle_Parteien_topic4= merge(Tabelle_AFD_topic4, Tabelle_Linke_topic4,
                            by="month")
alle_Parteien_topic4= merge(alle_Parteien_topic4, Tabelle_CDU_topic4,
                            by="month")
alle_Parteien_topic4= merge(alle_Parteien_topic4, Tabelle_CSU_topic4,
                            by="month")
alle_Parteien_topic4= merge(alle_Parteien_topic4, Tabelle_Grüne_topic4,
                            by="month")
alle_Parteien_topic4= merge(alle_Parteien_topic4, Tabelle_SPD_topic4,
                            by="month")

###################################
##### GGPLOT Graphic Topic 4 #####
###################################

colors = c("AFD" = "red", "Linke" = "yellow", "CDU" = "blue",
           "CSU" = "steelblue", "Grüne" = "green", "SPD" = "pink")
ggp_topic4= ggplot(as.data.frame(alle_Parteien_topic4), aes(x=month)) +
  ggtitle("Topic 4: Deutschland/Europa/Land/Türkei")+
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


###########################################################################
################################# TOPIC 5 #################################
###########################################################################

topic5 = c("thema", "bundestag")

#LINKE
tokens_Linke_topic5 = tokens_keep(tokens_Linke, pattern = topic5)
dfm_Linke_5 = dfm(tokens_Linke_topic5)
dfm_Linke_topic5 = dfm_group(dfm_Linke_5, groups = Date)
Tabelle_Linke_topic5 = convert(dfm_Linke_topic5, to="data.frame")
#Topic Spalten zusammen
Tabelle_Linke_topic5= Tabelle_Linke_topic5 %>% mutate(sumrow= thema + bundestag)
Tabelle_Linke_topic5$Linke= Tabelle_Linke_topic5$sumrow
Tabelle_Linke_topic5[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_Linke_topic5)
Tabelle_Linke_topic5$doc_id= ymd(Tabelle_Linke_topic5$doc_id)
Tabelle_Linke_topic5= Tabelle_Linke_topic5 %>% 
  group_by(month= lubridate::floor_date(doc_id, "month")) %>%
  summarize(Linke= sum(Linke))

#AFD
tokens_AFD_topic5= tokens_keep(tokens_AFD, pattern = topic5)
dfm_AFD_5= dfm(tokens_AFD_topic5)
dfm_AFD_topic5= dfm_group(dfm_AFD_5, groups = Date)
Tabelle_AFD_topic5= convert(dfm_AFD_topic5, to="data.frame")
#Topic Spalten zusammen
Tabelle_AFD_topic5= Tabelle_AFD_topic5 %>% mutate(sumrow= thema + bundestag)
Tabelle_AFD_topic5$AFD= Tabelle_AFD_topic5$sumrow
Tabelle_AFD_topic5[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_AFD_topic5)
Tabelle_AFD_topic5$doc_id= ymd(Tabelle_AFD_topic5$doc_id)
Tabelle_AFD_topic5= Tabelle_AFD_topic5 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(AFD = sum(AFD))

#CDU
tokens_CDU_topic5= tokens_keep(tokens_CDU, pattern = topic5)
dfm_CDU_5=dfm(tokens_CDU_topic5)
dfm_CDU_topic5= dfm_group(dfm_CDU_5, groups = Date)
Tabelle_CDU_topic5= convert(dfm_CDU_topic5, to="data.frame")
#Topic Spalten zusammen
Tabelle_CDU_topic5= Tabelle_CDU_topic5 %>% mutate(sumrow= thema + bundestag)
Tabelle_CDU_topic5$CDU= Tabelle_CDU_topic5$sumrow
Tabelle_CDU_topic5[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_CDU_topic5)
Tabelle_CDU_topic5$doc_id= ymd(Tabelle_CDU_topic5$doc_id)
Tabelle_CDU_topic5= Tabelle_CDU_topic5 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CDU = sum(CDU))

#CSU
tokens_CSU_topic5= tokens_keep(tokens_CSU, pattern = topic5)
dfm_CSU_5= dfm(tokens_CSU_topic5)
dfm_CSU_topic5= dfm_group(dfm_CSU_5, groups = Date)
Tabelle_CSU_topic5= convert(dfm_CSU_topic5, to="data.frame")
#Topic Spalten zusammen
Tabelle_CSU_topic5= Tabelle_CSU_topic5 %>% mutate(sumrow= thema + bundestag)
Tabelle_CSU_topic5$CSU= Tabelle_CSU_topic5$sumrow
Tabelle_CSU_topic5[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_CSU_topic5)
Tabelle_CSU_topic5$doc_id= ymd(Tabelle_CSU_topic5$doc_id)
Tabelle_CSU_topic5= Tabelle_CSU_topic5 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CSU = sum(CSU))

#Grüne
tokens_Grüne_topic5= tokens_keep(tokens_Grüne, pattern = topic5)
dfm_Grüne_5= dfm(tokens_Grüne_topic5)
dfm_Grüne_topic5= dfm_group(dfm_Grüne_5, groups = Date)
Tabelle_Grüne_topic5= convert(dfm_Grüne_topic5, to="data.frame")
#Topic Spalten zusammen
Tabelle_Grüne_topic5= Tabelle_Grüne_topic5 %>% mutate(sumrow= thema + bundestag)
Tabelle_Grüne_topic5$Grüne= Tabelle_Grüne_topic5$sumrow
Tabelle_Grüne_topic5[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_Grüne_topic5)
Tabelle_Grüne_topic5$doc_id= ymd(Tabelle_Grüne_topic5$doc_id)
Tabelle_Grüne_topic5= Tabelle_Grüne_topic5 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(Grüne = sum(Grüne))

#SPD
tokens_SPD_topic5= tokens_keep(tokens_SPD, pattern = topic5)
dfm_SPD_5= dfm(tokens_SPD_topic5)
dfm_SPD_topic5= dfm_group(dfm_SPD_5, groups = Date)
Tabelle_SPD_topic5= convert(dfm_SPD_topic5, to="data.frame")
#Topic Spalten zusammen
Tabelle_SPD_topic5= Tabelle_SPD_topic5 %>% mutate(sumrow= thema + bundestag)
Tabelle_SPD_topic5$SPD= Tabelle_SPD_topic5$sumrow
Tabelle_SPD_topic5[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_SPD_topic5)
Tabelle_SPD_topic5$doc_id= ymd(Tabelle_SPD_topic5$doc_id)
Tabelle_SPD_topic5= Tabelle_SPD_topic5 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(SPD = sum(SPD))

#Alle Parteien
alle_Parteien_topic5= merge(Tabelle_AFD_topic5, Tabelle_Linke_topic5,
                            by="month")
alle_Parteien_topic5= merge(alle_Parteien_topic5, Tabelle_CDU_topic5,
                            by="month")
alle_Parteien_topic5= merge(alle_Parteien_topic5, Tabelle_CSU_topic5,
                            by="month")
alle_Parteien_topic5= merge(alle_Parteien_topic5, Tabelle_Grüne_topic5,
                            by="month")
alle_Parteien_topic5= merge(alle_Parteien_topic5, Tabelle_SPD_topic5,
                            by="month")

###################################
##### GGPLOT Graphic Topic 5 #####
###################################

colors = c("AFD" = "red", "Linke" = "yellow", "CDU" = "blue",
           "CSU" = "steelblue", "Grüne" = "green", "SPD" = "pink")
ggp_topic5= ggplot(as.data.frame(alle_Parteien_topic5), aes(x=month)) +
  ggtitle("Topic 5: Thema und Bundestag")+
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

plot(ggp_topic5)




###########################################################################
################################# TOPIC 7 #################################
###########################################################################

topic7 = c("frauen", "leben","deutschland","demokratie", "gewalt",
           "opfer", "polizei","freiheit", "nazis", "gesellschaft")

#LINKE
tokens_Linke_topic7 = tokens_keep(tokens_Linke, pattern = topic7)
dfm_Linke_7 = dfm(tokens_Linke_topic7)
dfm_Linke_topic7 = dfm_group(dfm_Linke_7, groups = Date)
Tabelle_Linke_topic7 = convert(dfm_Linke_topic7, to="data.frame")
#Topic Spalten zusammen
Tabelle_Linke_topic7= Tabelle_Linke_topic7 %>% mutate(sumrow= frauen+leben+deutschland+demokratie+gewalt+
                                                        opfer+polizei+freiheit+nazis+gesellschaft)
Tabelle_Linke_topic7$Linke= Tabelle_Linke_topic7$sumrow
Tabelle_Linke_topic7[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_Linke_topic7)
Tabelle_Linke_topic7$doc_id= ymd(Tabelle_Linke_topic7$doc_id)
Tabelle_Linke_topic7= Tabelle_Linke_topic7 %>% 
  group_by(month= lubridate::floor_date(doc_id, "month")) %>%
  summarize(Linke= sum(Linke))

#AFD
tokens_AFD_topic7= tokens_keep(tokens_AFD, pattern = topic7)
dfm_AFD_7= dfm(tokens_AFD_topic7)
dfm_AFD_topic7= dfm_group(dfm_AFD_7, groups = Date)
Tabelle_AFD_topic7= convert(dfm_AFD_topic7, to="data.frame")
#Topic Spalten zusammen
Tabelle_AFD_topic7= Tabelle_AFD_topic7 %>% mutate(sumrow= frauen+leben+deutschland+demokratie+gewalt+
                                                    opfer+polizei+freiheit+nazis+gesellschaft)
Tabelle_AFD_topic7$AFD= Tabelle_AFD_topic7$sumrow
Tabelle_AFD_topic7[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_AFD_topic7)
Tabelle_AFD_topic7$doc_id= ymd(Tabelle_AFD_topic7$doc_id)
Tabelle_AFD_topic7= Tabelle_AFD_topic7 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(AFD = sum(AFD))

#CDU
tokens_CDU_topic7= tokens_keep(tokens_CDU, pattern = topic7)
dfm_CDU_7=dfm(tokens_CDU_topic7)
dfm_CDU_topic7= dfm_group(dfm_CDU_7, groups = Date)
Tabelle_CDU_topic7= convert(dfm_CDU_topic7, to="data.frame")
#Topic Spalten zusammen
Tabelle_CDU_topic7= Tabelle_CDU_topic7 %>% mutate(sumrow= frauen+leben+deutschland+demokratie+gewalt+
                                                    opfer+polizei+freiheit+nazis+gesellschaft)
Tabelle_CDU_topic7$CDU= Tabelle_CDU_topic7$sumrow
Tabelle_CDU_topic7[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_CDU_topic7)
Tabelle_CDU_topic7$doc_id= ymd(Tabelle_CDU_topic7$doc_id)
Tabelle_CDU_topic7= Tabelle_CDU_topic7 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CDU = sum(CDU))

#CSU
tokens_CSU_topic7= tokens_keep(tokens_CSU, pattern = topic7)
dfm_CSU_7= dfm(tokens_CSU_topic7)
dfm_CSU_topic7= dfm_group(dfm_CSU_7, groups = Date)
Tabelle_CSU_topic7= convert(dfm_CSU_topic7, to="data.frame")
#Topic Spalten zusammen
Tabelle_CSU_topic7= Tabelle_CSU_topic7 %>% mutate(sumrow= frauen+leben+deutschland+demokratie+gewalt+
                                                    opfer+polizei+freiheit+nazis+gesellschaft)
Tabelle_CSU_topic7$CSU= Tabelle_CSU_topic7$sumrow
Tabelle_CSU_topic7[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_CSU_topic7)
Tabelle_CSU_topic7$doc_id= ymd(Tabelle_CSU_topic7$doc_id)
Tabelle_CSU_topic7= Tabelle_CSU_topic7 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(CSU = sum(CSU))

#Grüne
tokens_Grüne_topic7= tokens_keep(tokens_Grüne, pattern = topic7)
dfm_Grüne_7= dfm(tokens_Grüne_topic7)
dfm_Grüne_topic7= dfm_group(dfm_Grüne_7, groups = Date)
Tabelle_Grüne_topic7= convert(dfm_Grüne_topic7, to="data.frame")
#Topic Spalten zusammen
Tabelle_Grüne_topic7= Tabelle_Grüne_topic7 %>% mutate(sumrow= frauen+leben+deutschland+demokratie+gewalt+
                                                        opfer+polizei+freiheit+nazis+gesellschaft)
Tabelle_Grüne_topic7$Grüne= Tabelle_Grüne_topic7$sumrow
Tabelle_Grüne_topic7[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_Grüne_topic7)
Tabelle_Grüne_topic7$doc_id= ymd(Tabelle_Grüne_topic7$doc_id)
Tabelle_Grüne_topic7= Tabelle_Grüne_topic7 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(Grüne = sum(Grüne))

#SPD
tokens_SPD_topic7= tokens_keep(tokens_SPD, pattern = topic7)
dfm_SPD_7= dfm(tokens_SPD_topic7)
dfm_SPD_topic7= dfm_group(dfm_SPD_7, groups = Date)
Tabelle_SPD_topic7= convert(dfm_SPD_topic7, to="data.frame")
#Topic Spalten zusammen
Tabelle_SPD_topic7= Tabelle_SPD_topic7 %>% mutate(sumrow= frauen+leben+deutschland+demokratie+gewalt+
                                                    opfer+polizei+freiheit+nazis+gesellschaft)
Tabelle_SPD_topic7$SPD= Tabelle_SPD_topic7$sumrow
Tabelle_SPD_topic7[2:4]= NULL
#von Tagen zu Monaten 
str(Tabelle_SPD_topic7)
Tabelle_SPD_topic7$doc_id= ymd(Tabelle_SPD_topic7$doc_id)
Tabelle_SPD_topic7= Tabelle_SPD_topic7 %>% 
  group_by(month = lubridate::floor_date(doc_id, "month")) %>%
  summarize(SPD = sum(SPD))

#Alle Parteien
alle_Parteien_topic7= merge(Tabelle_AFD_topic7, Tabelle_Linke_topic7,by="month")
alle_Parteien_topic7= merge(alle_Parteien_topic7, Tabelle_CDU_topic7,by="month")
alle_Parteien_topic7= merge(alle_Parteien_topic7, Tabelle_CSU_topic7,by="month")
alle_Parteien_topic7= merge(alle_Parteien_topic7, Tabelle_Grüne_topic7,by="month")
alle_Parteien_topic7= merge(alle_Parteien_topic7, Tabelle_SPD_topic7,by="month")

###################################
##### GGPLOT Graphic Topic 7 #####
###################################

colors = c("AFD" = "red", "Linke" = "yellow", "CDU" = "blue",
           "CSU" = "steelblue", "Grüne" = "green", "SPD" = "pink")
ggp_topic7= ggplot(as.data.frame(alle_Parteien_topic7), aes(x=month)) +
  ggtitle("Topic 7: Frauen, Leben, Deutschland, Demokratie, Polizei, Gewalt")+
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

plot(ggp_topic7)









colors = c("AFD" = "red", "Linke" = "yellow",
           "CDU" = "blue","CSU" = "steelblue",
           "Grüne" = "green", "SPD" = "pink",
           "Andere Parteien"= "brown")

ggp_topic1= ggplot(as.data.frame(alle_Parteien_topic1), aes(x=month))+
  ggtitle("Topic1 Verteilungspolitik")+
  geom_line(aes(y = row_AfD, color = "AFD"), size = 0.4) + 
  geom_line(linetype="dotdash",aes(y = row_Linke, color = "Linke"),size = 0.4) + 
  geom_line(linetype= "dotdash",aes(y = row_CDU, color = "CDU"), size = 0.4) + 
  geom_line(linetype= "dotdash",aes(y = row_CSU, color = "CSU"), size = 0.4) + 
  geom_line(linetype = "dotdash", aes(y = row_Grüne, color = "Grüne"), size = 0.4) + 
  geom_line(linetype = "dotdash", aes(y = row_SPD, color = "SPD"), size = 0.4)+
  geom_line(aes(y= keine_Nazi1, color="nicht populistische Parteien"), size = 0.8)+
  labs(x = "Jahr",y = "Prozent", color = "Legend") +
  scale_color_manual(values = colors)
plot(ggp_topic1)
alle_Parteien_topic1$keine_Nazi1<-rowSums(alle_Parteien_topic1[,3:7])/5
