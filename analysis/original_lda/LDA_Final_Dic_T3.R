###########################################################################
################################# TOPIC 3 #################################
###########################################################################

#LINKE
tokens_Linke_topic3 = tokens_keep(tokens_Linke, pattern = topic3)
dfm_Linke_3 = dfm(tokens_Linke_topic3)
dfm_Linke_topic3 = dfm_group(dfm_Linke_3, groups = Date)
Tabelle_Linke_topic3 = convert(dfm_Linke_topic3, to="data.frame")
#Topic Spalten zusammen
Tabelle_Linke_topic3= Tabelle_Linke_topic3 %>% mutate(sumrow= bundesregierung+ regierung)
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
Tabelle_AFD_topic3= Tabelle_AFD_topic3 %>% mutate(sumrow= bundesregierung+regierung)
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
Tabelle_CDU_topic3= Tabelle_CDU_topic3 %>% mutate(sumrow= bundesregierung+regierung)
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
Tabelle_CSU_topic3= Tabelle_CSU_topic3 %>% mutate(sumrow= bundesregierung+regierung)
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
Tabelle_Grüne_topic3= Tabelle_Grüne_topic3 %>% mutate(sumrow= bundesregierung+regierung)
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
Tabelle_SPD_topic3= Tabelle_SPD_topic3 %>% mutate(sumrow= bundesregierung+regierung)
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
  ggtitle("Topic 3: Bundesregierung und Regierung")+
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