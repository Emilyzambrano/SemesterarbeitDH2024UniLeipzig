#install.packages("cwbtools")
library("cwbtools")
library(polmineR)
library(tidyverse)
library(quanteda)
library(reprex)

cwbtools::corpus_install(doi = "10.5281/zenodo.10416536")

filter_data <- polmineR::corpus("GERMAPARL2") %>%
  polmineR::subset(protocol_lp == 19) %>%
  polmineR::subset(speaker_party == "AfD") 

##gefilterte Datenkorpus wird in das Speeches-Format umgewandelt:

speeches_data <- filter_data %>% as.speeches(
  s_attribute_date = "protocol_date",
  s_attribute_name = "speaker_name"
) 


##Kontextdaten(Metadaten): Datum und Sprecher werden aus Speeches weggemacht und in Datenrahmen (tibble) gespeichert.
meta <- lapply(speeches_data,FUN = function(x) {
  s_attributes(x, s_attribute = c("protocol_date", "speaker_name"))[,1:2] %>%
    as_tibble() %>%
    set_names(c("date","speaker"))  
})  %>% bind_rows() 


##Funktion terms extrahiert die Terme aus den Textdaten, Ausdruck lapply(speeches_data, FUN = terms, p_attribute = "word") führt die Funktion terms auf jedem Element von speeches_data aus

text <- lapply(speeches_data,FUN = terms,p_attribute = "word") %>% quanteda::tokens()

##Zusammenführen von Metadaten und Textdaten:
fulltext_dataset <- meta %>% mutate(text = lapply(text, paste,sep = " ", collapse = " ") %>% unlist())

####################################################################################################
lemma_data <- read.csv("Lemmaliste", encoding = "UTF-8", sep ="\t") %>%
  mutate(Basisform = tolower(Basisform)) %>%
  mutate(originalWort = tolower(originalWort))

# extended stopword list
stopwords_extended <- readLines("german_stopwords_full.txt",
                                encoding = "UTF-8")

# Tokenization of text data
corpus_tokens <- text %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(pattern = lemma_data$originalWort,replacement = lemma_data$Basisform,valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_extended, padding = TRUE)


#text_collocations <- quanteda.textstats::textstat_collocations(corpus_tokens,min_count = 25)
#text2_collocations <- text_collocations[1:250, ]
#corpus_tokens <- tokens_compound(corpus_tokens, text2_collocations)


DTM <- corpus_tokens %>%
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 3)

sort(colSums(DTM),decreasing = T)[1:20]

top_terms <- c("vielen_dank", "herr_präsident", "cdu_csu", "milliarden_euro",
               "geehrte_frau", "geehrte_damen", "damen","all","kollege","herr","beifall",
               "herren", "danke", "werte_kollegen", "beifall", "die", "und", "das", 
               "der", "sie", "es", "aber", "nächst", "redner", "wort", "kollegin", "dr.",
               "entschuldigen", "jahr", "präsident", "mensch", "malen", "wissen", "e", "d",
               "groß", "stehen", "schließen", "monat", "woche", "lieb", "jed", "dame", "de", 
               "h.", "thema", "letzt", "welch", "stellen", "frage", "fragen", "antwort", "bitten",
               "betreiben", "bereich", "abgeordnet")
DTM <- DTM[, !(colnames(DTM) %in% top_terms)]

sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
meta<- meta[sel_idx, ]
text<- text[sel_idx, ]
require(topicmodels)

K <- 20

topicModel <- LDA(DTM, K, method="Gibbs", control=list(
  iter = 500,
  seed = 1,
  verbose = 25,
  alpha = 0.02))

tmResult <- posterior(topicModel)

attributes(tmResult)

terms(topicModel, 5)

dim(tmResult$topics)

mean_theta <- colMeans(tmResult$topics)

###########################################################################################
barplot(mean_theta, 
        names.arg = 1:K,   # Nummerierung der Themen von 1 bis K auf der x-Achse
        xlab = "Topics AfD",    # Beschriftung der x-Achse
        ylab = "percentage", # Beschriftung der y-Achse
        main = "Percentage Distribution across Topics (AfD)") # Titel des Diagramms



beta <- tmResult$terms
theta <- tmResult$topics

final <- meta %>% add_column(tmResult$topics)
final %>% 
  filter(date > "2017-01-01") %>% 
  select(-date,-speaker) %>%
  colMeans() %>%
  sort(decreasing = T)

terms(topicModel, 10)


sample(text[tmResult$topics[,18] > 0.8], size = 3) %>% paste(collapse = " ")


#############################################################################################################################
####################################################################################################################################
###################################################################################################################################
##Daten des 19. Plenarprotokolls##

filter_data2 <- polmineR::corpus("GERMAPARL2") %>%
  polmineR::subset(protocol_lp == 19) #%>%
  #polmineR::subset(speaker_party == "AfD") 
##gefilterte Datenkorpus wird in das Speeches-Format umgewandelt:

speeches_data2 <- filter_data2 %>% as.speeches(
  s_attribute_date = "protocol_date",
  s_attribute_name = "speaker_name",
) 

##Kontextdaten(Metadaten): Datum und Sprecher werden aus Speeches weggemacht und in Datenrahmen (tibble) gespeichert.
meta2 <- lapply(speeches_data2,FUN = function(x) {
  s_attributes(x, s_attribute = c("protocol_date", "speaker_name","speaker_party"))[,1:3] %>%
    as_tibble() %>%
    set_names(c("date","speaker","party"))  
})  %>% bind_rows() 



##Funktion terms extrahiert die Terme aus den Textdaten, Ausdruck lapply(speeches_data, FUN = terms, p_attribute = "word") führt die Funktion terms auf jedem Element von speeches_data aus

text2 <- lapply(speeches_data2,FUN = terms,p_attribute = "word") %>% quanteda::tokens()

##Zusammenführen von Metadaten und Textdaten:
fulltext_dataset2 <- meta2 %>% mutate(text2 = lapply(text2, paste,sep = " ", collapse = " ") %>% unlist())

#################################################################################################################

lemma_data <- read.csv("Lemmaliste", encoding = "UTF-8", sep ="\t") %>%
  mutate(Basisform = tolower(Basisform)) %>%
  mutate(originalWort = tolower(originalWort))


stopwords_extended <- readLines("german_stopwords_full.txt",
                                encoding = "UTF-8")

# Tokenization of text data
corpus_tokens2 <- text2 %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(pattern = lemma_data$originalWort,replacement = lemma_data$Basisform,valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_extended, padding = TRUE)


#text_collocations <- quanteda.textstats::textstat_collocations(corpus_tokens,min_count = 25)
#text2_collocations <- text_collocations[1:250, ]
#corpus_tokens <- tokens_compound(corpus_tokens, text2_collocations)



DTM2 <- corpus_tokens2 %>%
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 3)

sort(colSums(DTM2),decreasing = T)[1:20]

top_terms2 <- c("vielen_dank", "herr_präsident", "cdu_csu", "milliarden_euro",
               "geehrte_frau", "geehrte_damen", "damen","all","kollege","herr","beifall",
               "herren", "danke", "werte_kollegen", "beifall", "die", "und", "das", 
               "der", "sie", "es", "aber", "nächst", "redner", "wort", "kollegin", "dr.",
               "entschuldigen", "jahr", "präsident", "mensch", "malen", "wissen", "e", "d",
               "groß", "stehen", "schließen", "monat", "woche", "lieb", "jed", "dame", "de", 
               "h.", "thema", "letzt", "welch", "stellen", "frage", "fragen", "antwort", "bitten",
               "betreiben", "bereich", "abgeordnet")

DTM2 <- DTM2[, !(colnames(DTM2) %in% top_terms2)]

sel_idx2 <- rowSums(DTM2) > 0
DTM2 <- DTM2[sel_idx2, ]
meta2<- meta2[sel_idx2, ]
text2<- text2[sel_idx2, ]

##############################################################################
require(topicmodels)
K2 <- 25

topicModel2 <- LDA(DTM2, K2, method="Gibbs", control=list(
  iter = 500,
  seed = 1,
  verbose = 25,
  alpha = 0.02))

tmResult2 <- posterior(topicModel2)

attributes(tmResult2)

terms(topicModel2, 5)

dim(tmResult2$topics)

beta2 <- tmResult2$terms
theta2 <- tmResult2$topics

#Visualization:

#library(LDAvis)
#library("tsne")
#svd_tsne <- function(x) tsne(svd(x)$u)
#json <- createJSON(phi = beta, theta = theta, doc.length = rowSums(DTM),
                   
#serVis(json)

mean_theta2 <- colMeans(tmResult2$topics)

#########################################################################
barplot(mean_theta2, 
        names.arg = 1:K2,   # Nummerierung der Themen von 1 bis K auf der x-Achse
        xlab = "Topic",    # Beschriftung der x-Achse
        ylab = "percentage", # Beschriftung der y-Achse
        main = "Percentage Distribution across Topics") # Titel des Diagramms

final2 <- meta2 %>% add_column(tmResult2$topics)
final2 <- final2 %>% mutate(year = substr(date,1,4)) %>% as_tibble()



final2 %>% 
  filter(date > "2017-01-01") %>% 
 # filter(party == "CDU") %>% 
  select(-date,-speaker,-party) %>%
  colMeans()%>%barplot() #%>%
  #sort(decreasing = T)
barplot(colMeans(tmResult2$topics))

sample(text2[tmResult2$topics[,8] > 0.9], size = 3) %>% paste(collapse = " ")
sample(text2[tmResult2$topics[,22] > 0.8 & final2$date > "2020-01-01" & final2$party == "AfD"], size = 3) %>% paste(collapse = " ")


time_series2 <- aggregate(tmResult2$topics[,25],list(final2$year,final2$party), mean2) 
colnames(time_series2) <- c("year","party","value")

library(ggplot2)
library(ggthemes)

time_series_filtered2 <- subset(time_series2, !(party %in% c("Die PARTEI", "LKR", "NA", "parteilos")))  


party_colors <- c("AfD" = "blue", "CDU" = "black", "CSU" = "darkblue", "SPD" = "red", "FDP" = "yellow", "DIE LINKE" = "purple", "GRUENE" = "green")

ggplot(time_series_filtered2, aes(x=year, y=value, group=party, color=party)) + 
  geom_line() +
  scale_color_manual(values = party_colors) + 
  theme_minimal() +
  labs(title = "Topic 25 bei den Parteien"
  )

###################################################################################

my_table <- data.frame(
  Column1 = character(), 
  Column2 = character(),  
  Column3 = character(),  
  Column4 = character(),  
  stringsAsFactors = FALSE  
)


my_table[1, ] <- c("TOPIC", "TOKENS", "TOPIC", "TOKENS")
my_table[2, ] <- c("17: Migration", "flüchtling, land, deutsch, deutschland, migration", "2: Migration", "deutsch, deutschland, illegal, land, migranten")
my_table[3, ] <- c("13: Militäreinsätze", "einsatz, deutsch, land, soldat, militärisch", "8: Militäreinsätze", "einsatz, soldat, bundeswehr, deutsch, militärisch")
my_table[4, ] <- c("22: Agrarwirtschaft", "grün, landwirt, landwirtschaft, deutschland, tier", "17: Agrarwirtschaft", "landwirt, landwirtschaft, bauer, ministerin, landwirtschaftlich")
my_table[5, ] <- c("18: Deutsche Geschichte", "deutsch, leben, land, deutschland, geschichte", " ", " ")
my_table[6, ] <- c("20: Parteikonflikte", "link, grün, afd, deutsch, bündnis", "9: Parteikonflikte", "grün, link, bündnis, afd, spd")
print(my_table)

colnames(my_table) <- c("Parteien:", "CDU, SPD, FDP, AfD, Die Linke, Grüne", "Partei:", "AfD")


library(kableExtra)

colnames(my_table) <- c("Parteien:", "CDU, SPD, FDP, AfD, Die Linke, Grüne", "Partei:", "AfD")


knitr::kable(my_table, caption = "Topic Vergleiche") %>%
  kable_styling(full_width = FALSE)
