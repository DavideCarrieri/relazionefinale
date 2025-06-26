library(readxl)
library(dplyr)
library(factoextra)
library(ggplot2)
library(tidyverse)
library(cluster)
df <- read_excel("sondaggiomod.xlsx")

summary(df)

# PROFILO MEDIO 
df_numeric <- df %>%
  select(ascolta_radio, where(is.numeric)) %>%
  filter(!is.na(ascolta_radio)) %>%
  group_by(ascolta_radio) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

df_categorical <- df %>%
  select(ascolta_radio, where(~!is.numeric(.))) %>%
  filter(!is.na(ascolta_radio)) %>%
  group_by(ascolta_radio) %>%
  summarise(across(everything(), ~ {
    x <- as.character(.)           
    tab <- table(na.omit(x))       
    if (length(tab) == 0) NA       
    else names(sort(tab, decreasing = TRUE))[1]  
  }))

profilo_completo <- left_join(df_categorical, df_numeric, by = "ascolta_radio")
tabellaprofilo<-as.data.frame(profilo_completo)


#cluster analysis
colnames(df) <- make.names(colnames(df))

df_small <- df %>%
  select(ascolta_radio, on_demand, qualità_spot,
         pubblicità_fastidio, frequenza_break, attenzione_interesse) %>%
  mutate(across(where(is.character), factor))

df_small <- na.omit(df_small)
 
#encoding
df_encoded <- model.matrix(~ . - 1, data = df_small) %>% as.data.frame()

#Standardizzazione
df_scaled <- scale(df_encoded)
(fviz_nbclust(df_scaled, kmeans, method = "silhouette"))

#KMeans
set.seed(123)
km <- kmeans(df_scaled, centers = 2,nstart = 25)


#Silhouette plot
sil <- silhouette(km$cluster, dist(df_scaled))
fviz_silhouette(sil)

silhouette_media <- mean(sil[, "sil_width"])
cat("✅ Silhouette media:", round(silhouette_media, 3), "\n")


df_small$cluster <- factor(km$cluster)

# cluster
fviz_cluster(km, data = df_scaled)


# Separa variabili numeriche e categoriche
numeriche <- df_small %>%
  select(where(is.numeric)) %>%
  names()

categoriche <- df_small %>%
  select(where(is.factor)) %>%
  select(-cluster) %>%
  names()
#Profilo delle variabili numeriche per cluster
cat("🔢 Profilo delle variabili numeriche per cluster:\n")
numerical_summary <- df_small %>%
  group_by(km$cluster) %>%
  summarise(across(all_of(numeriche), list(media = mean), na.rm = TRUE))
print(numerical_summary)

#Profilo delle variabili categoriche per cluster (conteggi)
cat("\n📊 Distribuzione delle variabili categoriche per cluster:\n")
categorie_summary<-for (var in categoriche) {
  cat("\nVariabile:", var, "\n")
  print(table(df_small[[var]], df_small$cluster))
}

# Aggiungiamo età e genere per l'analisi dei profili dei cluster
df_small$età <- df %>% filter(!is.na(on_demand)) %>% 
  select(età) %>% 
  drop_na() %>% 
  pull()
df_small$età <- as.numeric(as.factor((df_small$età)))



df_small$genere <- df %>% filter(!is.na(on_demand)) %>% 
  select(genere) %>% 
  drop_na() %>% 
  pull()

# Profilo per età
cat("\n👵👦 Età media e deviazione standard per cluster:\n")
età_summary <- df_small %>%
  group_by(cluster) %>%
  summarise(
    media_età = mean(età, na.rm = TRUE),
    .groups = 'drop'
  )
print(età_summary)

# Profilo per genere
cat("\n🚻 Distribuzione del genere per cluster:\n")
genere_summary<-print(table(df_small$genere, df_small$cluster))

