library(readxl)
library(dplyr)
library(factoextra)
library(ggplot2)
library(tidyverse)
library(cluster)
library(scales)
library(tidyr)
library(stringr)

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

#Frequenza on-demand
ggplot(df_small, aes(x = cluster, fill = on_demand)) +
  geom_bar(position = "stack") +
  labs(title = "Uso di strumenti on demand per cluster", x = "Cluster", y = "Proporzione", fill= "Strumenti On-demand") +
  theme_minimal()

## Frequenza d'ascolto radio per cluster
df_small$ascolta_radio <-  factor(df_small$ascolta_radio,
                              levels = c("1", "2", "3", "4"),
                              labels = c("Abitualmente", "Spesso", "Di tanto in tanto", "Raramente"))

ggplot(df_small, aes(x = cluster, fill = ascolta_radio)) +
  geom_bar(position = "stack", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count), group = ascolta_radio),
            position = position_stack(vjust = 0.5), color = "black", size = 3) +
  labs(title = "Frequenza d'ascolto radio per cluster",
       x = "Cluster", y = "Frequenza assoluta", fill = "Ascolta radio") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()

## Fastidio della pubblicità per cluster  da TABELLA

df_small$pubblicità_fastidio <- factor(df_small$pubblicità_fastidio,
                                       levels = c("1", "2", "3", "4", "5"),
                                       labels = c("Molto fastidiosa", "Abbastanza fastidiosa", "Né fastidiosa né piacevole", "Abbastanza piacevole", "Molto piacevole"))

table(df_small$pubblicità_fastidio, df_small$cluster)

## Attenzione e interesse agli spot per cluster

df_small$attenzione_interesse <- factor(df_small$attenzione_interesse,
                                        levels = c("1", "2", "3", "4", "5"),
                                        labels = c("Per nulla interessanti", "Poco interessanti","Mediamente interessanti", "Abbastanza interessanti", "Molto interessanti"))

ggplot(df_small, aes(x = cluster, fill = attenzione_interesse)) +
  geom_bar(position = "stack", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count), group = attenzione_interesse),
            position = position_stack(vjust = 0.5), color = "black", size = 3) +
  labs(title = "Attenzione e interesse agli spot per cluster", x = "Cluster", y = "Frequenza assoluta", fill = "Attenzione e interesse") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()

## Qualità percepita degli spot per cluster

df_small$qualità_spot <- factor(df_small$qualità_spot,
                                levels = c("1", "2", "3", "4", "5"),
                                labels = c("Del tutto insoddisfatto", "Abbastanza insoddisfatto",
                                           "Nè insoddisfatto nè soddisfatto", "Abbastanza soddisfatto", "Del tutto soddisfatto"))

table(df_small$qualità_spot, df_small$cluster)
#frequenza break
df_small$frequenza_break<- factor(df_small$frequenza_break,
                                levels = c("1", "2", "3", "4", "5"),
                                labels = c("Del tutto insoddisfatto", "Abbastanza insoddisfatto",
                                           "Nè insoddisfatto nè soddisfatto", "Abbastanza soddisfatto", "Del tutto soddisfatto"))
table(df_small$frequenza_break, df_small$cluster) 

# Aggiungiamo età e genere per l'analisi dei profili dei cluster
df_small$età <- df %>% filter(!is.na(on_demand)) %>% 
  select(età) %>% 
  drop_na() %>% 
  pull()
df_small$età2 <- as.numeric(as.factor((df_small$età)))


df_small$genere <- df %>% filter(!is.na(on_demand)) %>% 
  select(genere) %>% 
  drop_na() %>% 
  pull()

df_small$professione <- df %>% filter(!is.na(on_demand)) %>% 
  select(professione) %>% 
  drop_na() %>% 
  pull()

df_small$residenza <- df %>% filter(!is.na(on_demand)) %>% 
  select(residenza) %>% 
  drop_na() %>% 
  pull()

df_small$luoghi <- df %>% filter(!is.na(on_demand)) %>% 
  select(luoghi) %>% 
  drop_na() %>% 
  pull()

df_small$device <- df %>% filter(!is.na(on_demand)) %>% 
  select(device) %>% 
  drop_na() %>% 
  pull()

df_small$ascolto_orario<- df %>% filter(!is.na(on_demand)) %>% 
  select(Ascolto_orario) %>% 
  drop_na() %>% 
  pull()

df_small$emittente<-df %>% filter(!is.na(on_demand)) %>% 
  select(emittente_ascoltata) %>% 
  drop_na() %>% 
  pull()


# Profilo per età
cat("\n👵👦 Età media e deviazione standard per cluster:\n")
età_summary <- df_small %>%
  group_by(cluster) %>%
  summarise(
    media_età = mean(età2, na.rm = TRUE),
    .groups = 'drop'
  )
print(età_summary)

cat("\n🚻 Distribuzione anni per cluster:\n")
genere_summary<-print(table(df_small$età, df_small$cluster))

# Profilo per genere
cat("\n🚻 Distribuzione del genere per cluster:\n")
genere_summary<-print(table(df_small$genere, df_small$cluster))

#profilo per professione
cat("\n🚻 Distribuzione delle professioni per cluster:\n")
professione_summary<-print(table( df_small$cluster,df_small$professione))

#profilo per residenza
cat("\n🚻 Distribuzione della residenza per cluster:\n")
residenza_summary<-print(table( df_small$cluster,df_small$residenza))

#profilo per luoghi
cat("\n🚻 Distribuzione per luoghi d'ascolto per cluster:\n")
luoghi_summary<-print(table(df_small$luoghi, df_small$cluster))

#profilo per device
cat("\n🚻 Distribuzione per device per cluster:\n")
device_summary<-print(table(df_small$device, df_small$cluster))


#profilo per ascolto orario
cat("\n🚻 Distribuzione per orari ascolto per cluster:\n")
orari_summary<-print(table(df_small$ascolto_orario, df_small$cluster))

#profilo per emittente
cat("\n🚻 Distribuzione per emittente per cluster:\n")
emittente_summary<-print(table(df_small$emittente, df_small$cluster))


# Visualizzazioni
# Boxplot età

ggplot(df_small, aes(x = cluster, y = età2, fill = cluster)) +
  geom_boxplot() +
  scale_y_continuous(breaks = 1:7,
    labels = c("<18 anni", "18-24 anni", "25-34 anni", 
               "35-44 anni", "45-54 anni", "55-64 anni", ">64 anni")) +
  labs( title = "Distribuzione dell'età per fascia e cluster",
    x = "Cluster",
    y = "Fascia d'età") +
  theme_minimal()

# Barplot genere
# Calcolo proporzioni per genere
df_genere_prop <- df_small %>%
  group_by(cluster, genere) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster) %>%
  mutate(percentuale = n / sum(n))

# Grafico
ggplot(df_genere_prop, aes(x = cluster, y = percentuale, fill = genere)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(label = percent(percentuale, accuracy = 1)),
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Distribuzione del genere per cluster",
       x = "Cluster", y = "Percentuale", fill = "Genere") + scale_fill_brewer(palette = "Set1")
  theme_minimal()

#barplotn luoghi
  table(df_small$cluster, df_small$luoghi)
  
  df_luoghi_long <- df_small %>%
    select(cluster, luoghi) %>%
    mutate(luoghi = str_split(luoghi, ",\\s*")) %>%
    unnest(luoghi) %>%
    mutate(luoghi = str_trim(luoghi))
  
  df_luoghi_prop <- df_luoghi_long %>%
    group_by(cluster, luoghi) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(cluster) %>%
    mutate(percentuale = n / sum(n))
  
  # Grafico
  ggplot(df_luoghi_prop, aes(x = cluster, y = percentuale, fill = luoghi)) +
    geom_bar(stat = "identity", position = "stack", color = "black") +
    geom_text(aes(label = percent(percentuale, accuracy = 1)),
              position = position_stack(vjust = 0.5), size = 3, color = "black") +
    scale_y_continuous(labels = percent_format()) +
    labs(title = "Distribuzione per luoghi d'ascolto per cluster",
         x = "Cluster", y = "Percentuale", fill = "Luoghi") + scale_fill_brewer(palette = "Set1")
    theme_minimal()

  #condizione professionale 
    df_prof_long <- df_small %>%
      select(cluster, professione) %>%
      mutate(professione = str_split(professione, ",\\s*")) %>%
      unnest(professione) %>%
      mutate(professione = str_trim(professione))
    
    df_professione_prop <- df_prof_long %>%
      group_by(cluster, professione) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(cluster) %>%
      mutate(percentuale = n / sum(n))
    
    # Grafico
    ggplot(df_professione_prop, aes(x = cluster, y = percentuale, fill = professione)) +
      geom_bar(stat = "identity", position = "stack", color = "black") +
      geom_text(aes(label = percent(percentuale, accuracy = 1)),
                position = position_stack(vjust = 0.5), size = 3, color = "black") +
      scale_y_continuous(labels = percent_format()) +
      labs(title = "Distribuzione per professione per cluster",
           x = "Cluster", y = "Percentuale", fill = "Professione") + scale_fill_brewer(palette = "Set1")
    theme_minimal()
    
    
    #residenza
    df_residenza_long <- df_small %>%
      select(cluster, residenza) %>%
      mutate(residenza = str_split(residenza, ",\\s*")) %>%
      unnest(residenza) %>%
      mutate(residenza = str_trim(residenza))
    
    df_residenza_prop <- df_residenza_long %>%
      group_by(cluster, residenza) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(cluster) %>%
      mutate(percentuale = n / sum(n))
    
    # Grafico
    ggplot(df_residenza_prop, aes(x = cluster, y = percentuale, fill = residenza)) +
      geom_bar(stat = "identity", position = "stack", color = "black") +
      geom_text(aes(label = percent(percentuale, accuracy = 1)),
                position = position_stack(vjust = 0.5), size = 3, color = "black") +
      scale_y_continuous(labels = percent_format()) +
      labs(title = "Distribuzione per residenza per cluster",
           x = "Cluster", y = "Percentuale", fill = "Residenza") + scale_fill_brewer(palette = "Set1")
    theme_minimal()
    
#emittenti
  table(df_small$emittente, df_small$cluster)


df_small$ascolto_orario2<- as.numeric(as.factor(df_small$ascolto_orario))

ggplot(df_small, aes(x = cluster, y = ascolto_orario2, fill = cluster)) +
  geom_boxplot() +
  scale_y_continuous(breaks = 1:4,
                     labels = c("00:00-06:00","06:00-12.00", "12:00-18:00", "18:00-24:00")) +
  labs( title = "Distribuzione degli orari d'ascolto per fascia e cluster",
        x = "Cluster",
        y = "Fascia d'età") +
  theme_minimal()

# Espandi le fasce orarie
df_orari_long <- df_small %>%
  select(cluster, ascolto_orario) %>%
  mutate(ascolto_orario = str_split(ascolto_orario, ",\\s*")) %>%
  unnest(ascolto_orario) %>%
  mutate(ascolto_orario = str_trim(ascolto_orario))
fasce_ordine <- c("Notte (00:00 – 06:00)", "Mattina (06:00 – 12:00)",
                  "Pomeriggio (12:00 – 18:00)", "Sera (18:00 – 24:00)")

# Imposta l'ordine come fattore
df_orari_long <- df_orari_long %>%
  mutate(ascolto_orario = factor(ascolto_orario, levels = fasce_ordine))


# Calcola le proporzioni
df_prop <- df_orari_long %>%
  group_by(cluster, ascolto_orario) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster) %>%
  mutate(percentuale = n / sum(n))

# Plot con percentuali dentro le barre
ggplot(df_prop, aes(x = cluster, y = percentuale, fill = ascolto_orario)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(percentuale, accuracy = 1)),
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribuzione delle fasce orarie di ascolto per cluster",
    x = "Cluster",
    y = "Percentuale",
    fill = "Fascia oraria"
  ) +scale_fill_brewer(palette = "Set1") +
  theme_minimal()



#grafuco per device
df_device_long <- df_small %>%
  select(cluster, device) %>%
  mutate(device = str_split(device, ",\\s*")) %>%
  unnest(device) %>%
  mutate(device = str_trim(device))  # rimuove eventuali spazi

df_small$device <- factor(df_small$device)

df_device_prop <- df_device_long %>%
  group_by(cluster, device) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster) %>%
  mutate(percentuale = n / sum(n))

# 3. Grafico
ggplot(df_device_prop, aes(x = cluster, y = percentuale, fill = device)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = percent(percentuale, accuracy = 1)),
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Distribuzione dei dispositivi di ascolto per cluster",
    x = "Cluster",
    y = "Percentuale",
    fill = "Device"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()
