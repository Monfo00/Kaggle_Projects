#MONFORTE ANTONIO 857850

rm(list = ls())
#APRO IL DATASET
#setwd("") #<- togliere il commento e mettere il percorso in cui si estrae la cartella zip. 
#(In modo da avere la directory di ferimento con tutti i file)
#Nella directory devono essere presente anche i file training.csv e test.csv che non ho
#messo nel file zip consegnato.


taxi<-read.csv("training.csv", header = TRUE)
library(skimr) 
library(dplyr)
library(ggplot2)
skim(taxi)
set.seed(123)

#Divido in training e validation
#Il primo mi serve per addestrare i modelli mentre 
#il secondo per metterli alla prova e selezionare il migliore da usare nel kaggle_test.

train_size <- 0.70 * nrow(taxi) #70% per training, parto da qui per studiare le variabili e stimare i modelli.
val_size <- 0.30 * nrow(taxi) #30% per validation.

train_index <- sample(1:nrow(taxi), train_size, replace = FALSE)
train_taxi <- taxi[train_index, ]

validation_data <- taxi[-train_index, ]
x_validation<-validation_data[,-20]
y_validation<-validation_data$tip_amount


train_taxi<-train_taxi[,-1] #tolgo id

###########################VARIABILI####################################################
#train_taxi è l'intero train

#Orario pick-up####
#Sostituisco con una dummy che distingue orari notturni e non.
#cercando su internet : https://www.viaggi-usa.it/taxi-new-york/#:~:text=Ci%20sono%20infatti%20alcuni%20momenti,20%20alle%206%20del%20mattino
#Vedo che il pomeriggio (dalle 16 in poi) e dalle 20 alle 6 del mattino viene applicato un supplemento alla tariffa dei taxi
#il che potrebbe influenzare la mancia in quanto è proporzionata con il costo della corsa (https://www.viagginewyork.it/consigli-utili-faq-new-york/mancia-a-new-york/, leggo che di solito si lascia il 10% o 20% del prezzo della tratta).


train_taxi$pickup_hour<-ifelse(train_taxi$pickup_hour %in% c(16, 17, 18, 19, 20, 21, 22, 23, 0, 1, 2, 3, 4, 5, 6), "Orario Notturno", "Orario Normale")
train_taxi$pickup_hour<-as.factor(train_taxi$pickup_hour) 
table(train_taxi$pickup_hour)/(nrow(train_taxi)) #vedo che in "orario notturno" ho più osservazioni.
print(train_taxi %>% group_by(pickup_hour) %>%
        summarize(media_mancia = mean(tip_amount), media_costo_tratta = mean(fare_amount)), n=2) 
#Medie condizionate molto simili
#Non rimuovo in quanto ci potrebbero essere interazioni significative con altre variabili.

#Mese pick-up####
#Tolgo subito perchè è solo maggio
table(train_taxi$pickup_month)
train_taxi<-train_taxi[,-2]


#Weekend e giorni####
train_taxi<-train_taxi[,-2] #levo la variabile sul numero di weekend
#tratto i giorni come segue :
#Per trattare i giorni creo una dummy che distingue giorni lavorativi e weekend.
train_taxi$pickup_wday<-ifelse(train_taxi$pickup_wday %in% c(6,7), "Weekend", "NormalDay")
table(train_taxi$pickup_wday)/nrow(train_taxi)

train_taxi$pickup_wday<-as.factor(train_taxi$pickup_wday)

train_taxi %>% group_by(pickup_wday) %>%
  summarize(media_mancia = mean(tip_amount), media_costo_tratta = mean(fare_amount)) 

#Nei giorni lavorativi ho leggermente, in media, una mancia maggiore.

#Levo altre divisioni dei giorni in quanto li tratto con la dummy che ho creato
train_taxi<-train_taxi[,-2] #numero del giorno nell'anno 2015


#Macrozone####
#INFO SU NTA CODE :
#https://www.nyc.gov/assets/planning/download/pdf/data-maps/nyc-population/census2010/ntas.pdf mappa con BoroCode e NTAcode;
#PDF più preciso:
#https://s-media.nyc.gov/agencies/dcp/assets/files/pdf/data-tools/census/acs/acs_socio_transport_ntas_2010.pdf

table(train_taxi$pickup_BoroCode) #Quasi tutte le osservazioni sono relative a Manhattan (cuore di New York), seguito dal Queens e Brooklyn. Infine Bronx e Statn Island sono coinvolte in pochissime corse.
table(train_taxi$dropoff_BoroCode)
table(c(train_taxi$pickup_NTACode, train_taxi$dropoff_NTACode)) #Troppe modalità

#Suddivido in macroaree facendo riferimento ai codici NTA.
#Salvo le suddivisioni in un file excel e faccio un join.

#Per la maggior parte dei casi ho cercato di unire codici NTA vicini geograficamente in modo 
#da distinguere meglio le aree di NY.
#Per il Queens ho fatto la divisione aeroporto (JFK, "QN98") e non aeroporto perché, provando diversi collegamenti,
#mi ero accorto dell'elevata numerosità di tratte che riguardavano JFK rispetto a tutte le altre.
#Staten island l'ho lasciata uguale perché ha pochissime osservazioni rispetto alle altre classi.
#Tutte le zone con codice 99 le ho accorpate come una unica perché sono le tratte che coinvolgono i cimiteri
#quindi sono sparsi non riguardano una zona in particolare.


library(readxl)
#codice contorto ma efficace :D
division<-read_excel("area_specific_2.xlsx")
colnames(division)[colnames(division) == "Code"] <- "pickup_NTACode" #cambio il nome per fare il join (devono avere stesso nome)
train_taxi <- train_taxi %>% left_join(division, by="pickup_NTACode")
table(train_taxi$Place)
colnames(division)[colnames(division) == "pickup_NTACode"] <- "dropoff_NTACode" #ricambio ancora per fare il join dato che cambio anche dropoff_ntacode
colnames(division)[colnames(division) == "Place"] <- "Place2"
train_taxi <- train_taxi %>% left_join(division, by="dropoff_NTACode")
table(train_taxi$Place2)
table(c(train_taxi$Place, train_taxi$Place2))

#Levo codici NTA e BoroCode (che ho raggruppato creando nuove macroaree).
train_taxi<-train_taxi[,-c(4,5,6,7,17)] 
#Lavoro con le zone che ho creato io.
#Levo per il momento anche pairs, ossia la combinazione delle tratte, perché ha troppe modalità.

#Cambio i nomi
names(train_taxi)[names(train_taxi) == "Place"] <- "Area_pickup"
names(train_taxi)[names(train_taxi) == "Place2"] <- "Area_dropoff"

#Factor
train_taxi$Area_pickup<-as.factor(train_taxi$Area_pickup)
train_taxi$Area_dropoff<-as.factor(train_taxi$Area_dropoff)

#Vedo le medie condizionate
print(train_taxi %>% group_by(Area_pickup) %>%
        summarize(media_mancia = mean(tip_amount), media_costo_tratta = mean(fare_amount), numb=n(), var=sd(tip_amount)), n=24)

#train_taxi %>% 
#  filter(trip_distance < 10, Area_pickup == "Mahnattan_Centro_Ovest_e_East") #o midtown, per lo più tratte brevi.

#Vedo come Queens Airport abbia in media un costo della tratta e una mancia più alta rispetto alle altre zone.
#Tutte le altre stanno in un range tra 1.88 e 4.60, con leggere variazioni.

#BK_SUD ha una media alta (come Queens Airport), tuttavia sospetto che ci sia un valore anomalo che mi rende alta questa media.
#Infatti, a differenza delle altre aree, vedo un'elevata variabilità in BK_SUD, segno che ho valori molto diversi relative delle mance e, molto probabilmente, un valore anomalo.

#faccio i boxplot condizionati con Area Pickup
ggplot(train_taxi, aes(x = Area_pickup, y = tip_amount, fill = Area_pickup)) +
  geom_boxplot(color = "black", position = "dodge") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(width = 0.75)) +
  labs(title = "Boxplot delle Medie per Area di Pickup",
       x = "Area di Pickup",
       y = "Tip Amount") +
  ylim(0, 20) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_manual(values = c("BK_NORD" = "lightblue", "BK_SUD" = "lightblue2", "BX_NORD" = "purple3", "BX_SUD"="purple4", "Cimitery"="black", "Mahnattan_Centro_Ovest_e_East"="yellow2", "Mahnattan_Sud"="yellow3", "Mahnattan_Sud_Ovest_e_East"="orange2", "Midtown Mahnattan"="orange3", "Upper West e East Side"="red2", "Uptown Mahnattan"="red3", "Queens Airport"="green1", "Queens without Airport"="green3", "Staten Island"="pink" )) +
  guides(fill = guide_legend(title = "Aree di Pickup"))



#Controllo anche per area_dropoff
#Come prima con BK_SUD, vedo che anche le osservazioni che hanno Staten Island come destinazione potrebbero avere un valore anomalo,
#infatti anche in questo caso vedo una media della mancia piuttosto alta e una varianza di 7 su 46 righe.

#Queens Airport si riconferma la più alta. 
#A differenza di prima vedo nei boxblot delle variazioni di livello "più visibili", segno che l'area_dropoff probabilmente
#incide di più sulla differenza della mancia tra una tratta e un'altra.

print(train_taxi %>% group_by(Area_dropoff) %>%
        summarize(media_mancia = mean(tip_amount), media_costo_tratta = mean(fare_amount), numb=n(), var=sd(tip_amount)), n=24)

#boxplot con areadropoff
ggplot(train_taxi, aes(x = Area_dropoff, y = tip_amount, fill = Area_dropoff)) +
  geom_boxplot(color = "black", position = "dodge") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(width = 0.75)) +
  labs(title = "Boxplot delle Medie per Area di dropoff",
       x = "Area di dropoff",
       y = "Tip Amount") +
  ylim(0, 20) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_manual(values = c("BK_NORD" = "lightblue", "BK_SUD" = "lightblue2", "BX_NORD" = "purple3", "BX_SUD"="purple4", "Cimitery"="black", "Mahnattan_Centro_Ovest_e_East"="yellow2", "Mahnattan_Sud"="yellow3", "Mahnattan_Sud_Ovest_e_East"="orange2", "Midtown Mahnattan"="orange3", "Upper West e East Side"="red2", "Uptown Mahnattan"="red3", "Queens Airport"="green1", "Queens without Airport"="green3", "Staten Island"="pink"  )) +
  guides(fill = guide_legend(title = "Aree di dropoff"))


#Da notare che in entrambi i grafici ho dovuto "tagliare" l'Ylim in quanto, essendoci valori estremi 
#non rendavano possibile l'osservazione dei boxplot.

#pulisco environment
rm(division)
rm(train_index)
rm(train_size)
rm(val_size)

#Passenger count####
table(train_taxi$passenger_count)
#Vedendo le diverse numerosità, creo una dummy con 3 modalità, in questo modo accorpo quelle con meno osservazioni.
train_taxi$passenger_count <- ifelse(train_taxi$passenger_count == 1, "Solo", 
                                   ifelse(train_taxi$passenger_count == 2, "Coppia", "Group"))
train_taxi$passenger_count<-as.factor(train_taxi$passenger_count)

train_taxi %>% group_by(passenger_count) %>%
  summarize(media = mean(tip_amount), media2 = mean(fare_amount), numb=n()) 

#La maggior parte delle corse sono fatte da individui singoli.
#Coppie e gruppi sono quelli coinvolti in tratte con un costo ed una mancia leggermente maggiori.


#Vendo id####
table(train_taxi$vendor_id)
train_taxi$vendor_id<-as.factor(train_taxi$vendor_id)
train_taxi %>% group_by(vendor_id) %>%
  summarize(media = mean(tip_amount), media2 = mean(fare_amount)) 

#Verifone Inc sembra registrare in media delle mance più alte.

#In generale, tolte le variabili qualitative geografiche le altre non sembrano discriminare molto prese singolarmente,
#Tuttavia non rimuovo in quando potrebbero avere interazioni con altre variabili e risultare quindi discriminanti in quel caso.
#(Ad esempio magari c'è una tratta che in un determinato orario, in una determinata ora e con un determinato provider ha dei costi più alti del normale).



#trasformo latitudine e longitudine in distanza_check#####
#Mi serve dopo quando controllo le anomalie nel training
#infatti, calcolando la distanza del "navigatore" posso vedere se ci sono
#discrepanze con la trip_distance registrata.
#Di norma la distanza effettiva fatta (trip_distance) non può essere 
#minore di quella indicata dal navigatore. (Se ho 10 miglia da fare da GPS non posso aver percorso di effettivi 6 km ad esempio)


# Carica la libreria
#ho visto su internet una libreria che applica le formule e calcola questa distanza (in metri)
#a partire dalle coordinate.
library(geosphere)
# Applica la funzione distVincentySphere per calcolare la distanza (poi convertita in miglia) per ciascuna riga
train_taxi$distanza_gps <- apply(train_taxi[,c(4:7)], 1, function(row) {
  distVincentySphere(
    p1 = c(row["pickup_longitude"], row["pickup_latitude"]),
    p2 = c(row["dropoff_longitude"], row["dropoff_latitude"])
  ) * 0.000621371
})


##########################################ANOMALIE#########################################################
#vedo anomalie variabile numeriche
summary(train_taxi$trip_distance)
summary(train_taxi$length_time)
summary(train_taxi$fare_amount)

#vedo anomalie sulle interazioni tra le variabili numeriche
summary(train_taxi$trip_distance/train_taxi$length_time) #velocità

#Calcolo tempo medio, distanza media e costo_tratta media per ogni tratta lasciando fuori dei dati "ambigui".
#L'idea è : laddove ho valori ambigui di tempo, di distanza, di velocità e di tratta, 
#sostituisco con la media,
#in quanto è molto probabile ci sia stato un'errore di rilevazione o comunque qualcosa
#di non regolare.
#Le soglie le ho fissate "ad occhio", controllando col comando which
#(ad esempio which(train_taxi$trip_distance>30)) quante osservazioni escludevo. 

#+ l'ultima condizione di "regolarità" dei dati che ho messo è quella che il trip_distance non può 
#essere minore di più di un miglio rispetto alla distanza_gps (distanza gps).

unire <- train_taxi %>%
  filter((length_time >= 60 & length_time <= 7200), 
         (trip_distance >= 0.05 & trip_distance<=30),
         (trip_distance>=train_taxi$distanza_gps-1),
         (trip_distance / length_time >= 0.00087 & trip_distance / length_time <= 0.012),
         (fare_amount>=3 & fare_amount<=70)) %>%
  group_by(Area_pickup, Area_dropoff) %>%
  summarize(media_tempo = mean(length_time), media_distanza = mean(trip_distance), media_tratta=mean(fare_amount))

#unisco con taxi così da fare in seguito l'imputazione quando serve
train_taxi <- train_taxi %>% left_join(unire, by=c("Area_pickup", "Area_dropoff"))
head(train_taxi)
sum(is.na(train_taxi)==TRUE)

#Tempo viaggio####
#Considero come tempo minimo del viaggio 60 secondi.
#Come massimo 2 ore. 
train_taxi$length_time<-ifelse(train_taxi$length_time > 7200, train_taxi$media_tempo,
                             ifelse(train_taxi$length_time < 60, train_taxi$media_tempo, train_taxi$length_time*1))
#Trip Distance####
#Considero come minimo circa 400 metri che sono circa 0.05 miglia. 
#Come massimo ho messo 30 miglia.
train_taxi$trip_distance<-ifelse(train_taxi$trip_distance< 0.05, train_taxi$media_distanza, 
                               ifelse(train_taxi$trip_distance>30, train_taxi$media_distanza, train_taxi$trip_distance*1))
#Velocità####
train_taxi$velocita<-(train_taxi$trip_distance) / (train_taxi$length_time)
summary(train_taxi$velocita)
#Considero valori ambigui velocita medie minori di 5 km/h (0.00087)
#e valori più alti di 70 km/h (0.012)
#Imputo valori medi di tempo e spazio dove ho velocità ambigue
for (i in 1:nrow(train_taxi)) {
  if (train_taxi$velocita[i] < 0.00087 || train_taxi$velocita[i] > 0.012) {
    train_taxi$length_time[i] <- train_taxi$media_tempo[i]
    train_taxi$trip_distance[i] <- train_taxi$media_distanza[i]
  }
}
rm(i)

#ricalcolo le velocità "pulite"
train_taxi$velocita<-(train_taxi$trip_distance) / (train_taxi$length_time)
summary(train_taxi$velocita)

#Fare_amount####
#Costo minimo di una tratta è 3$ (https://www.scoprirenewyork.com/taxi)
#Come massimo ho scelto 70.
train_taxi$fare_amount<-ifelse(train_taxi$fare_amount < 3, train_taxi$media_tratta,
                             ifelse(train_taxi$fare_amount > 70, train_taxi$media_tratta, train_taxi$fare_amount*1))
summary(train_taxi$fare_amount)


#Tolgo le variabili che mi servivano per le imputazioni
usa<-train_taxi[,-c(16,17,18)] #tengo train_taxi come back_up

#############CORRELAZIONE#################
#dopo aver fatto le imputazioni osservo le correlazioni tra le varibili numeriche
#coordinate poco correlate con tutto.
#tempo e distanza correlata con fare_amount in quanto il costo dipende da questi due fattori principalmente.
#fare amount e tip amount correlate in quanto la mancia sarà più alta se il prezzo della tratta sarà alto.
library(corrplot)
correlazione<-cor(usa[,c(3,4,5,6,7,10,11,12,15,16)])
corrplot(correlazione, method="color", col=colorRampPalette(c("purple","cyan3","blue3"))(100), type="upper",   order="hclust", addCoef.col = "black", tl.col="black", tl.srt=45, diag=FALSE)

#Alcuni plot
#fare_amount con tip_amount
#vedo un legame abbastanza lineare tra le due variabili, sporcato da alcune anomalie rimaste.
#da notare una "linea" di valori su fare_amount=52, sono le tratte che coinvolgono JFK (aeroporto del Queens)
#come vedo, è una tratta che interessa sopratutto in orario notturno e, cosa più importante
#coinvolge un ampio range di "tip amount" il che significa che la mancia, sopratutto in quel caso, è influenzata
#da altri fattori.

ggplot(usa, aes(x = fare_amount, y = tip_amount, color = as.factor(pickup_hour))) +
  geom_point() +
  labs(title = "Fare Amount vs Tip Amount",
       x = "Fare Amount",
       y = "Tip Amount",
       color = "Pickup Hour") +
  theme_minimal() +
  ylim(0, 20) + #purtroppo ci sono ancora valori strani : casi in cui con fare_amount bassi si da una mancia altissima e casi in cui con un fare_amount alto si lascia una mancia bassa
  geom_vline(xintercept = 52, linetype = "dashed", color = "red") +
  annotate("text", x = 52, y = max(usa$tip_amount), label = "x = 52")

head(usa[c(which(usa$fare_amount==52)), ])


#length_time con tip_amount
#qui il legame è meno accentuato, anche qui noto valori
#piuttosto strani: alcune volte per tratte che durano poco vengono date mance abbastanza alte
#o casi di mance basse con percorsi che durano tanto.

ggplot(usa, aes(x = length_time, y = tip_amount, color = as.factor(pickup_wday))) +
  geom_point() +
  labs(title = "length time vs Tip Amount",
       x = "length time",
       y = "Tip Amount",
       color = "Pickup wday") +
  ylim(0,20) +
  theme_minimal() 

#trip_distance con tip_amount
#legame lineare un po' più accentuato rispetto prima ma noto ancora presenza di valori ambigui
#e inoltre delle "linee orizzontali" in cui per diverse distanze viene data la stessa mancia.
#Questo ci fa capire che anche in questo caso la mancia dipende da più fattori combinati.

ggplot(usa, aes(x = trip_distance, y = tip_amount, color = as.factor(vendor_id))) +
  geom_point() +
  labs(title = "trip distance vs Tip Amount",
       x = "trip distance",
       y = "Tip Amount",
       color = "vendor id") +
  ylim(0,20) +
  theme_minimal() 




##############FINE PRE PROCESSING###############
#Dai plot mi sono accorto di avere ancora dei valori ambigui nel train
#(infatti dovevo settare l'Ylim relativo al tip amount siccome, causa di valori estremi, 
#non vedevo bene la distribuizione dei dati)
#ho deciso di rimuovere in questo modo
#levo altri valori ambigui rimasti dal train:
#mancia percentuale : https://www.scoprirenewyork.com/taxi "resto mancia".

usa$percent <- ((usa$tip_amount/usa$fare_amount)* 100) #vedo la percentuale della mancia rispetto al costo della tratta
summary(usa$percent) #vedo alcuni valori estremi piuttosto strani come sospettavo prima.
usa<-usa[-c(which(usa$percent<10 | usa$percent>50)), ]  #-12k oss.
usa<-usa[,-17]

#################Stimo una prima regressione lineare################
#Decido di tenere, per il momento, tutte le variabili per la costruzione dei modelli in quanto
#credo che tutte possono avere, anche se in minima parte, una certa influenza nella previsione della mancia.

set.seed(32)
mod_final <- lm(tip_amount ~., data = usa)
summary(mod_final)

#Cerco di usare più avanti dei metodi di selezione di variabili al fine di avere dei 
#modelli più semplici ed interpretabili. 
#Alcuni coefficienti di variabili, opportunatamente penalizzati, verranno mandati a zero
#in questo modo terrò conto delle variabili più rilevanti e cerco di limitare l'impatto di quelle che informano meno.

###################################Faccio nel validation le stesse trasformazioni####################
validation_data$pickup_hour<-ifelse(validation_data$pickup_hour %in% c(16, 17, 18, 19, 20, 21, 22, 23, 0, 1, 2, 3, 4, 5, 6), "Orario Notturno", "Orario Normale")
validation_data$pickup_hour<-as.factor(validation_data$pickup_hour)
validation_data<-validation_data[,-3]
validation_data$pickup_wday<-ifelse(validation_data$pickup_wday %in% c(6,7), "Weekend", "NormalDay")
validation_data$pickup_wday<-as.factor(validation_data$pickup_wday)
validation_data<-validation_data[,-c(3,4)]
x_validation<-validation_data[,-17] #tolgo la variabile risposta
library(readxl)
division<-read_excel("area_specific_2.xlsx")
colnames(division)[colnames(division) == "Code"] <- "pickup_NTACode"
x_validation <- x_validation %>% left_join(division, by="pickup_NTACode")
table(x_validation$Place)
colnames(division)[colnames(division) == "pickup_NTACode"] <- "dropoff_NTACode"
colnames(division)[colnames(division) == "Place"] <- "Place2"
x_validation <- x_validation %>% left_join(division, by="dropoff_NTACode")
table(x_validation$Place2)
table(c(x_validation$Place, x_validation$Place2))
x_validation<-x_validation[,-c(5,6,7,8,17)] 
names(x_validation)[names(x_validation) == "Place"] <- "Area_pickup"
names(x_validation)[names(x_validation) == "Place2"] <- "Area_dropoff"
x_validation$Area_pickup<-as.factor(x_validation$Area_pickup)
x_validation$Area_dropoff<-as.factor(x_validation$Area_dropoff)
rm(division)
x_validation$passenger_count <- ifelse(x_validation$passenger_count == 1, "Solo", 
                                   ifelse(x_validation$passenger_count == 2, "Coppia", "Group"))
x_validation$passenger_count<-as.factor(x_validation$passenger_count)
x_validation$vendor_id<-as.factor(x_validation$vendor_id)


x_validation$distanza_gps <- apply(x_validation[,c(5:8)], 1, function(row) {
  distVincentySphere(
    p1 = c(row["pickup_longitude"], row["pickup_latitude"]),
    p2 = c(row["dropoff_longitude"], row["dropoff_latitude"])
  ) * 0.000621371
})


#anomalie
unire <- x_validation %>%
  filter((length_time >= 60 & length_time <= 7200), 
         (trip_distance >= 0.05 & trip_distance<=30),
         (trip_distance>=distanza_gps-1),
         (trip_distance / length_time >= 0.00087 & trip_distance / length_time <= 0.012),
         (fare_amount>=3 & fare_amount<=70)) %>%
  group_by(Area_pickup, Area_dropoff) %>%
  summarize(media_tempo = mean(length_time), media_distanza = mean(trip_distance), media_tratta=mean(fare_amount))

x_validation <- x_validation %>% left_join(unire, by=c("Area_pickup", "Area_dropoff"))
anyNA(x_validation)


x_validation$length_time<-ifelse(x_validation$length_time > 7200, x_validation$media_tempo,
                             ifelse(x_validation$length_time < 60, x_validation$media_tempo, x_validation$length_time*1))

x_validation$trip_distance<-ifelse(x_validation$trip_distance< 0.05, x_validation$media_distanza, 
                               ifelse(x_validation$trip_distance>30, x_validation$media_distanza, x_validation$trip_distance*1))

x_validation$velocita<-(x_validation$trip_distance/x_validation$length_time)

for (i in 1:nrow(x_validation)) {
  if (x_validation$velocita[i] < 0.00087 || x_validation$velocita[i] > 0.012) {
    x_validation$length_time[i] <- x_validation$media_tempo[i]
    x_validation$trip_distance[i] <- x_validation$media_distanza[i]
  }
}
rm(i)


x_validation$velocita<-(x_validation$trip_distance) / (x_validation$length_time)

x_validation$fare_amount<-ifelse(x_validation$fare_amount < 3, x_validation$media_tratta,
                             ifelse(x_validation$fare_amount > 70, x_validation$media_tratta, x_validation$fare_amount*1))

x_validation<-x_validation[,-c(18,17,16)]

#Provo a fare una prima previsione con il modello lineare
y_predict<-predict(mod_final, newdata=x_validation)
err_m1=nrow(x_validation)^-1 * sum(abs(y_predict-y_validation))
err_m1 #0.5996

#Al momento l'errore è intorno a 0.60. Il valore di per se non è alto, tuttavia 
#il modello coinvolge troppi parametri e rande difficile l'interpretazione.

#Inoltre è possibile che alcune covariate che ho lasciato per addestrare il modello
#non siano tutte utili e sopratutto non contribuiscono in modo rilevante alla stima 
#portando solo un'aumento di varianza e un trascurabile diminuizione di bias.


x_validation$tip_amount<-rep(1, nrow(x_validation)) #mi serve avere una colonna tip_amount nel set in cui voglio fare le previsioni.
x_validation<-x_validation[,-1]




##################VARIABLE SELECTION###################
# Ridge regression
#Questo è uno dei metodi che si utilizza per fare "shrinkage", ossia è un metodo di 
#regressione con l'aggiunta di un vincolo (una penalità) che serve per "avvicinare" i coefficienti
#stimati a zero. Questo colpisce sopratutto le variabili ridondanti (ossia che danno
#un informazione già presente nei dati e che influiscono poco sulla previsione).

library(glmnet)

#Sistemo i predittori nella forma matriciale che richiede la funzione glmnet
X_shrinkage <- model.matrix(tip_amount ~ ., data = usa)[, -1] 
#e anche la variabile risposta separatamente
Y_shrinkage <- usa$tip_amount 

#La penalità della ridge è stabilita da un parametro lambda. Questo lo scelgo
#creando un vasta lista di valori e facendo poi la cross validation implementata
#nel pacchetto glmnet.
lambda_ridge_grid <- exp(seq(-7, 7, length = 1000)) #va messo l'exp perché la libreria tratta poi lambda in scala logaritmica
#per la ridge ho impostato manualmente la griglia di valori in quanto di default non mi dava buoni risultati,
#Nella cross validation viene stimata la ridge per ogni valore di lambda da me scelto.  
#Per ognuno di esso si fa la media dell'errore commesso facendo la previsione su ogni k-fold (di default il training è diviso in 10 parti e a rotazione 1 gruppo fa da test set). 
ridge_crossvalidation<- cv.glmnet(X_shrinkage, Y_shrinkage, alpha = 0, lambda = lambda_ridge_grid) #alpha=0 per ridge
plot(ridge_crossvalidation) #visualizzo graficamente

lambda_ri_optimal<-ridge_crossvalidation$lambda.1se 
#decido di prendere il lambda del quantile perché da quel punto 
#in poi l'errore sembra calare di poco.
ridge_crossvalidation$lambda.1se

#stimo il modello con ridge con il lambda ottimale trovato
mod_ridge <- glmnet(X_shrinkage, Y_shrinkage, alpha = 0, lambda = lambda_ri_optimal)

#prevedo
y_hat_ridge <- predict(mod_ridge, newx = model.matrix(tip_amount ~ ., data = x_validation)[, -1])
err_ri=nrow(x_validation)^-1 * sum(abs(y_hat_ridge-y_validation))
err_ri #0.6003

#ottengo un valore simile al modello lineare precedente. In parte lo sospettavo in quanto dalla
#teoria, sappiamo che un valore di lambda vicino a zero mi da dei parametri simili a quella
#della stima normale OLS classica.



#(relaxed lasso) LARS
#Provo ad utilizzare la versione "rilassata" del lasso in cui, nel processo di implementazione
#vengono aggiunte una alla volta le variabili a seconda del loro legame con la variabile risposta.

#Utilizzando la funzione lars della libreria lars, posso decidere manualmente quando fermare 
#l'algoritmo stabilendo il max.steps, che sarebbe il massimo numero di variabili che può aggiungere.


library(lars)
#uso la cross validation implementata nella libreria lars per
#trovare il numero ottimale di steps.
set.seed(32)
cv_lar <- cv.lars(X_shrinkage, Y_shrinkage, type = "lar")
#Dal grafico vedo che già che dopo l'aggiunta della prima variabile
#l'errore si abbassa notevolmente e l'aggiunta delle altre lo fa abbassare in maniera minima.

cv_lar$cv.error
summary(cv_lar$cv.error) #vedo che il 20 indice corrisponde al 1 quantile (cerco di usare lo stesso criterio di scelta di prima e non prendo il valore più basso)

optimal_step<-20

#Creo il nuovo modello col valore ottimale scelto
model_lar <- lars(X_shrinkage, Y_shrinkage, type = "lar", max.steps = optimal_step)


x_validation_1<- model.matrix(tip_amount ~ ., data = x_validation)[, -1]
#prevedo
predictions <- predict(model_lar, newx = x_validation_1)
y_hat_lar<-predictions$fit[,optimal_step]

err_lar=nrow(x_validation)^-1 * sum(abs(y_hat_lar-y_validation))
err_lar #0.5970

model_lar$beta
#Con LAR riesco a fare un errore più basso rispetto agli altri modelli.
#Inoltre inizio un pochettino ad abbassare la complessità di quest'ultimi infatti
#ho 20 coefficienti stimati (negli altri 2 casi ne avevo anche più di 40).

#Osservo che la variabile in pole position è fare_amount seguita da trip_distance e
#poi una serie di coefficienti che si riferiscono ad alcune modalità di dropoff e pickup.
#Tra i coefficienti sono presenti anche length_time, orario notturno e vendor_idVerfione.


#lasso
#utlizzo il lasso "completo", il quale è un metodo di regressione che impone ai coefficienti
#una penalità differente (di tipo additiva) rispetto alla ridge regression che permette
#di mandare subito a 0 alcuni di essi dopo una certa soglia.
#Questo permette di "selezionare" solo le variabili più importanti rendendo i modelli
#più interpretabili.

#come nella ridge anche qui ho un parametro lambda da selezionare attraverso cross validation
set.seed(32)
lasso_cv <- cv.glmnet(X_shrinkage, Y_shrinkage, alpha = 1) #uso la griglia di default
plot(lasso_cv)

best_lambda <- lasso_cv$lambda.1se #scelgo il minimo del 1 quantile

#riaddestro il modello
lasso_model <- glmnet(X_shrinkage, Y_shrinkage, alpha = 1, lambda = best_lambda) #alpha 1 è per LASSO

#prevedo
y_hat_lasso <- c(predict(lasso_model, newx = x_validation_1))
err_las=nrow(x_validation)^-1 * sum(abs(y_hat_lasso-y_validation))
err_las #0.5958

lasso_model$beta[c(which(lasso_model$beta!=0)),]

#vedo che l'errore è leggermente più basso rispetto agli altri modelli.
#In questo caso vengono utilizzate 12 variabili. Che si possono visualizzare col comando sopra.

#vedendo che la metà di questi coefficienti si riferiscono quasi sempre a zone geografiche
#mi è venuto in mente di rimplementare il lasso con una sorta di interazione.

#L'idea è quella di non considerare area_dropoff e pick_up separate, ma considerarle "combinate".
#Avendo già singolarmente molte modalità, la loro interazione porterà il modello ad avere
#moltissimi coefficienti, tuttavia, quello che voglio è vedere se il lasso
#mi è in grado di catturare solo le combinazioni più importanti che servono 
#per stimare bene la mancia.

#lasso con interazione
#Ho deciso di riscrivere quindi tutto il modello con tutte le variabili.
#Ho deciso di lasciare solo fuori le coordinate in quanto le ho usate per calcolare distanza_gps e perché
#come unico riferimento geografico voglio le tratte, create dalla combinazione di dropoff e pickup.

X_shrinkage <- model.matrix(tip_amount ~ fare_amount+Area_dropoff*Area_pickup+trip_distance+length_time+velocita+distanza_gps+pickup_wday+pickup_hour+vendor_id+passenger_count, data = usa)[, -1] 

#rifaccio la cv
set.seed(32)
lasso2_cv <- cv.glmnet(X_shrinkage, Y_shrinkage, alpha = 1) #uso griglia di default
plot(lasso2_cv)

best_lam <- lasso2_cv$lambda.1se
lasso2_model <- glmnet(X_shrinkage, Y_shrinkage, alpha = 1, lambda = best_lam)
y_hat_lasso2 <- c(predict(lasso2_model, newx = model.matrix(tip_amount ~ fare_amount+Area_dropoff*Area_pickup+trip_distance+length_time+velocita+distanza_gps+pickup_wday+pickup_hour+vendor_id+passenger_count, data = x_validation)[, -1]))
err_las_in=nrow(x_validation)^-1 * sum(abs(y_hat_lasso2-y_validation))
err_las_in #0.5878

#L'errore vedo che cala rispetto a prima tuttavia il numero di coefficienti è aumentato.
(which(lasso2_model$beta!=0))
lasso2_model$beta[c(which(lasso2_model$beta!=0)),] 

#Sostanzialmente questo modello ci permette di fare meno errore aggiungiendo, oltre
#alle variabili trip_distance, fare_amount, length_time, pickup_hourorarionotturno che mi dava
#nel modello lasso normale. Una serie di coefficienti che "si attivano" e fanno aumentare/diminuire
#la mancia a seconda delle tratta (a parità di altre covariate).

#su kaggle è quello che mi ha fatto ottenere un punteggio più basso, tuttavia
#se dovessi selezionare un modello preferito chiaramente sceglierei il lasso precedente in quanto
#ho un terzo delle variabili, è un modello più semplice da capire e l'errore è di 
#poco più alto. (successivamente c'è un modello che fa leggermente meglio usando sempre poche variabili).



#Elastic net 
#Ho provato l'elastic net che sostanzialmente è una via di mezzo tra ridge e lasso
#infatti, attraverso una doppia penalità, riesce a fare sia shrinkage che variable selection.
set.seed(32)
X_shrinkage <- model.matrix(tip_amount ~ ., data = usa)[, -1] #risetto le due matrici senza interazioni
Y_shrinkage <- usa$tip_amount 

lambda_en_grid <- exp(seq(-8, 0, length = 2000))
en_cv <- cv.glmnet(X_shrinkage, Y_shrinkage, alpha = 0.5, lambda = lambda_en_grid)
plot(en_cv)

lambda_en_optimal<-en_cv$lambda.1se
mod_en<-glmnet(X_shrinkage, Y_shrinkage, alpha = 0.5, lambda = lambda_en_optimal)
y_hat_en <- predict(mod_en, newx = model.matrix(tip_amount ~ ., data = x_validation)[, -1])
err_en=nrow(x_validation)^-1 * sum(abs(y_hat_en-y_validation))
err_en #0.5951

as.data.frame(as.matrix(mod_en$beta)) %>% filter(s0!=0)

#Questo modello fa un errore minore del lasso semplice (ma maggiore del lasso complesso)
#ed usa 15 variabili.






#########################################################################################################################
##############################################################################################################
###################################Faccio nel test le stesse trasformazioni####################
test_data<-read.csv("test.csv", header = TRUE)
test_data$pickup_hour<-ifelse(test_data$pickup_hour %in% c(16, 17, 18, 19, 20, 21, 22, 23, 0, 1, 2, 3, 4, 5, 6), "Orario Notturno", "Orario Normale")
test_data$pickup_hour<-as.factor(test_data$pickup_hour)
test_data<-test_data[,-3]
test_data$pickup_wday<-ifelse(test_data$pickup_wday %in% c(6,7), "Weekend", "NormalDay")
test_data$pickup_wday<-as.factor(test_data$pickup_wday)
test_data<-test_data[,-c(3,4)]
x_test<-test_data
library(readxl)
division<-read_excel("area_specific_2.xlsx")
colnames(division)[colnames(division) == "Code"] <- "pickup_NTACode"
x_test <- x_test %>% left_join(division, by="pickup_NTACode")
table(x_test$Place)
colnames(division)[colnames(division) == "pickup_NTACode"] <- "dropoff_NTACode"
colnames(division)[colnames(division) == "Place"] <- "Place2"
x_test <- x_test %>% left_join(division, by="dropoff_NTACode")
table(x_test$Place2)
table(c(x_test$Place, x_test$Place2))
x_test<-x_test[,-c(5,6,7,8,17)] 
names(x_test)[names(x_test) == "Place"] <- "Area_pickup"
names(x_test)[names(x_test) == "Place2"] <- "Area_dropoff"
x_test$Area_pickup<-as.factor(x_test$Area_pickup)
x_test$Area_dropoff<-as.factor(x_test$Area_dropoff)
rm(division)
x_test$passenger_count <- ifelse(x_test$passenger_count == 1, "Solo", 
                                       ifelse(x_test$passenger_count == 2, "Coppia", "Group"))
x_test$passenger_count<-as.factor(x_test$passenger_count)
x_test$vendor_id<-as.factor(x_test$vendor_id)


#anomalie
library(geosphere)
x_test$distanza_gps <- apply(x_test[,c(5:8)], 1, function(row) {
  distVincentySphere(
    p1 = c(row["pickup_longitude"], row["pickup_latitude"]),
    p2 = c(row["dropoff_longitude"], row["dropoff_latitude"])
  ) * 0.000621371
})

unire <- x_test %>%
  filter((length_time >= 60 & length_time <= 7200), 
         (trip_distance >= 0.05 & trip_distance<=30),
         (trip_distance>=distanza_gps-1),
         (trip_distance / length_time >= 0.00087 & trip_distance / length_time <= 0.012),
         (fare_amount>=3 & fare_amount<=70)) %>%
  group_by(Area_pickup, Area_dropoff) %>%
  summarize(media_tempo = mean(length_time), media_distanza = mean(trip_distance), media_tratta=mean(fare_amount))

x_test <- x_test %>% left_join(unire, by=c("Area_pickup", "Area_dropoff"))
anyNA(x_test)
sum(is.na(x_test)==TRUE)

#Siccome ci sono 6 NA (pochi, ma ci sono)
#faccio un'altra imputazione basando le medie su orario, giorno e vendor_id
unire2 <- x_test %>% group_by(pickup_hour, pickup_wday, vendor_id) %>% summarise(media_tempona=mean(length_time), 
                                                                                       media_distanzana=mean(trip_distance), 
                                                                                       media_trattana=mean(fare_amount))

x_test <- x_test %>% left_join(unire2, by=c("pickup_hour", "pickup_wday", "vendor_id"))

#sostituisco le medie_na con quelle calcolate adesso
x_test$media_distanza<-ifelse(is.na(x_test$media_distanza)==TRUE, x_test$media_distanzana, x_test$media_distanza*1)
x_test$media_tratta<-ifelse(is.na(x_test$media_tratta)==TRUE, x_test$media_trattana, x_test$media_tratta*1)
x_test$media_tempo<-ifelse(is.na(x_test$media_tempo)==TRUE, x_test$media_tempona, x_test$media_tempo*1)

x_test$length_time<-ifelse(x_test$length_time > 7200, x_test$media_tempo,
                                 ifelse(x_test$length_time < 60, x_test$media_tempo, x_test$length_time*1))

x_test$trip_distance<-ifelse(x_test$trip_distance< 0.05, x_test$media_distanza, 
                                   ifelse(x_test$trip_distance>30, x_test$media_distanza, x_test$trip_distance*1))

x_test$velocita<-(x_test$trip_distance/x_test$length_time)

for (i in 1:nrow(x_test)) {
  if (x_test$velocita[i] < 0.00087 || x_test$velocita[i] > 0.012) {
    x_test$length_time[i] <- x_test$media_tempo[i]
    x_test$trip_distance[i] <- x_test$media_distanza[i]
  }
}
rm(i)
#ricalcolo le velocità "pulite"
x_test$velocita<-(x_test$trip_distance) / (x_test$length_time)

x_test$fare_amount<-ifelse(x_test$fare_amount < 3, x_test$media_tratta,
                                 ifelse(x_test$fare_amount > 70, x_test$media_tratta, x_test$fare_amount*1))

x_test<-x_test[,-c(21,20,19,18,17,16)]       




x_test$tip_amount<-rep(1, nrow(x_test)) #metto lasso_model per avere un test con 0.58317
y_hat_final <- c(predict(lasso2_model, newx = model.matrix(tip_amount ~ fare_amount+Area_dropoff*Area_pickup+trip_distance+length_time+velocita+distanza_gps+pickup_wday+pickup_hour+vendor_id+passenger_count, data = x_test)[, -1]))
head(y_hat_final)

#salvare file excel
dati <- data.frame( ID=(test_data$ID), predictions=c(y_hat_final))
percorso_file <- "submissionmonforte.csv"

# Esporta in CSV
write.csv(dati, file = percorso_file, row.names = FALSE)
