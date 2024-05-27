###### CREDIT TO AUTHORS : ANNA PETRANZAN & ANTONIO MONFORTE #################

rm(list=ls())
star<-read.csv('star_classification.csv')

######################################## PRE-PROCESSING ###################################################
which(is.na(star)) #non ci sono NA

#analisi variabili: boxplot delle variabili quantitative
boxplot(star[,-c(1,9,10,11,12,13,14,16,17,18)])
#si nota un valore anomalo
which.min(star[,"u"]) #79544
#rimuoviamo questa riga 

star_new <-star[-79544,]

#vediamo ora i boxplot
boxplot(star[-79544,-c(1,9,10,11,12,13,14,16,17,18)], las=2) #alpha,delta e redshift hanno range diverso

#boxplot delle variabili riscalate
boxplot(scale(star[-79544,-c(1,9,10,11,12,13,14,16,17,18)]), las=2) 

#rimuoviamo le varibili di tipo ID
star_new<-star_new[,- c(1, 9:13, 16:18)]


library(ggplot2)

#boxplot condizionato alle classi di redshift
ggplot(star_new, aes(x = as.factor(class), y = redshift, fill = class)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "blue", "green")) +
  coord_flip() +
  labs(title = "Boxplot di redshift separati per classe",
       x = "Classe",
       y = "Redshift") +
  facet_wrap(~ class, ncol = 1) +
  theme_minimal()
#molto discriminante

#boxplot condizionato alle classi di i
ggplot(star_new, aes(x = as.factor(class), y = i, fill = class)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "blue", "green")) +
  coord_flip() +
  labs(title = "Boxplot di redshift separati per classe",
       x = "Classe",
       y = "i") +
  facet_wrap(~ class, ncol = 1) +
  theme_minimal()

#istogrammi condizionati alle classi
ggplot(star_new, aes(x = i, fill = class)) +  
  geom_histogram(aes(y = after_stat(density)*100),binwidth = 1, position = "identity", alpha = 0.6) +
  labs(title = "Istogrammi separati per classe",
       x = "Valore",
       y = "Percentuale sul totale") +
  scale_fill_manual(values = c("red", "blue", "green")) +  # Specifica i colori delle classi
  facet_grid(cols = vars(class), scales = "free_x")  # Organizzazione dei pannelli orizzontale

#boxplot condizionato alle classi di z
ggplot(star_new, aes(x = as.factor(class), y = z, fill = class)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "blue", "green")) +
  coord_flip() +
  labs(title = "Boxplot di redshift separati per classe",
       x = "Classe",
       y = "z") +
  facet_wrap(~ class, ncol = 1) +
  theme_minimal()

#istogrammi di z condizionalti alle classi
library(scales)
ggplot(star_new, aes(x = z, fill = class)) +  
  geom_histogram(aes(y = after_stat(density)*100),binwidth = 1, position = "identity", alpha = 0.6) +
  labs(title = "Istogrammi separati per classe",
       x = "z",
       y = "Percentuale sul totale") +
  scale_fill_manual(values = c("red", "blue", "green")) +  # Specifica i colori delle classi
  facet_grid(cols = vars(class), scales = "free_x")  # Organizzazione dei pannelli orizzontale


#boxplot condizionato alle classi di g
ggplot(star_new, aes(x = as.factor(class), y = g, fill = class)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "blue", "green")) +
  coord_flip() +
  labs(title = "Boxplot di redshift separati per classe",
       x = "Classe",
       y = "g") +
  facet_wrap(~ class, ncol = 1) +
  theme_minimal()

#istogrammi di g condizionalti alle classi
library(scales)
ggplot(star_new, aes(x = g, fill = class)) +  
  geom_histogram(aes(y = after_stat(density)*100),binwidth = 1, position = "identity", alpha = 0.6) +
  labs(title = "Istogrammi separati per classe",
       x = "g",
       y = "Percentuale sul totale") +
  scale_fill_manual(values = c("red", "blue", "green")) +  # Specifica i colori delle classi
  facet_grid(cols = vars(class), scales = "free_x")  # Organizzazione dei pannelli orizzontale

#boxplot condizionato alle classi di u
ggplot(star_new, aes(x = as.factor(class), y = u, fill = class)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "blue", "green")) +
  coord_flip() +
  labs(title = "Boxplot di redshift separati per classe",
       x = "Classe",
       y = "u") +
  facet_wrap(~ class, ncol = 1) +
  theme_minimal()

#istogrammi di u condizionalti alle classi
library(scales)
ggplot(star_new, aes(x = u, fill = class)) +  
  geom_histogram(aes(y = after_stat(density)*100),binwidth = 1, position = "identity", alpha = 0.6) +
  labs(title = "Istogrammi separati per classe",
       x = "u",
       y = "Percentuale sul totale") +
  scale_fill_manual(values = c("red", "blue", "green")) +  # Specifica i colori delle classi
  facet_grid(cols = vars(class), scales = "free_x")  # Organizzazione dei pannelli orizzontale

#boxplot condizionato alle classi di r
ggplot(star_new, aes(x = as.factor(class), y = r, fill = class)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "blue", "green")) +
  coord_flip() +
  labs(title = "Boxplot di redshift separati per classe",
       x = "Classe",
       y = "r") +
  facet_wrap(~ class, ncol = 1) +
  theme_minimal()

#istogrammi di r condizionati alle classi
ggplot(star_new, aes(x = r, fill = class)) +  
  geom_histogram(aes(y = after_stat(density)*100),binwidth = 1, position = "identity", alpha = 0.6) +
  labs(title = "Istogrammi separati per classe",
       x = "Valore",
       y = "Percentuale sul totale") +
  scale_fill_manual(values = c("red", "blue", "green")) +  # Specifica i colori delle classi
  facet_grid(cols = vars(class), scales = "free_x")  # Organizzazione dei pannelli orizzontale

#boxplot condizionato alle classi di alpha
ggplot(star_new, aes(x = as.factor(class), y = alpha, fill = class)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "blue", "green")) +
  coord_flip() +
  labs(title = "Boxplot di redshift separati per classe",
       x = "Classe",
       y = "alpha") +
  facet_wrap(~ class, ncol = 1) +
  theme_minimal()
#discrimina molto poco

#boxplot condizionato alle classi di delta
ggplot(star_new, aes(x = as.factor(class), y = delta, fill = class)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "blue", "green")) +
  coord_flip() +
  labs(title = "Boxplot di redshift separati per classe",
       x = "Classe",
       y = "delta") +
  facet_wrap(~ class, ncol = 1) +
  theme_minimal()
#discrimina molto poco

#verifica bilanciamento classi
prop.table(table(star_new$class))
#non molto bilanciato, ma proporzioni accettabili

#CORRELAZIONE
library(corrplot)
correlazione<-cor(star_new[,-8])
corrplot(correlazione, method="color", col=colorRampPalette(c("purple","cyan3","blue3"))(100), type="upper",   order="hclust", addCoef.col = "black", tl.col="black", tl.srt=45, diag=FALSE)

#Possiamo rappresentare coppie di variabili
#pairs(star_new[,c("r", 'z','i')], col=star_cols, pch=20)
library(GGally)
ggpairs(star_new[,c("r", 'z','i')],aes(colour = star_new[,'class']))

#ci sono delle correlazioni molto alte per decidere quali variabili rimuovere facciamo la variable importance
library(randomForest)
library(caret)
library(dplyr)
mod_0<-randomForest(as.factor(star_new$class)~., data = star_new, ntree=70, mtry=4)
var_imp <- varImp(mod_0, scale = F)
var_imp <- data.frame(variables=row.names(var_imp), importance=var_imp$Overall)

##plot of variable importance
var_imp %>%
  arrange(importance) %>% ## Ordiniamo i dati per importanza
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  geom_point(stat='identity', size=2.3) + 
  coord_flip() +  #giriamo il grafico per ottenere un grafico a barre orizzontali
  xlab('Variables') +
  labs(title='Random forest variable importance') + 
  theme(axis.text = element_text(size = 13), 
        axis.title = element_text(size = 13), 
        plot.title = element_text(size = 25), 
  )

#redshift è la variabile che risulta essere molto più importante #come già avevamo osservato dal boxplot, discrimina molto bene fra le classi

#decidiamo di rimuovere le variabili i e r
star_new<-star_new[,-c(5,6)]

############################################ TRAINING E TEST SET ############################################
set.seed(123)
index<-sample(1:dim(star_new)[1], replace = F, dim(star_new)[1]*0.8)
train <- star_new[index,]
test  <- star_new[-index,]
prop.table(table(train$class)) #verifichiamo le proporzioni in train
prop.table(table(test$class)) #verifichiamo le proporzioni in test

y_train<-as.factor(train[,"class"])
y_test<-as.factor(test[,"class"])

train_scale<-train
train_scale[,-6]<-scale(train[,-6])

test_scale<-test
test_scale[,-6]<-scale(test[,-6])

#################################################### K-MEANS ###############################################
set.seed(17)
km.res=kmeans(scale(star_new[,-6]), centers = 3)
km.res
prop.table(table(km.res$cluster))
prop.table(table(star_new$class[which(km.res$cluster==3)]))

############################################ RANDOM-FOREST ################################################
set.seed(123)
library(caret)
library(randomForest)

#creiamo le funzioni di supporto per poter fare repeated CV con iperparametri mtry e ntree
customRF <- list(type = "Classification",
                 library = "randomForest",
                 loop = NULL)

customRF$parameters <- data.frame(parameter = c("mtry", "ntree"),
                                  class = rep("numeric", 2),
                                  label = c("mtry", "ntree"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs) {
  randomForest(x, y,
               mtry = param$mtry,
               ntree=param$ntree)
}

#Predict label
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)

#Predict prob
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")

customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

#consideriamo 8 fold
control <- trainControl(method="repeatedcv", 
                        number=8, 
                        repeats=3,
                        allowParallel = TRUE)

#inizializziamo la griglia degli iperparametri 
tunegrid <- expand.grid(.ntree=seq(1,600,by=250), .mtry=seq(1,6, by=1))

set.seed(123)
metric <- "Accuracy"
custom <- train(class~., data=train, 
                method=customRF, 
                metric=metric, 
                tuneGrid=tunegrid, 
                trControl=control)

rf_pars <- custom$bestTune #iperparametri che manterremo da qui in poi

print(rf_pars)
plot(custom)

mean(1- custom$resample$Accuracy) #generalized error 0.02250448

#per calcolare empirical error
set.seed(7)
custom_train<-randomForest(as.factor(class) ~ .,data=train, ntree = rf_pars$ntree, mtry = rf_pars$mtry)
cf <- confusionMatrix(custom_train$predicted, y_train)
print(cf) #confusion matrix
1-cf$overall['Accuracy'] #0.02265028 

#per calcolare test error e accuracy finale
set.seed(7)
prediction <- predict(custom_train, newdata = test)
cf_test <- confusionMatrix(prediction, y_test)
print(cf_test) #confusion matrix
cf_test$overall['Accuracy'] # 0.97775 
1-cf_test$overall['Accuracy'] #test error  0.02225 

############################################ KNN #########################################
# Consideriamo 8-fold cross validation
set.seed(123)
trainControl <- trainControl(method="repeatedcv", number=8, repeats=8) 
metric <- "Accuracy"

set.seed(7)
grid <- expand.grid(.k=seq(1,20,by=1)) #k da testare

#poichè lavoriamo con la distanza (euclidea), riscaliamo i dati 
fit.knn <- train(class~., data=train, method="knn",
                 preProcess=c("scale"),
                 metric = metric, tuneGrid=grid, trControl=trainControl)
knn.k2 <- fit.knn$bestTune #k ottimo 3, manteniamo questo k d'ora in poi
print(knn.k2)
plot(fit.knn)
mean(1-fit.knn$resample$Accuracy) #generalized error 0.06182734

#per calcolare empirical error
set.seed(7)
fit.knn_train<-knn3(as.factor(class)~., data=train_scale, k = knn.k2$k)
prediction <- predict(fit.knn_train, newdata = train_scale, type='class')
cf <- confusionMatrix(prediction, y_train)
print(cf) #confusion matrix
1-cf$overall['Accuracy'] #0.03788797

#per calcolare test error e accuracy finale
set.seed(7)
prediction <- predict(fit.knn_train, newdata = test_scale, type='class')
cf_test <- confusionMatrix(prediction, y_test)
print(cf_test) #confusion matrix
cf_test$overall['Accuracy'] # 0.93835 
1-cf_test$overall['Accuracy'] #test error   0.06165


##################################################### NN ################################################
set.seed(123)
library(caret)
#5 fold per la CV, 3 ripetizioni
control <- trainControl(method = "repeatedcv", number = 5, repeats = 2)

#inizializzazione iperparametri da testare
tuningGrid <- expand.grid(
  .size = seq(1,6, by=1),  
  .decay = c(0.01, 0.001, 0.0001, 0.2, 0.8, 2.3)  # Valori di decay
)

#vengono usati i dati scalati
modello <- train(
  class ~. , data=train,  preProcess=c("scale"),
  method = "nnet",
  trControl = control,
  tuneGrid = tuningGrid,
  trace=T,
  act.fct = 'softmax',  
  metric='Accuracy'
)

plot(modello)
nn <- modello$bestTune # iperparametri ottimi size 6, decay=0.0001
print(nn)
mean(1-modello$resample$Accuracy) #generalized error 0.03417543

mod_nn_best<-nnet(as.factor(class) ~. , data=train_scale, size=nn$size, decay=nn$decay)

#per calcolare empirical error
set.seed(7)
prediction <- predict(mod_nn_best, newdata = train_scale, type="class")
cf <- confusionMatrix(as.factor(prediction), y_train)
print(cf) #confusion matrix
1-cf$overall['Accuracy'] # empirical error 0.03158789

#per calcolare test error e accuracy finale
set.seed(7)
prediction <- predict(mod_nn_best, newdata = test_scale, type="class")
cf_test <- confusionMatrix(as.factor(prediction), y_test)
print(cf_test) #confusion matrix
cf_test$overall['Accuracy'] #  0.96875
1-cf_test$overall['Accuracy'] #test error 0.03125


############################################### SVM RADIAL KERNEL ##########################################
set.seed(123)
library(caret)

# Definisco i controlli per repeated CV
ctrl <- trainControl(method = "repeatedcv", 
                     number = 5, # Numero di fold di validazione incrociata
                     repeats = 2 # Numero di ripetizioni
) # Metodo di ricerca degli iperparametri

# Definisco la griglia degli iperparametri da testare
grid <- expand.grid(.C = c(0.1, 1, 10), # Valori di C da testare
                    .sigma = c(0.1, 1, 10)) # Valori di sigma da testare

#Addestro il modello SVM con repeated CV
model <- train(class ~ . , 
               data=train,  preProcess=c("scale"),
               method = "svmRadial", # Utilizziamo il kernel radiale per SVM
               trControl = ctrl,
               tuneGrid = grid)

plot(model)
svm_best <- model$bestTune #C e sigma scelti
print(svm_best)
mean(1-model$resample$Accuracy) #generalized error 0.0323879


#per calcolare empirical error
library(e1071)
set.seed(7)
model_train<- svm(as.factor(class) ~ . , data=train_scale, kernel= 'radial', cost= svm_best$C, gamma= 1/(2*svm_best$sigma^2))
cf <- confusionMatrix(model_train$fitted, y_train)
1-cf$overall['Accuracy'] # empirical error 1.250016e-05

#verifichiamo il numeor di SV utilizzato dal modello prescelto
model_train #75854

#per calcolare test error e accuracy finale
set.seed(7)
prediction <- predict(model_train, newdata = test_scale)
cf_test <- confusionMatrix(prediction, y_test)
print(cf_test)
cf_test$overall['Accuracy'] # 0.768  
1-cf_test$overall['Accuracy'] #test error   0.232

############################################### SVM POLYNOMIAL KERNEL ####################################
#set.seed(123)
#library(caret)
#
## Prepara i tuoi dati di addestramento e test
#
## Definisci i controlli per repeated CV
#ctrl <- trainControl(method = "repeatedcv", 
#                     number = 5, # Numero di fold di validazione incrociata
#                     repeats = 2 # Numero di ripetizioni
#) # Metodo di ricerca degli iperparametri
#
## Definisci la griglia degli iperparametri da testare
#grid1 <- expand.grid(.degree =c(1:4), #grado del polinomia
#                     .C = c(0.1, 1, 10), # Valori di C da testare
#                     .sigma = c(0.1, 1, 10)) # Valori di sigma da testare
#
## Addestra il modello SVM con repeated CV
#model1 <- train(class ~ . , 
#                data=train,  preProcess=c("scale"),
#                method = "svmPoly", # Utilizza il kernel polinomiale per SVM
#                trControl = ctrl,
#                tuneGrid = grid1)
#
#plot(model1)
#svm_best <- model1$bestTune # parametri selezionati
#print(svm_best)
#mean(1-model1$resample$Accuracy) 
#
#
##per calcolare empirical error
#set.seed(7)
#model1_train<- svm(as.factor(class) ~ . , data=train_scale, kernel= 'polynomial', cost= svm_best$C, gamma= 1/(2*svm_best$sigma^2), degree=svm_best$degree)
#
#verifichiamo il numeor di SV utilizzato dal modello prescelto
#model1_train$
#
#cf <- confusionMatrix(model1_train$fitted, y_train)
#1-cf$overall['Accuracy'] 
#
##per calcolare test error e accuracy finale
#set.seed(7)
#prediction <- predict(model1_train, newdata = test_scale)
#cf_test <- confusionMatrix(prediction, y_test)
#print(cf_test)
#cf_test$overall['Accuracy'] 
#1-cf_test$overall['Accuracy'] 

##################################################### KNN SENZA REDSHIFT ####################################
# Consideriamo 8-fold cross validation
set.seed(123)
trainControl <- trainControl(method="repeatedcv", number=8, repeats=8)
metric <- "Accuracy"

grid <- expand.grid(.k=seq(1,20,by=1)) #k da testare

#usiamo i dati riscalati, a meno di redshift
fit.knn_no_rs <- train(class~.-redshift, data=train, method="knn",
                       preProcess=c("scale"),
                       metric=metric, tuneGrid=grid, trControl=trainControl)
knn.k2_no_rs <- fit.knn_no_rs$bestTune
#k=9 best, manteniamo questo k d'ora in poi
print(knn.k2_no_rs)
plot(fit.knn_no_rs)
mean(1-fit.knn_no_rs$resample$Accuracy) #generalized error 0.1721537

#per calcolare empirical error
set.seed(7)
fit.knn_no_rs_train<-knn3(as.factor(class)~.-redshift, data=train_scale, k = knn.k2_no_rs$k)
prediction_no_rs <- predict(fit.knn_no_rs_train, newdata = train_scale, type='class')
cf_no_rs <- confusionMatrix(prediction_no_rs, y_train)
print(cf_no_rs) #confusion matrix
1-cf_no_rs$overall['Accuracy'] #0.1470643

#per calcolare test error e accuracy finale
set.seed(7)
prediction_no_rs <- predict(fit.knn_no_rs_train, newdata = test_scale, type='class')
cf_test_no_rs <- confusionMatrix(prediction_no_rs, y_test)
print(cf_test_no_rs) #confusion matrix
cf_test_no_rs$overall['Accuracy'] 
1-cf_test_no_rs$overall['Accuracy'] #test error  0.1688


##################################################### KNN SOLO CON REDSHIFT ##############################
# Consideriamo 8-fold cross validation
set.seed(7)
trainControl <- trainControl(method="repeatedcv", number=8, repeats=8)
metric <- "Accuracy"

grid <- expand.grid(.k=seq(10,30,by=1)) # k da testare 

#consideriamo come unica variabile redshift riscalata
fit.knn_rs <- train(class~redshift, data=train, method="knn",
                    preProcess=c("scale"),
                    metric=metric, tuneGrid=grid, trControl=trainControl)

knn.k2_rs <- fit.knn_rs$bestTune 
#k ottimo 26 
print(fit.knn_rs)
plot(fit.knn_rs)
mean(1-fit.knn_rs$resample$Accuracy) #generalized error 0.05004748

#per calcolare empirical error
set.seed(7)
fit.knn_rs_train<-knn3(as.factor(class)~redshift, data=train_scale, k = knn.k2_rs$k)
prediction_rs <- predict(fit.knn_rs_train, newdata = train_scale, type='class')
cf_rs <- confusionMatrix(prediction_rs, y_train)
print(cf_rs) #confusion matrix
1-cf_rs$overall['Accuracy'] #0.04905061

#per calcolare test error e accuracy finale
set.seed(7)
prediction_rs <- predict(fit.knn_rs_train, newdata = test_scale, type='class')
cf_test_rs <- confusionMatrix(prediction_rs, y_test)
print(cf_test_rs) #confusion matrix
cf_test_rs$overall['Accuracy']
1-cf_test_rs$overall['Accuracy'] #test error 0.0542

############################################ NN SOLO REDSHIFT #######################################
library(caret)
set.seed(123)
#5 fold per la CV, 2 ripetizioni
control <- trainControl(method = "repeatedcv", number = 5, repeats = 2)

#inizializzazione iperparametri da testare
tuningGrid <- expand.grid(
  .size = 1,  
  .decay = c(0.01, 0.001, 0.0001, 0.2, 0.8, 2.3)  # Valori di decay
)

#viene usata unicamente la variabile redshift riscalata
modello_rs <- train(
  class ~ redshift , data=train,  preProcess=c("scale"),
  method = "nnet",
  trControl = control,
  tuneGrid = tuningGrid,
  act.fct = 'softmax',
  trace=T
)
plot(modello_rs)
nn_rs <- modello_rs$bestTune # iperparametri ottimi decay=0.0001 e size=1
print(nn_rs)
mean(1-modello_rs$resample$Accuracy) #generalized error 0.05433194

#per calcolare empirical error
mod_nn_rs_best<-nnet(as.factor(class) ~ redshift , data=train_scale, size=nn_rs$size, decay=nn_rs$decay)
set.seed(7)
prediction_rs <- predict(mod_nn_rs_best, newdata = train_scale, type="class")
cf_rs <- confusionMatrix(as.factor(prediction_rs), y_train)
print(cf_rs) #confusion matrix
1-cf_rs$overall['Accuracy'] # empirical error 0.05391317 

#per calcolare test error e accuracy finale
set.seed(7)
prediction_rs <- predict(mod_nn_rs_best, newdata = test_scale, type="class")
cf_test_rs <- confusionMatrix(as.factor(prediction_rs), y_test)
print(cf_test_rs) #confusion matrix
cf_test_rs$overall['Accuracy'] #  0.94585
1-cf_test_rs$overall['Accuracy'] #test error  0.05415  

############################################ NN SENZA REDSHIFT #######################################
library(caret)
set.seed(123)
#5 fold per la CV, 3 ripetizioni
control <- trainControl(method = "repeatedcv", number = 5, repeats = 2)

#inizializzazione iperparametri da testare
tuningGrid <- expand.grid(
  .size = seq(1,5, by=1),  
  .decay = c(0.01, 0.001, 0.0001, 0.2, 0.8, 2.3)  # Valori di decay
)

#vengono usate le variabili riscalate, ma non redshift
modello_no_rs <- train(
  class ~ .- redshift , data=train,  preProcess=c("scale"),
  method = "nnet",
  trControl = control,
  act.fct = 'softmax',
  tuneGrid = tuningGrid
)
plot(modello_no_rs)
nn_no_rs <- modello_no_rs$bestTune # iperparametri ottimi #size 5, #decay 0.2
print(nn_no_rs)
mean(1-modello_no_rs$resample$Accuracy) #generalized error 0.1934024

mod_no_rs_best<-nnet(as.factor(class) ~ .- redshift , data=train_scale, size=nn_no_rs$size, decay=nn_no_rs$decay)

#per calcolare empirical error
set.seed(7)
prediction_no_rs <- predict(mod_no_rs_best, newdata = train_scale, type="class")
cf_no_rs <- confusionMatrix(as.factor(prediction_no_rs), y_train)
print(cf_no_rs) #confusion matrix
1-cf_no_rs$overall['Accuracy'] # empirical error 0.1787022  

#per calcolare test error e accuracy finale
set.seed(7)
prediction_no_rs <- predict(mod_no_rs_best, newdata = test_scale, type="class")
cf_test_no_rs <- confusionMatrix(as.factor(prediction_no_rs), y_test)
print(cf_test_no_rs) #confusion matrix
cf_test_no_rs$overall['Accuracy'] # 0.82315 
1-cf_test_no_rs$overall['Accuracy'] #test error   0.17685

####################################### SVM RADIAL KERNEL SOLO REDSHIFT ####################################
set.seed(123)
library(caret)
# Definisco i controlli per repeated CV
ctrl <- trainControl(method = "repeatedcv", 
                     number = 5, # Numero di fold di validazione incrociata
                     repeats = 2 # Numero di ripetizioni
) # Metodo di ricerca degli iperparametri

# Definisco la griglia degli iperparametri da testare
grid <- expand.grid(.C = c(0.1, 1, 10), # Valori di C da testare
                    .sigma = c(0.1, 1, 10)) # Valori di sigma da testare

# Addestro il modello SVM con repeated CV, utilizzando solo redshift scalata
model_rs <- train(class ~ redshift, 
                  data=train,  preProcess=c("scale"),
                  method = "svmRadial", # Utilizzo il kernel radiale per SVM
                  trControl = ctrl,
                  tuneGrid = grid,
                  trace=T)

plot(model_rs)
svm_best_rs <- model_rs$bestTune # iperparametri scelti
print(svm_best_rs)
mean(1-model_rs$resample$Accuracy) #generalized error 0.05291941
modello_rs$finalModel #per vedere il numero di SV utilizzate

#per calcolare empirical error
set.seed(7)
model_rs_train<- svm(as.factor(class) ~ redshift , data=train_scale, kernel= 'radial', cost= svm_best_rs$C, gamma= 1/(2*svm_best_rs$sigma^2))
cf_rs <- confusionMatrix(model_rs_train$fitted, y_train)
print(cf_rs) #confusion matrix
1-cf_rs$overall['Accuracy'] # 0.06810085 

#per calcolare test error e accuracy finale
set.seed(7)
prediction_rs <- predict(model_rs_train, newdata = test_scale)
cf_test_rs <- confusionMatrix(prediction_rs, y_test)
print(cf_test_rs) #confusion matrix
cf_test_rs$overall['Accuracy'] # 0.92795  
1-cf_test_rs$overall['Accuracy'] #test error   0.07205

####################################### SVM RADIAL KERNEL SENZA REDSHIFT ####################################
set.seed(123)
library(caret)

# Definisco i controlli per repeated CV
ctrl <- trainControl(method = "repeatedcv", 
                     number = 5, # Numero di fold di validazione incrociata
                     repeats = 2 # Numero di ripetizioni
) # Metodo di ricerca degli iperparametri

# Definisco la griglia degli iperparametri da testare
grid <- expand.grid(.C = c(0.1, 1, 10), # Valori di C da testare
                    .sigma = c(0.1, 1, 10)) # Valori di sigma da testare

# Addestro il modello SVM con repeated CV, utilizzo le variabili scalate, ma non uso redshift
model_no_rs <- train(class ~ .-redshift, 
                     data=train,    
                     preProcess=c("scale"),
                     method = "svmRadial", # Utilizza il kernel radiale per SVM
                     trControl = ctrl,
                     tuneGrid = grid)

plot(model_no_rs)
svm_best_no_rs <- model_no_rs$bestTune # iperparametri scelti
print(svm_best_no_rs)
mean(1-model_no_rs$resample$Accuracy) #generalized error 0.1414768
modello_rs$finalModel #per vedere il numero di SV utilizzate

#per calcolare empirical error
set.seed(7)
model_no_rs_train<- svm(as.factor(class) ~ .-redshift , data=train_scale, kernel= 'radial', cost= svm_best_no_rs$C, gamma= 1/(2*svm_best_no_rs$sigma^2))
cf_no_rs <- confusionMatrix(model_no_rs_train$fitted, y_train)
print(cf_no_rs) #confusion matrix
1-cf_no_rs$overall['Accuracy'] # empirical error 0.1380142

#per calcolare test error e accuracy finale
set.seed(7)
prediction_no_rs <- predict(model_no_rs_train, newdata = test_scale)
cf_test_no_rs <- confusionMatrix(prediction_no_rs, y_test)
print(cf_test_no_rs) #confusion matrix
cf_test_no_rs$overall['Accuracy'] # 0.8607
1-cf_test_no_rs$overall['Accuracy'] #test error   0.1393


############################################ RANDOM-FOREST SENZA REDSHIFT ################################################
set.seed(123)
library(caret)
library(randomForest)

#creiamo le funzioni di supporto per poter fare repeated CV con iperparametri mtry e ntree
customRF <- list(type = "Classification",
                 library = "randomForest",
                 loop = NULL)

customRF$parameters <- data.frame(parameter = c("mtry", "ntree"),
                                  class = rep("numeric", 2),
                                  label = c("mtry", "ntree"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs) {
  randomForest(x, y,
               mtry = param$mtry,
               ntree=param$ntree)
}

#Predict label
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)

#Predict prob
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")

customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

#consideriamo 8 fold
control <- trainControl(method="repeatedcv", 
                        number=8, 
                        repeats=3,
                        allowParallel = TRUE)

#inizializziamo la griglia degli iperparametri 
tunegrid <- expand.grid(.ntree=seq(249,800,by=250), .mtry=seq(1,5, by=1))

set.seed(123)
metric <- "Accuracy"
#non usiamo redshift
custom_no_rs <- train(class~.-redshift, data=train, 
                method=customRF, 
                metric=metric, 
                tuneGrid=tunegrid, 
                trControl=control)

rf_pars <- custom_no_rs$bestTune #iperparametri che manterremo da qui in poi

print(rf_pars)
plot(custom_no_rs)

mean(1- custom_no_rs$resample$Accuracy) #generalized error 0.1425684

#per calcolare empirical error
set.seed(7)
custom_no_rs_train<-randomForest(as.factor(class) ~ .- redshift, data=train, ntree = 501, mtry = 3)
cf_test <- confusionMatrix(custom_no_rs_train$predicted, y_train)
print(cf_test)
1-cf_test$overall['Accuracy'] #empirical error  0.1438393 

#per calcolare test error e accuracy finale
set.seed(7)
prediction <- predict(custom_no_rs_train, newdata = test)
cf_test <- confusionMatrix(prediction, y_test)
print(cf_test)
cf_test$overall['Accuracy'] # 0.8584
1-cf_test$overall['Accuracy'] #test error  0.1416 

############################################ RANDOM-FOREST SOLO REDSHIFT ################################################
set.seed(123)
library(caret)
library(randomForest)

#creiamo le funzioni di supporto per poter fare repeated CV con iperparametri mtry e ntree
customRF <- list(type = "Classification",
                 library = "randomForest",
                 loop = NULL)

customRF$parameters <- data.frame(parameter = c("ntree"),
                                  class = "numeric",
                                  label = c("ntree"))

customRF$grid <- function(x,y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs) {
  randomForest(x, y,
               mtry = 1,
               ntree=param$ntree)
}

#Predict label
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)

#Predict prob
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")

customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

#consideriamo 8 fold
control <- trainControl(method="repeatedcv", 
                        number=8, 
                        repeats=3,
                        allowParallel = TRUE)

#inizializziamo la griglia degli iperparametri 
tunegrid <- expand.grid(.ntree=seq(249,800,by=250))

set.seed(123)
metric <- "Accuracy"
#non usiamo redshift
custom_rs <- train(class~redshift, data=train, 
                method=customRF, 
                metric=metric, 
                tuneGrid=tunegrid, 
                trControl=control)

rf_pars <- custom_rs$bestTune #iperparametri che manterremo da qui in poi

print(rf_pars)
plot(custom_rs)

mean(1- custom_rs$resample$Accuracy) #generalized error 0.08052596

#per calcolare empirical error
set.seed(7)
custom_rs_train<-randomForest(as.factor(class) ~ redshift,data=train, ntree = 749, mtry = 1)
cf <- confusionMatrix(custom_rs_train$predicted, y_train)
1-cf$overall['Accuracy'] #0.08112601 

#per calcolare test error e accuracy finale
set.seed(7)
prediction <- predict(custom_rs_train, newdata = test)
cf_test <- confusionMatrix(prediction, y_test)
print(cf_test)
cf_test$overall['Accuracy'] # 0.92045 
1-cf_test$overall['Accuracy'] #test error  0.07955 


