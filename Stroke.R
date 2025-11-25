library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(scales)
library(randomForest)
library(yardstick)
library(knitr)
library(kableExtra)


stroke_data <- read.csv('stroke-data.csv', header = T)
head(stroke_data)
stroke_data = stroke_data %>% 
  select(-id) %>% 
  mutate(across(where (is.character), ~ na_if(., "N/A"))) %>% 
  #filter(smoking_status != 'Unknown') %>% 
  drop_na() %>% 
  mutate(across(where(is.integer), as.factor)) %>% 
  mutate(bmi = as.numeric(bmi))
##Inizialmente si hanno 5110 osservazioni. Eliminando le osservazioni per cui si rileva un 'Unknown' come valore in corrispondenza dello smoking_status, si passa a 3566 osservazioni. Da queste si passa a 3426, per cui vi sono 120 n.a.
table(stroke_data$smoking_status)
sum(stroke_data$stroke == 1)
dim(stroke_data)
180/3426
##Si ha una percentuale pari al 5.25% di indivdui che hanno avuto uno stroke.



table(stroke_data$gender)
stroke_data = stroke_data %>%
  filter(gender != 'Other')
#C'è uno sbilanciamento relativamente al genere degli individui compresi nel campione: il numero di donne è pari a 2086, mentre gli uomini sono 1339. Si rileva anche la presenza di un indivduo che ha riportato in corrispondenza del genere la modalità 'Other'. Essendo l'unica osservazione per cui è riportata questa modalità, è possibile procedere rimuovendola.

attach(stroke_data)
summary(age)
sum(age < 30 & stroke == 1)
#Nessuno tra i 651 individui con età inferiore ai 30 anni ha riportato di aver avuto un ictus.
ggplot(stroke_data, aes(y = age)) +
  geom_boxplot(color = "black", outlier.colour = "orange", outlier.shape = 8) +
  labs(title = "Boxplot for age") +
  theme_minimal()
################################################
ggplot(stroke_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10)  # numeri ogni 10 anni
  ) +
  labs(
    title = "Distribuzione dell'età",
    x = "Età",
    y = "Frequenza"
  ) +
  theme_minimal()
############################################
sum(age >= 80)
shapiro.test(age)
#Si ha un p-value significativamente minore di 0.001, pertanto il test suggerisce di rifiutare l'ipotesi nulla di normalità. La variabile Age ha quindi distribuzione lontana dalla normale.
grouped_age <- cut(stroke_data$age, breaks = c(0,9,25,40,55,70,85), right = T, include.lowest = T)
stroke_data <- stroke_data %>% 
  mutate(grouped_age)
table(grouped_age, stroke)
ggplot(data = stroke_data, aes(x = grouped_age, fill = stroke)) + 
  geom_bar(position = 'fill') +
  scale_y_continuous(labels = scales :: percent) +
  scale_fill_manual(values = c('0' = 'lightgreen', '1' = 'darkred'))+ 
  theme_minimal()
#Dai dati emerge che il maggior numero di casi è stato registrato per gli individui di età compresa tra i 55 e  70 anni. L'incidenza più elevata si ha invece tra coloro che presentano un'età tra i 70 e gli 85 anni.

age_hyp <- table(stroke_data$grouped_age, stroke_data$hypertension)
ggplot(data = stroke_data, aes(x = hypertension, y =age))+
  geom_boxplot(fill = 'lightblue') +
  theme_minimal()
table(ever_married)

table(hypertension)
table(cut(bmi, breaks = 10))
grouped_bmi <- cut(bmi, breaks = 10)
grouped_avggluclev <- cut(avg_glucose_level, breaks = 10)

table(Residence_type)
fisher.test(heart_disease, stroke)
stroke_data <- stroke_data %>% 
  select(-grouped_age)

library(GGally)
#ggpairs(stroke_data)

library(rsample)
set.seed(42)
split_data <- initial_split(stroke_data, prop = 0.70, strata = stroke)
train_set <- training(split_data)
test_set <- testing(split_data)
table(train_set$stroke) %>% prop.table()
table(test_set$stroke) %>% prop.table()
#Si utilizza il campionamento stratificato in quanto il dataset risulta essere sbilanciato. In questo modo, è possibile mantenere la proporzione di positivi e negativi, sia nel training che nel testing set.

mod1 <- glm(stroke ~ ., data = train_set, family = binomial(link = 'logit'))      
summary(mod1)
#Nel modello generale, che prende in considerazione tutte i regressori possibili, emergono come significative le variabili age, hypertension e avg_glucose_level. La variabile heart_disease riporta un p-value leggermente maggiore di 0.05, il che evidenzia che anche questa variabile potrebbe incidere nella previsione dell'output. 
#Si considera un modello più parsimonioso, ottenuto mediante l'utilizzo della stepwise selection. Si tratta di un metodo iterativo che permette di selezionare le variabili la cui aggiunta al modello comporta una riduzione del valore del criterio di informazione di Akaike del modello stesso.
mod_AIC <- step(mod1)
#Il modello di regresione logistica ottenuto mediante la stepwise selection presenta come variabili indipendenti heart_disease, hypertension, avg_glucose_level, age.
summary(mod_AIC)
#Tutte le variabili risultano essere significative. Si nota come il valore del log-odds
mod_BIC <- step(mod1, direction = 'both', k = log(nrow(train_set))) 
summary(mod_BIC)

# Creiamo il dataframe con i risultati
thresh <- 0.5
test_results <- test_set %>%
  mutate(prob_pred = predict(mod_AIC, newdata = ., type = "response"),
    class_pred = ifelse(prob_pred > thresh, 1, 0),
    class_pred = factor(class_pred, levels = c(0, 1))) %>%
  select(stroke, prob_pred, class_pred)

conf_matrix <- test_results %>%
  conf_mat(truth = stroke, estimate = class_pred)
print(conf_matrix)
#Evaluation metrics
risultato_accuracy <- test_results %>%
  accuracy(truth = stroke, estimate = class_pred)
risultato_recall <- test_results %>%
  recall(truth = stroke, estimate = class_pred)
risultato_f1score <- test_results %>%
  f_meas(truth = stroke, estimate = class_pred)
metrics_table <- tibble(risultato_accuracy$ .estimate, risultato_recall$ .estimate, risultato_f1score$ .estimate)
tabella_base <- kable(metrics_table, "html", caption = "Tabella Avanzata", align = 'c', digits = 3, col.names = c("Accuracy", "Recall", "F1Score"))
tabella_finale <- tabella_base %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
print(tabella_finale)

###Proviamo con il modello ottenuto con la stepwise effettuata utilizzando il criterio di informazione Bayesiano.
test_results_BIC <- test_set %>%
  mutate(prob_pred = predict(mod_BIC, newdata = ., type = "response"),
         class_pred = ifelse(prob_pred > thresh, 1, 0),
         class_pred = factor(class_pred, levels = c(0, 1))) %>%
  select(stroke, prob_pred, class_pred)

conf_matrix_BIC <- test_results_BIC %>%
  conf_mat(truth = stroke, estimate = class_pred)
print(conf_matrix_BIC)
#Evaluation metrics
risultato_accuracy_BIC <- test_results_BIC %>%
  accuracy(truth = stroke, estimate = class_pred)
risultato_recall_BIC <- test_results_BIC %>%
  recall(truth = stroke, estimate = class_pred)
risultato_f1score_BIC <- test_results_BIC %>%
  f_meas(truth = stroke, estimate = class_pred)
metrics_table_BIC <- tibble(risultato_accuracy_BIC$ .estimate, risultato_recall_BIC$ .estimate, risultato_f1score_BIC$ .estimate)
tabella_base_BIC <- kable(metrics_table_BIC, "html", caption = "Tabella Avanzata", align = 'c', digits = 3, col.names = c("Accuracy", "Recall", "F1Score"))
tabella_finale_BIC <- tabella_base_BIC %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
print(tabella_finale_BIC)

#Proviamo a cambiare la soglia
thresh_2 <- 0.3
test_results_2 <- test_set %>%
  mutate(prob_pred = predict(mod_AIC, newdata = ., type = "response"),
         class_pred = ifelse(prob_pred > thresh_2, 1, 0),
         class_pred = factor(class_pred, levels = c(0, 1))) %>%
  select(stroke, prob_pred, class_pred)

conf_matrix_2 <- test_results_2 %>%
  conf_mat(truth = stroke, estimate = class_pred)
print(conf_matrix_2)
#Evaluation metrics
risultato_accuracy_2 <- test_results_2 %>%
  accuracy(truth = stroke, estimate = class_pred)
risultato_recall_2 <- test_results_2 %>%
  recall(truth = stroke, estimate = class_pred)
risultato_f1score_2 <- test_results_2 %>%
  f_meas(truth = stroke, estimate = class_pred)
metrics_table_2 <- tibble(risultato_accuracy_2$ .estimate, risultato_recall_2$ .estimate, risultato_f1score_2$ .estimate)
tabella_base_2 <- kable(metrics_table_2, "html", caption = "Tabella Avanzata", align = 'c', digits = 3, col.names = c("Accuracy", "Recall", "F1Score"))
tabella_finale_2 <- tabella_base_2 %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
print(tabella_finale_2)

test_results_BIC_2 <- test_set %>%
  mutate(prob_pred = predict(mod_BIC, newdata = ., type = "response"),
         class_pred = ifelse(prob_pred > thresh_2, 1, 0),
         class_pred = factor(class_pred, levels = c(0, 1))) %>%
  select(stroke, prob_pred, class_pred)
conf_matrix_BIC_2 <- test_results_BIC_2 %>%
  conf_mat(truth = stroke, estimate = class_pred)
print(conf_matrix_BIC_2)
# Evaluation metrics
risultato_accuracy_BIC_2 <- test_results_BIC_2 %>%
  accuracy(truth = stroke, estimate = class_pred)
risultato_recall_BIC_2 <- test_results_BIC_2 %>%
  recall(truth = stroke, estimate = class_pred)
risultato_f1score_BIC_2 <- test_results_BIC_2 %>%
  f_meas(truth = stroke, estimate = class_pred)
metrics_table_BIC_2 <- tibble(risultato_accuracy_BIC_2$.estimate, risultato_recall_BIC_2$.estimate, risultato_f1score_BIC_2$.estimate)
tabella_base_BIC_2 <- kable(metrics_table_BIC_2, "html", caption = "Tabella Avanzata", align = 'c', digits = 3, col.names = c("Accuracy", "Recall", "F1Score"))
tabella_finale_BIC_2 <- tabella_base_BIC_2 %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
print(tabella_finale_BIC_2)

#Proviamo settando la soglia a 0.2
thresh_3 <- 0.2
test_results_3 <- test_set %>%
  mutate(prob_pred = predict(mod_AIC, newdata = ., type = "response"),
         class_pred = ifelse(prob_pred > thresh_3, 1, 0),
         class_pred = factor(class_pred, levels = c(0, 1))) %>%
  select(stroke, prob_pred, class_pred)

conf_matrix_3 <- test_results_3 %>%
  conf_mat(truth = stroke, estimate = class_pred)
print(conf_matrix_3)

# Evaluation metrics
risultato_accuracy_3 <- test_results_3 %>%
  accuracy(truth = stroke, estimate = class_pred)
risultato_recall_3 <- test_results_3 %>%
  recall(truth = stroke, estimate = class_pred)
risultato_f1score_3 <- test_results_3 %>%
  f_meas(truth = stroke, estimate = class_pred)

metrics_table_3 <- tibble(risultato_accuracy_3$.estimate, 
                          risultato_recall_3$.estimate, 
                          risultato_f1score_3$.estimate)

tabella_base_3 <- kable(metrics_table_3, "html", 
                        caption = "Tabella Avanzata", 
                        align = 'c', 
                        digits = 3, 
                        col.names = c("Accuracy", "Recall", "F1Score"))

tabella_finale_3 <- tabella_base_3 %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

print(tabella_finale_3)

test_results_BIC_3 <- test_set %>%
  mutate(prob_pred = predict(mod_BIC, newdata = ., type = "response"),
         class_pred = ifelse(prob_pred > thresh_3, 1, 0),
         class_pred = factor(class_pred, levels = c(0, 1))) %>%
  select(stroke, prob_pred, class_pred)

conf_matrix_BIC_3 <- test_results_BIC_3 %>%
  conf_mat(truth = stroke, estimate = class_pred)
print(conf_matrix_BIC_3)

# Evaluation metrics
risultato_accuracy_BIC_3 <- test_results_BIC_3 %>%
  accuracy(truth = stroke, estimate = class_pred)
risultato_recall_BIC_3 <- test_results_BIC_3 %>%
  recall(truth = stroke, estimate = class_pred)
risultato_f1score_BIC_3 <- test_results_BIC_3 %>%
  f_meas(truth = stroke, estimate = class_pred)

metrics_table_BIC_3 <- tibble(risultato_accuracy_BIC_3$.estimate, 
                              risultato_recall_BIC_3$.estimate, 
                              risultato_f1score_BIC_3$.estimate)

tabella_base_BIC_3 <- kable(metrics_table_BIC_3, "html", 
                            caption = "Tabella Avanzata", 
                            align = 'c', 
                            digits = 3, 
                            col.names = c("Accuracy", "Recall", "F1Score"))

tabella_finale_BIC_3 <- tabella_base_BIC_3 %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

print(tabella_finale_BIC_3)


library(pROC)
ROCAicOut <- roc(test_set$stroke, predict(mod_AIC, newdata=test_set, 
                                  type = "response"), plot = TRUE,
                 legacy.axes=TRUE, col="midnightblue", lwd=3,
                 auc.polygon=T, auc.polygon.col="lightblue", print.auc=T)
ROCBicOut <- roc(test_set$stroke, predict(mod_BIC, newdata=test_set, 
                                  type = "response"), plot = TRUE,
                 legacy.axes=TRUE, col="midnightblue", lwd=3,
                 auc.polygon=T, auc.polygon.col="lightblue", print.auc=T)
#Si è valutato utilizzando due diversi criteri di informazione, in modo da poter confrontare i risultati.



library(randomForestExplainer)
library(DALEX)
modrf <- randomForest(stroke ~ ., data= train_set, ntree = 100, set.seed(42))
modrf$confusion
important_variables(measure_importance(modrf) , k = 2)
explainer <- explain(modrf, data = train_set[,-11], y = as.numeric(train_set$stroke))
variable_importance <- variable_importance(explainer)
print(variable_importance)

#+ plot imp, echo=F, fig.width=10, fig.height=8
plot(variable_importance)
pred_rf <- predict(test_set$stroke, modrf)
model_results <- cbind(test_set$stroke, pred_rf)
accuracy(truth = stroke, estimate = pred_rf, data = model_results)
