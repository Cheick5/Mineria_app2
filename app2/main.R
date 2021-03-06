
# cargamos librerias
pacman::p_load(tidyverse, tidymodels, discrim, naivebayes, anytime)
set.seed(42)

Datos_Train = read.csv("ALUMNOS-trainData.csv", header = TRUE, sep = ",") %>% sample_n(1000) #Este csv tiene la columna noshow
Datos_Test = read.csv("ALUMNOS-evalData.csv", header = TRUE, sep = ",") %>% sample_n(1000)



#Seleccionamos todas las variables, el por qué está en el informe.
data <- 
  Datos_Train %>% 
  mutate(
    noshow = ifelse(noshow >= 4,1,0), #si un vuelo de Santiago a Concepción tiene 3 no show entonces el vuelo se cataloga como 0. Caso contrario, si 4 o más personas no se presentaron, entonces el vuelo se cataloga como 1
    date = lubridate::as_date(date)
  ) %>% 
  na.omit() %>% 
  mutate_if(is.character, as.factor)  %>% 
  sample_n(1000)


receta <- 
  recipe(noshow ~ .,data = data) %>% 
  update_role(fligth_number, id, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%               
  step_holiday(date, 
               holidays = timeDate::listHolidays("US"), 
               keep_original_cols = FALSE) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())


# definimos modelo de arbol con 5 niveles de profundidad y min 10 nodos por hoja
modelo <-
  decision_tree(tree_depth = 5, min_n = 10) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

modelo

# definimos funcion fitea para ajustar el model
fitea <- function(mod){
  
  modelo_fit <- 
    workflow() %>% 
    add_model(mod) %>% 
    add_recipe(receta) %>% 
    fit(data = Datos_Train)
  
  model_pred <- 
    predict(modelo_fit, test_data, type = "prob") %>% 
    bind_cols(test_data) 
  
  return(model_pred %>% 
           roc_auc(truth = arr_delay, .pred_late))
}

# usamos funcion fitea con modelo de arboles
fitea(modelo)

