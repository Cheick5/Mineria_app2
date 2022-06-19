pacman::p_load(tidyverse, tidymodels, discrim, klaR, dplyr)
set.seed(3)

# Manejo de los datos

Datos_Train = read.csv("ALUMNOS-trainData.csv", header = TRUE, sep = ",")
Datos_Test = read.csv("ALUMNOS-evalData.csv", header = TRUE, sep = ",")

Datos_Train %>%
  summarize(across(.cols = everything(), ~sum(is.na(.))))

head(Datos_Train)

summary(Datos_Train)

str (Datos_Train)

# Limpieza de los datos

Datos_Num = select_if(Datos_Train, is.numeric)
Datos_Num

# Variables tipo character = date, origin, destination, departure_time
# Variables tipo id = fligth_number, id


#Datos_Sin_Na <- Datos %>%
#filter(!(is.na(id) | is.na(date) | is.na(fligth_number)| is.na(origin) | is.na(destination)| is.na(noshow)| is.na(denied_boarding)| is.na(pax_midlow)| is.na(pax_high)| is.na(pax_midhigh)| is.na(pax_low)| is.na(pax_freqflyer)| is.na(group_bookings)| is.na(out_of_stock)| is.na(dom_cnx)| is.na(int_cnx) | is.na(p2p)| is.na(departure_time)| is.na(capacity)| is.na(revenues_usd)| is.na(bookings)))
#%>%select()

Muestra_Datos_Train = sample_n(Datos_Num, 300000)

# validacion cruzada

# Ver la cantidad de pliegues optima "picking folds for cross validation in r"
# Normalmente se utilizan 10 pliegues.
cv_folds_Naive_Bayes <- vfold_cv(data = Muestra_Datos_Train, v = 10) 

cv_folds_Naive_Bayes

# Como primer modelo utilizaremos naive bayes

nb_model <- naive_Bayes() %>% 
  set_mode("classification") %>% 
  set_engine("klaR")

# receta
Naive_Bayes_rec <- recipe(
  noshow ~ . ,
  data = Muestra_Datos_Train) %>%
  update_role(id, fligth_number, new_role = "ID")

# Generamos workflow 

Noshow_wf <- workflow() %>% 
  add_recipe(Naive_Bayes_rec) %>% 
  add_model(nb_model)

# Ajustamos el modelo ----

nb_fit <- Noshow_wf %>% 
  fit_resamples(resamples = cv_folds_Naive_Bayes)

collect_metrics(nb_fit)


# Evaluacion del modelo

evaluar_modelo <- function(model){
  penguins_wf <- workflow() %>% 
    add_recipe(penguins_rec) %>% 
    add_model(model)
  
  nb_fit <- penguins_wf %>% 
    fit_resamples(
      resamples = cv_folds
    )
  print(collect_metrics(nb_fit))
  
  nb_final <- penguins_wf %>% 
    last_fit(split = penguin_split)
  
  print(collect_metrics(nb_final))
  
  nb_test_pred <- bind_cols(
    test,
    nb_final %>% collect_predictions() %>% dplyr::select(starts_with(".pred_"))
  )
  
  print(table("predicted class" = nb_test_pred$.pred_class,
              "observed class" = nb_test_pred$species))
  
  nb_test_pred %>% 
    roc_curve(
      truth = species,
      .pred_Adelie, .pred_Chinstrap, .pred_Gentoo
    ) %>% 
    autoplot()
}
