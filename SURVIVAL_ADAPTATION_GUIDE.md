# Guide d'adaptation pour l'analyse de survie

## ✅ Modifications déjà effectuées

### 1. **global.R**
- ✅ Ajout des packages de survie :
  - `survminer` : pour les visualisations Kaplan-Meier
  - `ranger` : pour Random Survival Forest
  - `riskRegression` : pour le C-index
  - `pec` : pour le Brier Score
  - `prodlim` : pour l'estimation product limit

- ✅ Remplacement des fonctions de classification multi-classe par des fonctions d'analyse de survie :
  - `calculate_cindex()` : Calcul du C-index (concordance)
  - `calculate_ibs()` : Calcul du Integrated Brier Score
  - `calculate_brier_at_time()` : Brier Score à un temps donné
  - `get_risk_scores()` : Extraction des scores de risque
  - `get_median_survival()` : Calcul de la survie médiane

- ✅ Ajout des fonctions de modélisation de survie :
  - `fit_cox_model()` : Modèle Cox proportionnel
  - `fit_coxnet_model()` : Cox avec pénalisation (Lasso/ElasticNet/Ridge)
  - `fit_rsf_model()` : Random Survival Forest
  - `tune_rsf_model()` : Tuning des hyperparamètres RSF

### 2. **ui.R**
- ✅ Modification des instructions sur la structure des données :
  - Ligne 147 : "the second column: time to event (numeric), the third column: status (0=censored, 1=event)"

- ✅ Remplacement des modèles de classification par des modèles de survie (lignes 380-386) :
  - Cox Proportional Hazards
  - Random Survival Forest
  - Cox with Lasso
  - Cox with ElasticNet
  - Cox with Ridge

- ✅ Adaptation des métriques affichées (lignes 724-728 et 752-756) :
  - Sensibility/Specificity → C-index et Integrated Brier Score

- ✅ Adaptation des tests statistiques (lignes 263-270) :
  - Wilcoxon/Student → Cox Wald test et Log-rank test
  - Lasso/ElasticNet/Ridge → Cox Lasso/Cox ElasticNet/Cox Ridge

- ✅ Changement du titre de l'application : "Survival Analysis"

---

## 🔧 Modifications restantes à effectuer

### 3. **server.R** (CRITIQUES)

#### A. Validation de la structure des données
**Lignes à modifier** : Section de lecture des données (environ lignes 200-300)

**Modification requise** :
```r
# Vérifier que les colonnes time et status existent
if(!("time" %in% colnames(data)) || !("status" %in% colnames(data))){
  showModal(modalDialog(
    title = "Erreur de structure",
    "Les données doivent contenir une colonne 'time' et une colonne 'status'",
    easyClose = TRUE
  ))
  return(NULL)
}

# Vérifier que time est numérique et status est binaire (0/1)
if(!is.numeric(data$time)){
  stop("La colonne 'time' doit être numérique")
}
if(!all(data$status %in% c(0, 1))){
  stop("La colonne 'status' doit contenir uniquement 0 (censuré) ou 1 (événement)")
}
```

#### B. Remplacement des calculs de métriques
**Lignes à remplacer** :

**Lignes 226, 231** : Remplacer `specificity()` par calcul du C-index
```r
# Avant
specificity(MODEL()$DATALEARNINGMODEL$reslearningmodel$predictclasslearning,
            MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning)

# Après
calculate_cindex(predicted_risk = MODEL()$DATALEARNINGMODEL$reslearningmodel$riskscores,
                 time = MODEL()$DATALEARNINGMODEL$data$time,
                 status = MODEL()$DATALEARNINGMODEL$data$status)
```

**Lignes 214** : Adapter le tableau de résultats
```r
# Avant
table[19,1:8] <- c("#", "number of features", "AUC learning", "sensibility learning",
                   "specificity learning", "AUC validation", "sensibility validation",
                   "specificity validation")

# Après
table[19,1:8] <- c("#", "number of features",
                   "C-index learning", "IBS learning", "Median survival learning",
                   "C-index validation", "IBS validation", "Median survival validation")
```

#### C. Remplacement des graphiques ROC par Kaplan-Meier
**Lignes à modifier** : 1078, 1093, 1100, 1148, 1154, 1160

**Fonction Kaplan-Meier à créer dans global.R** :
```r
plot_kaplan_meier <- function(time, status, risk_groups = NULL, title = "Kaplan-Meier Survival Curve"){
  require(survminer)
  require(survival)

  # Créer l'objet survfit
  if(is.null(risk_groups)){
    fit <- survfit(Surv(time, status) ~ 1)
  } else {
    fit <- survfit(Surv(time, status) ~ risk_groups)
  }

  # Créer le graphique
  p <- ggsurvplot(fit,
                  data = data.frame(time, status, risk_groups),
                  risk.table = TRUE,
                  pval = !is.null(risk_groups),
                  conf.int = TRUE,
                  xlim = c(0, max(time, na.rm = TRUE)),
                  break.time.by = max(time, na.rm = TRUE)/10,
                  ggtheme = theme_minimal(),
                  title = title,
                  xlab = "Time",
                  ylab = "Survival probability")

  return(p)
}
```

**Remplacer dans server.R** :
```r
# Ligne 1078 - Avant
output$plotmodeldecouvroc <- renderPlot({
  ROCcurve(validation = datalearningmodel$reslearningmodel$classlearning,
           decisionvalues = datalearningmodel$reslearningmodel$scorelearning)
})

# Après
output$plotmodeldecouvroc <- renderPlot({
  # Créer des groupes de risque (haut/bas) basés sur la médiane des scores
  risk_scores <- datalearningmodel$reslearningmodel$riskscores
  risk_groups <- ifelse(risk_scores >= median(risk_scores), "High risk", "Low risk")

  plot_kaplan_meier(time = datalearningmodel$data$time,
                    status = datalearningmodel$data$status,
                    risk_groups = risk_groups,
                    title = "Kaplan-Meier Curve - Learning Set")
})
```

#### D. Adaptation des outputs de métriques
**Lignes à remplacer** :

**Ligne 1136** : Remplacer output$specificitydecouv
```r
# Avant
output$specificitydecouv <- renderText({
  specificity(predict = datalearningmodel$reslearningmodel$predictclasslearning,
              class = datalearningmodel$reslearningmodel$classlearning)
})

# Après
output$cindexdecouv <- renderText({
  round(calculate_cindex(predicted_risk = datalearningmodel$reslearningmodel$riskscores,
                         time = datalearningmodel$data$time,
                         status = datalearningmodel$data$status), 3)
})

output$ibsdecouv <- renderText({
  round(calculate_ibs(model = datalearningmodel$model,
                      data = datalearningmodel$data,
                      time_col = "time",
                      status_col = "status"), 3)
})
```

**Ligne 1201** : Même chose pour la validation
```r
output$cindexval <- renderText({
  round(calculate_cindex(predicted_risk = datavalidationmodel$resvalidationmodel$riskscores,
                         time = datavalidationmodel$data$time,
                         status = datavalidationmodel$data$status), 3)
})

output$ibsval <- renderText({
  round(calculate_ibs(model = datavalidationmodel$model,
                      data = datavalidationmodel$data,
                      time_col = "time",
                      status_col = "status"), 3)
})
```

---

### 4. **global.R** - Adaptation de la sélection de variables

#### Tests univariés pour la survie
**Ajouter ces fonctions dans global.R** :

```r
# Test Log-rank pour chaque variable
perform_logrank_test <- function(data, time_col = "time", status_col = "status"){
  # Variables à tester (exclure time et status)
  vars_to_test <- setdiff(colnames(data), c(time_col, status_col))

  results <- data.frame(
    variable = vars_to_test,
    pvalue = NA,
    stringsAsFactors = FALSE
  )

  for(i in seq_along(vars_to_test)){
    var <- vars_to_test[i]

    # Dichotomiser la variable par la médiane
    var_groups <- ifelse(data[[var]] >= median(data[[var]], na.rm = TRUE), "High", "Low")

    # Test log-rank
    tryCatch({
      test_result <- survdiff(Surv(data[[time_col]], data[[status_col]]) ~ var_groups)
      pval <- 1 - pchisq(test_result$chisq, df = 1)
      results$pvalue[i] <- pval
    }, error = function(e){
      results$pvalue[i] <- NA
    })
  }

  return(results)
}

# Test de Cox univarié (Wald test)
perform_cox_univariate <- function(data, time_col = "time", status_col = "status"){
  vars_to_test <- setdiff(colnames(data), c(time_col, status_col))

  results <- data.frame(
    variable = vars_to_test,
    hazard_ratio = NA,
    pvalue = NA,
    stringsAsFactors = FALSE
  )

  for(i in seq_along(vars_to_test)){
    var <- vars_to_test[i]

    formula_str <- paste("Surv(", time_col, ",", status_col, ") ~", var)

    tryCatch({
      cox_model <- coxph(as.formula(formula_str), data = data)
      coef_summary <- summary(cox_model)$coefficients

      results$hazard_ratio[i] <- exp(coef_summary[1, "coef"])
      results$pvalue[i] <- coef_summary[1, "Pr(>|z|)"]
    }, error = function(e){
      results$hazard_ratio[i] <- NA
      results$pvalue[i] <- NA
    })
  }

  return(results)
}
```

#### Modification de la fonction existante de sélection de variables
**Lignes 728-1300 environ** : Adapter la fonction qui gère les tests

Remplacer les sections :
```r
# Ligne ~860 - Remplacer Wilcoxon par Log-rank
if(test == "logrank"){
  results <- perform_logrank_test(data = toto,
                                   time_col = "time",
                                   status_col = "status")
}

# Ajouter Cox Wald test
if(test == "coxwald"){
  results <- perform_cox_univariate(data = toto,
                                    time_col = "time",
                                    status_col = "status")
}

# Ligne ~974 - Remplacer Lasso/ElasticNet/Ridge par versions Cox
if(method == "coxlasso"){
  model <- fit_coxnet_model(data = toto, alpha = 1)
} else if(method == "coxelasticnet"){
  model <- fit_coxnet_model(data = toto, alpha = 0.5)
} else if(method == "coxridge"){
  model <- fit_coxnet_model(data = toto, alpha = 0)
}
```

---

### 5. **Modifications dans la section de modélisation** (server.R et global.R)

#### Dans server.R - Section de construction des modèles
**Lignes 1845-2050 environ** : Remplacer les modèles de classification

```r
# Remplacer Random Forest par Random Survival Forest
if(modelparameters$modeltype == "rsf"){
  # Utiliser les fonctions créées dans global.R
  if(modelparameters$autotune){
    model <- tune_rsf_model(data = learningmodel,
                           time_col = "time",
                           status_col = "status")
  } else {
    model <- fit_rsf_model(data = learningmodel,
                          time_col = "time",
                          status_col = "status",
                          num_trees = modelparameters$ntree,
                          mtry = modelparameters$mtry)
  }

  # Extraire les risk scores
  riskscores <- get_risk_scores(model, learningmodel, model_type = "rsf")
}

# Remplacer SVM par Cox
if(modelparameters$modeltype == "cox"){
  model <- fit_cox_model(data = learningmodel,
                        time_col = "time",
                        status_col = "status")

  riskscores <- get_risk_scores(model, learningmodel, model_type = "cox")
}

# Cox avec pénalisation
if(modelparameters$modeltype %in% c("coxlasso", "coxelasticnet", "coxridge")){
  alpha <- switch(modelparameters$modeltype,
                 "coxlasso" = 1,
                 "coxelasticnet" = 0.5,
                 "coxridge" = 0)

  model <- fit_coxnet_model(data = learningmodel,
                           time_col = "time",
                           status_col = "status",
                           alpha = alpha)

  riskscores <- get_risk_scores(model, learningmodel, model_type = "coxnet")
}
```

---

## 📋 Résumé des fichiers à modifier

| Fichier | Sections | Priorité |
|---------|----------|----------|
| **server.R** | Validation des données | 🔴 CRITIQUE |
| **server.R** | Calculs de métriques (lignes 214-231) | 🔴 CRITIQUE |
| **server.R** | Outputs métriques (lignes 1136, 1201) | 🔴 CRITIQUE |
| **server.R** | Graphiques ROC → KM (lignes 1078-1160) | 🔴 CRITIQUE |
| **server.R** | Construction des modèles (lignes 1845-2050) | 🟡 IMPORTANT |
| **global.R** | Tests statistiques de survie (lignes 728-1300) | 🟡 IMPORTANT |
| **global.R** | Fonction plot_kaplan_meier | 🟡 IMPORTANT |

---

## 🎯 Prochaines étapes recommandées

1. **Modifier server.R** - Section validation des données
2. **Modifier server.R** - Remplacer calculs de métriques
3. **Ajouter plot_kaplan_meier() dans global.R**
4. **Modifier server.R** - Remplacer graphiques ROC
5. **Modifier global.R** - Ajouter tests statistiques de survie
6. **Tester l'application avec un jeu de données de survie**

---

## 📚 Ressources

- **Packages R utilisés** :
  - `survival` : modèles Cox, survfit, Surv
  - `survminer` : ggsurvplot pour Kaplan-Meier
  - `ranger` : Random Survival Forest
  - `glmnet` : Cox pénalisé (Lasso/ElasticNet/Ridge)
  - `riskRegression` : C-index
  - `pec` : Brier Score

- **Métriques d'évaluation** :
  - **C-index** : 0.5 = aléatoire, 1.0 = parfait (comme AUC)
  - **Integrated Brier Score** : Plus bas = meilleur (0 = parfait)
  - **Médiane de survie** : Temps auquel 50% survivent

---

**Note** : Les modifications effectuées jusqu'ici concernent principalement l'interface (ui.R) et les fonctions de base (global.R). Les modifications critiques dans server.R nécessitent une compréhension approfondie de la logique de l'application pour assurer la cohérence.
