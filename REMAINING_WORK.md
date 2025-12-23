# Travail restant pour finaliser l'adaptation à l'analyse de survie

## ✅ Modifications déjà effectuées et commitées

### Commit 1 : "Adapt application for survival analysis"
- ✅ Packages de survie ajoutés (survminer, ranger, riskRegression, pec)
- ✅ Fonctions de métriques de survie (calculate_cindex, calculate_ibs, get_risk_scores)
- ✅ Fonctions de modélisation (fit_cox_model, fit_coxnet_model, fit_rsf_model)
- ✅ Interface utilisateur adaptée (ui.R)
- ✅ Titre changé en "Survival Analysis"
- ✅ Documentation complète (SURVIVAL_ADAPTATION_GUIDE.md)

### Commit 2 : "Add survival analysis validation, metrics and visualizations"
- ✅ Validation time/status dans confirmdata() (global.R:628-665)
- ✅ Tests statistiques de survie (perform_logrank_test, perform_cox_univariate)
- ✅ Visualisations Kaplan-Meier (plot_kaplan_meier, plot_cumulative_hazard)
- ✅ Graphiques ROC → Kaplan-Meier (server.R:1082-1218)
- ✅ Métriques sensitivity/specificity → C-index/IBS (server.R:1135-1247)
- ✅ Validation structure des données (server.R:325-334)

---

## 🔧 Modifications restantes CRITIQUES

### 1. **Adaptation de la sélection de variables** (global.R)

**Fichier**: global.R
**Section**: Fonction de sélection de variables (environ lignes 850-1300)
**Priorité**: 🔴 CRITIQUE

**Problème actuel**:
- Les tests Wilcoxon/Student sont utilisés pour comparer des groupes
- En survie, on n'a pas de groupes mais des temps et événements

**Modifications requises**:
```r
# Chercher et remplacer dans la fonction de sélection de variables (testparameters$test)

# Ancien code (ligne ~860):
if(test == "Wtest"){
  pval[i] <- wilcox.test(lev1, lev2, exact = F)$p.value
}

# Nouveau code:
if(test == "logrank"){
  results <- perform_logrank_test(data = toto,
                                  time_col = "time",
                                  status_col = "status")
  # Extraire pvalues pour chaque variable
}

if(test == "coxwald"){
  results <- perform_cox_univariate(data = toto,
                                   time_col = "time",
                                   status_col = "status")
  # Extraire pvalues et hazard ratios
}
```

**Localisation exacte**:
- Chercher: `testparameters\$test` dans global.R
- Remplacer les sections qui testent `"Wtest"`, `"Ttest"` par `"logrank"`, `"coxwald"`
- Adapter la logique qui utilise des groupes (lev1, lev2) pour utiliser Surv(time, status)

---

### 2. **Adaptation de la construction des modèles** (global.R)

**Fichier**: global.R
**Section**: Fonction de modélisation (environ lignes 1840-2050)
**Priorité**: 🔴 CRITIQUE

**Problème actuel**:
- Les modèles sont Random Forest, SVM, XGBoost pour classification
- Structure attendue: `learningmodel[,1]` = groupe (facteur)

**Modifications requises**:

#### A. Remplacer Random Forest par Random Survival Forest
```r
# Ligne ~1845: Remplacer
if(modelparameters$modeltype == "randomforest"){
  # ... code actuel ...
  model <- randomForest(x = x, y = learningmodel[,1], ...)
}

# Par:
if(modelparameters$modeltype == "rsf"){
  # Utiliser fit_rsf_model()
  if(modelparameters$autotunerf){
    model <- tune_rsf_model(data = learningmodel,
                           time_col = "time",
                           status_col = "status",
                           ntree_values = c(100, 500, 1000))
  } else {
    model <- fit_rsf_model(data = learningmodel,
                          time_col = "time",
                          status_col = "status",
                          num_trees = modelparameters$ntree,
                          mtry = modelparameters$mtry)
  }

  # Extraire risk scores
  riskscores <- get_risk_scores(model, learningmodel, model_type = "rsf")

  # Stocker dans resultat
  reslearningmodel <- list(
    riskscores = riskscores,
    # Pas de predictclasslearning ni scorelearning pour survie
  )
}
```

#### B. Remplacer SVM par Cox
```r
# Ligne ~1971: Remplacer
if(modelparameters$modeltype == "svm"){
  # ... code SVM ...
}

# Par:
if(modelparameters$modeltype == "cox"){
  model <- fit_cox_model(data = learningmodel,
                        time_col = "time",
                        status_col = "status")

  riskscores <- get_risk_scores(model, learningmodel, model_type = "cox")

  reslearningmodel <- list(
    riskscores = riskscores
  )
}
```

#### C. Ajouter Cox pénalisé
```r
# Ajouter après section Cox:
if(modelparameters$modeltype %in% c("coxlasso", "coxelasticnet", "coxridge")){
  # Déterminer alpha
  alpha <- switch(modelparameters$modeltype,
                 "coxlasso" = 1,
                 "coxelasticnet" = modelparameters$alpha,  # ou 0.5 par défaut
                 "coxridge" = 0)

  model <- fit_coxnet_model(data = learningmodel,
                           time_col = "time",
                           status_col = "status",
                           alpha = alpha,
                           nfolds = 10)

  riskscores <- get_risk_scores(model, learningmodel, model_type = "coxnet")

  # Stocker variables sélectionnées
  selected_vars <- coef(model, s = "lambda.min")
  selected_vars <- names(selected_vars[selected_vars != 0, , drop = FALSE])

  reslearningmodel <- list(
    riskscores = riskscores,
    selected_variables = selected_vars
  )
}
```

**Structure de retour attendue**:
Au lieu de retourner `scorelearning`, `predictclasslearning`, `classlearning`, la fonction doit retourner:
- `riskscores` : vecteur de scores de risque (plus haut = plus à risque)
- Garder `learningmodel` avec colonnes time et status

---

### 3. **Adaptation des download handlers** (server.R)

**Fichier**: server.R
**Section**: Lignes 1096-1106, 1220-1222
**Priorité**: 🟡 IMPORTANT

**Modifications requises**:
```r
# Ligne 1096-1106: Download plot Kaplan-Meier learning
output$downloadplotdecouvroc = downloadHandler(
  filename = function() {paste('km_plot_learning', '.',input$paramdownplot, sep='')},
  content = function(file) {
    datalearningmodel <- MODEL()$DATALEARNINGMODEL
    if(!is.null(datalearningmodel$reslearningmodel$riskscores)){
      risk_scores <- datalearningmodel$reslearningmodel$riskscores
      risk_groups <- ifelse(risk_scores >= median(risk_scores, na.rm = TRUE), "High risk", "Low risk")

      km_plot <- plot_kaplan_meier(
        time = datalearningmodel$learningmodel$time,
        status = datalearningmodel$learningmodel$status,
        risk_groups = risk_groups,
        title = "Kaplan-Meier Curve - Learning Set"
      )

      # ggsurvplot retourne un objet complexe, utiliser ggsave sur km_plot$plot
      if(!is.null(km_plot) && !is.null(km_plot$plot)){
        ggsave(file, plot = km_plot$plot, device = input$paramdownplot)
      }
    }
  },
  contentType = NA
)

# Même chose pour validation (ligne ~1220)
```

---

### 4. **Adaptation des tableaux de résultats** (server.R)

**Fichier**: server.R
**Section**: Ligne 214
**Priorité**: 🟡 IMPORTANT

**Modifications requises**:
```r
# Ligne 214: Remplacer
table[19,1:8] <- c("#", "number of features", "AUC learning", "sensibility learning",
                  "specificity learning", "AUC validation", "sensibility validation",
                  "specificity validation")

# Par:
table[19,1:8] <- c("#", "number of features",
                  "C-index learning", "IBS learning", "Median survival learning",
                  "C-index validation", "IBS validation", "Median survival validation")

# Lignes 218-231: Adapter les valeurs
# Au lieu de:
# auc(...)
# sensibility(...)
# specificity(...)

# Utiliser:
cindex_learn <- calculate_cindex(...)
ibs_learn <- calculate_ibs(...)
median_surv_learn <- get_median_survival(...)
```

---

### 5. **Adaptation de output$youndendecouv et output$youndenval** (server.R)

**Fichier**: server.R
**Section**: Lignes 1086-1094, 1188-1195
**Priorité**: 🟡 IMPORTANT

**Problème**: La fonction `younden()` calcule le seuil optimal pour classification binaire. En survie, on n'a pas besoin de seuil.

**Modifications suggérées**:
```r
# Remplacer output$youndendecouv par output$survstats_decouv
output$survstats_decouv <- renderTable({
  datalearningmodel <- MODEL()$DATALEARNINGMODEL

  # Calculer statistiques de survie
  survival_stats <- data.frame(
    Metric = c("Median survival time", "1-year survival", "3-year survival", "5-year survival"),
    Value = c(
      get_median_survival(MODEL()$MODEL),
      # Calculer probabilités de survie à 1, 3, 5 ans
      # ... à implémenter selon le modèle
      NA, NA, NA
    )
  )

  survival_stats
}, include.rownames = FALSE)
```

---

### 6. **Adaptation de output$tabmodeldecouv** (server.R)

**Fichier**: server.R
**Section**: Lignes 1130-1133
**Priorité**: 🟡 IMPORTANT

**Problème**: Actuellement affiche une matrice de confusion (predicted vs actual class). En survie, on n'a pas de classes prédites.

**Modifications suggérées**:
```r
# Remplacer par tableau de quantiles de risque
output$tabmodeldecouv <- renderTable({
  datalearningmodel <- MODEL()$DATALEARNINGMODEL

  if(!is.null(datalearningmodel$reslearningmodel$riskscores)){
    risk_scores <- datalearningmodel$reslearningmodel$riskscores

    # Tableau des quantiles de scores de risque
    risk_summary <- data.frame(
      Quantile = c("Min", "Q1", "Median", "Q3", "Max", "Mean"),
      Risk_Score = c(
        min(risk_scores, na.rm = TRUE),
        quantile(risk_scores, 0.25, na.rm = TRUE),
        median(risk_scores, na.rm = TRUE),
        quantile(risk_scores, 0.75, na.rm = TRUE),
        max(risk_scores, na.rm = TRUE),
        mean(risk_scores, na.rm = TRUE)
      )
    )

    risk_summary
  }
}, include.rownames = FALSE)
```

---

## 📋 Checklist de finalisation

### Modifications global.R
- [ ] Adapter fonction de sélection de variables (tests Wilcoxon → log-rank/Cox)
- [ ] Remplacer construction Random Forest par RSF
- [ ] Remplacer construction SVM par Cox
- [ ] Ajouter construction Cox pénalisé (Lasso/ElasticNet/Ridge)
- [ ] Modifier structure de retour (riskscores au lieu de scorelearning/predictclass)

### Modifications server.R
- [ ] Adapter download handlers pour graphiques KM
- [ ] Modifier tableau de résultats (ligne 214)
- [ ] Remplacer output$youndendecouv/youndenval
- [ ] Adapter output$tabmodeldecouv/tabmodelval
- [ ] Mettre à jour observe() pour les modèles de survie (lignes 909-917)

### Tests à effectuer
- [ ] Tester import de données avec colonnes time/status
- [ ] Tester validation de la structure des données
- [ ] Tester affichage des métriques C-index et IBS
- [ ] Tester génération des courbes de Kaplan-Meier
- [ ] Tester construction d'un modèle Cox
- [ ] Tester construction d'un modèle RSF
- [ ] Tester sélection de variables avec tests de survie
- [ ] Tester export des graphiques KM

---

## 🎯 Ordre recommandé d'implémentation

1. **MAINTENANT** : Adapter la construction des modèles (global.R) - CRITIQUE
2. **ENSUITE** : Adapter la sélection de variables (global.R) - CRITIQUE
3. **PUIS** : Corriger download handlers et tableaux (server.R) - IMPORTANT
4. **ENFIN** : Tests complets avec un jeu de données réel - VALIDATION

---

## 📚 Ressources pour développement

### Jeux de données de test recommandés
- `survival::lung` - Cancer du poumon (R built-in)
- `survival::veteran` - Vétérans avec cancer (R built-in)
- `survival::ovarian` - Cancer ovarien (R built-in)

### Exemple de structure attendue
```r
# Colonnes minimales requises:
# Col 1: time (numeric) - temps jusqu'à événement ou censure
# Col 2: status (0/1) - 0 = censuré, 1 = événement
# Col 3+: variables/features (numeric)

example_data <- data.frame(
  time = c(100, 200, 150, 300, 250),
  status = c(1, 0, 1, 1, 0),
  age = c(60, 55, 70, 65, 58),
  biomarker1 = c(0.5, 0.8, 1.2, 0.3, 0.9),
  biomarker2 = c(100, 150, 120, 180, 140)
)
```

---

**Note importante**: Les modifications critiques concernent principalement global.R (fonctions de modélisation et sélection). Les corrections dans server.R sont surtout cosmétiques une fois que global.R est adapté.
