# Points d'amélioration de l'application Survival Analysis

## Date : 2025-12-24

## Vue d'ensemble

Ce document présente une analyse complète des points d'amélioration de l'application, classés par priorité et catégorie.

**État actuel** : Application fonctionnelle à 85-90% pour l'analyse de survie
**Points forts** : Validation UI, traduction FR/EN, tooltips, flexibilité des données
**Points à améliorer** : Listés ci-dessous

---

## 📊 Résumé par priorité

| Priorité | Nombre | Description |
|----------|--------|-------------|
| 🔴 **CRITIQUE** | 3 | Bloquants pour usage clinique |
| 🟠 **HAUTE** | 7 | Impactent significativement l'UX |
| 🟡 **MOYENNE** | 8 | Améliorations importantes |
| 🟢 **FAIBLE** | 6 | Nice-to-have |

**Total** : 24 points d'amélioration identifiés

---

# 🔴 PRIORITÉ CRITIQUE (3)

## 1. Images manquantes dans l'UI

**Fichier** : `server.R:45-59`
**Impact** : ❌ Images cassées affichées à l'utilisateur

### Problème
```r
output$image1 <- renderImage({
  return(list(src="pictures/Logo I2MC.jpg",  # ❌ Fichier n'existe pas
              contentType="image/jpeg",
              width=300, height=200,
              alt="I2MC logo"))
}, deleteFile = F)

output$image2 <- renderImage({
  return(list(src="pictures/rflabxx.png",    # ❌ Fichier n'existe pas
              ...))
}, deleteFile = F)

output$image3 <- renderImage({
  return(list(src="pictures/structurdata2.jpg",  # ❌ Fichier n'existe pas
              ...))
}, deleteFile = F)
```

### Solutions

**Option A** : Créer le dossier et les images
```bash
mkdir -p pictures/
# Ajouter les logos
```

**Option B** : Supprimer les images de l'UI (recommandé)
```r
# Commenter ou supprimer les renderImage
# L'application fonctionne sans ces images
```

**Option C** : Gérer les images manquantes gracieusement
```r
output$image1 <- renderImage({
  img_path <- "pictures/Logo I2MC.jpg"
  if(file.exists(img_path)){
    return(list(src=img_path, ...))
  } else {
    # Retourner une image placeholder ou NULL
    return(NULL)
  }
}, deleteFile = F)
```

**Effort** : 15 minutes (Option B) | 1-2 heures (Options A/C)

---

## 2. Authentification et autorisation utilisateur

**Fichier** : Nouveau module à créer
**Impact** : 🔒 Sécurité pour usage clinique

### Problème actuel
- N'importe qui peut accéder à l'application
- Pas de contrôle d'accès
- Pas de traçabilité des actions
- Données sensibles potentiellement exposées

### Solution recommandée

**Package** : `shinymanager` ou `shinyauthr`

**Implémentation avec shinymanager** :
```r
# global.R
usePackage("shinymanager")

# server.R (début)
res_auth <- secure_server(
  check_credentials = check_credentials(
    "credentials.sqlite"  # Base de données utilisateurs
  )
)

# ui.R (wrapper)
ui <- secure_app(
  fluidPage(...),
  theme = "cerulean"
)
```

**Fonctionnalités requises** :
- ✅ Login/mot de passe
- ✅ Rôles utilisateurs (admin, analyste, viewer)
- ✅ Sessions avec timeout
- ✅ Log des connexions
- ✅ Gestion des mots de passe (hash bcrypt)

**Effort** : 4-6 heures

---

## 3. Audit trail et logging

**Fichier** : Nouveau module à créer
**Impact** : 📋 Conformité réglementaire (RGPD, FDA 21 CFR Part 11)

### Problème actuel
- Aucun enregistrement des actions utilisateur
- Impossible de savoir qui a fait quoi et quand
- Pas de traçabilité des analyses
- Non conforme pour usage clinique

### Solution recommandée

**Package** : `logger` + base de données SQLite

**Implémentation** :
```r
# global.R
usePackage("logger")
usePackage("DBI")
usePackage("RSQLite")

# Initialiser le logger
log_appender(appender_file("logs/app.log"))
log_threshold(INFO)

# Fonction de logging personnalisée
log_action <- function(user, action, details = NULL) {
  con <- dbConnect(SQLite(), "logs/audit.db")

  dbExecute(con, "
    INSERT INTO audit_log (timestamp, user, action, details)
    VALUES (?, ?, ?, ?)
  ", params = list(
    Sys.time(),
    user,
    action,
    jsonlite::toJSON(details)
  ))

  dbDisconnect(con)

  log_info(paste(user, "-", action))
}

# Utilisation dans server.R
observeEvent(input$confirmdatabutton, {
  log_action(
    user = session$user,
    action = "DATA_CONFIRMED",
    details = list(
      file = input$learningfile$name,
      n_rows = nrow(DATA()$LEARNING),
      n_cols = ncol(DATA()$LEARNING)
    )
  )
})
```

**Événements à logger** :
- Connexion/déconnexion utilisateur
- Import de fichiers
- Confirmation des données
- Sélection de variables
- Construction de modèles
- Export de résultats
- Modification de paramètres

**Effort** : 6-8 heures

---

# 🟠 PRIORITÉ HAUTE (7)

## 4. Module de prédiction pour nouveaux patients

**Fichier** : Nouveau module à créer
**Impact** : 🎯 Utilité clinique majeure

### Problème actuel
- L'application entraîne des modèles mais ne peut pas prédire
- Impossible d'utiliser un modèle pour de nouveaux patients
- Limite l'utilité en contexte clinique réel

### Solution recommandée

**Nouvel onglet "Predict"** dans `ui.R` :
```r
tabPanel("Prediction",
  fluidRow(
    column(6,
      h4("Upload new patient data"),
      fileInput("new_patient_file", "CSV file with new patients"),
      actionButton("predict_btn", "Predict Survival", class = "btn-primary")
    ),
    column(6,
      h4("Prediction results"),
      tableOutput("prediction_table"),
      downloadButton("download_predictions", "Download predictions")
    )
  ),
  fluidRow(
    column(12,
      h4("Individual survival curves"),
      plotOutput("individual_survival_curves", height = "600px")
    )
  )
)
```

**Nouvelle fonction dans `global.R`** :
```r
predict_survival <- function(model, new_data, model_type, times = NULL) {
  # Valider que new_data a les mêmes variables que le modèle

  # Prédire selon le type de modèle
  if(model_type == "cox") {
    # Courbe de survie basée sur Cox
    surv_fit <- survfit(model, newdata = new_data)

  } else if(model_type == "rsf") {
    # Prédictions RSF
    pred <- predict(model, data = new_data)

  } else if(model_type %in% c("coxlasso", "coxelasticnet", "coxridge")) {
    # Prédictions Cox pénalisé
    risk_scores <- predict(model, newx = as.matrix(new_data),
                          s = "lambda.min", type = "link")
  }

  return(list(
    risk_scores = risk_scores,
    survival_probs = survival_probs,
    median_survival = median_survival
  ))
}
```

**Fonctionnalités** :
- Upload fichier CSV avec nouveaux patients
- Validation que les variables correspondent au modèle
- Calcul de :
  - Risk scores
  - Probabilités de survie à différents temps
  - Médiane de survie estimée
  - Intervalles de confiance
- Visualisation :
  - Courbes de survie individuelles
  - Comparaison avec groupes de risque du modèle
- Export des prédictions (CSV, Excel)

**Effort** : 8-10 heures

---

## 5. Configuration via fichier YAML

**Fichier** : Créer `config.yaml` + adapter `global.R`
**Impact** : ⚙️ Flexibilité déploiement dev/prod

### Problème actuel
- Paramètres hard-codés dans le code
- Difficile d'adapter selon l'environnement
- Nécessite de modifier le code pour changer un paramètre

**Exemples de paramètres hard-codés** :
```r
# server.R:1
options(shiny.maxRequestSize=60*1024^2)  # Fixe à 60 MB

# global.R:5-46
# Auto-installation des packages (risqué en production)
```

### Solution recommandée

**Créer `config.yaml`** :
```yaml
# Configuration de l'application Survival Analysis

app:
  name: "Survival Analysis Application"
  version: "2.0.0"
  max_upload_mb: 100  # Augmenté à 100 MB
  auto_install_packages: false  # Désactivé en production
  log_level: "INFO"  # DEBUG, INFO, WARN, ERROR
  session_timeout_minutes: 60

survival:
  default_time_column: "time"
  default_status_column: "status"
  min_events_required: 10
  max_time_points_roc: 15

models:
  rsf:
    default_ntree: 500
    default_mtry_ratio: 0.33
    max_nodesize: 10
  cox:
    confidence_level: 0.95
    tie_method: "efron"  # efron, breslow, exact

performance:
  enable_caching: true
  cache_dir: "cache/"
  parallel_cores: 2

security:
  enable_authentication: true
  password_min_length: 8
  session_encryption: true

ui:
  default_language: "en"  # en, fr
  theme: "cerulean"
  show_logos: false  # false car images manquantes
```

**Charger dans `global.R`** :
```r
usePackage("yaml")
usePackage("config")

# Charger configuration
app_config <- yaml::read_yaml("config.yaml")

# Appliquer les paramètres
options(shiny.maxRequestSize = app_config$app$max_upload_mb * 1024^2)

if(app_config$app$log_level == "DEBUG") {
  log_threshold(DEBUG)
} else if(app_config$app$log_level == "INFO") {
  log_threshold(INFO)
}
```

**Avantages** :
- Configuration centralisée
- Facile de créer `config.dev.yaml` et `config.prod.yaml`
- Pas besoin de modifier le code
- Déploiement simplifié

**Effort** : 3-4 heures

---

## 6. Gestion robuste des erreurs avec messages UI

**Fichier** : `server.R` - Tous les reactives
**Impact** : 🛡️ Éviter les crashes, meilleure UX

### Problème actuel
- Certaines erreurs affichées dans la console uniquement
- Application peut crasher sur erreurs inattendues
- Pas de récupération gracieuse

### Solution recommandée

**Pattern à appliquer partout** :
```r
# Avant (peut crasher)
SELECTDATA <- reactive({
  learning <- DATA()$LEARNING
  # ... traitement ...
  result
})

# Après (robuste)
SELECTDATA <- reactive({
  tryCatch({
    # Validation des pré-requis
    req(DATA()$LEARNING)
    learning <- DATA()$LEARNING

    validate(
      need(nrow(learning) > 0, "No data available"),
      need(ncol(learning) >= 3, "Need at least time, status, and 1 feature")
    )

    # Traitement
    result <- selectdatafunction(...)

    # Validation du résultat
    validate(need(!is.null(result), "Selection failed"))

    return(result)

  }, error = function(e) {
    # Logger l'erreur
    log_error(paste("SELECTDATA error:", e$message))

    # Notifier l'utilisateur
    showNotification(
      paste("Error during data selection:", e$message),
      type = "error",
      duration = 10
    )

    # Retourner NULL ou valeur par défaut
    return(NULL)
  })
})
```

**Zones critiques à protéger** :
- Import de fichiers (`DATA()`)
- Sélection de données (`SELECTDATA()`)
- Transformation (`TRANSFORMDATA()`)
- Tests statistiques (`TEST()`)
- Construction modèles (`MODEL()`)

**Effort** : 4-5 heures

---

## 7. Export des résultats complets

**Fichier** : Nouveau module export
**Impact** : 📊 Utilité pour rapports cliniques

### Problème actuel
- Exports partiels (seulement certains tableaux)
- Pas d'export complet d'une analyse
- Difficile de partager les résultats

### Solution recommandée

**Nouveau bouton "Export Complete Report"** :
```r
# ui.R - Dans chaque onglet de résultats
downloadButton("export_complete_report", "Export Complete Report (PDF)")
downloadButton("export_complete_data", "Export All Data (ZIP)")
```

**Fonction d'export PDF** :
```r
usePackage("rmarkdown")
usePackage("knitr")

export_complete_report <- function(data, model, metrics, plots) {
  # Créer un rapport RMarkdown
  rmd_template <- "
---
title: 'Survival Analysis Report'
date: '`r Sys.Date()`'
output: pdf_document
---

# Data Summary
- **N observations**: `r nrow(data)`
- **N events**: `r sum(data$status == 1)`
- **N censored**: `r sum(data$status == 0)`
- **N features**: `r ncol(data) - 2`

# Model Performance
- **C-index**: `r round(metrics$cindex, 3)`
- **IBS**: `r round(metrics$ibs, 3)`

# Kaplan-Meier Curves
```{r, echo=FALSE}
print(plots$km_plot)
```

# Selected Variables
```{r, echo=FALSE}
kable(selected_vars_table)
```

# Model Coefficients (if Cox)
```{r, echo=FALSE}
if(!is.null(model$coefficients)) {
  kable(coef_table)
}
```
  "

  # Render PDF
  rmarkdown::render(
    input = textConnection(rmd_template),
    output_file = tempfile(fileext = ".pdf"),
    quiet = TRUE
  )
}
```

**Contenu du rapport** :
- Résumé des données
- Variables sélectionnées (avec p-values)
- Performance du modèle (C-index, IBS)
- Courbes de Kaplan-Meier
- Coefficients du modèle (si Cox)
- Importance des variables (si RSF)
- Paramètres utilisés

**Format ZIP avec** :
- Rapport PDF
- Données d'entrée (CSV)
- Résultats de sélection (CSV)
- Prédictions (CSV)
- Graphiques (PNG)
- Objet modèle (RData)

**Effort** : 6-8 heures

---

## 8. Validation avant modélisation

**Fichier** : `server.R` - Observer avant MODEL()
**Impact** : 🚫 Éviter erreurs d'entraînement

### Problème actuel
- Pas de validation du nombre d'événements minimum
- Modèle peut être instable avec peu de données
- Pas de vérification du ratio événements/variables

### Solution recommandée

```r
# Avant MODEL()
observeEvent(input$constructmodelbutton, {
  # Récupérer les données
  learning <- TRANSFORMDATA()$LEARNINGTRANSFORM

  # Validations critiques
  n_events <- sum(learning$status == 1, na.rm = TRUE)
  n_features <- ncol(learning) - 2  # Exclure time et status

  # Règle empirique : 10 événements par variable
  min_events_required <- n_features * 10

  if(n_events < min_events_required) {
    showModal(modalDialog(
      title = "⚠️ Warning: Insufficient events",
      paste0(
        "The model may be unreliable.\n\n",
        "Events: ", n_events, "\n",
        "Features: ", n_features, "\n",
        "Recommended minimum events: ", min_events_required, " (10 per feature)\n\n",
        "Consider:\n",
        "- Reducing the number of features\n",
        "- Using regularized models (Cox Lasso/Ridge)\n",
        "- Collecting more data"
      ),
      easyClose = TRUE,
      footer = tagList(
        actionButton("continue_anyway", "Continue anyway"),
        modalButton("Cancel")
      )
    ))
    return()
  }

  # Si ratio OK, construire le modèle
  # ...
})
```

**Validations à ajouter** :
- ✅ Minimum 10 événements par variable
- ✅ Au moins 20 événements au total
- ✅ Pas plus de 50% de valeurs manquantes
- ✅ Temps maximum > temps médian × 2
- ✅ Vérifier proportional hazards (test de Schoenfeld pour Cox)

**Effort** : 2-3 heures

---

## 9. Gestion des sessions multiples

**Fichier** : `global.R` + `server.R`
**Impact** : 👥 Performance multi-utilisateurs

### Problème actuel
- Variables globales (`<<-`) partagées entre sessions
- Risque de conflits si plusieurs utilisateurs
- Pas de namespace par session

### Solution recommandée

**Remplacer assignations globales par reactiveValues** :
```r
# Avant (global, partagé)
server <- function(input, output, session) {
  importparameters <<- list(...)  # ❌ Global
  learning <<- DATA()$LEARNING    # ❌ Global
}

# Après (isolé par session)
server <- function(input, output, session) {
  # ReactiveValues pour l'état de la session
  session_state <- reactiveValues(
    importparameters = NULL,
    learning = NULL,
    selectdataparameters = NULL,
    model = NULL
  )

  # Utiliser session_state au lieu de variables globales
  observe({
    session_state$importparameters <- list(
      "learningfile" = input$learningfile,
      ...
    )
  })

  DATA <- reactive({
    out <- importfunction(session_state$importparameters)
    session_state$learning <- out$learning
    return(out)
  })
}
```

**Avantages** :
- Isolation complète entre utilisateurs
- Pas de conflits de données
- Meilleure gestion mémoire
- Compatible shinyapps.io et Shiny Server

**Effort** : 8-10 heures (refactoring important)

---

## 10. Tests unitaires et d'intégration

**Fichier** : Créer dossier `tests/`
**Impact** : 🧪 Qualité et maintenance du code

### Problème actuel
- Aucun test automatisé
- Risque de régression lors de modifications
- Difficile de valider que tout fonctionne

### Solution recommandée

**Package** : `testthat` + `shinytest2`

**Structure** :
```
tests/
├── testthat/
│   ├── test-confirmdata.R
│   ├── test-survival-functions.R
│   ├── test-models.R
│   └── test-validation.R
└── shinytest2/
    ├── test-ui-workflow.R
    └── test-validation-feedback.R
```

**Exemple de test** :
```r
# tests/testthat/test-confirmdata.R
library(testthat)
source("../../global.R")

test_that("confirmdata validates positive time values", {
  # Données de test avec temps négatif
  test_data <- data.frame(
    time = c(-1, 5, 10),
    status = c(1, 0, 1),
    feature1 = c(1, 2, 3)
  )

  # Devrait générer une erreur
  expect_error(
    confirmdata(test_data, time_col = "time", status_col = "status"),
    "time values must be strictly positive"
  )
})

test_that("confirmdata accepts valid survival data", {
  test_data <- data.frame(
    time = c(5, 10, 15),
    status = c(1, 0, 1),
    feature1 = c(1, 2, 3)
  )

  result <- confirmdata(test_data, time_col = "time", status_col = "status")

  expect_equal(colnames(result)[1:2], c("time", "status"))
  expect_equal(nrow(result), 3)
})
```

**Tests à créer** :
- Fonctions de survie (C-index, IBS, etc.)
- Validation des données
- Construction des modèles
- Export/Import
- UI workflows complets

**Effort** : 10-12 heures (test coverage ~70%)

---

# 🟡 PRIORITÉ MOYENNE (8)

## 11. Amélioration des graphiques

**Impact** : 📈 Qualité visuelle

### Points d'amélioration

**A. Courbes Kaplan-Meier** :
- Ajouter l'intervalle de confiance (optionnel)
- Afficher le nombre à risque sous le graphique
- Palette de couleurs professionnelle
- Export haute résolution (300 dpi)

**B. Graphiques interactifs** :
```r
usePackage("plotly")

# Convertir ggplot en plotly
km_plot_interactive <- ggplotly(km_plot$plot)
```

**C. Graphiques temporels AUC/Sens/Spec** :
- Ajouter intervalles de confiance
- Highlighter le temps médian
- Légende plus claire

**Effort** : 3-4 heures

---

## 12. Import depuis bases de données

**Impact** : 🗄️ Intégration avec systèmes existants

### Solution recommandée

```r
usePackage("DBI")
usePackage("RPostgres")  # ou RMySQL, RSQLite

# Nouvel onglet "Import from Database"
tabPanel("Database Import",
  selectInput("db_type", "Database Type",
             choices = c("PostgreSQL", "MySQL", "SQL Server")),
  textInput("db_host", "Host", value = "localhost"),
  numericInput("db_port", "Port", value = 5432),
  textInput("db_name", "Database Name"),
  textInput("db_user", "Username"),
  passwordInput("db_password", "Password"),
  textAreaInput("db_query", "SQL Query",
               value = "SELECT * FROM survival_data LIMIT 1000"),
  actionButton("connect_db", "Connect and Import")
)
```

**Effort** : 4-5 heures

---

## 13. Sauvegarde automatique de l'état

**Impact** : 💾 Récupération après crash

### Solution

```r
# Auto-save toutes les 5 minutes
observe({
  invalidateLater(300000)  # 5 minutes en ms

  if(!is.null(MODEL()$MODEL)) {
    # Sauvegarder l'état automatiquement
    saveRDS(list(
      data = DATA(),
      model = MODEL(),
      timestamp = Sys.time()
    ), file = paste0("autosave/session_", session$token, ".rds"))
  }
})

# Bouton de récupération
actionButton("restore_session", "Restore last session")
```

**Effort** : 2-3 heures

---

## 14. Diagnostic des modèles Cox

**Impact** : ✅ Validation du modèle

### Solution

```r
# Tests de diagnostic Cox
diagnose_cox_model <- function(model, data) {
  # Test proportional hazards (Schoenfeld residuals)
  ph_test <- cox.zph(model)

  # Residuals plot
  residuals_plot <- ggcoxdiagnostics(
    model,
    type = "schoenfeld",
    title = "Schoenfeld Residuals"
  )

  # Influence plot
  influence_plot <- ggcoxdiagnostics(
    model,
    type = "dfbeta",
    title = "Influence of observations"
  )

  return(list(
    ph_test = ph_test,
    plots = list(
      residuals = residuals_plot,
      influence = influence_plot
    )
  ))
}
```

**Effort** : 3-4 heures

---

## 15. Support de formats supplémentaires

**Impact** : 📁 Flexibilité import

### Formats à ajouter

- **Parquet** : Fichiers compressés
- **Feather** : Format Arrow
- **RDS** : Objets R natifs
- **SAS** : .sas7bdat
- **SPSS** : .sav
- **Stata** : .dta

```r
usePackage("arrow")    # Parquet, Feather
usePackage("haven")    # SAS, SPSS, Stata

# Adapter importfile()
if(extension == ".parquet"){
  data <- arrow::read_parquet(filepath)
} else if(extension == ".sas7bdat"){
  data <- haven::read_sas(filepath)
}
```

**Effort** : 2-3 heures

---

## 16. Comparaison de modèles

**Impact** : 🔬 Choix du meilleur modèle

### Solution

```r
# Nouvel onglet "Model Comparison"
tabPanel("Compare Models",
  checkboxGroupInput("models_to_compare",
                    "Select models to compare:",
                    choices = c("Cox", "Cox Lasso", "Cox Ridge",
                               "Cox ElasticNet", "RSF")),
  actionButton("compare_btn", "Compare Models"),

  tableOutput("model_comparison_table"),
  plotOutput("model_comparison_plot")
)

# Fonction de comparaison
compare_models <- function(models_list, validation_data) {
  results <- data.frame()

  for(model_name in names(models_list)) {
    model <- models_list[[model_name]]

    # Calculer métriques
    cindex <- calculate_cindex(...)
    ibs <- calculate_ibs(...)

    results <- rbind(results, data.frame(
      Model = model_name,
      C_index = cindex,
      IBS = ibs,
      N_variables = length(coef(model))
    ))
  }

  return(results)
}
```

**Effort** : 4-5 heures

---

## 17. Documentation utilisateur intégrée

**Impact** : 📖 Facilité d'utilisation

### Solution

```r
# Nouvel onglet "Help"
tabPanel("Help",
  navlistPanel(
    tabPanel("Getting Started",
      includeMarkdown("docs/getting_started.md")
    ),
    tabPanel("Data Format",
      includeMarkdown("docs/data_format.md")
    ),
    tabPanel("Models",
      includeMarkdown("docs/models_guide.md")
    ),
    tabPanel("Interpretation",
      includeMarkdown("docs/interpretation.md")
    ),
    tabPanel("FAQ",
      includeMarkdown("docs/faq.md")
    )
  )
)
```

**Effort** : 6-8 heures (rédaction + intégration)

---

## 18. Versionning des modèles

**Impact** : 🗂️ Traçabilité

### Solution

```r
# Sauvegarder avec métadonnées
save_model_version <- function(model, metadata) {
  version_id <- paste0("v", Sys.Date(), "_",
                      format(Sys.time(), "%H%M%S"))

  model_data <- list(
    model = model,
    version = version_id,
    timestamp = Sys.time(),
    user = session$user,
    parameters = metadata$parameters,
    performance = metadata$performance,
    data_hash = digest::digest(metadata$training_data)
  )

  saveRDS(model_data,
         file = paste0("models/", version_id, ".rds"))

  # Log dans une table de versions
  log_model_version(version_id, model_data)
}
```

**Effort** : 3-4 heures

---

# 🟢 PRIORITÉ FAIBLE (6)

## 19. Thèmes personnalisables

**Impact** : 🎨 Personnalisation visuelle

### Solution

```r
# Sélecteur de thème déjà partiellement présent
selectInput("theme_app",
           "Theme",
           choices = setNames(THEMES, tools::toTitleCase(THEMES)),
           selected = "cerulean")

# Appliquer dynamiquement
observeEvent(input$theme_app, {
  session$setCurrentTheme(bs_theme(bootswatch = input$theme_app))
}, ignoreInit = TRUE)
```

**Effort** : 1 heure

---

## 20. Export des graphiques vectoriels

**Impact** : 📊 Qualité publication

### Solution

```r
# Ajouter option SVG et EPS
radioButtons("plot_format", "Format:",
            choices = c("PNG", "PDF", "SVG", "EPS"),
            selected = "PNG")

downloadHandler(
  filename = function() {
    paste0("plot.", tolower(input$plot_format))
  },
  content = function(file) {
    if(input$plot_format == "SVG") {
      svglite::svglite(file, width = 10, height = 8)
    } else if(input$plot_format == "EPS") {
      cairo_ps(file, width = 10, height = 8)
    }
    print(plot)
    dev.off()
  }
)
```

**Effort** : 1-2 heures

---

## 21. Notifications par email

**Impact** : 📧 Alertes analyses longues

### Solution

```r
usePackage("mailR")

# Pour analyses longues
withProgress(message = "Building model...", {
  model <- build_model(...)

  # Envoyer email quand terminé
  if(input$notify_by_email) {
    send.mail(
      from = "app@survival-analysis.com",
      to = session$user_email,
      subject = "Analysis complete",
      body = paste("Your survival analysis is complete.",
                  "C-index:", round(cindex, 3)),
      smtp = list(host.name = "smtp.gmail.com", ...)
    )
  }
})
```

**Effort** : 2-3 heures

---

## 22. Mode sombre (Dark mode)

**Impact** : 🌙 Confort visuel

### Solution

```r
# Toggle déjà présent (commenté)
# Décommenter dans ui.R lignes 17-28

observe({
  new_theme <- if(input$mode == "dark") {
    bs_theme(bootswatch = "darkly")
  } else {
    bs_theme(bootswatch = "cerulean")
  }
  session$setCurrentTheme(new_theme)
})
```

**Effort** : 30 minutes

---

## 23. Raccourcis clavier

**Impact** : ⌨️ Productivité power users

### Solution

```r
# Ajouter JavaScript pour raccourcis
tags$script(HTML("
  $(document).keydown(function(e) {
    // Ctrl+S : Save
    if(e.ctrlKey && e.which == 83) {
      e.preventDefault();
      $('#savestate').click();
    }

    // Ctrl+Enter : Confirm data
    if(e.ctrlKey && e.which == 13) {
      $('#confirmdatabutton').click();
    }

    // Ctrl+M : Build model
    if(e.ctrlKey && e.which == 77) {
      $('#constructmodelbutton').click();
    }
  });
"))
```

**Effort** : 1 heure

---

## 24. Analyse de sensibilité

**Impact** : 🔍 Robustesse des résultats

### Solution

```r
# Nouvel onglet "Sensitivity Analysis"
perform_sensitivity_analysis <- function(data, base_model) {
  results <- list()

  # 1. Bootstrap réplications
  for(i in 1:100) {
    boot_sample <- data[sample(nrow(data), replace = TRUE), ]
    boot_model <- fit_model(boot_sample)
    results$bootstrap[[i]] <- calculate_metrics(boot_model, boot_sample)
  }

  # 2. Leave-one-out
  for(i in 1:nrow(data)) {
    loo_data <- data[-i, ]
    loo_model <- fit_model(loo_data)
    results$loo[[i]] <- calculate_metrics(loo_model, loo_data)
  }

  # 3. Variation paramètres
  for(param_value in seq(0.5, 2, 0.1)) {
    var_model <- fit_model(data, param = param_value)
    results$param_variation[[param_value]] <- calculate_metrics(var_model, data)
  }

  return(results)
}
```

**Effort** : 5-6 heures

---

# 📋 Résumé et recommandations

## Roadmap suggérée

### Phase 1 : Production-ready (2-3 semaines)
**Objectif** : Application utilisable en clinique

1. ✅ Supprimer/gérer images manquantes (15 min)
2. ✅ Authentification utilisateur (6h)
3. ✅ Audit trail et logging (8h)
4. ✅ Module de prédiction (10h)
5. ✅ Configuration YAML (4h)
6. ✅ Gestion erreurs robuste (5h)
7. ✅ Validation avant modélisation (3h)

**Total** : ~40 heures

### Phase 2 : Amélioration UX (2 semaines)
**Objectif** : Interface professionnelle

8. ✅ Export complet (8h)
9. ✅ Tests unitaires (12h)
10. ✅ Amélioration graphiques (4h)
11. ✅ Documentation intégrée (8h)

**Total** : ~32 heures

### Phase 3 : Fonctionnalités avancées (3 semaines)
**Objectif** : Application complète

12-24. Toutes les autres améliorations selon priorité

**Total** : ~50 heures

---

## Priorisation par ROI

| Amélioration | Effort | Impact | ROI |
|--------------|--------|--------|-----|
| Images manquantes | ⭐ | ⭐⭐ | ⭐⭐⭐⭐⭐ |
| Module prédiction | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| Configuration YAML | ⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| Gestion erreurs | ⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| Authentification | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| Audit trail | ⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐ |

---

**Document créé le** : 2025-12-24
**Auteur** : Analyse Claude
**Version** : 1.0
