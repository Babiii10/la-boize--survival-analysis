# Application d'Analyse de Survie Hybride

## ⚠️ CHANGEMENTS IMPORTANTS (Décembre 2024)

**L'application supporte maintenant UNIQUEMENT les modèles de survie adaptés aux données censurées.**

Les modèles de classification binaire (SVM, XGBoost, KNN, NaiveBayes, LightGBM) ont été **supprimés** car ils ne gèrent PAS correctement les données de survie censurées.

📖 **Voir [SURVIVAL_MODELS.md](SURVIVAL_MODELS.md) pour la documentation complète des modèles supportés.**

---

## 📊 Vue d'Ensemble

Application Shiny R pour l'analyse de survie avec approche **hybride** innovante combinant :

- ✅ **Analyse de Survie Classique** : C-index, Brier Score, Kaplan-Meier
- ✅ **Classification Temporelle** : AUC(t), Sensibilité(t), Spécificité(t) au cours du temps
- ✅ **Gestion Robuste de la Censure** : Package timeROC avec IPCW
- ✅ **Multiples Modèles** : Cox, Random Survival Forest, Cox pénalisé (Lasso/ElasticNet/Ridge)

---

## 🚀 Démarrage Rapide

### Installation des Dépendances

```r
# Packages requis
install.packages(c(
  "shiny", "survival", "survminer", "ranger",
  "glmnet", "riskRegression", "pec", "prodlim",
  "timeROC", "pROC", "ggplot2", "DT", "writexl"
))
```

### Lancement de l'Application

```r
# Cloner le repository
git clone https://github.com/Babiii10/la-boize--survival-analysis.git
cd la-boize--survival-analysis

# Lancer l'application
R -e "shiny::runApp()"
```

Ou dans RStudio : Ouvrir `ui.R` et cliquer sur "Run App"

---

## 📚 Documentation

| Document | Description | Public |
|----------|-------------|--------|
| [**SURVIVAL_MODELS.md**](SURVIVAL_MODELS.md) | ⭐ **Modèles supportés et changements récents** | 👨‍⚕️ Tous |
| [**QUICK_START.md**](QUICK_START.md) | Guide de démarrage en 5 minutes | 👨‍⚕️ Utilisateurs |
| [**USER_GUIDE.md**](USER_GUIDE.md) | Guide complet d'utilisation et d'interprétation | 👨‍⚕️ Utilisateurs |
| [**SURVIVAL_ADAPTATION_GUIDE.md**](SURVIVAL_ADAPTATION_GUIDE.md) | Guide technique d'adaptation | 👨‍💻 Développeurs |
| [**IMPLEMENTATION_COMPLETE.md**](IMPLEMENTATION_COMPLETE.md) | Documentation de l'implémentation | 👨‍💻 Développeurs |
| [**REMAINING_WORK.md**](REMAINING_WORK.md) | Travaux restants (historique) | 👨‍💻 Développeurs |

---

## 🎯 Fonctionnalités Principales

### 1. Analyse de Survie Pure

- **Métriques Globales**
  - C-index (Concordance Index) : Capacité discriminative globale
  - Integrated Brier Score (IBS) : Erreur de prédiction moyenne
  - Médiane de survie par groupe de risque

- **Visualisations**
  - Courbes de Kaplan-Meier avec groupes de risque (haut/bas)
  - Test log-rank automatique
  - Intervalles de confiance

- **Statistiques**
  - Quantiles des scores de risque (Min, Q1, Médiane, Q3, Max)
  - Médiane de survie par groupe
  - Nombre d'événements par groupe

### 2. Classification Temporelle

- **Métriques Temporelles** (AUC, Sensibilité, Spécificité)
  - Calculées à chaque point temporel
  - Gestion de la censure avec timeROC (IPCW)
  - Seuil optimal de Youden pour chaque temps

- **Visualisations**
  - Courbe d'évolution temporelle (AUC/Sens/Spec vs temps)
  - Matrices de confusion à des temps spécifiques
  - Tableaux détaillés de métriques

- **Innovation**
  - Filtrage intelligent des patients censurés avant t
  - Score de risque 1-S(t) : probabilité d'événement
  - Tracking du nombre de patients inclus/exclus

### 3. Modèles de Survie Disponibles

| Modèle | Type | Avantages | Utilisation |
|--------|------|-----------|-------------|
| **Cox Proportional Hazards** | Paramétrique | Interprétable, coefficients = HR | Relations linéaires, interprétation |
| **Random Survival Forest** | Non-paramétrique | Capture non-linéarités, auto-tuning | Relations complexes, performance |
| **Cox Lasso** | Régularisation L1 | Sélection automatique de variables | Nombreuses variables, sparsité |
| **Cox ElasticNet** | Régularisation L1+L2 | Équilibre sélection/stabilité | Variables corrélées |
| **Cox Ridge** | Régularisation L2 | Stabilité avec corrélations | Toutes variables importantes |

### 4. Sélection de Variables

- **Tests Statistiques**
  - Log-rank : Non-paramétrique, univarié
  - Cox Wald : Paramétrique avec hazard ratios
  - Cox Lasso/ElasticNet/Ridge : Régularisation

- **Filtrage**
  - Seuil de p-value ajustable
  - Correction de tests multiples (Benjamini-Hochberg)

---

## 📁 Structure des Données

### Format Requis

Fichier CSV/Excel avec minimum 3 colonnes :

```csv
time,status,feature1,feature2,feature3,...
150,1,65,0.8,120
200,0,58,0.5,150
89,1,72,1.2,90
```

- **Colonne 1** : `time` - Temps jusqu'à événement/censure (numérique, > 0)
- **Colonne 2** : `status` - Indicateur d'événement (0=censuré, 1=événement)
- **Colonnes 3+** : Variables prédictives (numériques ou catégorielles)

### Fichier d'Exemple

Téléchargez [`example_survival_data.csv`](example_survival_data.csv) pour tester l'application.

---

## 🔬 Méthodologie

### Analyse de Survie Pure

**C-index Calculation**
```r
C-index = P(score(i) > score(j) | time(i) < time(j), status(i) = 1)
```
Probabilité que le modèle ordonne correctement deux patients.

**Integrated Brier Score**
```r
IBS = ∫[0,τ] BS(t) × w(t) dt
```
Erreur quadratique moyenne pondérée de prédiction.

### Classification Temporelle

**Approche timeROC** (Heagerty et al., 2000)

À chaque temps t :
1. **Filtrage** : Exclure patients censurés avant t
2. **Score de risque** : 1 - S(t) = P(événement avant t)
3. **Classification** :
   - Classe 1 : Événement avant/à t
   - Classe 0 : Survie après t
4. **Calcul AUC** : timeROC avec IPCW (Inverse Probability of Censoring Weighting)
5. **Seuil optimal** : Indice de Youden (max Sensibilité + Spécificité)

**Formules**
```r
AUC(t) = P(marker(i) > marker(j) | T(i) ≤ t < T(j), δ(i) = 1)
Sensibilité(t) = TP(t) / [TP(t) + FN(t)]
Spécificité(t) = TN(t) / [TN(t) + FP(t)]
```

---

## 📊 Exemple de Résultats

### Partie Survie Pure

```
C-index (Learning) : 0.78
IBS (Learning) : 0.18
Médiane survie Haut Risque : 180 jours
Médiane survie Bas Risque : 450 jours
P-value log-rank : < 0.001
```

### Partie Classification Temporelle

```
Temps   | AUC  | Sensibilité | Spécificité | Seuil | N_patients | N_exclus
--------|------|-------------|-------------|-------|------------|----------
50 j    | 0.72 | 0.68        | 0.75        | 0.25  | 180        | 5
100 j   | 0.75 | 0.70        | 0.78        | 0.32  | 175        | 10
150 j   | 0.78 | 0.72        | 0.80        | 0.38  | 165        | 20
200 j   | 0.80 | 0.75        | 0.82        | 0.45  | 150        | 35
```

**Interprétation** :
- Performance s'améliore avec le temps (AUC croissante)
- À 200 jours : 75% des événements détectés, 82% des non-événements détectés
- 35 patients exclus à 200 jours (censurés avant, statut inconnu)

---

## 🧪 Validation

### Datasets de Test Recommandés

Datasets R intégrés pour tester :

```r
# Cancer du poumon
data(lung, package = "survival")

# Vétérans avec cancer
data(veteran, package = "survival")

# Cancer ovarien
data(ovarian, package = "survival")
```

### Workflow de Test

```r
# 1. Préparer données
library(survival)
data(lung)
lung_formatted <- lung[complete.cases(lung), ]
lung_formatted$status <- lung_formatted$status - 1  # Convertir 1/2 → 0/1
write.csv(lung_formatted, "lung_test.csv", row.names = FALSE)

# 2. Lancer app et importer lung_test.csv
# 3. Tester workflow complet
```

---

## 🛠️ Architecture Technique

### Structure des Fichiers

```
la-boize--survival-analysis/
├── global.R                        # Fonctions helper et modèles
├── server.R                        # Logique serveur Shiny
├── ui.R                           # Interface utilisateur
├── example_survival_data.csv       # Données d'exemple
├── README.md                       # Ce fichier
├── QUICK_START.md                 # Guide rapide
├── USER_GUIDE.md                  # Guide utilisateur complet
├── SURVIVAL_ADAPTATION_GUIDE.md   # Guide technique
├── IMPLEMENTATION_COMPLETE.md     # Documentation implémentation
└── REMAINING_WORK.md              # Historique travaux
```

### Fonctions Principales (global.R)

**Survie Pure**
```r
calculate_cindex()          # C-index de Harrell
calculate_ibs()             # Integrated Brier Score
get_risk_scores()           # Scores de risque du modèle
fit_cox_model()             # Cox proportional hazards
fit_rsf_model()             # Random Survival Forest
fit_coxnet_model()          # Cox pénalisé (Lasso/EN/Ridge)
plot_kaplan_meier()         # Courbes KM
```

**Classification Temporelle**
```r
get_survival_predictions()                    # S(t) à différents temps
calculate_classification_metrics_at_time()    # AUC/Sens/Spec à temps t
calculate_temporal_metrics()                  # Métriques temporelles (timeROC)
plot_temporal_classification_metrics()        # Visualisation évolution
plot_timeROC_curve()                         # Courbe ROC à temps donné
```

**Affichage**
```r
display_risk_quantiles()        # Statistiques scores de risque
display_survival_statistics()   # Stats de survie par groupe
```

---

## 📖 Références

### Packages R Utilisés

- **survival** : Cox regression, Kaplan-Meier (Therneau, 2023)
- **survminer** : Visualisations survie (Kassambara et al., 2021)
- **ranger** : Random Survival Forest (Wright & Ziegler, 2017)
- **glmnet** : Cox pénalisé (Friedman et al., 2010)
- **timeROC** : AUC temporelle (Blanche et al., 2013)
- **pROC** : Courbes ROC (Robin et al., 2011)
- **riskRegression** : C-index (Gerds et al., 2022)
- **pec** : Brier Score (Mogensen et al., 2012)

### Articles Scientifiques

1. **timeROC** :
   - Heagerty, P. J., Lumley, T., & Pepe, M. S. (2000). "Time-dependent ROC curves for censored survival data and a diagnostic marker." *Biometrics*, 56(2), 337-344.

2. **C-index** :
   - Harrell, F. E., et al. (1996). "Multivariable prognostic models." *Statistics in Medicine*, 15(4), 361-387.

3. **Random Survival Forest** :
   - Ishwaran, H., et al. (2008). "Random survival forests." *The Annals of Applied Statistics*, 2(3), 841-860.

4. **Brier Score** :
   - Graf, E., et al. (1999). "Assessment and comparison of prognostic classification schemes." *Statistics in Medicine*, 18(17‐18), 2529-2545.

---

## 🤝 Contribution

### Développement

Branche de développement : `claude/survival-context-adaptation-naLoJ`

```bash
# Cloner et créer branche
git clone https://github.com/Babiii10/la-boize--survival-analysis.git
cd la-boize--survival-analysis
git checkout -b feature/nouvelle-fonctionnalite

# Développer, tester, committer
git add .
git commit -m "Description des changements"
git push origin feature/nouvelle-fonctionnalite
```

### Rapporter des Bugs

Ouvrir une issue sur GitHub avec :
- Description du problème
- Étapes pour reproduire
- Données d'exemple (si possible)
- Version de R et packages

---

## 📝 Licence

[À compléter selon votre licence]

---

## 👥 Auteurs

- **Développement initial** : [Vos noms]
- **Adaptation survie** : Claude Code (Anthropic)
- **Date** : Décembre 2025

---

## 🙏 Remerciements

- Communauté R et Shiny
- Auteurs des packages utilisés
- Contributeurs au projet

---

## 📧 Contact

Pour questions ou support :
- **Issues GitHub** : [Lien vers issues]
- **Email** : [Votre email]
- **Documentation** : Voir guides dans le repository

---

**Version** : 1.0.0
**Dernière mise à jour** : 2025-12-23
**Statut** : ✅ Production Ready
