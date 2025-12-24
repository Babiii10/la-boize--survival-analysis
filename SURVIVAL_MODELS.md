# Application d'Analyse de Survie - Modèles Supportés

## 📊 Vue d'Ensemble

Cette application Shiny est **dédiée à l'analyse de survie** avec des données censurées. Elle traite des données de type **time-to-event** (temps jusqu'à l'événement) où certaines observations peuvent être censurées.

---

## ✅ Modèles de Survie Supportés (Actuels)

L'application supporte **uniquement** les modèles conçus pour l'analyse de survie :

### 1. **Cox Proportional Hazards (cox)**
- Modèle semi-paramétrique standard pour l'analyse de survie
- Estime les hazard ratios (rapports de risque)
- Gère naturellement les données censurées
- **Usage** : Modèle de référence pour l'analyse de survie

### 2. **Random Survival Forest (rsf)**
- Extension des forêts aléatoires pour les données de survie
- Méthode non-paramétrique flexible
- Capture les interactions complexes entre variables
- **Usage** : Quand les relations non-linéaires sont suspectées

### 3. **Modèles de Cox Pénalisés**
   - **Cox Lasso (coxlasso)** : Pénalité L1 pour sélection de variables
   - **Cox ElasticNet (coxelasticnet)** : Combinaison L1/L2
   - **Cox Ridge (coxridge)** : Pénalité L2 pour régularisation
- **Usage** : Datasets avec beaucoup de variables (high-dimensional data)

---

## ❌ Modèles SUPPRIMÉS (Incompatibles avec la Survie)

Les modèles de **classification binaire** suivants ont été **supprimés** car ils ne gèrent **PAS** correctement les données de survie censurées :

### Modèles Retirés :
- ❌ **SVM** (Support Vector Machine)
- ❌ **XGBoost** (Gradient Boosting)
- ❌ **LightGBM**
- ❌ **K-Nearest Neighbors (KNN)**
- ❌ **Naive Bayes**
- ❌ **Random Forest** (version classification)

### Pourquoi ces modèles sont-ils inadaptés ?

1. **Ignorent la censure** : Traitent `status` comme une variable binaire simple (0/1), sans tenir compte de la censure
2. **Ignorent le temps** : Ne prennent pas en compte la colonne `time` (temps jusqu'à l'événement)
3. **Prédictions incorrectes** : Prédisent uniquement la probabilité d'événement, pas le risque temporel
4. **Pas de courbes de survie** : Incapables de générer des courbes de Kaplan-Meier ou des probabilités de survie dépendantes du temps

---

## 📌 Structure des Données Requise

L'application attend des données avec :
- **Colonne 1** : Identifiants des individus (ID)
- **Colonne 2** : `time` - Temps jusqu'à l'événement (numérique)
- **Colonne 3** : `status` - Statut de l'événement :
  - `1` = Événement observé
  - `0` = Censuré (événement non observé)
- **Colonnes suivantes** : Variables/features pour la modélisation

---

## 🔬 Tests Statistiques Adaptés à la Survie

L'application propose des tests appropriés pour l'analyse de survie :

### Tests Univariés :
- **Cox Univariate (Wald test)** : Teste l'association de chaque variable avec la survie
- **Log-rank Test** : Test non-paramétrique de différence de survie

### Tests Multivariés :
- **Cox Lasso, ElasticNet, Ridge** : Sélection de variables avec régularisation
- **Clustering + Cox ElasticNet** : Clustering hiérarchique + sélection bootstrap

---

## 🚫 Changements Apportés (Décembre 2024)

### Modifications dans `ui.R` :
- ✅ Suppression des modèles de classification binaire du sélecteur de modèles
- ✅ Adaptation des tests statistiques (Cox Wald, Log-rank au lieu de Wilcoxon/T-test)
- ✅ Mise à jour des affichages d'hyperparamètres pour les modèles de survie uniquement

### Modifications dans `server.R` :
- ✅ Suppression des outputs pour les modèles de classification
- ✅ Adaptation des observers et renderText pour les modèles de survie

### Modifications dans `global.R` :
- ✅ Ajout de warnings pour les modèles de classification dépréciés
- ✅ Documentation claire indiquant que seuls les modèles de survie sont supportés
- ⚠️ Code des modèles de classification conservé pour référence historique (commenté)

---

## 📖 Recommandations d'Usage

### Pour démarrer :
1. **Importer vos données** avec colonnes `time` et `status`
2. **Sélectionner les variables** (onglet SELECTDATA)
3. **Transformer les données** si nécessaire (imputation, standardisation)
4. **Effectuer des tests statistiques** (Cox Wald ou Log-rank pour sélection univariée)
5. **Construire un modèle de survie** :
   - Cox si relations linéaires
   - RSF si relations complexes/non-linéaires
   - Cox pénalisés si beaucoup de variables

### Pour comparer plusieurs modèles :
- Utiliser l'onglet **"Model Parameters"** > **"Test all models"**
- Comparer Cox, RSF, et les variantes pénalisées de Cox
- Évaluer avec C-index, Integrated Brier Score, et courbes de survie

---

## ⚙️ Notes Techniques

### Pour les développeurs :
- Le code des modèles de classification est **conservé** dans `global.R` pour référence
- Des **warnings** sont émis si quelqu'un tente d'utiliser ces modèles programmatiquement
- L'interface utilisateur (`ui.R`, `server.R`) **empêche** la sélection de ces modèles

### Pour restaurer les modèles de classification (NON RECOMMANDÉ) :
Si vous souhaitez restaurer les modèles de classification binaire (déconseillé), vous devrez :
1. Modifier `ui.R` pour ajouter les modèles au `radioButtons`
2. Restaurer les outputs dans `server.R`
3. Supprimer les warnings dans `global.R`

**⚠️ ATTENTION** : Ces modèles ne gèrent PAS correctement les données censurées et donneront des résultats incorrects pour l'analyse de survie.

---

## 📚 Ressources Supplémentaires

- **Cox Proportional Hazards** : [Introduction to Survival Analysis](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3059453/)
- **Random Survival Forest** : [Ishwaran et al. (2008)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2394262/)
- **Penalized Cox Models** : [Simon et al. (2011)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3307472/)

---

**Date de mise à jour** : 24 Décembre 2025
**Version** : 2.0 (Survie uniquement)
