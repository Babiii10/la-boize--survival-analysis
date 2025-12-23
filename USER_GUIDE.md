# Guide d'Utilisation - Application d'Analyse de Survie Hybride

## 📋 Table des Matières

1. [Introduction](#introduction)
2. [Format des Données](#format-des-données)
3. [Workflow de l'Application](#workflow-de-lapplication)
4. [Les Deux Approches Analytiques](#les-deux-approches-analytiques)
5. [Interprétation des Métriques](#interprétation-des-métriques)
6. [Interprétation des Visualisations](#interprétation-des-visualisations)
7. [Exemples Pratiques](#exemples-pratiques)
8. [FAQ](#faq)

---

## Introduction

Cette application Shiny propose une approche **hybride** pour l'analyse de survie, combinant :

### 🔬 **Partie 1 : Analyse de Survie Pure**
- Métriques classiques : C-index, Integrated Brier Score
- Courbes de Kaplan-Meier par groupe de risque
- Statistiques de survie (médiane, quantiles)

### 📊 **Partie 2 : Classification Temporelle**
- Évaluation de la capacité prédictive au cours du temps
- AUC, Sensibilité, Spécificité à chaque point temporel
- Matrices de confusion à des temps spécifiques
- Gestion appropriée de la censure

### 🎯 **Objectif**
Évaluer simultanément la performance d'un modèle de survie sous deux angles complémentaires :
- **Survie** : Capacité à ordonner les patients par risque (concordance)
- **Classification** : Capacité à prédire qui aura l'événement à un temps donné

---

## Format des Données

### Structure Requise

Votre fichier de données doit contenir **au minimum 3 colonnes** :

| Colonne | Type | Description | Exemple |
|---------|------|-------------|---------|
| **Col 1 : time** | Numérique | Temps jusqu'à l'événement ou la censure (jours, mois, années) | 150, 200, 89 |
| **Col 2 : status** | Binaire (0/1) | 0 = Censuré (perdu de vue), 1 = Événement observé | 0, 1, 1 |
| **Col 3+ : features** | Numérique | Variables prédictives (biomarqueurs, âge, etc.) | 45, 0.8, 120 |

### Exemple de Fichier CSV

```csv
time,status,age,biomarker1,biomarker2,gene_expression
150,1,65,0.8,120,2.5
200,0,58,0.5,150,1.8
89,1,72,1.2,90,3.2
300,1,60,0.3,180,1.5
180,0,55,0.9,140,2.1
```

### ⚠️ Points Importants

1. **time** : Doit être strictement positif (> 0)
2. **status** : Uniquement 0 ou 1 (pas de valeurs manquantes)
3. **features** : Peuvent être continues ou catégorielles
4. **Noms de colonnes** : La 1ère ligne doit contenir les noms des variables
5. **Valeurs manquantes** : Gérer avant import ou utiliser les options de remplacement

---

## Workflow de l'Application

### Étape 1 : Import des Données

1. **Fichier d'apprentissage (Learning Set)** :
   - Cliquer sur "Browse" sous "Learning dataset"
   - Sélectionner votre fichier CSV/Excel
   - Ajuster les paramètres d'import (séparateur, décimale, feuille Excel)

2. **Fichier de validation (Validation Set)** *(optionnel)* :
   - Activer "Upload validation dataset"
   - Répéter le processus d'import

### Étape 2 : Sélection de Variables

**Tests disponibles** :

| Test | Type | Quand l'utiliser |
|------|------|------------------|
| **Log-rank** | Non-paramétrique | Distribution inconnue, variables continues/catégorielles |
| **Cox Wald** | Paramétrique | Relation linéaire attendue, obtenir hazard ratios |
| **Cox Lasso** | Régularisation L1 | Beaucoup de variables, sélection automatique |
| **Cox ElasticNet** | Régularisation L1+L2 | Variables corrélées, compromis stabilité/sélection |
| **Cox Ridge** | Régularisation L2 | Variables corrélées, pas de sélection |

**Paramètres** :
- **Seuil p-value** : Variables avec p < seuil sont retenues (ex: 0.05)
- **Correction multiple** : Benjamini-Hochberg recommandé pour éviter faux positifs

### Étape 3 : Construction du Modèle

**Modèles disponibles** :

| Modèle | Avantages | Inconvénients | Quand l'utiliser |
|--------|-----------|---------------|------------------|
| **Cox Proportional Hazards** | Interprétable, coefficients = hazard ratios | Assume proportionnalité des risques | Relations linéaires, interprétation nécessaire |
| **Random Survival Forest** | Non-paramétrique, capture non-linéarités | Boîte noire, lent sur gros datasets | Relations complexes, pas besoin d'interprétation |
| **Cox Lasso** | Sélection + prédiction | Peut être instable avec corrélations | Grand nombre de features, sélection automatique |
| **Cox ElasticNet** | Équilibre L1/L2 | Hyperparamètre alpha à choisir | Variables corrélées |
| **Cox Ridge** | Stable, toutes variables utilisées | Pas de sélection | Variables corrélées, toutes importantes |

**Paramètres RSF** :
- **Auto-tuning** : Laissez l'algorithme optimiser `mtry` et `nodesize`
- **Nombre d'arbres** : 500-1000 recommandé (plus = mieux mais plus lent)

### Étape 4 : Visualisation des Résultats

L'application affiche automatiquement :
- **Partie Survie Pure** (gauche)
- **Partie Classification Temporelle** (droite)
- Tableaux de métriques et statistiques

---

## Les Deux Approches Analytiques

### 🔬 **Approche 1 : Analyse de Survie Pure**

#### Objectif
Évaluer la capacité du modèle à **ordonner correctement** les patients par risque.

#### Métriques Principales

##### 1. **C-index (Concordance Index)**
- **Définition** : Probabilité qu'un patient avec événement plus précoce ait un score de risque plus élevé
- **Interprétation** :
  - 0.5 = Aléatoire (pire)
  - 0.7 = Acceptable
  - 0.8 = Bon
  - 0.9+ = Excellent
- **Exemple** : C-index = 0.75 signifie que dans 75% des paires de patients, le modèle ordonne correctement

##### 2. **Integrated Brier Score (IBS)**
- **Définition** : Erreur quadratique moyenne de prédiction de survie
- **Interprétation** :
  - 0 = Parfait
  - Plus bas = Meilleur
  - Comparaison relative entre modèles
- **Exemple** : IBS = 0.15 est meilleur que IBS = 0.25

##### 3. **Médiane de Survie**
- **Définition** : Temps auquel 50% des patients ont eu l'événement
- **Interprétation** : Donnée cliniquement interprétable
- **Exemple** : Médiane = 200 jours → 50% des patients ont survécu au-delà de 200 jours

#### Visualisations

##### **Courbes de Kaplan-Meier**
- Montrent la probabilité de survie au cours du temps
- **Groupes de risque** :
  - **Haut risque** : Score ≥ médiane (courbe en bas)
  - **Bas risque** : Score < médiane (courbe en haut)
- **P-value** : Test log-rank comparant les deux groupes
- **Interprétation** : Séparation des courbes = bonne discrimination

##### **Tableau de Quantiles de Risque**
- Résumé statistique des scores de risque
- Min, Q1, Médiane, Q3, Max, Moyenne, SD
- Permet de comprendre la distribution des scores

---

### 📊 **Approche 2 : Classification Temporelle**

#### Objectif
Évaluer la capacité du modèle à **prédire qui aura l'événement à un temps donné**.

#### Principe

À chaque temps **t** :
1. **Score de risque** : 1 - S(t) = Probabilité d'avoir eu l'événement avant t
2. **Classe réelle** :
   - **1 (événement)** : Patient a eu l'événement avant ou à t
   - **0 (événement-free)** : Patient a survécu au-delà de t
3. **Filtrage** : Exclusion des patients censurés avant t (statut inconnu)
4. **Calcul métriques** : AUC, Sensibilité, Spécificité avec seuil optimal (Youden)

#### Métriques Principales

##### 1. **AUC(t) - Aire sous la Courbe ROC**
- **Méthode** : timeROC avec gestion de la censure (IPCW)
- **Interprétation** :
  - 0.5 = Aléatoire
  - 0.7 = Acceptable
  - 0.8 = Bon
  - 0.9+ = Excellent
- **Exemple** : AUC(100 jours) = 0.75 → Le modèle discrimine correctement qui aura l'événement à 100 jours dans 75% des cas

##### 2. **Sensibilité(t) - Taux de Vrais Positifs**
- **Définition** : Parmi ceux qui ont eu l'événement avant t, combien ont été détectés ?
- **Formule** : TP / (TP + FN)
- **Interprétation** :
  - 0.9 = 90% des événements détectés
  - Important pour ne pas manquer des cas à risque
- **Exemple** : Sensibilité(100) = 0.85 → 85% des patients ayant eu l'événement avant 100 jours sont correctement identifiés

##### 3. **Spécificité(t) - Taux de Vrais Négatifs**
- **Définition** : Parmi ceux qui n'ont pas eu l'événement avant t, combien ont été détectés ?
- **Formule** : TN / (TN + FP)
- **Interprétation** :
  - 0.9 = 90% des non-événements détectés
  - Important pour ne pas alarmer inutilement
- **Exemple** : Spécificité(100) = 0.80 → 80% des patients sans événement avant 100 jours sont correctement identifiés

##### 4. **Seuil de Youden**
- **Définition** : Seuil de probabilité optimisant Sensibilité + Spécificité
- **Utilité** : Décision clinique (traiter si probabilité > seuil)
- **Exemple** : Seuil(100) = 0.35 → Traiter si P(événement avant 100 jours) > 35%

##### 5. **N_patients et N_excluded**
- **N_patients** : Nombre de patients inclus dans le calcul (statut connu)
- **N_excluded** : Nombre de patients exclus (censurés avant t)
- **Interprétation** : N_excluded augmente avec le temps (normal)

#### Visualisations

##### **Courbe d'Évolution Temporelle**
- **Axe X** : Temps
- **Axe Y** : Valeur de la métrique (0 à 1)
- **3 courbes** :
  - 🟡 **AUC** : Discrimination globale
  - 🔵 **Sensibilité** : Détection des événements
  - 🔴 **Spécificité** : Détection des non-événements

**Patterns typiques** :
- **AUC croissante** : Discrimination s'améliore avec le temps
- **AUC décroissante** : Modèle moins performant à long terme
- **AUC stable** : Performance constante

##### **Matrice de Confusion (au temps médian)**
- Tableau 2×2 comparant prédictions vs réalité
- **Lignes** : Prédictions (0 = événement-free, 1 = événement)
- **Colonnes** : Réalité (Event, Event-free)

Exemple au temps t = 150 jours :
```
              Réalité
           Event | Event-free
Prédit 1     45  |    12        → 45 vrais positifs, 12 faux positifs
Prédit 0      8  |    35        → 8 faux négatifs, 35 vrais négatifs
```

**Calculs** :
- Sensibilité = 45/(45+8) = 0.85
- Spécificité = 35/(35+12) = 0.74
- Précision = (45+35)/100 = 0.80

##### **Tableau de Métriques Temporelles**
- Une ligne par point temporel
- Colonnes : time, N_patients, N_excluded, AUC, Sensitivity, Specificity, Threshold
- Permet de choisir le temps optimal selon le contexte clinique

---

## Interprétation des Métriques

### Comparaison C-index vs AUC(t)

| Aspect | C-index | AUC(t) |
|--------|---------|--------|
| **Question posée** | "Le modèle ordonne-t-il bien les patients ?" | "Le modèle prédit-il bien l'événement au temps t ?" |
| **Données utilisées** | Toutes les paires de patients | Patients avec statut connu à t |
| **Temps** | Global (tous temps confondus) | Spécifique à un temps t |
| **Censure** | Paires concordantes seulement | Exclusion censurés avant t + IPCW |
| **Utilité** | Performance globale du modèle | Décision à un temps précis |

### Quand utiliser quelle métrique ?

#### Utiliser C-index si :
- Vous voulez une **métrique unique** résumant la performance
- Vous comparez **plusieurs modèles** globalement
- Vous publiez dans une **revue scientifique** (métrique standard)

#### Utiliser AUC(t) si :
- Vous devez prendre une **décision à un temps précis** (ex: 6 mois, 1 an)
- Vous voulez comprendre **comment évolue** la performance
- Vous développez un **outil de décision clinique** (seuil de Youden)

#### Utiliser les deux !
L'approche **hybride** de cette application permet d'avoir une vue complète :
- **C-index** : Performance globale
- **AUC(t)** : Performance à des temps cliniquement pertinents

---

## Interprétation des Visualisations

### 1. Courbes de Kaplan-Meier

#### Que regarder ?

✅ **Séparation des courbes**
- Courbes bien séparées = bonne discrimination
- Courbes superposées = mauvaise discrimination

✅ **P-value du log-rank**
- p < 0.05 = différence significative entre groupes
- p > 0.05 = pas de différence significative

✅ **Forme des courbes**
- Descente rapide au début = événements précoces
- Plateau = survie à long terme

#### Exemple d'interprétation

```
Graphique : Kaplan-Meier - Learning Set
- Groupe "Bas risque" : Survie à 200 jours = 85%
- Groupe "Haut risque" : Survie à 200 jours = 40%
- P-value = 0.001
```

**Interprétation** :
- Le modèle discrimine très bien les groupes de risque (p < 0.001)
- À 200 jours, 85% des patients bas risque sont encore vivants vs seulement 40% des patients haut risque
- Différence cliniquement et statistiquement significative

### 2. Courbe d'Évolution Temporelle (AUC/Sens/Spec)

#### Que regarder ?

✅ **Tendance de l'AUC**
- **↗ Croissante** : Le modèle s'améliore avec le temps (bon signe)
- **↘ Décroissante** : Le modèle se dégrade avec le temps (attention)
- **→ Stable** : Performance constante

✅ **Trade-off Sensibilité-Spécificité**
- Sensibilité haute + Spécificité basse = Beaucoup de faux positifs (sur-diagnostic)
- Sensibilité basse + Spécificité haute = Beaucoup de faux négatifs (sous-diagnostic)
- **Idéal** : Les deux > 0.75

✅ **Évolution au cours du temps**
- Métriques stables = modèle robuste
- Métriques volatiles = incertitude, peu de patients à risque

#### Exemple d'interprétation

```
Graphique : Time-Dependent Classification Metrics - Learning Set

Temps 50j  : AUC=0.70, Sens=0.65, Spec=0.72
Temps 100j : AUC=0.75, Sens=0.70, Spec=0.78
Temps 150j : AUC=0.80, Sens=0.75, Spec=0.82
Temps 200j : AUC=0.83, Sens=0.78, Spec=0.85
```

**Interprétation** :
- AUC croissante (0.70 → 0.83) : Le modèle discrimine de mieux en mieux avec le temps
- À 200 jours : 78% des événements détectés (sens), 85% des non-événements détectés (spec)
- Performance excellente à long terme (AUC > 0.80)
- Recommandation : Utiliser le modèle pour prédictions à 150-200 jours

### 3. Matrice de Confusion

#### Que regarder ?

✅ **Diagonale**
- Vrais Positifs (haut-gauche) + Vrais Négatifs (bas-droite)
- Plus la diagonale est élevée, mieux c'est

✅ **Faux Positifs (haut-droite)**
- Patients prédits "événement" mais qui n'ont pas eu l'événement
- **Impact clinique** : Traitement inutile, anxiété

✅ **Faux Négatifs (bas-gauche)**
- Patients prédits "événement-free" mais qui ont eu l'événement
- **Impact clinique** : Événement manqué, absence de traitement

#### Exemple d'interprétation

```
Matrice de Confusion au temps t=150 jours

             Event | Event-free
Prédit 1       48  |    15
Prédit 0       12  |    65
```

**Calculs** :
- Sensibilité = 48/(48+12) = 80% ✅ Bon
- Spécificité = 65/(65+15) = 81% ✅ Bon
- Faux positifs = 15 → 15 patients traités inutilement
- Faux négatifs = 12 → 12 événements manqués

**Interprétation** :
- Équilibre correct entre sensibilité et spécificité
- Si coût du traitement est bas : acceptable
- Si coût du traitement est élevé : considérer augmenter le seuil pour réduire les faux positifs

---

## Exemples Pratiques

### Exemple 1 : Cancer du Poumon

**Contexte** :
- 200 patients avec cancer du poumon
- Variables : âge, stade, biomarqueurs
- Événement : décès
- Objectif : Prédire la survie à 1 an

**Résultats** :

**Survie Pure** :
- C-index = 0.78 (bon)
- IBS = 0.18 (acceptable)
- Médiane survie groupe haut risque = 180 jours
- Médiane survie groupe bas risque = 450 jours
- P-value log-rank < 0.001

**Classification Temporelle** :
```
Temps 365 jours (1 an) :
- AUC = 0.82
- Sensibilité = 0.77
- Spécificité = 0.83
- Seuil Youden = 0.42
- N_patients = 165 (35 exclus car censurés avant 1 an)
```

**Interprétation Clinique** :
- Le modèle discrimine bien les patients (C-index = 0.78)
- À 1 an, le modèle prédit correctement le décès dans 82% des cas (AUC)
- Recommandation : Traiter agressivement si P(décès à 1 an) > 42%
- 77% des décès à 1 an sont détectés
- 83% des survivants à 1 an sont correctement identifiés

### Exemple 2 : Récidive Tumorale

**Contexte** :
- 150 patients après chirurgie
- Variables : grade tumoral, âge, marges
- Événement : récidive
- Objectif : Surveillance post-opératoire

**Résultats** :

**Survie Pure** :
- C-index = 0.72 (acceptable)
- Courbes KM : séparation claire (p = 0.005)

**Classification Temporelle** :
```
Temps   | AUC  | Sens | Spec | Seuil
6 mois  | 0.68 | 0.62 | 0.71 | 0.25
12 mois | 0.73 | 0.70 | 0.75 | 0.35
18 mois | 0.78 | 0.75 | 0.80 | 0.45
24 mois | 0.81 | 0.78 | 0.82 | 0.50
```

**Interprétation Clinique** :
- Performance s'améliore avec le temps (AUC : 0.68 → 0.81)
- À 6 mois : modèle peu performant (AUC = 0.68), ne pas utiliser pour décisions
- À 24 mois : modèle très bon (AUC = 0.81)
- **Recommandation de surveillance** :
  - Si P(récidive à 24 mois) > 50% : Surveillance intensive (imagerie tous les 3 mois)
  - Si P(récidive à 24 mois) < 50% : Surveillance standard (imagerie tous les 6 mois)
- 78% des récidives à 24 mois sont détectées avec ce seuil

---

## FAQ

### Q1 : Quelle taille d'échantillon minimum ?

**Réponse** :
- **Minimum absolu** : 50 patients avec au moins 20 événements
- **Recommandé** : 100+ patients avec 30+ événements
- **Idéal** : 200+ patients avec 50+ événements
- **Règle générale** : Au moins 10 événements par variable dans le modèle

### Q2 : Que faire si peu d'événements ?

**Réponse** :
- Utiliser Cox Lasso ou Ridge pour régulariser
- Limiter le nombre de variables (sélection stricte, p < 0.01)
- Augmenter la durée de suivi si possible
- Considérer combiner avec d'autres cohortes

### Q3 : Mon AUC(t) diminue avec le temps, pourquoi ?

**Causes possibles** :
1. **Peu de patients à risque** aux temps tardifs → Incertitude
2. **Variables prédictives à court terme** mais pas à long terme
3. **Censure informative** : Les patients censurés sont différents

**Solutions** :
- Vérifier N_patients et N_excluded dans le tableau
- Si N_patients < 30 à un temps : Ne pas interpréter ce point
- Considérer variables additionnelles pour long terme

### Q4 : Différence entre seuil de Youden et seuil clinique ?

**Réponse** :
- **Youden** : Maximise Sensibilité + Spécificité (mathématique)
- **Clinique** : Dépend du contexte (coût traitement, gravité événement)

**Exemple** :
- Youden = 0.40
- Mais si traitement très cher : Augmenter seuil à 0.60 (réduire faux positifs)
- Mais si événement grave : Diminuer seuil à 0.30 (réduire faux négatifs)

### Q5 : Comment comparer deux modèles ?

**Sur Survie Pure** :
- C-index plus élevé = meilleur
- IBS plus bas = meilleur
- Séparation KM plus large = meilleur (visuellement)

**Sur Classification Temporelle** :
- AUC(t) plus élevé = meilleur à chaque temps
- Comparer les courbes d'évolution
- Si un modèle meilleur à court terme, l'autre à long terme : Choisir selon objectif

**Test statistique** :
- Utilisez bootstrap ou cross-validation pour comparer les C-index
- Test de DeLong pour comparer les AUC

### Q6 : Que signifie "N_excluded" élevé ?

**Réponse** :
- **Normal** aux temps tardifs : Beaucoup de patients censurés avant
- **Problématique** aux temps précoces : Problème de données ou censure précoce excessive

**Exemple** :
```
Temps 50j : N_excluded = 5   → OK
Temps 200j : N_excluded = 40  → Normal (censure cumulative)
```

Si N_excluded > 50% des patients : Attention à l'interprétation

### Q7 : Mon modèle RSF est lent, que faire ?

**Optimisations** :
- Réduire nombre d'arbres (500 au lieu de 1000)
- Désactiver l'auto-tuning
- Sélectionner moins de variables
- Utiliser Cox Lasso à la place (plus rapide)

### Q8 : Puis-je utiliser des variables catégorielles ?

**Réponse** :
- **Oui** pour Cox et RSF
- **Non** pour Cox Lasso/ElasticNet/Ridge (encoder en dummy variables d'abord)

**Encodage recommandé** :
```
Variable "Stade" avec 3 niveaux (I, II, III)
→ Créer 2 variables binaires :
  - Stade_II (0/1)
  - Stade_III (0/1)
  - Référence : Stade I
```

### Q9 : Comment interpréter une sensibilité haute mais spécificité basse ?

**Interprétation** :
- Le modèle détecte bien les événements (peu de faux négatifs)
- Mais génère beaucoup de faux positifs (alarmes inutiles)

**Causes** :
- Seuil trop bas
- Modèle sur-prudent

**Solutions** :
- Augmenter le seuil de classification
- Accepter ce trade-off si événement grave (mieux prévenir que manquer)

### Q10 : Différence entre Learning et Validation ?

**Réponse** :
- **Learning** : Données utilisées pour entraîner le modèle
  - Métriques optimistes (sur-ajustement possible)

- **Validation** : Données indépendantes jamais vues
  - Métriques réalistes (vraie performance)

**Recommandation** :
- Toujours rapporter les métriques de **validation**
- Si validation < learning de beaucoup : Sur-ajustement
- Si validation ≈ learning : Modèle robuste ✅

---

## Glossaire des Termes

| Terme | Définition |
|-------|------------|
| **Censure** | Patient dont on ne connaît pas le temps exact d'événement (perdu de vue, fin d'étude) |
| **Censure à droite** | On sait que l'événement n'a pas eu lieu avant un temps T, mais on ne sait pas quand il aura lieu |
| **Hazard Ratio (HR)** | Rapport des risques instantanés entre deux groupes |
| **IPCW** | Inverse Probability of Censoring Weighting - méthode pour gérer la censure |
| **Concordance** | Paire de patients correctement ordonnée par le modèle |
| **Score de risque** | Valeur numérique indiquant le risque d'événement (plus haut = plus risqué) |
| **S(t)** | Fonction de survie : probabilité d'être encore sans événement au temps t |
| **1-S(t)** | Fonction de risque cumulé : probabilité d'avoir eu l'événement avant le temps t |

---

## Références Scientifiques

1. **timeROC Package** :
   - Heagerty, P. J., Lumley, T., & Pepe, M. S. (2000). "Time-dependent ROC curves for censored survival data and a diagnostic marker." *Biometrics*, 56(2), 337-344.

2. **C-index** :
   - Harrell, F. E., et al. (1996). "Multivariable prognostic models: issues in developing models, evaluating assumptions and adequacy." *Statistics in Medicine*, 15(4), 361-387.

3. **Brier Score** :
   - Graf, E., et al. (1999). "Assessment and comparison of prognostic classification schemes for survival data." *Statistics in Medicine*, 18(17‐18), 2529-2545.

4. **Random Survival Forest** :
   - Ishwaran, H., et al. (2008). "Random survival forests." *The Annals of Applied Statistics*, 2(3), 841-860.

---

## Support et Contact

Pour toute question ou problème :

1. **Documentation technique** : Voir `SURVIVAL_ADAPTATION_GUIDE.md`
2. **Implémentation** : Voir `IMPLEMENTATION_COMPLETE.md`
3. **Issues GitHub** : [Lien vers le repo]
4. **Email** : [Votre email de support]

---

**Version** : 1.0
**Dernière mise à jour** : 2025-12-23
**Auteurs** : [Vos noms]
