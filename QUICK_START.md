# Guide de Démarrage Rapide

## 🚀 En 5 Minutes

### Étape 1 : Préparez vos données

Fichier CSV avec minimum 3 colonnes :

```csv
time,status,age,biomarker1,biomarker2
150,1,65,0.8,120
200,0,58,0.5,150
89,1,72,1.2,90
```

- **Colonne 1** : `time` (temps jusqu'à événement/censure)
- **Colonne 2** : `status` (0=censuré, 1=événement)
- **Colonnes 3+** : Variables prédictives

### Étape 2 : Lancez l'application

```r
# Dans R ou RStudio
shiny::runApp()
```

### Étape 3 : Importez vos données

1. Cliquez sur **"Browse"** sous "Learning dataset"
2. Sélectionnez votre fichier CSV
3. Ajustez séparateur/décimale si nécessaire
4. Vérifiez l'aperçu des données

### Étape 4 : Sélectionnez les variables

1. Choisissez un test :
   - **Log-rank** : Recommandé pour démarrer
   - **Cox Wald** : Si vous voulez les hazard ratios
2. Définissez seuil p-value : `0.05`
3. Cliquez **"Validate"**

### Étape 5 : Construisez le modèle

1. Choisissez un modèle :
   - **Cox** : Simple et interprétable
   - **Random Survival Forest** : Meilleure performance généralement
2. Cliquez **"Build Model"**
3. Attendez (quelques secondes à quelques minutes selon taille)

### Étape 6 : Consultez les résultats

Deux parties s'affichent :

#### 📊 Gauche : Survie Pure
- **Courbe Kaplan-Meier** : Séparation des groupes de risque
- **C-index** : Performance globale (> 0.7 = bon)
- **IBS** : Erreur de prédiction (plus bas = mieux)

#### 📈 Droite : Classification Temporelle
- **Courbe d'évolution** : AUC/Sens/Spec au cours du temps
- **Matrice de confusion** : Au temps médian
- **Tableau de métriques** : Détails par temps

---

## 📝 Exemple Minimal

### Données d'Exemple

Créez `example_data.csv` :

```csv
time,status,age,biomarker
150,1,65,0.8
200,0,58,0.5
89,1,72,1.2
300,1,60,0.3
180,0,55,0.9
250,1,68,1.1
120,1,70,1.3
350,0,52,0.4
```

### Workflow Complet

```
1. Import → example_data.csv
2. Test → Log-rank, p < 0.05
3. Modèle → Cox Proportional Hazards
4. Résultats → Interprétez !
```

### Résultats Attendus

- **C-index** : ~0.70-0.80 (avec plus de données)
- **AUC(t)** : Devrait augmenter avec le temps
- **Courbes KM** : Séparation visible des groupes

---

## ⚠️ Erreurs Courantes

### "No events detected"
- **Cause** : Colonne `status` ne contient que des 0
- **Solution** : Vérifiez vos données, il doit y avoir des 1

### "Time must be positive"
- **Cause** : Valeurs négatives ou 0 dans colonne `time`
- **Solution** : Toutes les valeurs doivent être > 0

### "Model building failed"
- **Cause** : Trop peu d'événements ou variables
- **Solution** : Minimum 20 événements et 3 variables

### "NA values detected"
- **Cause** : Valeurs manquantes dans les données
- **Solution** : Activez "mean by group" dans options de remplacement NA

---

## 📊 Interprétation Rapide

### C-index
- `< 0.6` : Mauvais
- `0.6-0.7` : Acceptable
- `0.7-0.8` : Bon ✅
- `> 0.8` : Excellent ✅✅

### AUC(t)
- `< 0.7` : Faible
- `0.7-0.8` : Acceptable ✅
- `> 0.8` : Bon ✅✅

### P-value (Kaplan-Meier)
- `< 0.05` : Groupes significativement différents ✅
- `> 0.05` : Pas de différence significative

---

## 🎯 Conseils Rapides

1. **Commencez simple** : Cox avec log-rank
2. **Vérifiez validation** : Uploadez un set de validation
3. **Comparez modèles** : Essayez Cox puis RSF
4. **Exportez résultats** : Utilisez boutons "Download"
5. **Consultez guide complet** : `USER_GUIDE.md` pour détails

---

## 📚 Ressources

- **Guide complet** : `USER_GUIDE.md`
- **Documentation technique** : `SURVIVAL_ADAPTATION_GUIDE.md`
- **Détails implémentation** : `IMPLEMENTATION_COMPLETE.md`

---

**Besoin d'aide ?** Consultez la section FAQ du guide complet !
