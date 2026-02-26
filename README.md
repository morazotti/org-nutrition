# org-nutrition

Small helpers to log meals/macros in an Org file, plus a simple catalog of foods/recipes.

## Installation

### Manual (`load-path`)

```elisp
(add-to-list 'load-path "~/repos/org-nutrition/")
(require 'org-nutrition)

;; Optional: default file
(setq org-nutrition-target-file (expand-file-name "nutrition.org" org-directory))
```

### `use-package`

```elisp
(use-package org-nutrition
  :load-path "~/repos/org-nutrition"
  :custom
  (org-nutrition-target-file (expand-file-name "nutrition.org" org-directory)))
```

## Commands

- `M-x org-nutrition-capture` (or `C-c n c`)
  - Prompts for a meal entry (fetches data from Open Food Facts API or manual input) and appends it to the daily table inside `org-nutrition-target-file`.
  - Automatically recalculates Daily Totals, Monthly Averages, and Annual Averages.

- `M-x org-nutrition-food-capture`
- `M-x org-nutrition-recipe-capture`
  - Catalogs a food/recipe under `* Foods and recipes` in `org-nutrition-target-file`.
  - The Interactive Recipe Builder will ask for ingredients one by one, scaling their macros perfectly based on the weight provided.
  - If the item already exists (case-insensitive exact heading match), it offers to use the saved macros to fill a meal entry.

- `M-x org-nutrition-bmr-capture` (or `C-c n b`)
  - Triggers an interactive prompt for Weight (kg), Height (cm), Age, Activity Level, and Goal (+/- 300kcal).
  - Computes the Harris-Benedict BMR, BMI, and your target calorie range.
  - Generates and appends a row to the `* Body Info` table.

If you enable the minor mode:

```elisp
(org-nutrition-mode 1)
;; Keybindings:
;; C-c n c -> org-nutrition-capture
;; C-c n i -> org-nutrition-insert-entry
;; C-c n b -> org-nutrition-bmr-capture
```

## `nutrition.org` format

`org-nutrition` writes into a single Org file (default: `~/org/nutrition.org`, depending on `org-directory`).

### Foods and recipes catalog

A top-level heading is used to store your catalog:

```org
* Foods and recipes
** Chicken breast
:PROPERTIES:
:PORTION: 100g
:CALORIES: 165
:FATS: 3.6
:CARBS: 0
:PROTEIN: 31
:TYPE: food
:END:

** Pancakes
:PROPERTIES:
:PORTION: 1 serving
:CALORIES: 350
:FATS: 10
:CARBS: 50
:PROTEIN: 12
:TYPE: recipe
:END:
```

### Daily log

Meal entries are stored under a Year/Month/Day tree, with an Org table.
`org-nutrition-capture` creates the structure/table automatically when needed.

#### Automated Summary Tables
On top of your daily log, `org-nutrition` dynamically tracks and aggregates your macros in real time at the Month and Year levels. 
Every time a new entry is added, an `#+NAME: monthly_summary_YYYY_MM` table is constructed under the current month containing all your daily totals and computing the average day. Simultaneously, an `#+NAME: annual_summary_YYYY` table computes the monthly averages directly under the year heading.

### Body Targets & BMR

Measurements and BMR goals are kept track of under the `* Body Info` heading using `org-nutrition-bmr-capture`:

```org
* Body Info
| Date       | Weight(kg) | Height(cm) | Age | BMI  | Activity | Goal             | BMR   | Target Cals |
|------------+------------+------------+-----+------+----------+------------------+-------+-------------|
| 2026-02-26 | 72.0       | 175.0      | 30  | 23.5 | 1.55     | Gain (+300 kcal) | 2604  | 2904        |
```

## Customization

- `org-nutrition-target-file`: where entries are written.
- `org-nutrition-foods-and-recipes-heading`: name of the catalog heading (default: `Foods and recipes`).
- `org-nutrition-default-portion`: default portion when cataloging (default: `100g`).

## License

GPL-3.0-or-later. See [LICENSE](./LICENSE).
