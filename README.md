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

- `M-x org-nutrition-capture`
  - Prompts for a meal entry and appends it to the daily table inside `org-nutrition-target-file`.

- `M-x org-nutrition-food-or-recipe-capture`
  - Catalogs a food/recipe under `* Foods and recipes` in `org-nutrition-target-file`.
  - If the item already exists (case-insensitive exact heading match), it offers to use the saved macros to fill a meal entry (delegates to `org-nutrition-capture`).

If you enable the minor mode:

```elisp
(org-nutrition-mode 1)
;; Keybindings:
;; C-c n c -> org-nutrition-capture
;; C-c n i -> org-nutrition-insert-entry
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

## Customization

- `org-nutrition-target-file`: where entries are written.
- `org-nutrition-foods-and-recipes-heading`: name of the catalog heading (default: `Foods and recipes`).
- `org-nutrition-default-portion`: default portion when cataloging (default: `100g`).

## License

GPL-3.0-or-later. See [LICENSE](./LICENSE).
