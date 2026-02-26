;;; org-nutrition.el --- Nutrition logging helpers for Org -*- lexical-binding: t; -*-

;; Author: Nícolas Morazotti (nicolas.morazotti@gmail.com)
;; Maintainer: Nícolas Morazotti (nicolas.morazotti@gmail.com)
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: outlines, org, nutrition, health
;; URL: https://github.com/morazotti/org-nutrition

;;; Commentary:

;; org-nutrition-mode: a minor mode to help log nutrition entries in Org files.
;; This file is an initial template (skeleton) library.

;;; Code:
(require 'org)
(require 'subr-x)
(require 'cl-lib)
(require 'json)
(require 'url)

(defgroup org-nutrition nil
  "Nutrition logging helpers for Org."
  :group 'org
  :prefix "org-nutrition-")

(defcustom org-nutrition-target-file (expand-file-name "nutrition.org" org-directory)
  "Default Org file where nutrition entries will be saved."
  :type 'file
  :group 'org-nutrition)

(defcustom org-nutrition-foods-heading "Foods"
  "Top-level heading where foods are cataloged in `org-nutrition-target-file'."
  :type 'string
  :group 'org-nutrition)

(defcustom org-nutrition-recipes-heading "Recipes"
  "Top-level heading where recipes are cataloged in `org-nutrition-target-file'."
  :type 'string
  :group 'org-nutrition)

(defcustom org-nutrition-category-alist
  '(("alcohol" . "🍺")
    ("beverage" . "🥤")
    ("bread" . "🍞")
    ("chicken" . "🐔")
    ("coffee" . "☕")
    ("dairy" . "🧀")
    ("dish" . "🍲")
    ("egg" . "🥚")
    ("fastfood" . "🍔")
    ("fish" . "🐟")
    ("fruits" . "🍓")
    ("grains" . "🍚")
    ("greens" . "🥬")
    ("legumes" . "🫘")
    ("nuts" . "🥜")
    ("pasta" . "🍝")
    ("seafood" . "🦐")
    ("snacks" . "🍕")
    ("sweets" . "🍰")
    ("vegetable" . "🥕")
    ("meat" . "🍖"))
  "Alist mapping category names to emoji strings."
  :type '(alist :key-type string :value-type string)
  :group 'org-nutrition)

(defcustom org-nutrition-default-portion "100g"
  "Default portion used when cataloging foods/recipes."
  :type 'string
  :group 'org-nutrition)
(defcustom org-nutrition-meal-choices '("Breakfast" "Lunch" "Dinner" "Snack")
  "Allowed meal names for completion."
  :type '(repeat string)
  :group 'org-nutrition)

(defcustom org-nutrition-default-meal "Lunch"
  "Default meal name when inserting entries."
  :type 'string
  :group 'org-nutrition)

(defcustom org-nutrition-mode-lighter " Nutri"
  "Mode line lighter for `org-nutrition-mode'."
  :type 'string
  :group 'org-nutrition)

(defcustom org-nutrition-date-format "%Y-%m-%d"
  "Date format used for capture."
  :type 'string
  :group 'org-nutrition)

(defcustom org-nutrition-table-columns
  '("Meal" "Food" "Weight(g)" "Calories" "Protein(g)" "Carbs(g)" "Fat(g)" "Notes")
  "Columns for the nutrition org-table."
  :type '(repeat string)
  :group 'org-nutrition)

(defcustom org-nutrition-daily-total-label "TOTAL"
  "Label used in the Food column for the daily totals row."
  :type 'string
  :group 'org-nutrition)

(defcustom org-nutrition-month-headline-format "%Y-%m"
  "Month heading format."
  :type 'string
  :group 'org-nutrition)

(defcustom org-nutrition-day-headline-format "%Y-%m-%d %a"
  "Day heading format."
  :type 'string
  :group 'org-nutrition)

(defcustom org-nutrition-use-api-default t
  "Default answer when asked to use API search."
  :type 'boolean
  :group 'org-nutrition)

(defcustom org-nutrition-align-table t
  "Whether to align tables after insertion."
  :type 'boolean
  :group 'org-nutrition)

(defun org-nutrition--ensure-org-buffer ()
  (unless (derived-mode-p 'org-mode)
    (user-error "org-nutrition-mode works only in Org buffers")))

(defun org-nutrition--string-empty-p (s)
  (or (null s) (string-empty-p s)))

(defun org-nutrition--normalize-meal (meal)
  (if (org-nutrition--string-empty-p meal) org-nutrition-default-meal meal))

(defun org-nutrition--read-meal ()
  (org-nutrition--normalize-meal
   (completing-read "Meal: " org-nutrition-meal-choices nil t nil nil org-nutrition-default-meal)))

(defun org-nutrition--read-field (prompt &optional initial)
  (read-string prompt initial))

(defun org-nutrition--read-number-as-string (prompt &optional initial)
  (let* ((s (org-nutrition--read-field prompt initial))
         (s (string-trim s)))
    s))

(defun org-nutrition--format-year ()
  (format-time-string "%Y"))

(defun org-nutrition--format-month ()
  (format-time-string org-nutrition-month-headline-format))

(defun org-nutrition--format-day ()
  (format-time-string org-nutrition-day-headline-format))

(defun org-nutrition--goto-or-create-child-heading (title level)
  "Go to (or create) a heading TITLE at LEVEL inside current subtree."
  (let ((re (format "^\\*\\{%d\\}[ \t]+%s\\([ \t]+.*\\)?$"
                    level (regexp-quote title)))
        (end (save-excursion (org-end-of-subtree t t) (point)))
        found)
    (save-excursion
      (forward-line 1)
      (when (re-search-forward re end t)
        (setq found (match-beginning 0))))
    (if found
        (goto-char found)
      (goto-char end)
      (unless (bolp) (insert "\n"))
      (insert (make-string level ?*) " " title "\n")
      (forward-line -1)
      (beginning-of-line))
    (org-show-entry)
    (org-show-subtree)
    (point)))

(defun org-nutrition--ensure-tree ()
  "Ensure Year/Month/Day headings exist and move point to the day heading."
  (let ((year (org-nutrition--format-year))
        (month (org-nutrition--format-month))
        (day (org-nutrition--format-day)))
    (goto-char (point-min))
    (unless (re-search-forward (format "^\\* %s\\([ \t]+.*\\)?$" (regexp-quote year)) nil t)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* " year "\n"))
    (goto-char (match-beginning 0))
    (org-nutrition--goto-or-create-child-heading month 2)
    (org-nutrition--goto-or-create-child-heading day 3)
    (point)))

(defun org-nutrition--table-header-row ()
  (concat "| " (mapconcat #'identity org-nutrition-table-columns " | ") " |"))

(defun org-nutrition--table-separator-row ()
  (concat "|-" (mapconcat (lambda (_c) "-") org-nutrition-table-columns "+-") "-|"))

(defun org-nutrition--table-row (cells)
  (concat "| " (mapconcat (lambda (c) (or c "")) cells " | ") " |"))

(defun org-nutrition--goto-headline-end ()
  (org-end-of-subtree t t)
  (unless (bolp) (insert "\n")))

(defun org-nutrition--day-subtree-limits ()
  (save-excursion
    (let* ((beg (org-entry-beginning-position))
           (end (save-excursion (org-end-of-subtree t t) (point))))
      (cons beg end))))

(defun org-nutrition--goto-table-in-day ()
  (org-nutrition--ensure-tree)
  (org-show-entry)
  (org-show-subtree)
  (let* ((limits (org-nutrition--day-subtree-limits))
         (beg (car limits))
         (end (cdr limits)))
    (goto-char beg)
    (unless (re-search-forward org-table-any-line-regexp end t)
      (error "No table found in day subtree"))
    (beginning-of-line)
    (org-table-begin)))

(defun org-nutrition--daily-total-row-p ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at-p org-table-dataline-regexp)
      (let ((cells (org-split-string (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position))
                                    "[ \t]*|[ \t]*")))
        (setq cells (cl-remove-if (lambda (s) (org-nutrition--string-empty-p (string-trim s))) cells))
        (let* ((meal (nth 0 cells)))
          (and (stringp meal)
               (string= (string-trim meal) org-nutrition-daily-total-label)))))))

(defun org-nutrition--table-last-data-row-position ()
  (save-excursion
    (let ((tbeg (org-table-begin))
          (tend (org-table-end)))
      (goto-char tend)
      (forward-line -1)
      (while (and (>= (point) tbeg)
                  (or (looking-at-p org-table-hline-regexp)
                      (org-nutrition--daily-total-row-p)))
        (forward-line -1))
      (point))))

(defun org-nutrition--table-total-row-position ()
  (save-excursion
    (let ((tbeg (org-table-begin))
          (tend (org-table-end))
          found)
      (goto-char tbeg)
      (while (and (not found)
                  (< (point) tend))
        (when (org-nutrition--daily-total-row-p)
          (setq found (point)))
        (forward-line 1))
      found)))

(defun org-nutrition--set-row-as-daily-total ()
  (let ((meal-col 1)
        (food-col 2))
    (org-table-goto-column meal-col)
    (org-table-blank-field)
    (insert org-nutrition-daily-total-label)
    (org-table-goto-column food-col)
    (org-table-blank-field)
    (insert "")))

(defun org-nutrition--maybe-number (s)
  (let ((s (string-trim (or s ""))))
    (if (org-nutrition--string-empty-p s) 0 (string-to-number s))))

(defun org-nutrition--format-number (n)
  (if (and (numberp n) (zerop n))
      ""
    (let ((s (format "%.1f" (float n))))
      (replace-regexp-in-string "\\.0\\'" "" s))))

(defun org-nutrition--compute-daily-totals ()
  (save-excursion
    (org-nutrition--goto-table-in-day)
    (let* ((tbeg (org-table-begin))
           (tend (org-table-end))
           (col-meal 1)
           (col-weight 3)
           (col-cal 4)
           (col-prot 5)
           (col-carbs 6)
           (col-fat 7)
           (sum-weight 0.0)
           (sum-cal 0.0)
           (sum-prot 0.0)
           (sum-carbs 0.0)
           (sum-fat 0.0))
      (goto-char tbeg)
      (forward-line 1)
      (while (< (point) tend)
        (cond
         ((looking-at-p org-table-hline-regexp) nil)
         ((org-nutrition--daily-total-row-p) nil)
         ((looking-at-p org-table-dataline-regexp)
          (let ((meal (org-table-get-field col-meal)))
            (unless (or (org-nutrition--string-empty-p (string-trim meal))
                        (string= (string-trim meal) org-nutrition-daily-total-label))
              (setq sum-weight (+ sum-weight (org-nutrition--maybe-number (org-table-get-field col-weight))))
              (setq sum-cal (+ sum-cal (org-nutrition--maybe-number (org-table-get-field col-cal))))
              (setq sum-prot (+ sum-prot (org-nutrition--maybe-number (org-table-get-field col-prot))))
              (setq sum-carbs (+ sum-carbs (org-nutrition--maybe-number (org-table-get-field col-carbs))))
              (setq sum-fat (+ sum-fat (org-nutrition--maybe-number (org-table-get-field col-fat))))))))
        (forward-line 1))
      (list (org-nutrition--format-number sum-weight)
            (org-nutrition--format-number sum-cal)
            (org-nutrition--format-number sum-prot)
            (org-nutrition--format-number sum-carbs)
            (org-nutrition--format-number sum-fat)))))

(defun org-nutrition--update-daily-total-row ()
  (save-excursion
    (org-nutrition--goto-table-in-day)
    (let ((pos (org-nutrition--table-total-row-position)))
      (when pos
        (goto-char pos)
        (let* ((totals (org-nutrition--compute-daily-totals))
               (col-weight 3)
               (col-cal 4)
               (col-prot 5)
               (col-carbs 6)
               (col-fat 7))
          (org-nutrition--set-row-as-daily-total)
          (org-table-goto-column col-weight)
          (org-table-blank-field)
          (insert (nth 0 totals))
          (org-table-goto-column col-cal)
          (org-table-blank-field)
          (insert (nth 1 totals))
          (org-table-goto-column col-prot)
          (org-table-blank-field)
          (insert (nth 2 totals))
          (org-table-goto-column col-carbs)
          (org-table-blank-field)
          (insert (nth 3 totals))
          (org-table-goto-column col-fat)
          (org-table-blank-field)
          (insert (nth 4 totals))
          (when org-nutrition-align-table
            (org-table-align)))))))

(defun org-nutrition--table-formulas ()
  "Return a TBLFM line placed at the end of the table."
  (let ((col-weight 3)
        (col-cal 4)
        (col-prot 5)
        (col-carbs 6)
        (col-fat 7))
    (format "#+TBLFM: @>$%d=vsum(@2..@-1);N::@>$%d=vsum(@2..@-1);N::@>$%d=vsum(@2..@-1);N::@>$%d=vsum(@2..@-1);N::@>$%d=vsum(@2..@-1);N"
            col-weight col-cal col-prot col-carbs col-fat)))

(defun org-nutrition--ensure-table-formulas ()
  "Ensure there's a #+TBLFM line at the end of the current table."
  (save-excursion
    (let* ((tend (org-table-end))
           (tblfm (org-nutrition--table-formulas)))
      (goto-char tend)
      (when (looking-at-p (regexp-quote "#+TBLFM:"))
        (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position)))))
      (goto-char tend)
      (let ((limit (save-excursion (forward-line 10) (point)))
            found)
        (while (and (not found) (< (point) limit))
          (cond
           ((looking-at-p "^#\\+TBLFM:") (setq found (point)))
           ((looking-at-p "^\\*+\\s-") (setq found :stop))
           (t (forward-line 1))))
        (when (numberp found)
          (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position))))))
      (goto-char (org-table-end))
      (unless (bolp) (insert "\n"))
      (insert tblfm "\n"))))

(defun org-nutrition--ensure-table-present ()
  (save-excursion
    (org-nutrition--ensure-tree)
    (org-nutrition--goto-headline-end)
    (let* ((limits (org-nutrition--day-subtree-limits))
           (beg (car limits))
           (end (cdr limits))
           found)
      (goto-char beg)
      (while (and (not found)
                  (re-search-forward org-table-any-line-regexp end t))
        (setq found t))
      (unless found
        (goto-char end)
        (unless (bolp) (insert "\n"))
        (insert (org-nutrition--table-header-row) "\n")
        (insert (org-nutrition--table-separator-row) "\n")
        (insert (org-nutrition--table-row (make-list (length org-nutrition-table-columns) "")) "\n")
        (insert (org-nutrition--table-row (make-list (length org-nutrition-table-columns) "")) "\n")
        (forward-line -1)
        (org-table-align)
        (org-nutrition--set-row-as-daily-total)
        (forward-line 1)
        (org-nutrition--ensure-table-formulas)
        (when org-nutrition-align-table
          (org-table-align))))))

(defun org-nutrition--ensure-daily-total-row ()
  (save-excursion
    (org-nutrition--ensure-table-present)
    (org-nutrition--goto-table-in-day)
    (let ((pos (org-nutrition--table-total-row-position)))
      (unless pos
        (goto-char (org-table-end))
        (forward-line -1)
        (while (looking-at-p org-table-hline-regexp)
          (forward-line -1))
        (end-of-line)
        (insert "\n" (org-nutrition--table-row (make-list (length org-nutrition-table-columns) "")))
        (setq pos (line-beginning-position)))
      (goto-char pos)
      (org-nutrition--set-row-as-daily-total)
      (org-nutrition--ensure-table-formulas)
      (when org-nutrition-align-table
        (org-table-align)))))

(defun org-nutrition--append-row-to-day-table (cells)
  (org-nutrition--ensure-table-present)
  (org-nutrition--ensure-daily-total-row)
  (org-nutrition--goto-table-in-day)
  (let ((row (org-nutrition--table-row cells)))
    (goto-char (org-nutrition--table-last-data-row-position))
    (end-of-line)
    (insert "\n" row)
    (org-nutrition--ensure-table-formulas)
    (when org-nutrition-align-table
      (org-table-align)))
  (org-table-recalculate t)
  (org-nutrition--update-daily-total-row)
  (org-table-recalculate t))

(defun org-nutrition--with-target-buffer (fn)
  (let* ((file org-nutrition-target-file)
         (buf (or (find-buffer-visiting file)
                  (find-file-noselect file)))
         (result nil))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (save-excursion
        (setq result (funcall fn)))
      (save-buffer))
    result))

(defun org-nutrition--execute-api-search (url name)
  "Helper to fetch JSON from Open Food Facts URL and extract a plist."
  (message "Fetching '%s' from API (%s)..." name (url-host (url-generic-parse-url url)))
  (let ((url-request-method "GET")
        (p nil))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$" nil t) ;; Skip HTTP headers
      (let* ((json-object-type 'alist)
             (data (ignore-errors (json-read)))
             (products (alist-get 'products data)))
        (setq p (and (> (length products) 0) (elt products 0))))
      (kill-buffer (current-buffer)))
    (when p
      (let ((nutri (alist-get 'nutriments p)))
        (list :name (alist-get 'product_name p)
              :calories (format "%s" (or (alist-get 'energy-kcal_100g nutri) 0))
              :protein (format "%s" (or (alist-get 'proteins_100g nutri) 0))
              :carbs (format "%s" (or (alist-get 'carbohydrates_100g nutri) 0))
              :fats (format "%s" (or (alist-get 'fat_100g nutri) 0))
              :portion "100g"
              :type "food")))))
              
(defun org-nutrition--search-food (name _weight)
  "Search Open Food Facts API for NAME. Returns a plist or nil.
Uses `url-retrieve-synchronously` to avoid external dependencies.
Tries the br.openfoodfacts.org endpoint first, falling back to world if needed."
  (let* ((br-url (format "https://br.openfoodfacts.org/cgi/search.pl?search_terms=%s&search_simple=1&action=process&json=1"
                         (url-hexify-string name)))
         (world-url (format "https://world.openfoodfacts.org/cgi/search.pl?search_terms=%s&search_simple=1&action=process&json=1"
                            (url-hexify-string name)))
         (result (org-nutrition--execute-api-search br-url name)))
    (unless result
      (setq result (org-nutrition--execute-api-search world-url name)))
    result))

(defun org-nutrition--catalog-entry-at-point ()
  "Return catalog entry plist at point if it looks like a food/recipe entry.

Expected properties (strings):
  PORTION, CALORIES, FATS, CARBS, PROTEIN, TYPE.

Returns a plist like:
  (:name ... :portion ... :calories ... :fats ... :carbs ... :protein ... :type ...)
All values are strings (possibly empty)."
  (when (org-at-heading-p)
    (list :name (org-get-heading t t t t)
          :portion (or (org-entry-get nil "PORTION") "")
          :calories (or (org-entry-get nil "CALORIES") "")
          :fats (or (org-entry-get nil "FATS") "")
          :carbs (or (org-entry-get nil "CARBS") "")
          :protein (or (org-entry-get nil "PROTEIN") "")
          :type (or (org-entry-get nil "TYPE") ""))))

(defun org-nutrition--goto-or-create-top-heading (title)
  "Go to (or create) a top-level heading TITLE in current buffer."
  (goto-char (point-min))
  (let ((re (format "^\\*\\s-+%s\\s-*$" (regexp-quote title))))
    (if (re-search-forward re nil t)
        (goto-char (match-beginning 0))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* " title "\n")
      (forward-line -1)
      (beginning-of-line)))
  (org-show-entry)
  (org-show-subtree)
  (point))

(defun org-nutrition--find-entry-in-subtree (parent name)
  "Find NAME under PARENT top-level heading. Returns point or nil."
  (let ((case-fold-search t)
        found)
    (org-nutrition--goto-or-create-top-heading parent)
    (let ((end (save-excursion (org-end-of-subtree t t) (point)))
          (re (format "^\\*\\*\\s-+%s\\s-*$" (regexp-quote name))))
      (save-excursion
        (forward-line 1)
        (when (re-search-forward re end t)
          (setq found (match-beginning 0)))))
    (when found (goto-char found))
    found))

(defun org-nutrition--find-food-or-recipe (name)
  "Find NAME in Foods or Recipes headings.
Returns point at the matching heading or nil."
  (or (org-nutrition--find-entry-in-subtree org-nutrition-foods-heading name)
      (org-nutrition--find-entry-in-subtree org-nutrition-recipes-heading name)))

(defun org-nutrition--read-category ()
  "Prompt user for a category from `org-nutrition-category-alist`."
  (let* ((choices (mapcar (lambda (x)
                            (cons (format "%s %s" (cdr x) (car x)) (cdr x)))
                          org-nutrition-category-alist))
         (choice (completing-read "Category: " choices nil t)))
    (cdr (assoc choice choices))))

(defun org-nutrition--ensure-food-or-recipe-entry (entry)
  "Ensure an ENTRY exists under Foods or Recipes depending on type.

ENTRY is a plist:
  (:name STRING :portion STRING :calories STRING :fats STRING :carbs STRING :protein STRING :type STRING)

Returns point at the entry heading."
  (let* ((name (plist-get entry :name))
         (portion (or (plist-get entry :portion) org-nutrition-default-portion))
         (calories (or (plist-get entry :calories) ""))
         (fats (or (plist-get entry :fats) ""))
         (carbs (or (plist-get entry :carbs) ""))
         (protein (or (plist-get entry :protein) ""))
         (type (or (plist-get entry :type) "food"))
         (target-heading (if (string= type "recipe")
                             org-nutrition-recipes-heading
                           org-nutrition-foods-heading)))
    (org-nutrition--goto-or-create-top-heading target-heading)
    (let ((pos (org-nutrition--find-entry-in-subtree target-heading name)))
      (unless pos
        ;; Create new entry at end of subtree.
        (goto-char (save-excursion (org-end-of-subtree t t) (point)))
        (unless (bolp) (insert "\n"))
        (insert "** " name "\n")
        (forward-line -1)
        (beginning-of-line))
      ;; Ensure properties.
      (org-set-property "PORTION" portion)
      (org-set-property "CALORIES" calories)
      (org-set-property "FATS" fats)
      (org-set-property "CARBS" carbs)
      (org-set-property "PROTEIN" protein)
      (org-set-property "TYPE" type)
      (point))))

(defun org-nutrition--catalog-entry-from-user (name)
  "Prompt user for catalog info for NAME and return plist entry."
  (let* ((type (completing-read "Type: " '("food" "recipe") nil t nil nil "food"))
         (portion (read-string "Portion (e.g. 100g): " org-nutrition-default-portion))
         (calories (org-nutrition--read-number-as-string (format "Calories (kcal) per %s: " portion)))
         (fats (org-nutrition--read-number-as-string (format "Fats (g) per %s: " portion)))
         (carbs (org-nutrition--read-number-as-string (format "Carbs (g) per %s: " portion)))
         (protein (org-nutrition--read-number-as-string (format "Protein (g) per %s: " portion))))
    (list :name name :portion portion
          :calories calories :fats fats :carbs carbs :protein protein
          :type type)))

(defun org-nutrition--catalog-entry-from-api (api-plist)
  "Convert API plist to internal entry plist.

Expected keys (all optional):
  :name, :portion, :calories, :fats, :carbs, :protein, :type

Returns a normalized entry plist, or nil if API plist is nil."
  (when (listp api-plist)
    (list :name (plist-get api-plist :name)
          :portion (or (plist-get api-plist :portion) org-nutrition-default-portion)
          :calories (format "%s" (or (plist-get api-plist :calories) ""))
          :fats (format "%s" (or (plist-get api-plist :fats) ""))
          :carbs (format "%s" (or (plist-get api-plist :carbs) ""))
          :protein (format "%s" (or (plist-get api-plist :protein) ""))
          :type (format "%s" (or (plist-get api-plist :type) "")))))

;;;###autoload
(defun org-nutrition--catalog-food-entry-from-user (name)
  "Prompt user for catalog info for food NAME and return plist entry."
  (let* ((portion (read-string "Portion (e.g. 100g): " org-nutrition-default-portion))
         (calories (org-nutrition--read-number-as-string (format "Calories (kcal) per %s: " portion)))
         (fats (org-nutrition--read-number-as-string (format "Fats (g) per %s: " portion)))
         (carbs (org-nutrition--read-number-as-string (format "Carbs (g) per %s: " portion)))
         (protein (org-nutrition--read-number-as-string (format "Protein (g) per %s: " portion))))
    (list :name name :portion portion
          :calories calories :fats fats :carbs carbs :protein protein
          :type "food")))

(defun org-nutrition--log-existing-entry (name existing)
  "Log an EXISTING entry for NAME into the daily table."
  (let ((org-nutrition-use-api-default nil))
    (cl-letf (((symbol-function #'org-nutrition--read-field)
               (lambda (prompt &optional initial)
                 (cond
                  ((string-match-p "\\`Food name:" prompt) name)
                  (t (read-string prompt initial)))))
              ((symbol-function #'org-nutrition--nutrition-from-api-or-manual)
               (lambda (_food weight)
                 (let* ((portion (or (plist-get existing :portion) org-nutrition-default-portion))
                        (portion-num (string-to-number (replace-regexp-in-string "[^0-9.]" "" portion)))
                        (w (string-to-number (string-trim (or weight "0"))))
                        (factor (if (> portion-num 0) (/ (float w) portion-num) 1.0))
                        (cal (org-nutrition--format-number
                              (* factor (string-to-number (or (plist-get existing :calories) "0")))))
                        (prot (org-nutrition--format-number
                               (* factor (string-to-number (or (plist-get existing :protein) "0")))))
                        (carbs (org-nutrition--format-number
                                (* factor (string-to-number (or (plist-get existing :carbs) "0")))))
                        (fat (org-nutrition--format-number
                              (* factor (string-to-number (or (plist-get existing :fats) "0"))))))
                   (list cal prot carbs fat "")))))
      (org-nutrition-capture))))

(defun org-nutrition-food-capture ()
  "Catalog a food and optionally use it to fill a meal entry."
  (interactive)
  (let ((name (string-trim (read-string "Food Name: "))))
    (when (org-nutrition--string-empty-p name)
      (user-error "Empty name"))
    (org-nutrition--with-target-buffer
     (lambda ()
       (let* ((found-pos (org-nutrition--find-entry-in-subtree org-nutrition-foods-heading name))
              (existing (when found-pos
                          (save-excursion
                            (goto-char found-pos)
                            (let ((entry (org-nutrition--catalog-entry-at-point)))
                              (when (listp entry)
                                (setq entry (plist-put entry :category (or (org-entry-get nil "CATEGORY") ""))))
                              entry)))))
         (cl-labels ((parse-float (s)
                       (string-to-number (replace-regexp-in-string "[^0-9.]" "" (or s "0"))))
                     (read-valid-num (prompt initial &optional max-val)
                       (let* ((map (make-sparse-keymap))
                              (check-and-exit
                               (lambda ()
                                 (interactive)
                                 (let* ((val (string-trim (minibuffer-contents)))
                                        (num nil))
                                   (cond
                                    ((string-empty-p val)
                                     (exit-minibuffer))
                                    ((not (string-match-p "^[0-9]*\\.?[0-9]+$" val))
                                     (minibuffer-message (propertize " [Invalid value! Must be numeric.]" 'face 'error)))
                                    ((and max-val (> max-val 0) (> (setq num (string-to-number val)) max-val))
                                     (minibuffer-message (propertize (format " [Value (%.2f) greater than portion (%.2f)!]" num max-val) 'face 'error)))
                                    (t
                                     (exit-minibuffer)))))))
                         (set-keymap-parent map minibuffer-local-map)
                         (define-key map (kbd "RET") check-and-exit)
                         (define-key map (kbd "C-j") check-and-exit)
                         (let ((minibuffer-local-map map))
                           (string-trim (read-string prompt initial)))))
                     (get-entry-values (entry-name &optional default-entry)
                       (let* ((def-portion (or (plist-get default-entry :portion) org-nutrition-default-portion))
                              (portion (read-string "Portion (e.g. 100g): " def-portion))
                              (limit (parse-float portion))
                              (def-cal (plist-get default-entry :calories))
                              (def-prot (plist-get default-entry :protein))
                              (def-carbs (plist-get default-entry :carbs))
                              (def-fat (plist-get default-entry :fats))
                              (cat (org-nutrition--read-category))
                              (cal (read-valid-num (format "Calories (kcal) per %s: " portion) def-cal))
                              (fat (read-valid-num (format "Fats (g) per %s: " portion) def-fat limit))
                              (carbs (read-valid-num (format "Carbs (g) per %s: " portion) def-carbs limit))
                              (prot (read-valid-num (format "Protein (g) per %s: " portion) def-prot limit)))
                         (list :name entry-name :portion portion
                               :calories cal :fats fat :carbs carbs :protein prot
                               :type "food" :category cat))))
           (cond
            (existing
             (when (y-or-n-p (format "'%s' already cataloged. Use it to fill a meal entry now? " name))
               (org-nutrition--log-existing-entry name existing)))
            (t
             (let* ((use-api (org-nutrition--ask-use-api-p))
                    (api (and use-api
                              (org-nutrition--search-food name org-nutrition-default-portion)))
                    (api-entry (org-nutrition--catalog-entry-from-api api))
                    (entry (if api-entry
                               (if (or (org-nutrition--string-empty-p (plist-get api-entry :calories))
                                       (org-nutrition--string-empty-p (plist-get api-entry :protein))
                                       (org-nutrition--string-empty-p (plist-get api-entry :carbs))
                                       (org-nutrition--string-empty-p (plist-get api-entry :fats))
                                       (y-or-n-p (format "API data received for '%s'. Edit/Verify? " name)))
                                   (get-entry-values name api-entry)
                                 api-entry)
                             (get-entry-values name)))
                    (entry-name (or (plist-get entry :name) name))
                    (category (or (plist-get entry :category)
                                  (org-nutrition--read-category))))
               (setq entry (plist-put entry :name entry-name))
               (setq entry (plist-put entry :type "food"))
               
               (org-nutrition--ensure-food-or-recipe-entry entry)
               
               (when (and (stringp category) (not (org-nutrition--string-empty-p category)))
                 (org-set-property "CATEGORY" category))
               (when (org-entry-get nil "TYPE")
                 (org-delete-property "TYPE"))
               (message "Cataloged: %s" entry-name)
               (when (y-or-n-p (format "Use '%s' to fill a meal entry now? " entry-name))
                 (org-nutrition--log-existing-entry entry-name entry)))))))))))

(defun org-nutrition-recipe-capture ()
  "Catalog a recipe and optionally use it to fill a meal entry."
  (interactive)
  (let ((name (string-trim (read-string "Recipe Name: "))))
    (when (org-nutrition--string-empty-p name)
      (user-error "Empty name"))
    (org-nutrition--with-target-buffer
     (lambda ()
       (let* ((found-pos (org-nutrition--find-entry-in-subtree org-nutrition-recipes-heading name))
              (existing (when found-pos
                          (save-excursion
                            (goto-char found-pos)
                            (let ((entry (org-nutrition--catalog-entry-at-point)))
                              entry)))))
         (cl-labels ((parse-float (s)
                       (string-to-number (replace-regexp-in-string "[^0-9.]" "" (or s "0"))))
                     (read-valid-num (prompt initial &optional max-val)
                       (let* ((map (make-sparse-keymap))
                              (check-and-exit
                               (lambda ()
                                 (interactive)
                                 (let* ((val (string-trim (minibuffer-contents)))
                                        (num nil))
                                   (cond
                                    ((string-empty-p val)
                                     (exit-minibuffer))
                                    ((not (string-match-p "^[0-9]*\\.?[0-9]+$" val))
                                     (minibuffer-message (propertize " [Invalid value! Must be numeric.]" 'face 'error)))
                                    ((and max-val (> max-val 0) (> (setq num (string-to-number val)) max-val))
                                     (minibuffer-message (propertize (format " [Value (%.2f) greater than portion (%.2f)!]" num max-val) 'face 'error)))
                                    (t
                                     (exit-minibuffer)))))))
                         (set-keymap-parent map minibuffer-local-map)
                         (define-key map (kbd "RET") check-and-exit)
                         (define-key map (kbd "C-j") check-and-exit)
                         (let ((minibuffer-local-map map))
                           (string-trim (read-string prompt initial)))))
                     (get-entry-values (entry-name &optional default-entry)
                       (let* ((def-portion (or (plist-get default-entry :portion) org-nutrition-default-portion))
                              (portion (read-string "Portion (e.g. 100g): " def-portion))
                              (limit (parse-float portion))
                              (def-cal (plist-get default-entry :calories))
                              (def-prot (plist-get default-entry :protein))
                              (def-carbs (plist-get default-entry :carbs))
                              (def-fat (plist-get default-entry :fats))
                              (cal (read-valid-num (format "Calories (kcal) per %s: " portion) def-cal))
                              (fat (read-valid-num (format "Fats (g) per %s: " portion) def-fat limit))
                              (carbs (read-valid-num (format "Carbs (g) per %s: " portion) def-carbs limit))
                              (prot (read-valid-num (format "Protein (g) per %s: " portion) def-prot limit)))
                         (list :name entry-name :portion portion
                               :calories cal :fats fat :carbs carbs :protein prot
                               :type "recipe"))))
           (cond
            (existing
             (when (y-or-n-p (format "'%s' already cataloged. Use it to fill a meal entry now? " name))
               (org-nutrition--log-existing-entry name existing)))
            (t
             (let* ((entry (get-entry-values name))
                    (entry-name (or (plist-get entry :name) name)))
               (setq entry (plist-put entry :name entry-name))
               (setq entry (plist-put entry :type "recipe"))
               
               (org-nutrition--ensure-food-or-recipe-entry entry)
               
               (when (org-entry-get nil "TYPE")
                 (org-delete-property "TYPE"))
               (message "Cataloged: %s" entry-name)
               (when (y-or-n-p (format "Use '%s' to fill a meal entry now? " entry-name))
                 (org-nutrition--log-existing-entry entry-name entry)))))))))))

(defun org-nutrition--ask-use-api-p ()
  (y-or-n-p (format "Fetch API data for this item? (%s) "
                    (if org-nutrition-use-api-default "Y/n" "y/N"))))

(defun org-nutrition--nutrition-from-api-or-manual (food weight)
  (let* ((catalog-entry (org-nutrition--with-target-buffer
                         (lambda ()
                           (let ((found-pos (org-nutrition--find-food-or-recipe food)))
                             (when found-pos
                               (let (entry)
                                 (save-excursion
                                   (goto-char found-pos)
                                   (setq entry (org-nutrition--catalog-entry-at-point)))
                                 entry))))))
         (api (unless catalog-entry
                (and (org-nutrition--ask-use-api-p)
                     (org-nutrition--search-food food weight))))
         (data-plist (or catalog-entry (and (listp api) api)))
         (cal (or (plist-get data-plist :calories) ""))
         (protein (or (plist-get data-plist :protein) ""))
         (carbs (or (plist-get data-plist :carbs) ""))
         (fat (or (plist-get data-plist :fats) (plist-get data-plist :fat) ""))
         (portion (or (plist-get data-plist :portion) org-nutrition-default-portion))
         (notes (or (plist-get data-plist :notes) ""))
         (is-manual nil))
         
    (if catalog-entry
        (message "Using cataloged item '%s'." food)
      (when api
        (message "Fetched '%s' from API." food)))
        
    (when (or (org-nutrition--string-empty-p (format "%s" cal))
              (org-nutrition--string-empty-p (format "%s" protein))
              (org-nutrition--string-empty-p (format "%s" carbs))
              (org-nutrition--string-empty-p (format "%s" fat)))
      (when (y-or-n-p "No complete data found (or skipped API). Insert manually? ")
        (setq is-manual t)
        (setq cal (org-nutrition--read-number-as-string "Calories (kcal): " (format "%s" cal)))
        (setq protein (org-nutrition--read-number-as-string "Protein (g): " (format "%s" protein)))
        (setq carbs (org-nutrition--read-number-as-string "Carbs (g): " (format "%s" carbs)))
        (setq fat (org-nutrition--read-number-as-string "Fat (g): " (format "%s" fat)))
        (setq notes (org-nutrition--read-field "Notes: " (format "%s" notes)))))
        
    (if is-manual
        (list (format "%s" cal)
              (format "%s" protein)
              (format "%s" carbs)
              (format "%s" fat)
              (format "%s" notes))
      (let* ((portion-num (string-to-number (replace-regexp-in-string "[^0-9.]" "" portion)))
             (w (string-to-number (string-trim (or weight "0"))))
             (factor (if (> portion-num 0) (/ (float w) portion-num) 1.0)))
        (list (org-nutrition--format-number (* factor (string-to-number (format "%s" cal))))
              (org-nutrition--format-number (* factor (string-to-number (format "%s" protein))))
              (org-nutrition--format-number (* factor (string-to-number (format "%s" carbs))))
              (org-nutrition--format-number (* factor (string-to-number (format "%s" fat))))
              (format "%s" notes))))))

(defun org-nutrition--read-entry ()
  (let* ((meal (org-nutrition--read-meal))
         (food (string-trim (org-nutrition--read-field "Food name: ")))
         (weight (org-nutrition--read-number-as-string "Weight (g): "))
         (nutri (org-nutrition--nutrition-from-api-or-manual food weight))
         (cal (nth 0 nutri))
         (protein (nth 1 nutri))
         (carbs (nth 2 nutri))
         (fat (nth 3 nutri))
         (notes (nth 4 nutri)))
    (list meal food weight cal protein carbs fat notes)))

;;;###autoload
(defun org-nutrition-capture ()
  "Capture a nutrition entry into the target Org file in a Year/Month/Day tree."
  (interactive)
  (let ((cells (org-nutrition--read-entry)))
    (org-nutrition--with-target-buffer
     (lambda ()
       (org-nutrition--append-row-to-day-table cells)))
    (message "Nutrition entry added")))

;;;###autoload
(defun org-nutrition-insert-entry ()
  "Insert a nutrition entry into an org-table in the current Org buffer in a Year/Month/Day tree."
  (interactive)
  (org-nutrition--ensure-org-buffer)
  (let ((cells (org-nutrition--read-entry)))
    (save-excursion
      (org-nutrition--append-row-to-day-table cells))
    (message "Nutrition entry added")))

(defvar org-nutrition-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c n c") #'org-nutrition-capture)
    (define-key map (kbd "C-c n i") #'org-nutrition-insert-entry)
    map)
  "Keymap for `org-nutrition-mode'.")

;;;###autoload
(define-minor-mode org-nutrition-mode
  "Minor mode to make nutrition logging in Org easier."
  :init-value nil
  :lighter org-nutrition-mode-lighter
  :keymap org-nutrition-mode-map)

;;;###autoload
(define-globalized-minor-mode global-org-nutrition-mode
  org-nutrition-mode
  (lambda ()
    (when (derived-mode-p 'org-mode)
      (org-nutrition-mode 1)))
  :group 'org-nutrition)

(provide 'org-nutrition)
;; org-nutrition.el ends here
