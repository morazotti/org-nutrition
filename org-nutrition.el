;;; org-nutrition.el --- Nutrition logging helpers for Org -*- lexical-binding: t; -*-

;; Author: Nícolas Morazotti (nicolas.morazotti@gmail.com)
;; Maintainer: Nícolas Morazotti (nicolas.morazotti@gmail.com)
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: outlines, org, nutrition, health
;; URL: https://github.com/morazotti/org-nutrition

;;; Commentary:

;; org-nutrition-mode: a minor mode to help log nutrition entries in Org files.

;;; Code:
(require 'org)
(require 'subr-x)
(require 'cl-lib)
(require 'json)
(require 'url-util) ; for url-hexify-string

;;; ─── Customization ──────────────────────────────────────────────────────────

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

(defcustom org-nutrition-monthly-table-columns
  '("Date" "Calories" "Protein(g)" "Carbs(g)" "Fat(g)" "Weight(g)")
  "Columns for the monthly summary org-table."
  :type '(repeat string)
  :group 'org-nutrition)

(defcustom org-nutrition-annual-table-columns
  '("Month" "Calories" "Protein(g)" "Carbs(g)" "Fat(g)" "Weight(g)")
  "Columns for the annual summary org-table."
  :type '(repeat string)
  :group 'org-nutrition)

(defcustom org-nutrition-body-targets-heading "Body Info"
  "Heading name for storing the BMR/targets."
  :type 'string
  :group 'org-nutrition)

(defcustom org-nutrition-bmr-table-columns
  '("Date" "Weight(kg)" "Height(cm)" "Age" "BMI" "Activity" "Goal" "BMR" "Target Cals")
  "Columns for the body targets/BMR org-table."
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

;;; ─── Utility ────────────────────────────────────────────────────────────────

(defun org-nutrition--ensure-org-buffer ()
  (unless (derived-mode-p 'org-mode)
    (user-error "org-nutrition-mode works only in Org buffers")))

(defun org-nutrition--string-empty-p (s)
  (or (null s) (string-empty-p s)))

(defun org-nutrition--maybe-number (s)
  (let ((s (string-trim (or s ""))))
    (if (org-nutrition--string-empty-p s) 0 (string-to-number s))))

(defun org-nutrition--format-number (n)
  (if (and (numberp n) (zerop n))
      ""
    (let ((s (format "%.1f" (float n))))
      (replace-regexp-in-string "\\.0\\'" "" s))))

;;; ─── User Input ─────────────────────────────────────────────────────────────

(defun org-nutrition--normalize-meal (meal)
  (if (org-nutrition--string-empty-p meal) org-nutrition-default-meal meal))

(defun org-nutrition--read-meal ()
  (org-nutrition--normalize-meal
   (completing-read "Meal: " org-nutrition-meal-choices nil t nil nil org-nutrition-default-meal)))

(defun org-nutrition--read-field (prompt &optional initial)
  (read-string prompt initial))

(defun org-nutrition--read-number-as-string (prompt &optional initial)
  (string-trim (read-string prompt initial)))

(defun org-nutrition--read-category ()
  "Prompt user for a category from `org-nutrition-category-alist'."
  (let* ((choices (mapcar (lambda (x)
                            (cons (format "%s %s" (cdr x) (car x)) (cdr x)))
                          org-nutrition-category-alist))
         (choice (completing-read "Category: " choices nil t)))
    (cdr (assoc choice choices))))

(defun org-nutrition--ask-use-api-p ()
  (y-or-n-p (format "Fetch API data for this item? (%s) "
                    (if org-nutrition-use-api-default "Y/n" "y/N"))))

;;; ─── Date Helpers ───────────────────────────────────────────────────────────

(defun org-nutrition--format-year  () (format-time-string "%Y"))
(defun org-nutrition--format-month () (format-time-string org-nutrition-month-headline-format))
(defun org-nutrition--format-day   () (format-time-string org-nutrition-day-headline-format))

;;; ─── Heading Navigation ─────────────────────────────────────────────────────

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

(defun org-nutrition--ensure-tree ()
  "Ensure Year/Month/Day headings exist and move point to the day heading."
  (let ((year  (org-nutrition--format-year))
        (month (org-nutrition--format-month))
        (day   (org-nutrition--format-day)))
    (goto-char (point-min))
    (unless (re-search-forward (format "^\\* %s\\([ \t]+.*\\)?$" (regexp-quote year)) nil t)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* " year "\n"))
    (goto-char (match-beginning 0))
    (org-nutrition--goto-or-create-child-heading month 2)
    (org-nutrition--goto-or-create-child-heading day 3)
    (point)))

;;; ─── Org-table Helpers ──────────────────────────────────────────────────────

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
    (cons (org-entry-beginning-position)
          (save-excursion (org-end-of-subtree t t) (point)))))

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
        (and (stringp (nth 0 cells))
             (string= (string-trim (nth 0 cells)) org-nutrition-daily-total-label))))))

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
      (while (and (not found) (< (point) tend))
        (when (org-nutrition--daily-total-row-p)
          (setq found (point)))
        (forward-line 1))
      found)))

(defun org-nutrition--set-row-as-daily-total ()
  (org-table-goto-column 1) (org-table-blank-field) (insert org-nutrition-daily-total-label)
  (org-table-goto-column 2) (org-table-blank-field))

(defun org-nutrition--table-formulas ()
  "Return a TBLFM line for summing nutrient columns."
  (let ((cols '(3 4 5 6 7)))
    (concat "#+TBLFM: "
            (mapconcat (lambda (c) (format "@>$%d=vsum(@2..@-1);N" c))
                       cols "::"))))

(defun org-nutrition--ensure-table-formulas ()
  "Ensure there is a #+TBLFM line at the end of the current table."
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
           ((looking-at-p "^\\*+\\s-")   (setq found :stop))
           (t (forward-line 1))))
        (when (numberp found)
          (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position))))))
      (goto-char (org-table-end))
      (unless (bolp) (insert "\n"))
      (insert tblfm "\n"))))

;;; ─── Daily Table Management ─────────────────────────────────────────────────

(defun org-nutrition--compute-daily-totals ()
  (save-excursion
    (org-nutrition--goto-table-in-day)
    (let* ((tbeg (org-table-begin))
           (tend (org-table-end))
           (sums (make-list 5 0.0))) ; weight cal prot carbs fat
      (goto-char tbeg)
      (forward-line 1)
      (while (< (point) tend)
        (when (and (looking-at-p org-table-dataline-regexp)
                   (not (org-nutrition--daily-total-row-p)))
          (let ((meal (org-table-get-field 1)))
            (unless (or (org-nutrition--string-empty-p (string-trim meal))
                        (string= (string-trim meal) org-nutrition-daily-total-label))
              (cl-loop for col in '(3 4 5 6 7)
                       for i from 0
                       do (setf (nth i sums)
                                (+ (nth i sums)
                                   (org-nutrition--maybe-number (org-table-get-field col))))))))
        (forward-line 1))
      (mapcar #'org-nutrition--format-number sums))))

(defun org-nutrition--update-daily-total-row ()
  (save-excursion
    (org-nutrition--goto-table-in-day)
    (let ((pos (org-nutrition--table-total-row-position)))
      (when pos
        (goto-char pos)
        (let ((totals (org-nutrition--compute-daily-totals)))
          (org-nutrition--set-row-as-daily-total)
          (cl-loop for col in '(3 4 5 6 7)
                   for val in totals
                   do (org-table-goto-column col)
                      (org-table-blank-field)
                      (insert val))
          (when org-nutrition-align-table
            (org-table-align)))))))

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
        (insert (org-nutrition--table-separator-row) "\n")
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
        (insert "\n" (org-nutrition--table-separator-row))
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
  (org-table-recalculate t)
  (org-nutrition--with-target-buffer #'org-nutrition--update-monthly-summary-table))

;;; ─── Summary Tables (Monthly & Annual) ──────────────────────────────────────

(defun org-nutrition--delete-named-table (name bound)
  "Delete a #+NAME: NAME block (including preceding blank line and TBLFM) within BOUND."
  (while (re-search-forward (format "^#\\+NAME:[ \t]*%s[ \t]*$" name) bound t)
    ;; Include the preceding blank line in the deletion range so it doesn't accumulate.
    (let ((tbeg (save-excursion
                  (goto-char (line-beginning-position))
                  (if (and (> (point) (point-min))
                           (save-excursion (forward-line -1) (looking-at "^[ \t]*$")))
                      (line-beginning-position 0)
                    (line-beginning-position)))))
      (forward-line 1)
      (while (and (< (point) (point-max))
                  (looking-at "^[ \t]*\\(|\\|#\\+TBLFM:\\|$\\)"))
        (forward-line 1))
      (delete-region tbeg (point)))))

(defun org-nutrition--insert-summary-table (table-name columns rows)
  "Insert a named summary org-table with COLUMNS, ROWS, and an AVERAGE footer.
Each row in ROWS is (label num num num num num).
Point should be at the beginning of a blank line (or just after the heading line)."
  ;; Ensure exactly one blank line before #+NAME:
  (unless (save-excursion (beginning-of-line) (looking-at "^[ \t]*$"))
    (insert "\n"))
  (insert "#+NAME: " table-name "\n")
  (insert "| " (mapconcat #'identity columns " | ") " |\n")
  (insert "|-" (mapconcat (lambda (_) "-") columns "+-") "-|\n")
  (let ((sums (make-list 5 0.0)) (count 0))
    (dolist (row rows)
      (insert "| " (car row) " | "
              (mapconcat (lambda (v) (org-nutrition--format-number v)) (cdr row) " | ")
              " |\n")
      (cl-loop for v in (cdr row)
               for i from 0
               do (setf (nth i sums) (+ (nth i sums) v)))
      (setq count (1+ count)))
    (insert "|-" (mapconcat (lambda (_) "-") columns "+-") "-|\n")
    (if (> count 0)
        (insert "| AVERAGE | "
                (mapconcat (lambda (s) (org-nutrition--format-number (/ s count))) sums " | ")
                " |\n")
      (insert "| AVERAGE | " (mapconcat (lambda (_) "0") (cdr columns) " | ") " |\n")))
  (forward-line -1)
  (org-table-align)
  (insert "\n"))

(defun org-nutrition--update-monthly-summary-table ()
  "Compile daily totals for the current month into a summary table."
  (save-excursion
    (let* ((month-str   (org-nutrition--format-month))
           (month-regex (format "^\\*\\* [ \t]*%s[ \t]*$" (regexp-quote month-str)))
           (day-regex   "^\\*\\*\\* [ \t]*\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\).*")
           (table-name  (format "monthly_summary_%s" (replace-regexp-in-string "-" "_" month-str)))
           totals month-pos month-end)
      (goto-char (point-min))
      (when (re-search-forward month-regex nil t)
        (setq month-pos (match-beginning 0))
        (setq month-end (save-excursion (org-end-of-subtree t t) (point)))
        ;; Collect totals from each day
        (goto-char month-pos)
        (while (re-search-forward day-regex month-end t)
          (let* ((date-str (match-string 1))
                 (day-end  (save-excursion (org-end-of-subtree t t) (point)))
                 (day-pos  (match-beginning 0)))
            (save-excursion
              (goto-char day-pos)
              (when (re-search-forward org-table-any-line-regexp day-end t)
                (goto-char (match-beginning 0))
                (let (totals-row)
                  (save-excursion
                    (while (and (re-search-forward org-table-dataline-regexp day-end t)
                                (not totals-row))
                      (let ((cells (org-split-string
                                    (buffer-substring-no-properties
                                     (line-beginning-position) (line-end-position))
                                    "[ \t]*|[ \t]*")))
                        (when (and (> (length cells) 2)
                                   (string= (string-trim (car cells)) org-nutrition-daily-total-label))
                          (setq totals-row cells)))))
                  (when totals-row
                    ;; columns: date cal prot carbs fat weight
                    (push (list date-str
                                (org-nutrition--maybe-number (nth 3 totals-row))
                                (org-nutrition--maybe-number (nth 4 totals-row))
                                (org-nutrition--maybe-number (nth 5 totals-row))
                                (org-nutrition--maybe-number (nth 6 totals-row))
                                (org-nutrition--maybe-number (nth 2 totals-row)))
                          totals)))))))
        (setq totals (nreverse totals))
        ;; Rebuild the table right after the month heading
        (goto-char month-pos)
        (forward-line 1)
        (org-nutrition--delete-named-table table-name (save-excursion (org-end-of-subtree t t) (point)))
        (goto-char month-pos)
        (forward-line 1)
        (org-nutrition--insert-summary-table table-name org-nutrition-monthly-table-columns totals)))
    (org-nutrition--update-annual-summary-table)))

(defun org-nutrition--update-annual-summary-table ()
  "Compile monthly averages for the current year into a summary table."
  (save-excursion
    (let* ((year-str    (org-nutrition--format-year))
           (year-regex  (format "^\\* [ \t]*%s[ \t]*$" (regexp-quote year-str)))
           (month-regex "^\\*\\* [ \t]*\\([0-9]\\{4\\}-[0-9]\\{2\\}\\).*")
           (table-name  (format "annual_summary_%s" year-str))
           totals year-pos year-end)
      (goto-char (point-min))
      (when (re-search-forward year-regex nil t)
        (setq year-pos (match-beginning 0))
        (setq year-end (save-excursion (org-end-of-subtree t t) (point)))
        ;; Collect average rows from each month's summary table
        (goto-char year-pos)
        (while (re-search-forward month-regex year-end t)
          (let* ((month-date-str (match-string 1))
                 (month-end-pos  (save-excursion (org-end-of-subtree t t) (point)))
                 (month-pos-match (match-beginning 0))
                 (mname (format "monthly_summary_%s"
                                (replace-regexp-in-string "-" "_" month-date-str))))
            (save-excursion
              (goto-char month-pos-match)
              (when (re-search-forward (format "^#\\+NAME:[ \t]*%s[ \t]*$" mname) month-end-pos t)
                (let (average-row)
                  (while (and (re-search-forward org-table-dataline-regexp month-end-pos t)
                              (not average-row))
                    (let ((cells (org-split-string
                                  (buffer-substring-no-properties
                                   (line-beginning-position) (line-end-position))
                                  "[ \t]*|[ \t]*")))
                      (when (and (> (length cells) 2)
                                 (string= (string-trim (car cells)) "AVERAGE"))
                        (setq average-row cells))))
                  (when average-row
                    (push (list month-date-str
                                (org-nutrition--maybe-number (nth 1 average-row))
                                (org-nutrition--maybe-number (nth 2 average-row))
                                (org-nutrition--maybe-number (nth 3 average-row))
                                (org-nutrition--maybe-number (nth 4 average-row))
                                (org-nutrition--maybe-number (nth 5 average-row)))
                          totals)))))))
        (setq totals (nreverse totals))
        ;; Rebuild
        (goto-char year-pos)
        (forward-line 1)
        (org-nutrition--delete-named-table table-name (save-excursion (org-end-of-subtree t t) (point)))
        (goto-char year-pos)
        (forward-line 1)
        (org-nutrition--insert-summary-table table-name org-nutrition-annual-table-columns totals)))))

;;; ─── Target Buffer ───────────────────────────────────────────────────────────

(defun org-nutrition--with-target-buffer (fn)
  (let* ((file org-nutrition-target-file)
         (buf  (or (find-buffer-visiting file) (find-file-noselect file)))
         result)
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode) (org-mode))
      (save-excursion (setq result (funcall fn)))
      (save-buffer))
    result))

;;; ─── API Search ──────────────────────────────────────────────────────────────

(defcustom org-nutrition-curl-executable "curl"
  "Path to the curl executable."
  :type 'string
  :group 'org-nutrition)

(defcustom org-nutrition-jq-executable "jq"
  "Path to the jq executable."
  :type 'string
  :group 'org-nutrition)

(defun org-nutrition--check-dependencies ()
  "Signal a user-error if curl or jq are not available."
  (unless (executable-find org-nutrition-curl-executable)
    (user-error "org-nutrition: `curl' not found. Install it or set `org-nutrition-curl-executable'"))
  (unless (executable-find org-nutrition-jq-executable)
    (user-error "org-nutrition: `jq' not found. Install it or set `org-nutrition-jq-executable'")))

(defconst org-nutrition--jq-filter
  ".products[0] | \
{ name: .product_name, \
  calories: (.nutriments[\"energy-kcal_100g\"] // 0 | tostring), \
  protein:  (.nutriments.proteins_100g         // 0 | tostring), \
  carbs:    (.nutriments.carbohydrates_100g     // 0 | tostring), \
  fats:     (.nutriments.fat_100g               // 0 | tostring) }"
  "jq filter to extract the first product's nutrition facts from the OFF API response.")

(defun org-nutrition--execute-api-search (url name)
  "Fetch nutrition data for NAME from URL using curl and jq.
Returns a plist or nil on failure/no results."
  (message "Fetching '%s' from API..." name)
  (org-nutrition--check-dependencies)
  (with-temp-buffer
    (let* ((curl-status
            (call-process org-nutrition-curl-executable nil t nil
                          "--silent" url))
           (curl-output (buffer-string)))
      (when (= curl-status 0)
        (with-temp-buffer
          (insert curl-output)
          (let* ((jq-status
                  (call-process-region (point-min) (point-max)
                                       org-nutrition-jq-executable
                                       t t nil
                                       "--raw-output" "--exit-status"
                                       org-nutrition--jq-filter))
                 (jq-output (string-trim (buffer-string))))
            (when (and (= jq-status 0)
                       (not (string= jq-output "null"))
                       (not (string-empty-p jq-output)))
              (condition-case nil
                  (let* ((json-object-type 'alist)
                         (data (json-read-from-string jq-output)))
                    (list :name     (alist-get 'name     data)
                          :calories (alist-get 'calories data)
                          :protein  (alist-get 'protein  data)
                          :carbs    (alist-get 'carbs    data)
                          :fats     (alist-get 'fats     data)
                          :portion  "100g"
                          :type     "food"))
                (error nil)))))))))

(defun org-nutrition--search-food (name _weight)
  "Search Open Food Facts for NAME via curl+jq.
Tries br.openfoodfacts.org first; falls back to world if BR returns no results."
  (let* ((query     (url-hexify-string name))
         (br-url    (format "https://br.openfoodfacts.org/cgi/search.pl?search_terms=%s&search_simple=1&action=process&json=1" query))
         (world-url (format "https://world.openfoodfacts.org/cgi/search.pl?search_terms=%s&search_simple=1&action=process&json=1" query)))
    (or (org-nutrition--execute-api-search br-url name)
        (org-nutrition--execute-api-search world-url name))))

;;; ─── Catalog ─────────────────────────────────────────────────────────────────

(defun org-nutrition--catalog-entry-at-point ()
  "Return catalog plist for the food/recipe heading at point."
  (when (org-at-heading-p)
    (list :name     (org-get-heading t t t t)
          :portion  (or (org-entry-get nil "PORTION")  "")
          :calories (or (org-entry-get nil "CALORIES") "")
          :fats     (or (org-entry-get nil "FATS")     "")
          :carbs    (or (org-entry-get nil "CARBS")    "")
          :protein  (or (org-entry-get nil "PROTEIN")  "")
          :type     (or (org-entry-get nil "TYPE")     ""))))

(defun org-nutrition--get-all-catalog-entries ()
  "Return an alist of all cataloged foods and recipes: ((NAME . PLIST) ...)"
  (org-nutrition--with-target-buffer
   (lambda ()
     (let (entries)
       (org-map-entries
        (lambda ()
          (let ((type (org-entry-get nil "TYPE"))
                (cal  (org-entry-get nil "CALORIES")))
            (when (or (member type '("food" "recipe")) cal)
              (push (cons (org-get-heading t t t t)
                          (org-nutrition--catalog-entry-at-point))
                    entries))))
        t 'file)
       (nreverse entries)))))

(defun org-nutrition--find-entry-in-subtree (parent name)
  "Find NAME under PARENT top-level heading. Returns point or nil."
  (let ((case-fold-search t) found)
    (org-nutrition--goto-or-create-top-heading parent)
    (let ((end (save-excursion (org-end-of-subtree t t) (point)))
          (re  (format "^\\*\\*\\s-+%s\\s-*$" (regexp-quote name))))
      (save-excursion
        (forward-line 1)
        (when (re-search-forward re end t)
          (setq found (match-beginning 0)))))
    (when found (goto-char found))
    found))

(defun org-nutrition--find-food-or-recipe (name)
  "Find NAME in Foods or Recipes headings. Returns point or nil."
  (or (org-nutrition--find-entry-in-subtree org-nutrition-foods-heading name)
      (org-nutrition--find-entry-in-subtree org-nutrition-recipes-heading name)))

(defun org-nutrition--catalog-entry-from-api (api-plist)
  "Normalize an API plist to an internal entry plist, or nil."
  (when (listp api-plist)
    (list :name     (plist-get api-plist :name)
          :portion  (or (plist-get api-plist :portion) org-nutrition-default-portion)
          :calories (format "%s" (or (plist-get api-plist :calories) ""))
          :fats     (format "%s" (or (plist-get api-plist :fats)     ""))
          :carbs    (format "%s" (or (plist-get api-plist :carbs)    ""))
          :protein  (format "%s" (or (plist-get api-plist :protein)  ""))
          :type     (format "%s" (or (plist-get api-plist :type)     "")))))

(defun org-nutrition--ensure-food-or-recipe-entry (entry &optional ingredients)
  "Ensure ENTRY exists under Foods or Recipes heading.

ENTRY is a plist with :name :portion :calories :fats :carbs :protein :type.
INGREDIENTS is a list of (NAME . WEIGHT-STRING) cons cells."
  (let* ((name    (plist-get entry :name))
         (portion (or (plist-get entry :portion)  org-nutrition-default-portion))
         (calories (or (plist-get entry :calories) ""))
         (fats     (or (plist-get entry :fats)    ""))
         (carbs    (or (plist-get entry :carbs)   ""))
         (protein  (or (plist-get entry :protein) ""))
         (type     (or (plist-get entry :type)    "food"))
         (heading  (if (string= type "recipe")
                       org-nutrition-recipes-heading
                     org-nutrition-foods-heading)))
    (org-nutrition--goto-or-create-top-heading heading)
    (unless (org-nutrition--find-entry-in-subtree heading name)
      (goto-char (save-excursion (org-end-of-subtree t t) (point)))
      (unless (bolp) (insert "\n"))
      (insert "** " name "\n")
      (forward-line -1)
      (beginning-of-line))
    (org-set-property "PORTION"   portion)
    (org-set-property "CALORIES"  calories)
    (org-set-property "FATS"      fats)
    (org-set-property "CARBS"     carbs)
    (org-set-property "PROTEIN"   protein)
    (org-set-property "TYPE"      type)
    (when ingredients
      (save-excursion
        (org-back-to-heading t)
        (let ((end-of-drawer (save-excursion (org-end-of-meta-data) (point))))
          (while (re-search-forward "^[ \t]*:INGREDIENT:.*$" end-of-drawer t)
            (delete-region (line-beginning-position) (1+ (line-end-position)))
            (setq end-of-drawer (save-excursion (org-end-of-meta-data) (point))))
          (org-back-to-heading t)
          (when (re-search-forward "^[ \t]*:END:[ \t]*$" end-of-drawer t)
            (goto-char (match-beginning 0))
            (dolist (ing ingredients)
              (insert (format "    :INGREDIENT: [%s][%s]\n" (car ing) (cdr ing))))))))
    (point)))

;;; ─── Nutrition Lookup ────────────────────────────────────────────────────────

(defun org-nutrition--read-validated-macro (prompt &optional initial max-val)
  "Read a numeric string from the minibuffer for a macronutrient field.
Rejects non-numeric input and values greater than MAX-VAL (when provided).
INITIAL is the pre-filled value. Returns the trimmed string."
  (let* ((map (make-sparse-keymap))
         (check-and-exit
          (lambda ()
            (interactive)
            (let* ((val (string-trim (minibuffer-contents)))
                   num)
              (cond
               ((string-empty-p val)
                (exit-minibuffer))
               ((not (string-match-p "^[0-9]*\\.?[0-9]+$" val))
                (minibuffer-message
                 (propertize " [Valor inválido! Deve ser numérico.]" 'face 'error)))
               ((and max-val (> max-val 0)
                     (> (setq num (string-to-number val)) max-val))
                (minibuffer-message
                 (propertize (format " [Valor (%.2f) maior que o peso (%.2f)!]" num max-val)
                             'face 'error)))
               (t (exit-minibuffer)))))))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "RET") check-and-exit)
    (define-key map (kbd "C-j") check-and-exit)
    (let ((minibuffer-local-map map))
      (string-trim (read-string prompt initial)))))


(defun org-nutrition--read-food-name ()
  "Prompt for a food/recipe name with completion from the catalog.
Candidates are annotated with their type (food/recipe).
Accepts free text — the user is not restricted to cataloged items.
Returns (NAME . CATALOG-ENTRY-OR-NIL)."
  (let* ((catalog (org-nutrition--get-all-catalog-entries))
         (table
          (lambda (str pred action)
            (if (eq action 'metadata)
                `(metadata
                  (annotation-function
                   . ,(lambda (candidate)
                        (let* ((entry (cdr (assoc candidate catalog)))
                               (type  (or (plist-get entry :type) "")))
                          (unless (org-nutrition--string-empty-p type)
                            (concat " :: " type))))))
              (complete-with-action action (mapcar #'car catalog) str pred))))
         (name (string-trim (completing-read "Food name: " table nil nil))))
    (cons name (cdr (assoc name catalog)))))

(defun org-nutrition--nutrition-from-catalog-or-api (food weight catalog-entry)
  "Return (cal protein carbs fat notes) for FOOD at WEIGHT grams.
CATALOG-ENTRY is a plist if already resolved, or nil to trigger API/manual."
  (let* ((api        (unless catalog-entry
                       (and (org-nutrition--ask-use-api-p)
                            (org-nutrition--search-food food weight))))
         (data-plist (or catalog-entry (and (listp api) api)))
         (cal     (or (plist-get data-plist :calories) ""))
         (protein (or (plist-get data-plist :protein)  ""))
         (carbs   (or (plist-get data-plist :carbs)    ""))
         (fat     (or (plist-get data-plist :fats) (plist-get data-plist :fat) ""))
         (portion (or (plist-get data-plist :portion) org-nutrition-default-portion))
         (notes   (or (plist-get data-plist :notes) ""))
         is-manual)
    (if catalog-entry
        (message "Using cataloged item '%s'." food)
      (when api (message "Fetched '%s' from API." food)))
    (when (cl-some (lambda (v) (org-nutrition--string-empty-p (format "%s" v)))
                   (list cal protein carbs fat))
      (when (y-or-n-p "No complete data found (or skipped API). Insert manually? ")
        (setq is-manual t)
        (let ((w (string-to-number (string-trim (or weight "0")))))
          (setq cal     (org-nutrition--read-validated-macro "Calories (kcal): "  (format "%s" cal)))
          (setq protein (org-nutrition--read-validated-macro "Protein (g): "      (format "%s" protein) w))
          (setq carbs   (org-nutrition--read-validated-macro "Carbs (g): "        (format "%s" carbs)   w))
          (setq fat     (org-nutrition--read-validated-macro "Fat (g): "          (format "%s" fat)     w))
          (setq notes   (org-nutrition--read-field           "Notes: "            (format "%s" notes))))))
    (if is-manual
        (list (format "%s" cal) (format "%s" protein) (format "%s" carbs)
              (format "%s" fat) (format "%s" notes))
      (let* ((pnum   (string-to-number (replace-regexp-in-string "[^0-9.]" "" portion)))
             (w      (string-to-number (string-trim (or weight "0"))))
             (factor (if (> pnum 0) (/ (float w) pnum) 1.0)))
        (list (org-nutrition--format-number (* factor (string-to-number (format "%s" cal))))
              (org-nutrition--format-number (* factor (string-to-number (format "%s" protein))))
              (org-nutrition--format-number (* factor (string-to-number (format "%s" carbs))))
              (org-nutrition--format-number (* factor (string-to-number (format "%s" fat))))
              (format "%s" notes))))))

(defun org-nutrition--read-entry ()
  (let* ((meal       (org-nutrition--read-meal))
         (name+entry (org-nutrition--read-food-name))
         (food       (car name+entry))
         (cat-entry  (cdr name+entry))
         (weight     (org-nutrition--read-number-as-string "Weight (g): "))
         (nutri      (org-nutrition--nutrition-from-catalog-or-api food weight cat-entry)))
    (list meal food weight
          (nth 0 nutri) (nth 1 nutri) (nth 2 nutri) (nth 3 nutri) (nth 4 nutri))))

;;; ─── Food & Recipe Capture ───────────────────────────────────────────────────

(defun org-nutrition--log-existing-entry (name existing)
  "Log EXISTING catalog entry for NAME into the daily table, prompting for meal/weight."
  (let* ((meal    (org-nutrition--read-meal))
         (weight  (org-nutrition--read-number-as-string "Weight (g): "))
         (portion (or (plist-get existing :portion) org-nutrition-default-portion))
         (pnum    (string-to-number (replace-regexp-in-string "[^0-9.]" "" portion)))
         (w       (string-to-number (string-trim (or weight "0"))))
         (factor  (if (> pnum 0) (/ (float w) pnum) 1.0))
         (scale   (lambda (key)
                    (org-nutrition--format-number
                     (* factor (string-to-number (or (plist-get existing key) "0"))))))
         (cells   (list meal name weight
                        (funcall scale :calories)
                        (funcall scale :protein)
                        (funcall scale :carbs)
                        (funcall scale :fats)
                        "")))
    (org-nutrition--with-target-buffer
     (lambda ()
       (org-nutrition--append-row-to-day-table cells)))
    (message "Nutrition entry added")))

;;;###autoload
(defun org-nutrition-food-capture ()
  "Catalog a food and optionally use it to fill a meal entry."
  (interactive)
  (let ((name (string-trim (read-string "Food Name: "))))
    (when (org-nutrition--string-empty-p name)
      (user-error "Empty name"))
    (org-nutrition--with-target-buffer
     (lambda ()
       (let* ((found-pos (org-nutrition--find-entry-in-subtree org-nutrition-foods-heading name))
              (existing  (when found-pos
                           (save-excursion
                             (goto-char found-pos)
                             (let ((e (org-nutrition--catalog-entry-at-point)))
                               (when (listp e)
                                 (plist-put e :category (or (org-entry-get nil "CATEGORY") "")))
                               e)))))
         (cl-labels
             ((parse-float (s)
                (string-to-number (replace-regexp-in-string "[^0-9.]" "" (or s "0"))))
              (get-entry-values (entry-name &optional default-entry)
                (let* ((def-portion (or (plist-get default-entry :portion) org-nutrition-default-portion))
                       (portion  (read-string "Portion (e.g. 100g): " def-portion))
                       (limit    (parse-float portion))
                       (cat      (org-nutrition--read-category))
                       (cal      (org-nutrition--read-validated-macro
                                  (format "Calories (kcal) per %s: " portion)
                                  (plist-get default-entry :calories)))
                       (fat      (org-nutrition--read-validated-macro
                                  (format "Fats (g) per %s: "    portion)
                                  (plist-get default-entry :fats)    limit))
                       (carbs    (org-nutrition--read-validated-macro
                                  (format "Carbs (g) per %s: "   portion)
                                  (plist-get default-entry :carbs)   limit))
                       (prot     (org-nutrition--read-validated-macro
                                  (format "Protein (g) per %s: " portion)
                                  (plist-get default-entry :protein) limit)))
                  (list :name entry-name :portion portion
                        :calories cal :fats fat :carbs carbs :protein prot
                        :type "food" :category cat))))
           (cond
            (existing
             (when (y-or-n-p (format "'%s' already cataloged. Use it to fill a meal entry now? " name))
               (org-nutrition--log-existing-entry name existing)))
            (t
             (let* ((api-raw   (and (org-nutrition--ask-use-api-p)
                                    (org-nutrition--search-food name org-nutrition-default-portion)))
                    (api-entry (org-nutrition--catalog-entry-from-api api-raw))
                    (entry     (if api-entry
                                   (if (or (cl-some (lambda (k)
                                                      (org-nutrition--string-empty-p
                                                       (plist-get api-entry k)))
                                                    '(:calories :protein :carbs :fats))
                                           (y-or-n-p (format "API data received for '%s'. Edit/Verify? " name)))
                                       (get-entry-values name api-entry)
                                     api-entry)
                                 (get-entry-values name)))
                    (entry-name (or (plist-get entry :name) name))
                    (category   (or (plist-get entry :category)
                                    (org-nutrition--read-category))))
               (setq entry (plist-put (plist-put entry :name entry-name) :type "food"))
               (org-nutrition--ensure-food-or-recipe-entry entry)
               (when (and (stringp category) (not (org-nutrition--string-empty-p category)))
                 (org-set-property "CATEGORY" category))
               (message "Cataloged: %s" entry-name)
               (when (y-or-n-p (format "Use '%s' to fill a meal entry now? " entry-name))
                 (org-nutrition--log-existing-entry entry-name entry))))))))))
;;;###autoload
(defun org-nutrition-recipe-capture ()
  "Catalog a custom recipe composed of ingredients."
  (interactive)
  (let ((name (string-trim (read-string "Recipe Name: "))))
    (when (org-nutrition--string-empty-p name)
      (user-error "Empty name"))
    (let* ((catalog  (org-nutrition--get-all-catalog-entries))
           (choices  (cons "[ DONE ]" (mapcar #'car catalog)))
           ingredients
           (sums     (make-list 5 0.0))) ; weight cal prot carbs fat
      (catch 'done
        (while t
          (let ((ing-name (completing-read "\nIngredient ([ DONE ] to finish): " choices nil t)))
            (cond
             ((or (org-nutrition--string-empty-p ing-name) (string= ing-name "[ DONE ]"))
              (throw 'done nil))
             ((not (assoc ing-name catalog))
              (message "Ingredient not found: %s" ing-name)
              (sit-for 1.5))
             (t
              (let* ((ing-entry  (cdr (assoc ing-name catalog)))
                     (pstr       (or (plist-get ing-entry :portion) org-nutrition-default-portion))
                     (pnum       (string-to-number (replace-regexp-in-string "[^0-9.]" "" pstr)))
                     (def-w-str  (format "%sg" pnum))
                     (weight-str (read-string (format "Weight for %s (default %s): " ing-name def-w-str)
                                              nil nil def-w-str))
                     (weight-num (string-to-number (replace-regexp-in-string "[^0-9.]" "" weight-str)))
                     (factor     (if (> pnum 0) (/ (float weight-num) pnum) 1.0)))
                (push (cons ing-name weight-str) ingredients)
                (cl-loop for key in '(:calories :protein :carbs :fats)
                         for i from 1
                         do (setf (nth i sums)
                                  (+ (nth i sums)
                                     (* factor (string-to-number (or (plist-get ing-entry key) "0"))))))
                (setf (nth 0 sums) (+ (nth 0 sums) weight-num))))))))
      (let* ((default-portion (format "%dg" (round (nth 0 sums))))
             (portion (read-string (format "Total Recipe Portion (default %s): " default-portion)
                                   nil nil default-portion))
             (entry (list :name     name
                          :portion  portion
                          :calories (org-nutrition--format-number (nth 1 sums))
                          :protein  (org-nutrition--format-number (nth 2 sums))
                          :carbs    (org-nutrition--format-number (nth 3 sums))
                          :fats     (org-nutrition--format-number (nth 4 sums))
                          :type     "recipe")))
        (org-nutrition--with-target-buffer
         (lambda ()
           (org-nutrition--ensure-food-or-recipe-entry entry (nreverse ingredients))))
        (message "Cataloged Recipe: %s" name)
        (when (y-or-n-p (format "Use '%s' to fill a meal entry now? " name))
          (org-nutrition--log-existing-entry name entry))))))

;;; ─── Entry Capture ───────────────────────────────────────────────────────────

;;;###autoload
(defun org-nutrition-capture ()
  "Capture a nutrition entry into the target Org file."
  (interactive)
  (let ((cells (org-nutrition--read-entry)))
    (org-nutrition--with-target-buffer
     (lambda ()
       (org-nutrition--append-row-to-day-table cells)))
    (message "Nutrition entry added")))

;;;###autoload
(defun org-nutrition-insert-entry ()
  "Insert a nutrition entry into an org-table in the current Org buffer."
  (interactive)
  (org-nutrition--ensure-org-buffer)
  (let ((cells (org-nutrition--read-entry)))
    (save-excursion
      (org-nutrition--append-row-to-day-table cells))
    (message "Nutrition entry added")))

;;; ─── BMR Capture ─────────────────────────────────────────────────────────────

(defun org-nutrition--ensure-body-targets-table ()
  "Ensure the Body Info heading and table exist."
  (goto-char (point-min))
  (let* ((heading-regex (format "^\\* [ \t]*%s[ \t]*$" (regexp-quote org-nutrition-body-targets-heading)))
         (heading-pos nil)
         (heading-end nil))
    (if (re-search-forward heading-regex nil t)
        (progn
          (setq heading-pos (match-beginning 0))
          (setq heading-end (save-excursion (org-end-of-subtree t t) (point))))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n* " org-nutrition-body-targets-heading "\n")
      (setq heading-pos (line-beginning-position 0))
      (setq heading-end (point-max)))
    (goto-char heading-pos)
    (if (re-search-forward org-table-any-line-regexp heading-end t)
        (goto-char (org-table-end))
      (goto-char heading-end)
      (insert "\n| " (mapconcat #'identity org-nutrition-bmr-table-columns " | ") " |\n")
      (insert "|-" (mapconcat (lambda (_c) "-") org-nutrition-bmr-table-columns "+-") "-|\n"))))

;;;###autoload
(defun org-nutrition-bmr-capture ()
  "Capture body measurements and calculate BMR/Caloric targets."
  (interactive)
  (let* ((weight (read-number "Weight (kg): "))
         (height (read-number "Height (cm): "))
         (age    (read-number "Age: "))
         (activity-choices '(("Sedentary (1.2)"          . 1.2)
                             ("Lightly active (1.375)"   . 1.375)
                             ("Moderately active (1.55)" . 1.55)
                             ("Very active (1.725)"      . 1.725)
                             ("Extremely active (1.9)"   . 1.9)))
         (activity-str    (completing-read "Activity level: " (mapcar #'car activity-choices) nil t))
         (activity-factor (cdr (assoc activity-str activity-choices)))
         (goal-choices '(("Loss (-300 kcal)"  . -300)
                         ("Keep (+0 kcal)"    .    0)
                         ("Gain (+300 kcal)"  .  300)))
         (goal-str    (completing-read "Goal: " (mapcar #'car goal-choices) nil t))
         (goal-offset (cdr (assoc goal-str goal-choices)))
         (date   (format-time-string "%Y-%m-%d"))
         (bmi    (/ (float weight) (expt (/ (float height) 100) 2)))
         (bmr    (* activity-factor (+ (* 10 weight) (* 6.25 height) (* -5 age) 5)))
         (target (+ bmr goal-offset)))
    (org-nutrition--with-target-buffer
     (lambda ()
       (org-nutrition--ensure-body-targets-table)
       (insert "| " date " | "
               (org-nutrition--format-number weight) " | "
               (org-nutrition--format-number height) " | "
               (number-to-string age) " | "
               (format "%.1f" bmi) " | "
               (org-nutrition--format-number activity-factor) " | "
               goal-str " | "
               (org-nutrition--format-number bmr) " | "
               (org-nutrition--format-number target) " |\n")
       (org-table-align)))
    (message "Calculated BMI: %.1f | BMR: %s | Daily Target: %s kcal"
             bmi
             (org-nutrition--format-number bmr)
             (org-nutrition--format-number target))))

;;; ─── Minor Mode ──────────────────────────────────────────────────────────────

(defvar org-nutrition-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c n c") #'org-nutrition-capture)
    (define-key map (kbd "C-c n i") #'org-nutrition-insert-entry)
    (define-key map (kbd "C-c n b") #'org-nutrition-bmr-capture)
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
;;; org-nutrition.el ends here
