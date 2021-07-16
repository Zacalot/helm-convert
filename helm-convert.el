(require 'calc-units)

;; <symbol> (<Type> <noMetricPrefix>)
(setq helm-convert-unit-types
      '(
        (m . ("Length"))
        (mu . ("Length" t))
        (nmi . ("Length" t))
        (pc . ("Length" t))
        (lyr . ("Length" t))
        (au . ("Length" t))
        (in . ("Length" t))
        (Bohr . ("Length" t))
        (Ang . ("Length" t))

        (bit . ("Digital Information" t))

        (texpt ("TeX Length" t))
        (texbp ("TeX Length" t))

        (hect . ("Area"))
        (a . ("Area" t))
        (acre . ("Area" t))
        (b . ("Area" t))

        (tsp . ("Volume" t))
        (galC . ("Volume" t))
        (galUK . ("Volume" t))
        (L . ("Volume"))

        (s . ("Time"))
        (min . ("Time" t))

        (Hz . ("Frequency"))

        (mph . ("Speed" t))
        (kph . ("Speed" t))
        (knot . ("Speed" t))
        (c . ("Speed" t))

        (ga . ("Acceleration"))

        (g . ("Mass"))
        (oz . ("Mass" t))
        (ozt . ("Mass" t))
        (ct . ("Mass" t))

        (N . ("Force"))
        (lbf . ("Force" t))

        (J . ("Energy"))
        (eV . ("Energy"))
        (invcm . ("Energy"))
        (Hzen . ("Energy"))
        (Ken . ("Energy"))
        (Wh . ("Energy"))
        (Ws . ("Energy"))

        (W . ("Power"))
        (hp . ("Power" t))
        (hpm . ("Power"))

        (K . ("Temperature" t))

        (Pa . ("Pressure"))
        (psi . ("Pressure" t))

        (P . ("Viscosity"))
        (St . ("Viscosity"))

        (A . ("Electromagnetism"))
        (C . ("Electromagnetism"))
        (Fdy . ("Electromagnetism"))
        (e . ("Electromagnetism"))
        (ech . ("Electromagnetism"))
        (V . ("Electromagnetism"))
        (ohm . ("Electromagnetism"))
        (Ω . ("Electromagnetism"))
        (mho . ("Electromagnetism"))
        (S . ("Electromagnetism"))
        (F . ("Electromagnetism"))
        (H . ("Electromagnetism"))
        (T . ("Electromagnetism"))
        (Gs . ("Electromagnetism"))
        (Wb . ("Electromagnetism"))

        (cd . ("Luminous Intensity"))
        (sb . ("Luminous Intensity"))
        (lm . ("Luminous Intensity"))
        (lx . ("Luminous Intensity"))
        (ph . ("Luminous Intensity"))
        (fc . ("Luminous Intensity"))
        (lam . ("Luminous Intensity"))
        (flam . ("Luminous Intensity"))

        (Bq . ("Radioactivity"))
        (Gy . ("Radioactivity"))
        (R . ("Radioactivity"))

        (mol . ("Substance Amount"))

        (rad . ("Plane Angle" t))
        (circ . ("Plane Angle" t))
        (rpm . ("Plane Angle" t))

        (Np . ("Logarithmic" t))
        (dB . ("Logarithmic" t))

        (sr . ("Solid Angle" t))
        ))

(setq helm-convert-unit-names '(
                                (yr nil "Year")
                                (K nil "Kelvin")
                                (pc nil "Parsec")
                                (pt nil "Pint")
                                ))

(setq helm-convert-unit-prefix-blacklist '(
                                           ;;Blank
                                           0
                                           ;;Redundant Kilo
                                           75
                                           ))

(setq helm-convert-unit-blacklist '(
                                    ;;Redundancies
                                    l
                                    sec
                                    ;;Sums
                                    mfi
                                    vol
                                    hms
                                    tpo

                                    ;;Apparently redundant temperature units?
                                    dK
                                    degK
                                    dF
                                    dC
                                    ;;Constants
                                    h
                                    hbar
                                    eps0
                                    ε0
                                    mu0
                                    μ0
                                    G
                                    Nav
                                    me
                                    mp
                                    mn
                                    mmu
                                    mμ
                                    Ryd
                                    k
                                    sigma
                                    σ
                                    alpha
                                    α
                                    muB
                                    muN
                                    mue
                                    mup
                                    R0
                                    V0
                                    ))

(defun helm-convert-unit-get-unit-info(unit-symbol)
  "Returns definition list of a unit"
  (and (not (equal "" (symbol-name unit-symbol))) (or (assq
                                                       unit-symbol
                                                       (append math-standard-units math-additional-units))
                                                      (assq ;;Try symbol without prefix
                                                       (intern-soft (substring (symbol-name unit-symbol) 1))
                                                       (append math-standard-units math-additional-units)))))

(defun helm-convert-unit-get-unit-unit(unit-symbol)
  "Useful for converting prefixed unit to nonprefixed version"
  (nth 0 (helm-convert-unit-get-unit-info unit-symbol)))

(defun helm-convert-unit-get-unit-exists(unit-symbol)
  (and (helm-convert-unit-get-unit-info unit-symbol) t))

(defun helm-convert-unit-get-unit-name(unit-symbol)
  "Returns nice name of a unit"
  (replace-regexp-in-string "\*" "" (nth 2 (or (assoc (helm-convert-unit-get-unit-unit unit-symbol) helm-convert-unit-names) (helm-convert-unit-get-unit-info unit-symbol)))))

(defun helm-convert-unit-get-unit-base(unit-symbol)
  "Returns the base of a unit"
  (let ((derivation
         (nth 1 (helm-convert-unit-get-unit-info unit-symbol))
         ))
    (if derivation ;;Extract deriving unit from definition, not fullproof but works good enough.
        (intern-soft (helm-convert-unit-extract-unit derivation)))))

(defun helm-convert-unit-get-unit-fundamental-conversion(unit-symbol)
  "Returns fundamental conversion of a unit, the base of its type-defined inheritance"
  (let (
        (base unit-symbol)
        new-base
        (n 0)
        )
    (cl-loop
     ;; (when (> n 100)
     ;;   (progn (print (concat "BAD UNIT: " unit-symbol)) (cl-return "")))
     (cl-incf n)
     (setq new-base (helm-convert-unit-get-unit-base base))
     (if (and new-base (not (assoc base helm-convert-unit-types)))
         (setq base new-base)
       (cl-return (helm-convert-unit-get-unit-unit base))))))

(defun helm-convert-unit-get-unit-type(unit)
  "Returns the type of a unit, what it measures"
  (nth 0 (cdr (assoc (helm-convert-unit-get-unit-fundamental-conversion unit) helm-convert-unit-types))))

(defun helm-convert-unit-can-prefix(unit)
  "Returns whether or not the unit can use metric prefixes"
  (not (nth 1 (cdr (assoc (helm-convert-unit-get-unit-fundamental-conversion unit) helm-convert-unit-types)))))

(defun helm-convert-unit-extract-unit(string)
  "Extracts unit from a unit-value pair string"
  (replace-regexp-in-string "[^a-z]" "" string))

(defun helm-convert-unit-extract-value(string)
  "Extracts value from unit-value pair string"
  (replace-regexp-in-string "[^0-9\.]" "" string))

(defun helm-convert-units (value-unit new-unit)
  "Converts from one value-unit pair to a another"
  (if (equal "Temperature" (helm-convert-unit-get-unit-type (intern-soft new-unit)))
      (calc-eval
       (math-convert-temperature 
        (calc-eval value-unit 'raw) (calc-eval (helm-convert-unit-extract-unit value-unit) 'raw) (calc-eval new-unit 'raw)))
    (calc-eval
     (math-convert-units 
      (calc-eval value-unit 'raw) (calc-eval new-unit 'raw)))))

(defun helm-convert-unit-list-all-units(value)
  "Makes a list of all units with relevant prefixes for helm browsing."
  (let (
        units ;;The goal is to populate this list. There's probably a better method than below, I just don't know it. mapcar doesn't work cause I may need multiple entries per entry, it's not a 1:1 transformation.
        (type-restriction (helm-convert-unit-get-unit-type (intern-soft (helm-convert-unit-extract-unit value)))))
    (dolist (unit (append math-standard-units math-additional-units)) ;;For each unit
      (if (and
           (not (member (nth 0 unit) helm-convert-unit-blacklist)) ;;Unit must not be blacklisted
           (or ;;Unit must match type restriction
            (not type-restriction)
            (equal type-restriction (helm-convert-unit-get-unit-type (nth 0 unit)))))
          (progn (setq units ;;Add unprefixed unit
                       (cons
                        (cons
                         (concat value (helm-convert-unit-get-unit-name (nth 0 unit)) " (" (symbol-name (nth 0 unit)) ")")
                         (symbol-name (nth 0 unit)))
                        units))
                 (if (helm-convert-unit-can-prefix (nth 0 unit)) ;;If unit can use prefixes (likely metric)
                     (dolist (prefix math-unit-prefixes)
                       (if (not (member (nth 0 prefix) helm-convert-unit-prefix-blacklist))
                           (let* ((symbol ;;Get symbol with prefix as string
                                   (concat (char-to-string (nth 0 prefix)) (symbol-name (nth 0 unit)))
                                   ))
                             (setq units
                                   (cons
                                    (cons
                                     (concat value (nth 2 prefix) (helm-convert-unit-get-unit-name (nth 0 unit)) " (" symbol ")")
                                     symbol)
                                    units)))))))))
    (progn units)))

(defun helm-convert-unit-helm-pick-unit(prompt value)
  (helm :sources (helm-build-sync-source "Units"
                   :candidates (helm-convert-unit-list-all-units value)
                   :fuzzy-match t)
        :prompt prompt
        :buffer "*units*"))

(defun helm-convert-unit-display(value-unit converted-value-unit)
  (message (concat value-unit " -> " converted-value-unit))
  (kill-new converted-value-unit))

(defun helm-convert-unit()
  "Convert from one unit to another. Initial input may be a number or a number/unit pair."
  (interactive)
  (catch 'no-input
    (let (
          (value (or (read-from-minibuffer "Value/Unit: " (and (use-region-p) (downcase (buffer-substring-no-properties (region-beginning) (region-end))))) (throw 'no-input "no value")))
          unit
          value-unit
          )
      (setq unit (helm-convert-unit-extract-unit value))
      (setq value (helm-convert-unit-extract-value value))
      (if (or (equal "" unit) (not (helm-convert-unit-get-unit-exists (intern-soft unit)))) ;;If value entry does not contain unit or if it's invalid, query for it.
          (setq value-unit (concat value (or (helm-convert-unit-helm-pick-unit "From: " (concat value " ")) (throw 'no-input "no unit"))))
        (setq value-unit (concat value unit)))
      (helm-convert-unit-display value-unit (helm-convert-units value-unit (or (helm-convert-unit-helm-pick-unit "To: " (concat value-unit " -> ")) (throw 'no-input "no unit")))))))

(defun helm-convert-unit-quick ()
  "Convert from one value/unit pair to another unit. See `calc-view-units-table' for list of units"
  (interactive)
  (catch 'no-input
    (let ((value-unit
           (or (read-from-minibuffer "Unit Value: ") (throw 'no-input "no input"))
           ))
      (helm-convert-unit-display value-unit (helm-convert-units value-unit (read-from-minibuffer (concat value-unit " ->: ")))))))
