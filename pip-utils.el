;;; pip-utils.el: utility functions to run pip in current project
;;; https://gitlab.com/emacs-stuff/pip-utils
;;; author: vindarel
;;; licence: wtf public licence

(require 'ido)
(require 'f)
(require 'hydra)
(require 's)
(require 'virtualenvwrapper)

(defvar pip-requirements-glob "*requirements*.txt"
  "Glob pattern of the requirements file to look for at the project root, at root/<project-name>/ and inside a 'requirements' directory") ;; unused yet

(defun pip-package-version ()
  "Get the package version number with a pip freeze. Local packages autocompletion. Display the package name and its version, return the version number. For a script use, see --get-package-version (package)"
  ;; with pip freeze, we have the package version inline.
  (let* ((packages (pip--get-all-packages))
         (package (ido-completing-read "Package? " packages)))
    (if (s-blank? package)
        (error "no package selected"))
    (message package)))

(defun pip-utils--get-package-version (package)
  "Return the given package version (str). Uses pip freeze."
  (let* ((packages (pip--get-all-packages))
         (package (car (--filter (s-starts-with-p package it) packages)))
         (version (cadr (s-split "==" package))))
    version))

(defun pip-install (&optional package add-to-requirements)
  "install package with pip in the right virtual env.
   Use venv-workon to change it.
   Add the package to the requirements.txt file (works for Django projects).
  "
  ;; TODO: (ido) completing read with the current venv as default
  ;; and an option for a global install.
  ;; TODO: word at point as suggestion. see npm.
  ;; TODO: set the version.
  (let* ((package (read-from-minibuffer (format "Package? (in venv %s) "
                                               venv-current-name)))
         (candidates (pip--get-requirements-file))
         (ido-separator "\n")
         (reqfile (if add-to-requirements
                      (ido-completing-read "Requirements file ? " candidates))))
    (message "installing %s in venv %s" package venv-current-name)
    (compile (concat "pip install " package))
    ;; if compile fails, nothing should be added.
    (if add-to-requirements
        (progn
          (if reqfile
              (append-to-file (format "\n%s" package) nil reqfile))
      ))))

(defun pip-install-add-to-requirements (&optional package)
  "Install the package and add it to the project's requirements file."
  (interactive) ;;(list (read-from-minibuffer (format "Package ? (in venv %s) " venv-current-name))))
  (call-interactively (pip-install package t)) ; to finish
  )

(defun pip-get-packages-list (req-file)
  "parses the requirements file and returns a list of packages to be installed, with their version.
   Must have one package per line."
  (setq pip-packages-list (with-temp-buffer
    (progn
      (insert-file-contents req-file)
      (goto-char 1)
      ;; remove everything that's after a #, considered a comment. Watch out, this is weak.
      (replace-regexp "#.*" "")
      (buffer-string)
      )))
  ;; split and trim lines
  (setq pip-packages-list (s-lines pip-packages-list))
  (setq pip-packages-list (mapcar (lambda (x) (s-trim x)) pip-packages-list))
  ;; caution: the string Django>=1.6 would mean something to the shell.
  (setq pip-packages-list (mapcar (lambda (it)
                                    (unless (string-equal it "")
                                      (s-wrap it "\""))) pip-packages-list)))

(defun pip--install-requirements (reqfile)
  "Ask to install the packages in the given file and run it in a compilation process."
  (if (yes-or-no-p (format "install packages from %s in venv %s ?" reqfile venv-current-name))
      ;; ONGOING is this change ok for files in other directories than root ?
      (compile (concat "pip install -r " reqfile))))

(defun pip--get-requirements-file ()
  "Return a list of requirements files, or error out if no one is found.

We look for files matching 'requirements', with any extension (so that we match .in and .txt files as well as 'dev-requirements').
We look into:
- the project root
- project_root/app_name directory (necessary for Django projects for instance),
- any directory called 'requirements'."
  (let* ((root (projectile-project-root))
         (req-candidates (f-glob "*requirements*" root))
         (inproject (f-glob "*requirements*" (concat root (projectile-project-name))))
         (req-dir (f-directories root (lambda (dir) (equal (f-filename dir) "requirements"))))
         (req-dir-files (if req-dir
                            (--map (f-files it) req-dir)))
         (candidates (-concat req-candidates inproject (car req-dir-files))))
    (or candidates
        (error "We didn't find any requirements file.")
        candidates)))

(defun pip-install-requirements ()
  "Install packages from requirements files. Look for different
  requirements files in a few locations (see
  `pip--get-requirements-file`)."
  (interactive)
  (let ((candidates (pip--get-requirements-file))
        (reqfile nil))
    (if (> (length candidates) 1)
        (setq reqfile (ido-completing-read "Choose a requirements file: " candidates))
      (setq reqfile candidates))
    (pip--install-requirements reqfile)))

(defun pip--get-all-packages ()
  "Get all packages of the current venv (with pip freeze). Keep their inline version. Return a list of package==x.y.z"
  (let* ((txt (shell-command-to-string "pip freeze"))
         (packages (s-split "\n" txt))
         (packages (--remove (s-starts-with-p "You " it) packages))
         ;; (packages (--map (car (s-split "==" it)) packages))
         )
    packages))

(defun pip-uninstall ()
  "Uninstall a packages (ido completion on package list of current venv)."
  (interactive)
  (let* ((packages (pip--get-all-packages))
        (package (ido-completing-read "Package ? " packages)))
    (compile (concat "pip uninstall --yes" package))))

(defun pip--pypi-url (package)
  "Takes a package and returns its pypi url."
  (format "https://pypi.python.org/pypi/%s" package)
)

(defun pip--clean-symbol (str)
  ;; XXX also < etc
  (if (s-contains-p "==" str)
      (car (s-split "==" str))
    str))

(defun pip-homepage (&optional package)
  "Open the pypi homepage of the given package, or the one at point, or one we choose from a pip freeze."
  (interactive)
  (let* ((package (or package (thing-at-point 'symbol)))
         ;; Prompt with thing at point as default.
         (package (and package (if (s-blank? (read-from-minibuffer
                                              (format "Package? [%s]"
                                                      (pip--clean-symbol package))))
                                   package))) ;; selecting the default choice between brackets returns ""
         ;; otherwise, completion with all packages.
         (package (or package (ido-completing-read "Package ?" (pip--get-all-packages))))
         ;; remove "=="
         (package (and package (pip--clean-symbol package))))
    (browse-url-xdg-open (pip--pypi-url package))))

(defun pip-doc-popup ()
  ;TODO:
  "Display the presentation of the package (from pypi) at point in a pop-up."
  (interactive)
  ;; goal: get the html source, insert in temp buffer, parse the html of this buffer.
  (with-temp-buffer "pip-parser"
  ;; (set-buffer "pip-parser")
    (insert (with-current-buffer (url-retrieve-synchronously "http://Pypi.python.org/pypi/django-sampledatahelper")
              ;; returns error bad request.
      (progn
        (buffer-string)
        ;; (kill-buffer)
        ))
)
    (libxml-parse-html-region (point-min) (point-max))
)
)

(defhydra pip-utils-hydra (:color blue :columns 4)
  "
Pip utils. venv: %`venv-current-name "
  ("i" (pip-install) "Install a package in current venv")
  ("I" (call-interactively 'pip-install-add-to-requirements) "Install and add in requirements.txt")
  ("r" (pip-install-requirements) "-r requirements")
  ("u" (pip-uninstall) "uninstall")
  ("v" (pip-package-version) "Get package version")
  ("h" (pip-homepage) "Open the homepage in the browser")
  ("w" (venv-workon) "Workon venv…" :color red)
  )

;;; pip-utils.el ends here
