;;; pip-utils.el: utility functions to run pip in current project
;;; https://gitlab.com/emacs-stuff/pip-utils
;;; author: vindarel
;;; licence: wtf public licence

(require 'virtualenvwrapper)
(require 'f)
(require 'hydra)
(require 's)

(defun pip-utils-package-version (&optional package)
  "get the package version number with 'pip show package' and a
bit of processing."
  (let* ((package (or package
                     (read-from-minibuffer "Package? ")))
         ;; would be more robust if pip add a --json flag, like npm.
         (pip-output (s-split "\n" (shell-command-to-string (format "pip show %s" package))))
         ;; for --map, see its doc: https://github.com/magnars/dash.el "it" is the list item.
         (pip-output-list (--map (s-split ":" it) pip-output))
         (version (s-trim (nth 1 (nth 2 pip-output-list)))))
    (message (format "%s v%s" package version))
    version))

(defun pip-install (package &optional add-to-requirements)
  "install package with pip in the right virtual env.
   Use venv-workon to change it.
   Add the package to the requirements.txt file (works for Django projects).
  "
  ;; TODO: (ido) completing read with the current venv as default
  ;; and an option for a global install.
  ;; TODO: word at point as suggestion. see npm.
  ;; TODO: set the version.
  (interactive (list (read-from-minibuffer (format "Package? (in venv %s) "
                                                   venv-current-name))))
  (message "installing %s in venv %s" package venv-current-name)
  (compile (concat "pip install " package))
  ;; if compile fails, nothing's added.
  (if add-to-requirements
      (progn
        (append-to-file (format "\n%s" package) nil (pip--get-requirements-file))) ;to finish
    )
  )

(defun pip-install-add-to-requirements (package)
  "Install the package and add it to the project's requirements file."
  (interactive "sPackage ? ")
  (call-interactively (pip-install package t)) ; to finish
  )

(defvar pip-requirements-file "requirements.txt"
  "name of the requirements file to look for at the project root and at root/<project-name>/")

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
  (setq pip-packages-list (mapcar (lambda (x)
                                    (unless (string-equal x "")
                                      (s-wrap x "\""))) pip-packages-list))
)

(defun pip--install-requirements (pip--requirements-project)
  "Ask to install the packages in the given file and run it in a compilation process."
  (if (yes-or-no-p (format "install packages from %s in venv %s ?" pip--requirements-project venv-current-name))
      (progn
        (message "go, install")
        (setq pip-packages-list (pip-get-packages-list pip--requirements-project))
        ;; install everything:
        (compile (concat "pip install " (s-join " " pip-packages-list)))
        )
    )
)

(defun pip--get-requirements-file ()
  "Works for Django projects.
  "
  (concat (projectile-project-root) (projectile-project-name) "/requirements.txt")
)

(defun pip-install-requirements ()
  "Install packages from requirements.txt. Looks for a
  'requirements.txt' file at the project root and in the
  root/<project-name>/ (useful for django projects)."
  (interactive)
  (let ((pip--requirements-project (pip--get-requirements-file))
        (pip--requirements-root (concat (projectile-project-root) "requirements.txt") ))
    (if (file-exists-p pip--requirements-project)
        (pip--install-requirements pip--requirements-project)
      (message "no requirements file in root/project dir."))
    (if (file-exists-p pip--requirements-root)
        (pip--install-requirements pip--requirements-root)
        (message "no requirements at root"))
    ))

;; pip install in venv
  ;; venv-workon foo  ;; see venv-current-name may be nil
  ;; (compile pip install truc
  ;; and add in requirements and installed apps (asking)
(defun pip-workon (env)
  "Prompt with a default option."
  ;; use ido-completing-read to suggest venvs (or just use venv function)
  ;; exple: http://www.lunaryorn.com/2014/07/18/ansible-docs-in-emacs.html
  (interactive (list (read-from-minibuffer (concat "Workon ["
                                                   (s-join " " (venv-get-candidates))
                                                   "]: "))
                     ))
  (message env)
  )

(defun pip--pypi-url (package)
  "Takes a package and returns its pypi url."
  (format "https://pypi.python.org/pypi/%s" package)
)

(defun pip-homepage (&optional package)
  "Open the pypi homepage of the given package, or the one at point."
  (interactive)
  (let* ((package (or package (read-from-minibuffer (format "Package? [%s]"
                                                           (thing-at-point 'word)))))
         (package (or package (thing-at-point 'word))))
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
  ("v" (pip-utils-package-version) "Get package version")
  ("w" (venv-workon) "Workon venv…" :color red)
  )

;;; pip-utils.el ends here
