(defpackage :lem-treesitter-mode
  (:use :cl :lem)
  ; (:shadow #:point #:cursor)
  (:export
   #:load-treesitter))

; (defpackage :lem-treesitter-mode
;   (:use :cl :lem))
(in-package :lem-treesitter-mode)
; 1. Loading a treesitter
;   - fetching & compiling a treesitter lib if it's not found locally
;   - loading a treesitter lib from a particular directory (lem cache?)
;     - do this on-demand when we enter a particular dir?
; 
; How does neovim do this?
; - mason (no, this is for LSPs)
; - nvim-treesitter
; 
; function! nvim_treesitter#installable_parsers(arglead, cmdline, cursorpos) abort
; return join(luaeval("require'nvim-treesitter.parsers'.available_parsers()") + ['all'], "\n")
; endfunction
; 
; function! nvim_treesitter#installed_parsers(arglead, cmdline, cursorpos) abort
; return join(luaeval("require'nvim-treesitter.info'.installed_parsers()") + ['all'], "\n")
; endfunction
; 
; 
; (:export))

(defvar *lem-cache-dir* (uiop:parse-native-namestring (uiop:native-namestring "~/.local/share/lem") :ensure-directory t))

(ensure-directories-exist *lem-cache-dir* :verbose t :mode #o755)

(defun join-directory (base-directory-pathname &rest sub-directory-path)
  (make-pathname :directory (append (pathname-directory base-directory-pathname)
                                    sub-directory-path)))

(defun join-pathnames (base-pathname adjoining-pathname)
  (let* ((adjoining-directory (rest (pathname-directory adjoining-pathname)))
         (adjoining-file (pathname-name adjoining-pathname))
         (adjoining-type (pathname-type adjoining-pathname)))
    (make-pathname :directory (append (pathname-directory base-pathname) adjoining-directory)
                   :name adjoining-file
                   :type adjoining-type)))

(defvar *lem-treesitter-library* (join-directory *lem-cache-dir* "treesitter" "compiled"))
(ensure-directories-exist *lem-treesitter-library* :verbose t :mode #o755)


(defparameter *treesitter-locs*
  '(:c_sharp
    (:install_info (:url "https://github.com/tree-sitter/tree-sitter-c-sharp"
                    :files ("src/parser.c" "src/scanner.c"))
     :filetype "cs"
     :maintainers ("@Luxed"))
    :c
    (:install_info (:url "https://github.com/tree-sitter/tree-sitter-c"
                    :files ("src/parser.c"))
     :maintainers ("@amaanq"))
    :rust
    (:install_info (:url "https://github.com/tree-sitter/tree-sitter-rust"
                    :files ("src/parser.c" "src/scanner.c"))
     :maintainers ("@amaanq"))
    :bash
    (:install_info (:url "https://github.com/tree-sitter/tree-sitter-bash"
                    :files ("src/parser.c" "src/scanner.c"))
     :filetype "sh"
     :maintainers ("@TravonteD"))))

(defun make-temporary-directory ()
  (let ((tmpdir (make-pathname :directory (append (pathname-directory (uiop:temporary-directory)) 
                                                  '("lem-treesitter")))))
    (ensure-directories-exist tmpdir :verbose t :mode #o755)))

(defun get-ts-repo (ts-loc)
  (getf (getf ts-loc :install_info) :url))

(defun get-ts-files (ts-loc)
  (getf (getf ts-loc :install_info) :files))

(defun fetch-source (lang cache-pathname)
  "Fetches the source files for a particular ts-loc
     TODO:
     [ ]: typecheck lang, needs to be a keyword symbol
     [ ]: throw if we don't get a lang-spec successfully?
     [x]: make this thread safe? see uiop:with-current-directory [refactored to not use uiop:with-current-directory]
     [ ]: make output stream somehow, to show in a lem status bar? or something?
     [x]: actually get repo name somehow [Decided to clone to lang-str]
     [x]: handle instance where dir already exists? [see download-source]
     [x]: cross-platform copy of source files [see copy-to-dir]
     [ ]: get source tarball and extract instead of cloning?

     Returns the relative path to the source repository.
"
  (let* ((lang-spec (getf *treesitter-locs* lang))
         (lang-str (string-downcase (symbol-name lang)))
         (repo (getf (getf lang-spec :install_info) :url))
         (local-repo-pathname (join-directory cache-pathname lang-str))
         (local-repo-namestring (pathname-utils:native-namestring local-repo-pathname)))
    (if (uiop:directory-exists-p local-repo-pathname)
        (format t "Directory ~a already exists. Refusing to clone.~%" local-repo-pathname)
        (uiop:run-program `("git" "clone" ,repo ,local-repo-namestring) :error-output t :output t))
    local-repo-pathname))

(defun compile-lang (lang cache-pathname)
  "compiles treesitter language object and copies it to *lem-treesitter-library*"
  (let* ((lang-str (string-downcase (symbol-name lang)))
         (lang-spec (getf *treesitter-locs* lang))
         (local-repo-pathname (join-directory cache-pathname lang-str))
         (input-files (getf (getf lang-spec :install_info) :files))
         (output-file (uiop:merge-pathnames* *lem-treesitter-library* 
                                             (format nil "libtree-sitter-~a.so" lang-str)))
         (input-files-absolute (mapcar (lambda (file) 
                                         (join-pathnames local-repo-pathname 
                                                         (uiop:parse-native-namestring file)))
                                       input-files)))
    (format t "~a -> ~a~%" input-files-absolute output-file)
    (cffi-toolchain:link-shared-library output-file input-files-absolute)))

(defun install-lang (lang)
  "Attempts to fetch and install the treesitter for a particular language"
  (let* ((tmpdir (make-temporary-directory))) 
    (fetch-source lang tmpdir)
    (compile-lang lang tmpdir)))


(defun ts-installed-p (lang)
  "Checks if a treesitter language object exists in *lem-treesitter-library*"
  (let* ((lang-str (string-downcase (symbol-name lang)))
         (lang-so-name (format nil "libtree-sitter-~a.so" lang-str))
         (lang-so-filepath (uiop:merge-pathnames* *lem-treesitter-library* lang-so-name)))
    (uiop:file-exists-p lang-so-filepath)))

(defun ensure-language-treesitter-installed (lang)
  "Determines if the treesitter library for the given language is installed. Installs it if it is not found."
  (unless (ts-installed-p lang)
    (install-lang lang)))

(defvar *tree-sitters* (make-hash-table :test #'equal))

; This needs to be a macro, otherwise
; (treesitter:include-language lang-str ...) will try and load a library named "libtree-sitter-lang-str.so"
; so, I think I understand why it's like this
(defmacro load-treesitter (lang &key (ensure-installed t))
  (let ((lang-str (string-downcase (symbol-name lang))))
    `(progn
      (when ,ensure-installed
        (ensure-language-treesitter-installed ,lang))
      (treesitter:include-language ,lang-str :search-path *lem-treesitter-library*)
      (setf (gethash ,lang *tree-sitters*) (treesitter:make-language ,lang-str)))))

;(load-tree-sitter :c_sharp)

;(let ((parser (treesitter:make-parser :language (gethash :c_sharp *tree-sitters*))))
;  (treesitter:node-string
;   (treesitter:tree-root-node
;    (treesitter:parser-parse-string parser "int foo() {return 0;}"))))


; these will also work:
; (load-tree-sitter :c)
; (load-tree-sitter :rust)
; (load-tree-sitter :bash)