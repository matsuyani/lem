; (defpackage #:lem-treesitter-mode
;   (:use :cl :lem :cl-treesitter))

(ql:quickload :alexandria)
(ql:quickload :cl-treesitter)



; 1. Loading a treesitter
;   - fetching & compiling a treesitter lib if it's not found locally
;   - loading a treesitter lib from a particular directory (lem cache?)
;     - do this on-demand when we enter a particular dir?

; How does neovim do this?
; - mason (no, this is for LSPs)
; - nvim-treesitter

; function! nvim_treesitter#installable_parsers(arglead, cmdline, cursorpos) abort
; return join(luaeval("require'nvim-treesitter.parsers'.available_parsers()") + ['all'], "\n")
; endfunction
; 
; function! nvim_treesitter#installed_parsers(arglead, cmdline, cursorpos) abort
; return join(luaeval("require'nvim-treesitter.info'.installed_parsers()") + ['all'], "\n")
; endfunction


  ; (:export))


(in-package :lem-user)

(defvar *lem-cache-dir* (uiop:parse-native-namestring (uiop:native-namestring "~/.local/share/lem") :ensure-directory t))
(setf *lem-cache-dir* (uiop:parse-native-namestring (uiop:native-namestring "~/.local/share/lem") :ensure-directory t))
(ensure-directories-exist *lem-cache-dir* :verbose t :mode 755)

(defvar *lem-treesitter-library* (uiop:merge-pathnames* *lem-cache-dir* "treesitter/compiled"))
(defvar *lem-treesitter-source* (uiop:merge-pathnames* *lem-cache-dir* "treesitter/src"))

(defparameter *treesitter-locs*
'(:c_sharp
      (:install_info (:url "https://github.com/tree-sitter/tree-sitter-c-sharp"
                      :files ("src/parser.c" "src/scanner.c"))
            :filetype "cs"
            :maintainers ("@Luxed"))))

(defun make-temporary-directory ()
  (let ((tmpdir (make-pathname :directory (append (pathname-directory (uiop:temporary-directory)) 
                                                  '("lem-treesitter")))))
    (ensure-directories-exist tmpdir :verbose t :mode 755)))

(defun get-ts-repo (ts-loc)
  (getf (getf ts-loc :install_info) :url))

(defun get-ts-files (ts-loc)
  (getf (getf ts-loc :install_info) :files))

(get-ts-repo (getf *treesitter-locs* :c_sharp))
(get-ts-files (getf *treesitter-locs* :c_sharp))

(string-downcase (symbol-name :c_sharp))
(defun fetch-source (lang)
  "Fetches the source files for a particular ts-loc
     TODO: typecheck lang, needs to be a keyword symbol
     TODO: throw if we don't get a lang-spec successfully?
     TODO: make this thread safe? see uiop:with-current-directory
     TODO: make output stream somehow, to show in a lem status bar?
     TODO: actually get repo name somehow
"
  (let* ((lang-str (string-downcase (symbol-name lang)))
         (lang-spec (getf *treesitter-locs* lang))
         (repo (getf (getf lang-spec :install_info) :url))
         (files (getf (getf lang-spec :install_info) :files))
         (tmpdir (make-temporary-directory))
         (lang-ts-src-dir (make-pathname :directory (append (pathname-directory *lem-treesitter-source*)
                                                            (list lang-str))))
         (lang-ts-src-dir-str (format nil "/~{~A~^/~}" (cdr (pathname-directory lang-ts-src-dir))))
         (repo-name "tree-sitter-c-sharp"))
    (ensure-directories-exist lang-ts-src-dir :verbose t)
    (uiop:with-current-directory ((make-temporary-directory))
      ;(uiop:run-program `("git" "clone" ,repo) :output t)
      (uiop:with-current-directory (repo-name)
        (uiop:run-program `("cp" "-v" ,@files ,lang-ts-src-dir-str) :output t)
      
  ))))

(fetch-source :c_sharp)

(uiop:with-current-directory ((make-temporary-directory))
  (uiop:run-program "ls" :output t))



; "~/.local/share/lem/treesitter/compiled/"
; "~/.local/share/lem/treesitter/src/<lang>" ; e.g. c_sharp
;(uiop:run-program '("echo" "foo" "bar") :output t)
; (uiop:run-program '("echo" "foo" "bar") :output t)