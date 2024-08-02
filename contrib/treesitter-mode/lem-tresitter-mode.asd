(defsystem "lem-treesitter-mode"
  :serial t
  :depends-on (:lem :cl-treesitter)
  :components ((:file "scratch")))
