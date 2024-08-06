(defsystem "lem-treesitter-mode"
  :serial t
  :depends-on ("lem" "cl-treesitter" "cffi-toolchain")
  :components ((:file "scratch")))
