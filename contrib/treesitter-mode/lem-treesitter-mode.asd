(defsystem "lem-treesitter-mode"
  :depends-on ("lem" "cl-treesitter" "cffi-toolchain" "pathname-utils")
  ; :depends-on ("cl-treesitter" "cffi-toolchain" "pathname-utils")
  :components ((:file "scratch")))
