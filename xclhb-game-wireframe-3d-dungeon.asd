(defsystem "xclhb-game-wireframe-3d-dungeon"
  :version "0.1"
  :author "yoshida koji"
  :license "MIT"
  :depends-on ("xclhb" "opticl")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "window")
               (:file "draw")
               (:file "wire-frame")
               (:file "game")
               (:file "player")
               (:file "items")
               (:file "enemies")
               (:file "dungeon")
               (:file "main")))
