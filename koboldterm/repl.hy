(require hyrule.argmove [-> ->> as->])

(import rich.console [Console])
(import rich.padding [Padding])

(import koboldterm [parser])


(setv console (Console :highlight None)
      margin 1)


(defn banner []
  (console.clear)
  (setv banner-text r"
  █  █▀ ████▄ ███   ████▄ █     ██▄   ██   ▄█ :
  █▄█   █   █ █  █  █   █ █     █  █  █ █  ██ :
  █▀▄   █   █ █ ▀ ▄ █   █ █     █   █ █▄▄█ ██ :
  █  █  ▀████ █  ▄▀ ▀████ ███▄  █  █  █  █ ▐█ :
    █         ███             ▀ ███▀     █  ▐ :
   ▀                                    █     :
                                       ▀      :
:")
  (lfor [l c] (zip (.split banner-text ":")
                   ["#FF0000" "#DD0000" "#BB0000" "#990000" "#770000" "#550000" "#330000" "#220000"])
        (console.print l
                       :end None
                       :style f"bold {c}"
                       :overflow "crop")))

(defn set-width [line]
  (try
    (let [arg (get (.partition line " ") 2)]
      (global console)
      (setv console (Console :highlight None :width (int arg)))
      "")
    (except [[IndexError ValueError]]
      "[red]Bad console width value.[/red]")))
    
(defn set-margin [line]
    (let [arg (get (.partition line " ") 2)]
      (global margin)
      (if (.isnumeric arg)
        (do (setv margin (int arg)) "")
        "[red]That needs to be a number.[/red]")))
    

(defn run []
  (banner)
  ; the repl
  (try
    (while True
      ; Console.input has a bug that can obliterate the prompt
      (let [line (.strip (input "... "))]
        (cond (or (.startswith line "/q") (.startswith line "/exit")) (break)
              (.startswith line "/clear") (console.clear)
              (.startswith line "/width") (console.print (set-width line))
              (.startswith line "/margin") (console.print (set-margin line))
              :else (-> (parser.parse line)
                        (or  "")
                        (Padding  #(0 margin 1 margin))
                        (console.print :justify "left")))))
    (except [EOFError]))
  (print "Bye!"))

