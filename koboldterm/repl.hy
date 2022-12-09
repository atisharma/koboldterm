(import rich.console [Console])

(import koboldterm [parser])


(setv console (Console :highlight None))


(defn banner []
  (console.clear)
  (setv banner-text r"
      __ __     __        __   _____   ____:
     / //_/__  / /  ___  / /__/ / _ | /  _/:
    / ,< / _ \/ _ \/ _ \/ / _  / __ |_/ /  :
   /_/|_|\___/_.__/\___/_/\_,_/_/ |_/___/  :
:
:")
  (lfor [l c] (zip (.split banner-text ":")
                   ["#FF0000" "#DD0000" "#BB0000" "#990000" "#770000" "#550000" "#330000" "#220000"])
        (console.print l
                       :end None
                       :style f"bold {c}"
                       :overflow "crop")))

(defn run []
  (banner)
  ; the repl
  (while True
    ; Console.input has a bug that can obliterate the prompt
    (let [line (.strip (input "> "))]
      (cond (or (.startswith line "/q") (.startswith line "/exit")) (do (print "Bye!") (break))
            (.startswith line "/clear") (console.clear)
            :else (console.print (or (parser.parse line) "")
                                 :justify "left")))))

