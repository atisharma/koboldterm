(require hyrule.argmove [-> ->>])
(require hyrule.control [unless])

(import koboldterm.api)


(setv generate-args {})

(setv help-str
      "
      To add to the story, just enter the text.
      Lines beginning with [bold]/[/bold] will be parsed as commands.

  [bold]Commands:[/bold]

      [bold]/help /h[/bold]                   show this helpful text
      [bold]/quit /q /exit[/bold]             quit
      [bold]/version[/bold]                   show the version of this client

      [bold]/undo[/bold]                      delete the last action
      [bold]/retry[/bold]                     synonym for [bold]/undo[/bold] then [bold]/generate[/bold]

      [bold]/add 'text'[/bold]                add text to the story without generating text
      [bold]/append /a[/bold]                 synonyms of add
      [bold]/more /generate /g[/bold]         generate more story without adding anything yourself

      [bold]/recap 'n'[/bold]                 display the last 'n' turns (or the whole story without 'n')
      [bold]/last[/bold]                      synonym for [bold]/recap 1[/bold]
      [bold]/print /p /story[/bold]           synonyms for [bold]/recap[/bold]

      [bold]/save 'fname'[/bold]                save to story under the name 'fname'
      [bold]/load 'fname'[/bold]                load a story from a previous save

      [bold]/delete! /reset![/bold]           delete the whole story!
      [bold]/server-version[/bold]            show the server version
      [bold]/server[/bold]                    show (or set) the server in 'ip:port' format
      [bold]/model 'model-name'[/bold]        show (or set) the current model
      [bold]/config <setting> <value>[/bold]  get or set a setting

      ")


(defn format-story [story [separator " "]]
  """
  Format the story list-of-dicts.
  """
  (.join separator
         (list (map (fn [l] (.strip (:text l "<text field is missing>")))
                    story))))

(defn trim [story-str]
  """
  Remove last incomplete sentence.
  """
  (+ (.join "."
            (get (.split story-str ".") (slice 0 -1)))
     "."))
  
(defn deblank [story-str]
  (-> story-str
    (.replace "\n\n\n" "\n\n")
    (.strip))) 

(defn recap [[n None]]
  """
  Return the last `n` turns of the story as a formatted string.
  Return all of them if `n` is None or omitted.
  """
  (try
    (let [ns        (api.story "/nums")
          chunk-ns  (get ns (slice (- (int n)) None))
          chunks    (lfor j chunk-ns (api.story f"/{j}"))]
      (format-story chunks "\n\n"))
    (except [ValueError]
            (format-story (api.story) "\n\n"))))

(defn generate []
  (let [story (trim (format-story (api.story)))
        new-text (-> story
                     (api.generate #**generate-args)
                     (deblank)
                     (trim))]
    (api.end new-text) 
    (+ new-text "\n")))

(defn take-turn [line]
  (api.end line)
  (generate))

(defn parse [line]
  """
  Do the action implied by `line`, and return the result as a string.
  """
  (setv [command _ args] (.partition line " "))
  (or (match command
             "/generate" (generate)
             "/g" (parse "/generate")
             "/more" (parse "/generate")
             "/undo" (api.end :delete True)
             "/retry" (do (parse "/undo") (parse "/generate"))
             "/delete!" (or (api.story :delete True) "Story deleted.")
             "/reset!" (parse "/delete")
             "/help" help-str
             "/h" help-str
             "/server-version" (api.server-version)
             "/server" (api.set-server args)
             "/model" (or (api.model args) "Loaded.")
             "/save" (or (api.save args) "Saved.")
             "/load" (or (api.load args) "Loaded.")
             "/add" (api.end (+ args "\n"))
             "/append" (api.end (+ args "\n"))
             "/a" (api.end (+ args "\n"))
             "/recap" (recap args)
             "/story" (recap args)
             "/print" (recap args)
             "/p" (recap args)
             "/last" (recap 1)
             "/config" (let [[setting _ value] (.partition args " ")]
                         (str (api.config setting :value value))))
      (when line
        (unless (.startswith line "/")
               (take-turn line)))))

