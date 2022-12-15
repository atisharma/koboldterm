(import os)
(import re)
(import itertools [filterfalse])

(require hyrule.argmove [-> ->> as->])
(require hyrule.control [unless])

(import koboldterm [api version])


(setv generate-args {})

(setv help-str
      "
      To add to the story, just enter some text.

      Lines beginning with [bold]/[/bold] are parsed as commands.
      In adventure mode, lines beginning with [bold]>[/bold] are parsed as actions.
      The usual readline shortcuts should be available.

  [bold]Commands:[/bold]

      [bold]/help /h[/bold]                   Show this helpful text
      [bold]/quit /q /exit[/bold]             Quit
      [bold]/version[/bold]                   Show the version of this client
      [bold]/clear[/bold]                     Clear the display

      [bold]/undo[/bold]                      Delete the last action
      [bold]/retry[/bold]                     Synonym for [bold]/undo[/bold] then [bold]/generate[/bold]

      [bold]/add 'text'[/bold]                Add text to the story without generating text
      [bold]/append /a[/bold]                 Synonyms of add
      [bold]/more /generate /g[/bold]         Generate more story without adding anything yourself

      [bold]/start dirname[bold]              Set the prompt, notes, memory and world info from a directory 'dirname'.
      [bold]/prompt dirname[/bold]            Adds story text from a file called 'prompt' under the directory
      [bold]/note dirname[/bold]              Set author's note from a file called 'note' saved under the directory
      [bold]/memory dirname[/bold]            Set the story memory from a file called 'memory' saved under the directory
      [bold]/world dirname[/bold]             Set the world info from all files saved under 'dirname/world'
      [bold]/world delete[/bold]              Delete all world info

      [bold]/recap 'n'[/bold]                 Display the last 'n' turns (or the whole story)
      [bold]/last /l[/bold]                   Synonym for [bold]/recap 1[/bold]
      [bold]/print /story /p[/bold]           Synonyms for [bold]/recap[/bold]

      [bold]/save 'fname'[/bold]              Save to story under the name 'fname' (on the server)
      [bold]/load 'fname'[/bold]              Load a story from a previous save (on the server)

      [bold]/delete! /reset![/bold]           Delete the whole story!
      [bold]/server-version[/bold]            Show the server version
      [bold]/server[/bold]                    Show (or set) the server in 'ip:port' format
      [bold]/model 'model-name'[/bold]        Show (or set) the current model
      [bold red]/preset <fname>            Load a (json) settings file, 'fname'.settings from the 'presets' directory (not implemented)[/bold red]
      [bold]/config <setting> <value>[/bold]  Get or set a setting

      [bold]TL;DR[/bold] type [bold]/start dirname[/bold] where dirname is a directory.
      ")


(defn sbf [linelist]
  """
  `linelist` is a list of strings.
  Put each line in square bracket format.
  Remove blank lines. Return as one string.
  """
  (as-> linelist lines
      (filterfalse str.isspace lines)
      (filter None lines)
      (.join " ] [ " lines) 
      (.replace lines "\n" "")
      (+ "[ " lines " ]")))

(defn format-story [story [separator " "]]
  """
  Format the story list-of-dicts.
  """
  (try
    (.join separator
           (list (map (fn [l] (.strip (:text l "<text field is missing>")))
                      story)))
    (except [ValueError TypeError]
            story)))

(defn deblank [story-str]
  (-> story-str
      (.replace "<|endoftext|>" "\n")
      (.replace "\n\n\n" "\n\n")
      (.replace "\n\"\"" "")
      (.strip))) 

(defn trim [story-str]
  """
  Remove last incomplete sentence.
  """
  (let [sentences (re.split "([\\.\"\\?]+\\s)" story-str)
        last-sentence (get sentences -1)
        last-char (-> last-sentence (.strip) (get -1))
        leading-sentences (.join "" (get sentences (slice 0 -1)))]
    (+ leading-sentences
       (if (in last-char "\".?")
         last-sentence
         ""))))

(defn close-quotes [story-str]
  """
  If there is an odd number of quotes in a line, close the quote.
  """
  (.join "\n"
    (lfor line (-> story-str (.replace "\"\"" "\"") (.splitlines))
          (if (% (.count line "\"") 2)  ; if an odd number of quotes
            (if (= (get line -1) "\"")  ; if ends in a quote
              (+ "\"" line)             ; close at start
              (+ line "\""))            ; close at end
            line))))
  
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
    (except [[ValueError TypeError]]
            (format-story (api.story) "\n\n"))))

(defn generate []
  (let [story (trim (format-story (api.story)))
        new-text (-> story
                     (api.generate #**generate-args)
                     (deblank)
                     (trim)
                     (close-quotes))]
    (api.end new-text) 
    new-text))

(defn world [dirname]
  """
  Load any world info files saved under `dirname/world/`.
  The format is per line. The first line is a comment. The second line contains keywords, separated by spaces.
  The rest is descriptive text.
    `descriptive title
     keyword1, keyword2, keyword3
     Some text here, each line should stand separately.
     Some more text.
    `
  """
  ; TODO: move some of this to api module
  ; TODO: maybe use folders
  (cond
    (= dirname "delete") (+ "[italic]World info with uid"
                            (.join ", "
                                   (lfor uid (:entries (api.get-endpoint "/world_info/uids") [])
                                        (str (or (api.delete-endpoint f"/world_info/{uid}") f"{uid}"))))
                            " deleted.[/italic]")
    dirname (if (os.path.isdir f"{dirname}/world")
              (+ "[italic]World info "
                 (.join ""
                        (lfor fname (os.listdir f"{dirname}/world")
                              (try
                                (with [f (open f"{dirname}/world/{fname}" "r")]
                                  (let [wi (f.readlines)
                                        uid (api.post-endpoint "/world_info/folders/none" {})
                                        comment (.strip (.pop wi 0))
                                        key (.strip (.pop wi 0))]
                                    (api.put-endpoint f"/world_info/{uid}/comment" {"value" comment})
                                    (api.put-endpoint f"/world_info/{uid}/key" {"value" key})
                                    (api.put-endpoint f"/world_info/{uid}/content" {"value" (sbf wi)}))
                                  f".")
                                (except [FileNotFoundError]
                                        f"\n[red]No world info file: {dirname}/world/{fname}, not set[/red]\n"))))
                 " loaded.[/italic]")
              f"[red]No world info directory: {dirname}/world[/red]")
    :else (api.get-endpoint "/world_info")))

(defn note [dirname]
  (try
    (with [f (open f"{dirname}/note" "r")]
      (api.config "authors_note" :value (sbf (f.readlines)))
      f"[italic]Author's note '{dirname}' loaded.[/italic]")
    (except [FileNotFoundError]
            f"[red]No such file: {dirname}/note, authors_note not set[/red]")))

(defn memory [dirname]
  (try
    (with [f (open f"{dirname}/memory" "r")]
      (api.config "memory" :value (sbf (f.readlines)))
      f"[italic]Story memory '{dirname}' loaded.[/italic]")
    (except [FileNotFoundError]
            f"[red]No such file: {dirname}/memory, memory not set[/red]")))

(defn prompt [dirname]
  (try
    (with [f (open f"{dirname}/prompt" "r")]
      (let [p (f.read)]
        (api.end p)
        p))
    (except [FileNotFoundError]
            f"[red]No such file: {dirname}/prompt, prompt not set[/red]")))

(defn start [dirname]
  """
  Load prompt, author's note and memory.
  Also load any world info files available in the 'world' subdirectory.
  """
  (if dirname
    (.join "\n"
      [(world dirname)
       (memory dirname)
       (note dirname)
       "\n"
       (prompt dirname)])
    "[red]Please specify a directory name.[/red]"))

(defn preset [fname]
  """
  Load a json file of settings.
  """
  (import json)
  (try
      (with [f (open f"presets/{fname}.settings" "r")]
        (+ "[italic]Loading presets from 'presets/{fname}.settings':\n"
          (.join "\n"
            (lfor [k v] (.items (json.load f))
                  (cond (isinstance v dict) (.join "\n"
                                                   (lfor [k2 v2] (.items v)
                                                         (+  f"\t[/italic]{k2}: {v2}[italic]  ...  "
                                                            (if (api.config f"{k2}" :value (str v))
                                                              "[red](failed)[/red]"
                                                              "(set)"))))
                        (isinstance k str) (+ f"\t[/italic]{k}: {v}[italic]  ...  "
                                              (if (api.config (str k) :value v)
                                                "[red](failed)[/red]"
                                                "(set)"))
                        :else f"[red]Cannot interpret {k}: {v}[/red]")))
          "\n[/italic]"))
      (except [FileNotFoundError]
              f"[red]No such settings file: presets/{fname}.settings[/red]")))
  
(defn take-turn [line]
  (-> line
    (.replace ">" "\n\n>")
    (+ "\n")
    (close-quotes)
    (api.end))
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
            "/delete!" (or (api.story :delete True) "[italic]Story reset.[/italic]")
            "/reset!" (parse "/delete!")
            "/help" help-str
            "/h" help-str
            "/version" (version "koboldterm")
            "/server-version" (api.server-version)
            "/server" (api.set-server args)
            "/model" (or (api.model args) "[italic]Loaded.[/italic]")
            "/save" (or (api.save args) "[italic]Saved.[/italic]")
            "/load" (or (api.load args) "[italic]Loaded.[/italic]")
            "/add" (api.end (+ args "\n"))
            "/append" (api.end (+ args "\n"))
            "/a" (api.end (+ args "\n"))
            "/recap" (recap args)
            "/story" (recap args)
            "/print" (recap args)
            "/p" (recap args)
            "/last" (recap 1)
            "/l" (recap 1)
            "/prompt" (prompt args)
            "/memory" (memory args)
            "/note" (note args)
            "/world" (world args)
            "/start" (start args)
            "/preset" (preset args)
            "/config" (let [[setting _ value] (.partition args " ")]
                        (if value
                          (-> (api.config setting :value value)
                              (or  f"[italic]{setting} set.[/italic]"))
                          (-> (api.config setting)
                              str
                              (.replace  "[" "\\[")))))
      (if (.startswith line "/")
        f"[red]Unknown command: {line}[/red]"
        (take-turn (+ "\n" line "\n\n")))))
