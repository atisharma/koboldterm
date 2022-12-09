(require hyrule.argmove [-> ->>])

(import hyrule.iterables [flatten])

(import requests)
(import requests.exceptions [HTTPError JSONDecodeError])


(setv server "localhost:5000")


(defn set-server [[new-server None]]
  (global server)
  (when new-server (setv server new-server))
  server)

(defn base-url []
  f"http://{server}/api/v1")

(defn format-response [response]
  (let [error-str f"[red]{response.status-code}: {response.reason}"]
    (try
      (let [data (response.json)]
        (cond (in "result" data)   (:result data)
              (in "results" data)  (:results data)
              (in "value" data)    (:value data)
              (in "detail" data)   (+ "[red]" (-> data :detail (:msg error-str)))
              :else               data))
      (except [JSONDecodeError]
              error-str))))

(defn get-endpoint [endpoint]
  (-> (base-url)
      (+ endpoint)
      (requests.get)
      (format-response)))

(defn post-endpoint [endpoint payload]
  (-> (base-url)
      (+ endpoint)
      (requests.post :json payload)
      (format-response)))

(defn put-endpoint [endpoint payload]
  (-> (base-url)
      (+ endpoint)
      (requests.put :json payload)
      (format-response)))

(defn delete-endpoint [endpoint]
  (try
    (setv data {})
    (setv response (requests.delete (+ (base-url) endpoint)))
    (setv data (response.json))
    (response.raise-for-status)
    None
    (except [e HTTPError]
            (str {#**data "error" e}))
    (except [e JSONDecodeError]
            (str response))
    (except [e Exception]
            (str {#**data "error" e}))))

(defn server-version []
  (get-endpoint "/info/version"))

(defn generate [prompt #**kwargs]
  ; should get, sanitise, and then put the last generated text.
  (let [response (post-endpoint "/generate" {"prompt" prompt #**kwargs})]
    (try
      (get response 0 "text")
      (except [KeyError]
        (str response)))))

(defn model [[model None]]
  (if model
    (put-endpoint "/model" :payload {"model" model})
    (get-endpoint "/model")))

(defn config [setting [value None]]
  (if value
    (put-endpoint (+ "/config/" setting) {"value" value})
    (get-endpoint (+ "/config/" setting))))
    
(defn world-info [setting [uid None] [value None]]
  (if uid
    (put-endpoint (+ "/world_info/" uid "/" setting) {"value" value})
    (get-endpoint (+ "/world_info/" uid "/" setting))))

(defn story [[endpoint None] * [delete None]]
  (cond delete (delete-endpoint "/story")
        endpoint (get-endpoint (+ "/story" endpoint))
        :else (get-endpoint "/story")))

(defn end [[action None] * [delete None]]
  (cond delete (post-endpoint "/story/end/delete" {})
        action (post-endpoint "/story/end" {"prompt" action})
        :else (get-endpoint "/story/end")))

(defn load [fname]
  (put-endpoint "/story/load" {"name" fname}))

(defn save [fname]
  (put-endpoint "/story/save" {"name" fname}))

