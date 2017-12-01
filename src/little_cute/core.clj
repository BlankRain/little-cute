(ns little-cute.core
  (:require [catacumba.core :as ct]
            [clojure.xml :as xml]
            [catacumba.handlers.parse :as parse]
            [catacumba.handlers.misc :as misc]
            [catacumba.plugins.prone :as prone]
            [clojure.tools.logging :as logger]
            [catacumba.http :as http])
  (:import [com.qq.weixin.mp.aes WXBizMsgCrypt])
  (:gen-class))

(import 'ratpack.http.TypedData
        'ratpack.handling.Context)

(defn- parse-msg-seq [msg-xml-str]
  (xml-seq 
    (xml/parse 
      (java.io.ByteArrayInputStream. 
        (.getBytes msg-xml-str "utf8"))))) 
    
(defn the-only-value-in-col [col]
  (cond
    (or 
      (string? col)
      (number? col))
    col
    (coll? col) (the-only-value-in-col (first col))
    (nil? col) ""))
  
(defn get-value-with-tag [xml tag]
  (the-only-value-in-col
    (for [node (cond 
                 (= (type xml ) "String")
                 (parse-msg-seq xml)
                 (seq? xml)
                 xml)
          :when (= tag (:tag node))]
      (:content node))))  

(defmethod parse/parse-body :application/xml
  [^Context ctx ^TypedData body]
  (let [^String data (slurp body)]
    (parse-msg-seq data)))

(defmethod parse/parse-body :text/xml
  [^Context ctx ^TypedData body]
  (let [^String data (slurp body)]
    (parse-msg-seq data)))

(defn wx-reply-msg [data]
  (clojure.string/join "" 
    (for [[k v] data]
      (cond 
        (map? v)
        (str "<" (name k) ">" (wx-reply-msg v) "</" (name k) ">")
        (string? v)
        (str "<" (name k) "><![CDATA[" v  "]]></" (name k) ">")
        :else 
        (str "<" (name k) ">" v  "</" (name k) ">")))))
        
(defn echo [text from-server to-user]
  (let [r {:xml 
            {:ToUserName to-user
             :FromUserName from-server
             :CreateTime (System/currentTimeMillis)
             :MsgType "text"
             :Content text}}]
    (wx-reply-msg r)))

(defn smart-reply [x]
  (str "hi " x))


(defn do-service 
  "read POST data from body,and handle it."
  [data]
 (logger/info data)
 (let [to-user-name (get-value-with-tag data :ToUserName)
       client-user-id (get-value-with-tag data :FromUserName)
       server-corp-id (get-value-with-tag data :ToUserName)
       input (get-value-with-tag data :Content)
       msg-type (get-value-with-tag data :MsgType)
       tc (System/currentTimeMillis)
       reply (echo (smart-reply input) server-corp-id client-user-id)]
  (logger/info reply)
  (http/ok  reply)))

(defn webot 
  [context]
  (let [{{:keys [signature timestamp nonce echostr]} :query-params} context]
    (let [has-echostr? (not (nil? echostr))
          rep (cond 
                has-echostr?
                echostr
                (not has-echostr?)
                (try (do-service  (:data context)) (catch Exception e (logger/info e)))
                :else  
                (http/ok "NotFound"))]
      rep)))  


(def app
  (ct/routes [[:setup (prone/handler {:namespaces ["little-cute.core"]})]
              [:any (misc/log)]
              [:any (parse/body-params)]
              [:any "webot" webot]
              [:get "hello" (fn [& args] (http/ok "world"))]]))

(defn -main
  [& args]
  (ct/run-server app {:port 9000}))