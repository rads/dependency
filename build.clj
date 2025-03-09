(ns build
  (:refer-clojure :exclude [compile])
  (:require [clojure.tools.build.api :as b]
            [clojure.edn :as edn]))

(def deps-edn (edn/read-string (slurp "deps.edn")))
(def version (-> deps-edn :aliases :neil :project :version))
(def outfile (format "resources/public/js/rads/dependency-v%s.min.js" version))

(defn compile [_]
  (b/process {:command-args ["npm" "ci"]})
  (b/process {:command-args ["./node_modules/.bin/cherry"
                             "compile" "src/rads/dependency.cljc"
                             "--output-dir" "target/cherry"]})
  (b/process {:command-args ["./node_modules/.bin/esbuild"
                             "target/cherry/src/rads/dependency.mjs"
                             "--format=esm"
                             "--bundle"
                             "--minify"
                             "--external:cherry-cljs"
                             (format "--outfile=%s" outfile)]}))
