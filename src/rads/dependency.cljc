;; Copyright (c) Stuart Sierra, 2012-2015. All rights reserved. The use and
;; distribution terms for this software are covered by the Eclipse Public
;; License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be
;; found in the file epl-v10.html at the root of this distribution. By using
;; this software in any fashion, you are agreeing to be bound by the terms of
;; this license. You must not remove this notice, or any other, from this
;; software.

(ns ^{:author "Stuart Sierra"
      :doc "Bidirectional graphs of dependencies and dependent objects."}
  rads.dependency
  (:require [clojure.set :as set]))

(defn- remove-from-map [amap x]
  (reduce (fn [m [k vs]]
            (assoc m k (disj vs x)))
          {} (dissoc amap x)))

(defn- transitive
  "Recursively expands the set of dependency relationships starting
  at (get neighbors x), for each x in node-set"
  [neighbors node-set]
  (loop [unexpanded (mapcat neighbors node-set)
         expanded #{}]
    (if-let [[node & more] (seq unexpanded)]
      (if (contains? expanded node)
        (recur more expanded)
        (recur (concat more (neighbors node))
               (conj expanded node)))
      expanded)))

(declare depends?)

(def set-conj (fnil conj #{}))

(defn immediate-dependencies [graph node]
  (let [{:keys [dependencies]} graph]
    (get dependencies node #{})))

(defn immediate-dependents [graph node]
  (let [{:keys [dependents]} graph]
    (get dependents node #{})))

(defn transitive-dependencies [graph node]
  (let [{:keys [dependencies]} graph]
    (transitive dependencies #{node})))

(defn transitive-dependencies-set [graph node-set]
  (let [{:keys [dependencies]} graph]
    (transitive dependencies node-set)))

(defn transitive-dependents [graph node]
  (let [{:keys [dependents]} graph]
    (transitive dependents #{node})))

(defn transitive-dependents-set [graph node-set]
  (let [{:keys [dependents]} graph]
    (transitive dependents node-set)))

(defn nodes [graph]
  (let [{:keys [dependencies dependents]} graph]
    (set/union (set (keys dependencies))
               (set (keys dependents)))))

(defn- ->graph [dependencies dependents]
  {:dependencies dependencies
   :dependents dependents})

(defn depend [graph node dep]
  (let [{:keys [dependencies dependents]} graph]
    (when (or (= node dep) (depends? graph dep node))
      (throw (ex-info (str "Circular dependency between "
                           (pr-str node) " and " (pr-str dep))
                      {:reason ::circular-dependency
                       :node node
                       :dependency dep})))
    (->graph
      (update-in dependencies [node] set-conj dep)
      (update-in dependents [dep] set-conj node))))

(defn remove-edge [graph node dep]
  (let [{:keys [dependencies dependents]} graph]
    (->graph
      (update-in dependencies [node] disj dep)
      (update-in dependents [dep] disj node))))

(defn remove-all [graph node]
  (let [{:keys [dependencies dependents]} graph]
    (->graph
      (remove-from-map dependencies node)
      (remove-from-map dependents node))))

(defn remove-node [graph node]
  (let [{:keys [dependencies dependents]} graph]
    (->graph
      (dissoc dependencies node)
      dependents)))

(defn graph "Returns a new, empty, dependency graph." []
  (->graph {} {}))

(defn depends?
  "True if x is directly or transitively dependent on y."
  [graph x y]
  (contains? (transitive-dependencies graph x) y))

(defn dependent?
  "True if y is a dependent of x."
  [graph x y]
  (contains? (transitive-dependents graph x) y))

(defn topo-sort
  "Returns a topologically-sorted list of nodes in graph."
  [graph]
  (loop [sorted ()
         g graph
         todo (set (filter #(empty? (immediate-dependents graph %))
                           (nodes graph)))]
    (if (empty? todo)
      sorted
      (let [[node & more] (seq todo)
            deps (immediate-dependencies g node)
            [add g'] (loop [deps deps
                            g g
                            add #{}]
                       (if (seq deps)
                         (let [d (first deps)
                               g' (remove-edge g node d)]
                           (if (empty? (immediate-dependents g' d))
                             (recur (rest deps) g' (conj add d))
                             (recur (rest deps) g' add)))
                         [add g]))]
        (recur (cons node sorted)
               (remove-node g' node)
               (set/union (set more) (set add)))))))

(def ^:private max-number
  #?(:clj Long/MAX_VALUE
     :cljs js/Number.MAX_VALUE))

(defn topo-comparator
  "Returns a comparator fn which produces a topological sort based on
  the dependencies in graph. Nodes not present in the graph will sort
  after nodes in the graph."
  [graph]
  (let [pos (zipmap (topo-sort graph) (range))]
    (fn [a b]
      (compare (get pos a max-number)
               (get pos b max-number)))))
