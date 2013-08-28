(ns io.evolvability.gpai.cgp-viz
  "Visualisation of CGP genomes using Graphviz."
  (:require [io.evolvability.gpai.cgp :as cgp]
            [clojure.java.shell :as sh]
            [clojure.java.browse :as br]))

(defn print-active-nodes
  "Prints the graph of active nodes in DOT format."
  [{:as gm :keys [nodes out-idx inputs]}]
  (let [n-in (count inputs)
        active (sort (seq (cgp/active-idx gm)))
        pr-node (fn [i nm]
                  (println (format "nd%d [label=\"%s\"];"
                                   i (str nm))))
        pr-in-node (fn [i nm]
                     (println (format "nd%d [label=\"%s\",shape=box];"
                                      i (str nm))))
        pr-link (fn [i1 i2 j]
                  (println (format "nd%d -> nd%d [label=%d];"
                                   i1 i2 j)))
        prettyval (fn [x] (if (float? x) (format "%.2f" x) (str x)))]
    (println "digraph activenodes {")
    (println "ordering=out;")
    (dorun (map-indexed pr-in-node inputs))
    (doseq [i active
            :when (>= i n-in)]
      (let [nd (nth nodes i)
            in-idx (map (partial - i) (:in nd))
            nm (if (:fn nd) (name (:fn nd))
                   (prettyval (:value nd)))]
        (pr-node i nm)
        (dorun (map pr-link in-idx (repeat i)
                    (range (count in-idx))))))
    ;(println "out [label=\"output(s)\",shape=plaintext];")
    (println "node [shape=plaintext];")
    (dorun (map-indexed (fn [j i]
                          (-> (format "nd%d -> out%d [style=dashed];" i j)
                              println))
                        out-idx))
    (println "}")))

(defn viz-active-nodes
  "Generates an SVG graphic of the active nodes graph and optionally
   opens it. Executes the `dot` program, part of Graphviz."
  [gm & {:keys [name svg-file open?]
         :or {name "gpai-active-nodes", open? true}}]
  (let [s (with-out-str (print-active-nodes gm))
        svg-file (or svg-file (format "/tmp/%s.svg" name))
        dot-file (str svg-file ".dot")]
    (spit dot-file s)
    (sh/sh "dot" "-Tsvg" "-o" svg-file dot-file)
    (println "wrote" svg-file)
    (when open?
      (br/browse-url (str "file://" svg-file)))))
