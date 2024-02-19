mod interpreter;
mod library;
mod model;
mod parse;

use interpreter::{evaluate_program, repl};

fn main() {
  //let _ = repl();
  let _ = evaluate_program(vec![
  "(def magnitude (fn [v] (sqrt (apply + (map (fn [x] (* x x)) v)))))".to_owned(),
  "(def distance (. magnitude (| map -)))".to_owned(),
  "(def kd-insert \
    (fn kd-insert [tree x depth] \
      (if tree \
        (let [dim (mod depth (count x))] \
          (update tree \
                  (if (< (x dim) ((tree 0) dim)) 1 2) \
                  (fn [inner-tree] \
                    (kd-insert inner-tree \
                              x \
                              (inc depth))))) \
        [x nil nil])))".to_owned(),
  "(def kd-nearest \
    (fn kd-nearest [tree x depth] \
      (let [node-point (tree 0)] \
        (let [d (if (= x node-point) \
                  #inf \
                  (distance x node-point))] \
          (let [left-child (tree 1)] \
            (let [right-child (tree 2)] \
              (let [node-result [d node-point]] \
                (let [choose-result (fn [a b] (if (< (a 0) (b 0)) a b))] \
                  (if (and (nil? left-child) (nil? right-child)) \
                    node-result \
                    (if (or (nil? left-child) (nil? right-child)) \
                      (let [child-result (kd-nearest (or left-child right-child) \
                                                    x \
                                                    (inc depth))] \
                        (if (< (child-result 0) d) \
                          child-result \
                          node-result)) \
                      (let [dim (mod depth (count x))] \
                        (let [primary-child (if (< (x dim) ((tree 0) dim)) \
                                              left-child \
                                              right-child)] \
                          (let [best-result (choose-result node-result \
                                                          (kd-nearest primary-child \
                                                                      x \
                                                                      (inc depth)))] \
                            (if (< (best-result 0) \
                                  (abs (- (x dim) \
                                          (node-point dim)))) \
                              best-result \
                              (choose-result best-result \
                                            (kd-nearest (if (= primary-child left-child) \
                                                          right-child \
                                                          left-child) \
                                                        x \
                                                        (inc depth)))))))))))))))))".to_owned(),
  "(let [tree (time (iterate (fn [t] (kd-insert t [(rand) (rand) (rand)] 0)) \
                            nil \
                            10000))] \
    (time (count (map (fn [p] (kd-nearest tree p 0)) \
                      (repeatedly (fn [] [(rand) (rand) (rand)]) \
                                  10000)))))".to_owned()]);
}
