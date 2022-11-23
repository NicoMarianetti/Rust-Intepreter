; @formatter:off

(ns rust-interpreter.core-test

  (:require [clojure.test :refer :all]

            [rust-interpreter.core :refer :all]
            
            [rust-interpreter.main :refer :all]))

(deftest es-el-doble?-test

  (testing "Prueba de la funcion: es-el-doble?"

    (is (= true (es-el-doble? 4 8)))

    (is (= false (es-el-doble? 4 7)))))


;; (deftest listar-test

;;   (testing "Prueba de la funcion: listar"

;;     (is (= 
;;       "fn main ( )
;;       {
;;         println! ( \"Hola, mundo!\" )
;;       }" 
;;       (listar (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "Hola, mundo!" (symbol ")") (symbol "}")))))))

;; (deftest agregar-ptocoma-test
;;   (testing "Prueba de la funcion: agregar-ptocoma"
    
;;     (is (= ))))

(deftest palabra-reservada?-test
  (testing "Prueba positiva de la funcion: palabra-reservada?"
    (is (= true (palabra-reservada? 'as)))
    (is (= true (palabra-reservada? 'async)))
    (is (= true (palabra-reservada? 'await)))
    (is (= true (palabra-reservada? 'break)))
    (is (= true (palabra-reservada? 'const)))
    (is (= true (palabra-reservada? 'continue)))
    (is (= true (palabra-reservada? 'crate)))
    (is (= true (palabra-reservada? 'dyn)))
    (is (= true (palabra-reservada? 'else)))
    (is (= true (palabra-reservada? 'enum)))
    (is (= true (palabra-reservada? 'extern)))
    (is (= true (palabra-reservada? 'false)))
    (is (= true (palabra-reservada? 'fn)))
    (is (= true (palabra-reservada? 'for)))
    (is (= true (palabra-reservada? 'if)))
    (is (= true (palabra-reservada? 'impl)))
    (is (= true (palabra-reservada? 'in)))
    (is (= true (palabra-reservada? 'let)))
    (is (= true (palabra-reservada? 'loop)))
    (is (= true (palabra-reservada? 'match)))
    (is (= true (palabra-reservada? 'mod)))
    (is (= true (palabra-reservada? 'move)))
    (is (= true (palabra-reservada? 'mut)))
    (is (= true (palabra-reservada? 'pub)))
    (is (= true (palabra-reservada? 'ref)))
    (is (= true (palabra-reservada? 'return)))
    (is (= true (palabra-reservada? 'self)))
    (is (= true (palabra-reservada? 'Self)))
    (is (= true (palabra-reservada? 'static)))
    (is (= true (palabra-reservada? 'struct)))
    (is (= true (palabra-reservada? 'super)))
    (is (= true (palabra-reservada? 'trait)))
    (is (= true (palabra-reservada? 'true)))
    (is (= true (palabra-reservada? 'type)))
    (is (= true (palabra-reservada? 'union)))
    (is (= true (palabra-reservada? 'unsafe)))
    (is (= true (palabra-reservada? 'use)))
    (is (= true (palabra-reservada? 'where)))
    (is (= true (palabra-reservada? 'while)))
    (is (= true (palabra-reservada? 'abstract)))
    (is (= true (palabra-reservada? 'become)))
    (is (= true (palabra-reservada? 'box)))
    (is (= true (palabra-reservada? 'do)))
    (is (= true (palabra-reservada? 'final)))
    (is (= true (palabra-reservada? 'macro)))
    (is (= true (palabra-reservada? 'override)))
    (is (= true (palabra-reservada? 'priv)))
    (is (= true (palabra-reservada? 'try)))
    (is (= true (palabra-reservada? 'typeof)))
    (is (= true (palabra-reservada? 'unsized)))
    (is (= true (palabra-reservada? 'virtual)))
    (is (= true (palabra-reservada? 'yield))))
    
    (testing "Prueba negativa de la funcion: palabra-reservada?"
      (is (= false (palabra-reservada? 'hola)))
      (is (= false (palabra-reservada? 'adios)))
      (is (= false (palabra-reservada? 'holaMundo)))))

(deftest dump-test
  (testing "Prueba de la funcion: dump"
    (is (= "0 [POPREF 2]
      1 [PUSHFI 2]
      2 MUL
      3 [PUSHFI 1]
      4 ADD
      5 NEG
      nil
      " (dump '[[POPREF 2] [PUSHFI 2] MUL [PUSHFI 1] ADD NEG])))
    (is (= "0 HLTc
      nil"
      (dump '[HLT])))
    (is (= "0 nil
      nil" (dump nil)))))
  
(deftest ya-declarado-localmente?-test
  (testing "Prueba de la funcion: ya-declarado-localmente?"
    (is (= true (ya-declarado-localmente? 'Write [[0] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])))
    (is (= false (ya-declarado-localmente? 'Read [[0] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])))
    (is (= true (ya-declarado-localmente? 'Write [[0 1] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])))
    (is (= false (ya-declarado-localmente? 'Write [[0 2] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])))))
  

(deftest identificador?-test
  (testing "Prueba de la funcion: identificador?"
    (is (= true (identificador? 'boolean)))
    (is (= false (identificador? 'bool))) ; Palabra reservada
    (is (= true (identificador? 'e120))) ; Alfanumerico empezando con un caracter
    (is (= false ((identificador? '12e0)))) ; Alfanumerico empezando con un numero
    (is (= false (identificador? 'e120!))) ; No alfanumerico
    (is (= true (identificador? 'e120_test))) ; No alfanumerico pero únicamente _ como caracter especial
  )  
)

;; (deftest cargar-const-en-tabla-test
;;   (testing "Prueba de la funcion: cargar-const-en-tabla"
;;     ()  
;;   )  
;; )