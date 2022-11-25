
(ns rust-interpreter.core-test

  (:require [clojure.test :refer :all]

            [rust-interpreter.core :refer :all]
            
            [rust-interpreter.main :refer :all]))

(deftest es-el-doble?-test

  (testing "Prueba de la funcion: es-el-doble?"

    (is (= true (es-el-doble? 4 8)))

    (is (= false (es-el-doble? 4 7)))))


(deftest listar-test

  (testing "Prueba de la funcion: listar"

    (is (= 
      "fn main ( ) \n{\n  println! ( Hola, mundo! ) \n}\n" 
      (_listar (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "Hola, mundo!" (symbol ")") (symbol "}")) 0 [] [] 0)))
    (is (=
      "fn main ( ) \n{\n  println! ( Hola, mundo! );\n  if true \n  {\n    println! ( true ) \n  }\n}\n"
      (_listar (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "Hola, mundo!" (symbol ")") (symbol ";") 'if 'true (symbol "{") 'println! (symbol "(") "true" (symbol ")") (symbol "}") (symbol "}")) 0 [] [] 0)))))

(deftest agregar-ptocoma-test
  (testing "Prueba de la funcion: agregar-ptocoma"
    
    (is (= 
      (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'if 'x '< '0 (symbol "{") 'x '= '- 'x (symbol ";") (symbol "}") (symbol ";") 'renglon '= 'x (symbol ";") 'if 'z '< '0 (symbol "{") 'z '= '- 'z (symbol ";") (symbol "}") (symbol "}") 'fn 'foo (symbol "(") (symbol ")") (symbol "{") 'if 'y '> '0 (symbol "{") 'y '= '- 'y (symbol ";") (symbol "}") 'else (symbol "{") 'x '= '- 'y (symbol ";") (symbol "}") (symbol "}"))
      (agregar-ptocoma (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'if 'x '< '0 (symbol "{") 'x '= '- 'x (symbol ";") (symbol "}") 'renglon '= 'x (symbol ";") 'if 'z '< '0 (symbol "{") 'z '= '- 'z (symbol ";") (symbol "}") (symbol "}") 'fn 'foo (symbol "(") (symbol ")") (symbol "{") 'if 'y '> '0 (symbol "{") 'y '= '- 'y (symbol ";") (symbol "}") 'else (symbol "{") 'x '= '- 'y (symbol ";") (symbol "}") (symbol "}")))))))

(deftest palabra-reservada?-test
  (testing "Prueba positiva de la funcion: palabra-reservada?"
    (is (= true (palabra-reservada? 'as)))
    (is (= true (palabra-reservada? 'use)))
    (is (= true (palabra-reservada? 'const)))
    (is (= true (palabra-reservada? 'fn)))
    (is (= true (palabra-reservada? 'std)))
    (is (= true (palabra-reservada? 'io)))
    (is (= true (palabra-reservada? 'process)))
    (is (= true (palabra-reservada? 'Write)))
    (is (= true (palabra-reservada? 'i64)))
    (is (= true (palabra-reservada? 'f64)))
    (is (= true (palabra-reservada? 'mut)))
    (is (= true (palabra-reservada? 'bool)))
    (is (= true (palabra-reservada? 'String)))
    (is (= true (palabra-reservada? 'let)))
    (is (= true (palabra-reservada? 'char)))
    (is (= true (palabra-reservada? 'if)))
    (is (= true (palabra-reservada? 'else)))
    (is (= true (palabra-reservada? 'while)))
    (is (= true (palabra-reservada? 'return)))
    (is (= true (palabra-reservada? 'exit)))
    (is (= true (palabra-reservada? 'format!)))
    (is (= true (palabra-reservada? 'print!)))
    (is (= true (palabra-reservada? 'println!)))
    (is (= true (palabra-reservada? 'stdout)))
    (is (= true (palabra-reservada? 'stdin)))
    (is (= true (palabra-reservada? 'flush)))
    (is (= true (palabra-reservada? 'read_line)))
    (is (= true (palabra-reservada? 'expect)))
    (is (= true (palabra-reservada? 'new)))
    (is (= true (palabra-reservada? 'from)))
    (is (= true (palabra-reservada? 'as_str)))
    (is (= true (palabra-reservada? 'trim)))
    (is (= true (palabra-reservada? 'chars)))
    (is (= true (palabra-reservada? 'to_string)))
    (is (= true (palabra-reservada? 'parse)))
    (is (= true (palabra-reservada? 'nth)))
    (is (= true (palabra-reservada? 'unwrap)))
    (is (= true (palabra-reservada? 'sqrt)))
    (is (= true (palabra-reservada? 'sin)))
    (is (= true (palabra-reservada? 'atan)))
    (is (= true (palabra-reservada? 'abs)))
    (is (= true (palabra-reservada? 'as)))
    (is (= true (palabra-reservada? 'usize))))
    
    (testing "Prueba negativa de la funcion: palabra-reservada?"
      (is (= false (palabra-reservada? 'hola)))
      (is (= false (palabra-reservada? 'adios)))
      (is (= false (palabra-reservada? 'holaMundo)))))

(deftest dump-test
  (testing "Prueba de la funcion: dump"
    (is (= 
      ["0 [POPREF 2]" "1 [PUSHFI 2]" "2 MUL" "3 [PUSHFI 1]" "4 ADD" "5 NEG"]
      (_dump '[[POPREF 2] [PUSHFI 2] MUL [PUSHFI 1] ADD NEG])))
    (is (= 
      ["0 HLT"]
      (_dump '[HLT])))
    (is (= 
      ["0 nil"]
      (_dump nil)))))
  
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
    (is (= false (identificador? '12e0))) ; Alfanumerico empezando con un numero
    (is (= false (identificador? 'e120!))) ; No alfanumerico
    (is (= true (identificador? 'e120_test))) ; No alfanumerico pero únicamente _ como caracter especial
  )  
)

(deftest cargar-const-en-tabla-test
  (testing "Prueba de la funcion: cargar-const-en-tabla"
    (is (= 
      [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 '= 3] 8 [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []]
      (cargar-const-en-tabla [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] 8 [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []])))  
    (is (=
      [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] :sin-errores [[0] [['io ['lib '()] 0] ['TRES ['const 'i64] 3]]] 0 [['CAL 0] 'HLT] []]
      (cargar-const-en-tabla [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] :sin-errores [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []])
      ))  
  )
)

(deftest buscar-tipo-de-retorno-test
  (testing "Prueba de la funcion: buscar-tipo-de-retorno"
    (is (= 'i64 (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 2)))
    (is (= '() (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 8)))
    (is (nil? (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 1)))
  )  
)

(deftest convertir-formato-impresion-test
  (testing "Prueba de la funcion: convertir-formato-impresion"
    (is (= '("Hola, mundo!") (convertir-formato-impresion '("Hola, mundo!"))))
    (is (= '("Hola, mundo!, me llamo {%s}" "Nico") (convertir-formato-impresion '("Hola, mundo!, me llamo {{{}}}" "Nico" ))))
    (is (= '("- My name is %s, James %s.\n- Hello, %d%d%d!" "Bond" "Bond" 0 0 7) (convertir-formato-impresion '("- My name is {}, James {}.\n- Hello, {}{}{}!" "Bond" "Bond" 0 0 7))))
    (is (= '("%.0f elevado a la %d es\t%.0f" 2.0 2 4.0) (convertir-formato-impresion '("{} elevado a la {} es\t{}" 2.0 2 4.0))))
    (is (= '("Las raices cuadradas de %.0f son +%.8f y -%.8f" 4.0 1.999999999985448 1.999999999985448) (convertir-formato-impresion '("Las raices cuadradas de {} son +{:.8} y -{:.8}" 4.0 1.999999999985448 1.999999999985448))))
    (is (= '("Las raices cuadradas de %.0f son +%.32f y -%.32f" 4.0 1.999999999985448 1.999999999985448) (convertir-formato-impresion '("Las raices cuadradas de {} son +{:.32} y -{:.32}" 4.0 1.999999999985448 1.999999999985448))))
  )  
)

(deftest dividir-test
  (testing "Prueba de la funcion: dividir"
    (is (= 4 (dividir 12 3)))  
    (is (= 4.0 (dividir 12.0 3)))
    (is (= 4.0 (dividir 12 3.0)))
    (is (= 4.0 (dividir 12.0 3.0)))
    (is (= 0 (dividir 1 2)))
    (is (= 0.5 (dividir 1 2.0)))
  )  
)

(deftest pasar-a-int-test
  (testing "Prueba de la funcion: pasar-a-int"
    (is (= 10 (pasar-a-int 10)))
    (is (= 10 (pasar-a-int "10")))
    (is (= 10 (pasar-a-int 10.0)))
    (is (= 10 (pasar-a-int 10.8)))
    (is (= [10.0] (pasar-a-int [10.0])))
    (is (= 'a (pasar-a-int 'a)))
    (is (= \a (pasar-a-int \a)))
    (is (= 0 (pasar-a-int 2/4)))
  )  
)

(deftest pasar-a-float-test
  (testing "Prueba de la funcion: pasar-a-float"
    (is (= 10.0 (pasar-a-float 10)))
    (is (= 10.0 (pasar-a-float "10")))
    (is (= 10.0 (pasar-a-float 10.0)))
    (is (= 10.8 (pasar-a-float 10.8)))
    (is (= [10.0] (pasar-a-float [10.0])))
    (is (= 'a (pasar-a-float 'a)))
    (is (= \a (pasar-a-float \a)))
    (is (= 0.5 (pasar-a-float 2/4)))
  )  
)

(deftest compatibles?-test
  (testing "Prueba de la funcion: compatibles?"
    (is (= true (compatibles? 'i64 5)))
    (is (= false (compatibles? 'i64 5.0)))
    (is (= true (compatibles? 'i64 [5.0])))
    (is (= true (compatibles? 'f64 5.0)))
    (is (= true (compatibles? 'String "Hola")))
    (is (= true (compatibles? 'bool true)))
    (is (= false (compatibles? 'bool 1)))
    (is (= true (compatibles? 'usize 1)))
    (is (= false (compatibles? 'usize -1)))
    (is (= true (compatibles? 'char \a)))
    (is (= false (compatibles? 'char 'a)))
    (is (= true (compatibles? 'char ['a])))
  )  
)

(deftest cargar-en-ult-reg-test
  (testing "Prueba de la funcion: cargar-en-ult-reg"
    (is (= [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['i64 nil] ['i64 0]]] (cargar-en-ult-reg [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['i64 nil] ['i64 nil]]] 1 'i64 0)))  
    (is (= [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['f64 3] ['i64 0]]] (cargar-en-ult-reg [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['i64 nil] ['i64 0]]] 0 'f64 3)))
  )  
)

(deftest cargar-en-reg-dest-test
  (testing "Prueba de la funcion: cargar-en-reg-dest"
    (is (= [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 0]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] (cargar-en-reg-dest [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 2]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] [0 4] 'i64 0)))
    (is (= [[['String "2"] ['i64 6] ['i64 2] ['f64 3] ['i64 0]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] (cargar-en-reg-dest [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 0]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] [0 3] 'f64 3)))  
  )  
)