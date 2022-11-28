
(ns rust-interpreter.core-test

  (:require [clojure.test :refer :all]

            [rust-interpreter.core :refer :all]
            
            [rust-interpreter.main :refer :all]))

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

(deftest inicializar-contexto-local-test
  (testing "Prueba de la funcion: inicializar-contexto-local"
    (is (= 
      [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] 8 [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []]
      (inicializar-contexto-local [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] 8 [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []])))  
    (is (= 
      [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] :sin-errores [[0 1] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []]
      (inicializar-contexto-local [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] :sin-errores [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []])))
  )  
)

(deftest restaurar-contexto-anterior-test
  (testing "Prueba de la funcion: restaurar-contexto-anterior"
    (is (=
      ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] 8 [[0 1] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0] ['y ['var-inmut 'i64] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]]
      (restaurar-contexto-anterior ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] 8 [[0 1] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0] ['y ['var-inmut 'i64] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]])))  
    (is (=
      ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] :sin-errores [[0] [['main ['fn [() ()]] 2]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]]
      (restaurar-contexto-anterior ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] :sin-errores [[0 1] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0] ['y ['var-inmut 'i64] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]])))
  
  )  
)

(deftest buscar-tipo-de-retorno-test
  (testing "Prueba de la funcion: buscar-tipo-de-retorno"
    (is (= 'i64 (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 2)))
    (is (= '() (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 8)))
    (is (nil? (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 1)))
  )  
)

(deftest generar-ref-test
  (testing "Prueba de la funcion: generar-ref"
    (is (= 
      [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] 8 [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]]
      (generar-ref [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] 8 [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]])))  
    (is (=
      [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] :sin-errores [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0] ['PUSHADDR 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]]
      (generar-ref [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] :sin-errores [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]])))
  )  
)

(deftest fixup-test
  (testing "Prueba de la funcion: fixup"
    (is (= 
      [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] 8 [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]]
      (fixup [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] 8 [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]] 4)))  
    (is (=
      [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] :sin-errores [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP 8] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]]
      (fixup [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] :sin-errores [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]] 4)))
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

(deftest interpretar-test
  (testing "Prueba del opcode: HLT"
    (is (nil? (interpretar ['HLT] [] 0 [] [])))
  )  

  (testing "Prueba del opcode: PUSHREF"
    (is (=
        [[['PUSHREF 3] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 0] ['i64 23]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 nil] ['i64 nil]]] 1 [23] []]
        (pushref [['PUSHREF 3] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 0] ['i64 23]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 nil] ['i64 nil]]] 0 [] [] ['PUSHREF 3] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 nil] ['i64 nil]])))
  )

  (testing "Prueba del opcode: PUSHADDR"
    (is (= 
      [[['PUSHADDR 3] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 0] ['i64 0]]] 1 [[0 3]] []]
      (pushaddr [['PUSHADDR 3] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 0] ['i64 0]]] 0 [] [] ['PUSHADDR 3])))  
  )

  (testing "Prueba del opcode: POP"
    (is (= 
      [[['POP 4] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 0] ['i64 23]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 5] ['i64 nil]]] 1 [1 150] []]
      (_pop [['POP 4] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 0] ['i64 23]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 nil] ['i64 nil]]] 0 [1 150 5] [] ['POP 4] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 nil] ['i64 nil]])))  
    (is (=
      [['POP 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 0] ['i64 23]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 nil] ['i64 nil]]] 1 [1 150] []]
      (_pop ['POP 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 0] ['i64 23]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 nil] ['i64 nil]]] 0 [1 150 5] [] 'POP [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 nil] ['i64 nil]])))
  )

  (testing "Prueba del opcode: POPARG"
    (is (=
      [[['POPARG 3] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 0] ['i64 0]] [['i64 nil] ['i64 nil] ['i64 nil] ['i64 [0 4]] ['i64 nil] ['i64 nil]]] 1 [1 23 5 [0 3] 150] []]
      (poparg [['POPARG 3] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 0] ['i64 0]] [['i64 nil] ['i64 nil] ['i64 nil] ['i64 nil] ['i64 nil] ['i64 nil]]] 0 [1 23 5 [0 3] [0 4] 150] [] ['POPARG 3] [['i64 nil] ['i64 nil] ['i64 nil] ['i64 nil] ['i64 nil] ['i64 nil]])))  
  )

  (testing "Prueba del opcode: POPREF"
    (is (=
      [[['POPREF 3] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 0] ['i64 23]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 nil] ['i64 nil]]] 1 [1 150] []]
      (popref [['POPREF 3] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 0] ['i64 0]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 nil] ['i64 nil]]] 0 [1 150 23] [] ['POPREF 3] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 nil] ['i64 nil]])))  
  )

  (testing "Prueba del opcode: FMT"
    (is (=
      [['FMT 'HLT] [] 1 [1 153 "Resto: 9"] []]
      (fmt ['FMT 'HLT] [] 0 [1 153 "Resto: {}" 9 2] [])))
  )

  (testing "Prueba del opcode: OUT"
    (is (=
      [['OUT 'HLT] [] 1 [1 153] []]
      (out ['OUT 'HLT] [] 0 [1 153 "Resto: {}" 9 2] [])))  
  )

  (testing "Prueba del opcode: RET"
    (is (=
      [['RET 'HLT] [[['String "15"] ['i64 12] ['i64 15]]] 81 [1 "{} es el MCD entre " 3] []]
      (ret ['RET 'HLT] [[['String "15"] ['i64 12] ['i64 15]] [['i64 3] ['i64 3]]] 40 [1 "{} es el MCD entre " 81 3] [])))  
  )

  (testing "Prueba del opcode: POPADD"
    (is (=
      [[['POPADD 2] 'HLT] [[['String "6"] ['i64 6] ['i64 7]]] 1 [1] []]
      (popadd [['POPADD 2] 'HLT] [[['String "6"] ['i64 6] ['i64 5]]] 0 [1 2] [] ['POPADD 2] [['String "6"] ['i64 6] ['i64 5]]))
    )
  )

  (testing "Prueba del opcode: POPADDREF"
    (is (=
      [[['POPADDREF 2] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 1] ['i64 3]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 5] ['i64 20]]] 1 [1 150] []]
      (popaddref [['POPADDREF 2] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 0] ['i64 3]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 5] ['i64 20]]] 0 [1 150 1] [] ['POPADDREF 2] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 5] ['i64 20]])))  
  )

  (testing "Prueba del opcode: ADD"
    (is (=
      [['ADD 'HLT] [] 1 [1 0 0 7] []]
      (add ['ADD 'HLT] [] 0 [1 0 0 3 4] [])))
  )

  (testing "Prueba del opcode: PUSHFI"
    (is (=
      [[['PUSHFI 3] 'HLT] [] 1 [1 0 0 3 4 3] []]
      (pushfi [['PUSHFI 3] 'HLT] [] 0 [1 0 0 3 4] [] ['PUSHFI 3])))
  )  
    
  (testing "Prueba del opcode: PUSHFM"
    (is (=
      [[['PUSHFM 2] 'HLT] [[['String "6"] ['i64 6] ['i64 5]]] 1 [5] []]
      (pushfm [['PUSHFM 2] 'HLT] [[['String "6"] ['i64 6] ['i64 5]]] 0 [] [] ['PUSHFM 2] [['String "6"] ['i64 6] ['i64 5]])))  
  )

  (testing "Prueba del opcode: JMP"
    (is (=
      [[['JMP 3] 'HLT] [] 3 [] []]
      (jmp [['JMP 3] 'HLT] [] 0 [] [] ['JMP 3]))) 
  )

  (testing "Prueba del opcode: JC"
    (is (=
      [[['JC 3] 'HLT] [] 1 [] []]
      (jc [['JC 3] 'HLT] [] 0 [5] [] ['JC 3])))
    (is (=
      [[['JC 3] 'HLT] [] 3 [] []]
      (jc [['JC 3] 'HLT] [] 0 [true] [] ['JC 3])))
  )

  (testing "Prueba del opcode: CAL"
    (is (=
      [[['CAL 0] 'HLT] [[['String "1"] ['i64 6] ['i64 5]] [[['String "2"] ['i64 6] ['i64 5]]] [[['String "3"] ['i64 6] ['i64 5]]]] 0 [11] [[[['String "3"] ['i64 6] ['i64 5]]]]]
      (cal [['CAL 0] 'HLT] [[['String "1"] ['i64 6] ['i64 5]] [[['String "2"] ['i64 6] ['i64 5]]]] 10 [] [[[['String "3"] ['i64 6] ['i64 5]]]] ['CAL 0])))  
  )

  (testing "Prueba del opcode: RETN"
    (is (=
      [[['CAL 0] 'HLT] [[['String "1"] ['i64 6] ['i64 5]]] 1 [] []]
      (retn [['CAL 0] 'HLT] [[['String "1"] ['i64 6] ['i64 5]] [[['String "2"] ['i64 6] ['i64 5]]]] 0 [1] [])))  
  )

  (testing "Prueba del opcode: NL"
    (is (=
      [['NL 'HLT] [] 1 [] []]
      (nl ['NL 'HLT] [] 0 [] [])))  
  )

  (testing "Prueba del opcode: FLUSH"
    (is (=
      [['FLUSH 'HLT] [] 1 [] []]
      (_flush ['FLUSH 'HLT] [] 0 [] [])))
  )

  (testing "Prueba del opcode: POPSUB"
    (is (=
      [['POPSUB 'HLT] [[['String "6"] ['i64 6] ['i64 3]]] 1 [1] []]
      (popsub ['POPSUB 'HLT] [[['String "6"] ['i64 6] ['i64 5]]] 0 [1 2] [] ['POPSUB 2] [['String "6"] ['i64 6] ['i64 5]]))
    )
  )

  (testing "Prueba del opcode: POPMUL"
    (is (=
      [[['POPMUL 2] 'HLT] [[['String "6"] ['i64 6] ['i64 10]]] 1 [1] []]
      (popmul [['POPMUL 2] 'HLT] [[['String "6"] ['i64 6] ['i64 5]]] 0 [1 2] [] ['POPMUL 2] [['String "6"] ['i64 6] ['i64 5]]))
    )
  )

  (testing "Prueba del opcode: POPDIV"
    (is (=
      [[['POPDIV 2] 'HLT] [[['String "6"] ['i64 6] ['i64 3]]] 1 [1] []]
      (popdiv [['POPDIV 2] 'HLT] [[['String "6"] ['i64 6] ['i64 9]]] 0 [1 3] [] ['POPDIV 2] [['String "6"] ['i64 6] ['i64 9]]))
    )
  )

  (testing "Prueba del opcode: POPMOD"
    (is (=
      [[['POPMOD 2] 'HLT] [[['String "6"] ['i64 6] ['i64 1]]] 1 [1] []]
      (popmod [['POPMOD 2] 'HLT] [[['String "6"] ['i64 6] ['i64 5]]] 0 [1 2] [] ['POPMOD 2] [['String "6"] ['i64 6] ['i64 5]]))
    )
  )

  (testing "Prueba del opcode: POPSUBREF"
    (is (=
      [[['POPSUBREF 2] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 4] ['i64 3]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 5] ['i64 20]]] 1 [1 150] []]
      (popsubref [['POPSUBREF 2] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 5] ['i64 3]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 5] ['i64 20]]] 0 [1 150 1] [] ['POPSUBREF 2] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 5] ['i64 20]])))  
  )

  (testing "Prueba del opcode: POPMULREF"
    (is (=
      [[['POPMULREF 2] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 10] ['i64 3]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 5] ['i64 20]]] 1 [1 150] []]
      (popmulref [['POPMULREF 2] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 5] ['i64 3]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 5] ['i64 20]]] 0 [1 150 2] [] ['POPMULREF 2] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 5] ['i64 20]])))  
  )

  (testing "Prueba del opcode: POPDIVREF"
    (is (=
      [[['POPDIVREF 2] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 3] ['i64 3]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 5] ['i64 20]]] 1 [1 150] []]
      (popdivref [['POPDIVREF 2] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 9] ['i64 3]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 5] ['i64 20]]] 0 [1 150 3] [] ['POPDIVREF 2] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 5] ['i64 20]])))  
  )

  (testing "Prueba del opcode: POPMODREF"
    (is (=
      [[['POPMODREF 2] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 2] ['i64 3]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 5] ['i64 20]]] 1 [1 150] []]
      (popmodref [['POPMODREF 2] 'HLT] [[['String "5"] ['i64 23] ['i64 5] ['i64 8] ['i64 3]] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 5] ['i64 20]]] 0 [1 150 3] [] ['POPMODREF 2] [['i64 23] ['i64 5] ['i64 [0 3]] ['i64 [0 4]] ['i64 5] ['i64 20]])))  
  )

  (testing "Prueba del opcode: SUB"
    (is (=
      [['SUB 'HLT] [] 1 [1 0 0 -1] []]
      (sub ['SUB 'HLT] [] 0 [1 0 0 3 4] [])))
  )

  (testing "Prueba del opcode: MUL"
    (is (=
      [['MUL 'HLT] [] 1 [1 0 0 12] []]
      (mul ['MUL 'HLT] [] 0 [1 0 0 3 4] [])))
  )

  (testing "Prueba del opcode: DIV"
    (is (=
      [['DIV 'HLT] [] 1 [1 0 0 2] []]
      (div ['DIV 'HLT] [] 0 [1 0 0 6 3] [])))
  )  

  (testing "Prueba del opcode: MOD"
    (is (=
      [['MOD 'HLT] [] 1 [1 0 0 1] []]
      (_mod ['MOD 'HLT] [] 0 [1 0 0 7 3] [])))
  )

  (testing "Prueba del opcode: CHR"
    (is (=
      [['CHR 'HLT] [] 1 [1 \a] []]
      (chr ['CHR 'HLT] [] 0 [1 "Juan" 2] [])))  
  )

  (testing "Prueba del opcode: OR"
    (is (=
      [['OR 'HLT] [] 1 [1 0 0 true] []]
      (_or ['OR 'HLT] [] 0 [1 0 0 false true] [])))
  )

  (testing "Prueba del opcode: AND"
    (is (=
      [['AND 'HLT] [] 1 [1 0 0 false] []]
      (_and ['AND 'HLT] [] 0 [1 0 0 false true] [])))
  )
  
  (testing "Prueba del opcode: EQ"
    (is (=
      [['EQ 'HLT] [] 1 [1 0 0 false] []]
      (eq ['EQ 'HLT] [] 0 [1 0 0 4 8] [])))
  )

  (testing "Prueba del opcode: NEQ"
    (is (=
      [['NEQ 'HLT] [] 1 [1 0 0 true] []]
      (neq ['NEQ 'HLT] [] 0 [1 0 0 4 8] [])))
  )

  (testing "Prueba del opcode: GT"
    (is (=
      [['GT 'HLT] [] 1 [1 0 0 false] []]
      (gt ['GT 'HLT] [] 0 [1 0 0 4 8] [])))
  )

  (testing "Prueba del opcode: GTE"
    (is (=
      [['GTE 'HLT] [] 1 [1 0 0 true] []]
      (gte ['GTE 'HLT] [] 0 [1 0 0 4 4] [])))
  )

  (testing "Prueba del opcode: LT"
    (is (=
      [['LT 'HLT] [] 1 [1 0 0 true] []]
      (lt ['LT 'HLT] [] 0 [1 0 0 4 8] [])))
  )

  (testing "Prueba del opcode: LTE"
    (is (=
      [['LTE 'HLT] [] 1 [1 0 0 true] []]
      (gte ['LTE 'HLT] [] 0 [1 0 0 4 4] [])))
  )

  (testing "Prueba del opcode: NEG"
    (is (=
      [['NEG 'HLT] [] 1 [1 0 0 -5] []]
      (neg ['NEG 'HLT] [] 0 [1 0 0 5] [])))
  )

  (testing "Prueba del opcode: NOT"
    (is (=
      [['NOT 'HLT] [] 1 [1 0 0 false] []]
      (_not ['NOT 'HLT] [] 0 [1 0 0 true] [])))
  )

  (testing "Prueba del opcode: TOI"
    (is (=
      [['TOI 'HLT] [] 1 [1 0 0 5] []]
      (toi ['TOI 'HLT] [] 0 [1 0 0 "5"] []))) 
  )

  (testing "Prueba del opcode: TOF"
    (is (=
      [['TOS 'HLT] [] 1 [1 0 0 5.0] []]
      (tof ['TOS 'HLT] [] 0 [1 0 0 5] []))) 
  )

  (testing "Prueba del opcode: SQRT"
    (is (=
      [['SQRT 'HLT] [] 1 [1 0 0 2.0] []]
      (sqrt ['SQRT 'HLT] [] 0 [1 0 0 4] [])))
  )

  (testing "Prueba del opcode: SIN"
    (is (=
      [['SIN 'HLT] [] 1 [1 0 0 0.0] []]
      (sin ['SIN 'HLT] [] 0 [1 0 0 0] [])))
  )  

  (testing "Prueba del opcode: ATAN"
    (is (=
      [['ATAN 'HLT] [] 1 [1 0 0 0.0] []]
      (atan ['ATAN 'HLT] [] 0 [1 0 0 0] []))) 
  )

  (testing "Prueba del opcode: ABS"
    (is (=
      [['ABS 'HLT] [] 1 [1 0 0 5] []]
      (_abs ['ABS 'HLT] [] 0 [1 0 0 -5] [])))
  )  
)
