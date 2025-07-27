;;;; -----------------------------------------------------
;;;; MODULE: symbolic-genome.lisp
;;;; DESCRIPTION: Symbolic genome system for self-modification
;;;; DEPENDENCIES: Alexandria, CL-PPCRE, synth-u-core
;;;; -----------------------------------------------------

(defpackage :synth-u-genome
  (:use :cl :alexandria)
  (:export #:make-genome
           #:genome
           #:genome-id
           #:genome-version
           #:genome-capabilities
           #:genome-traits
           #:genome-mutation-rate
           #:genome-evolution-history
           #:mutate-genome
           #:replicate-genome
           #:merge-genomes
           #:compatible-p
           #:extract-trait
           #:add-trait
           #:remove-trait
           #:modify-trait
           #:calculate-compatibility
           #:analyze-genome))

(in-package :synth-u-genome)

;;;; Constantes y Variables Globales
(defconstant +genome-version+ "0.1.0"
  "Versión actual del sistema genómico simbólico")

(defparameter *mutation-types* 
  '(:add-trait :remove-trait :modify-trait :merge-traits :split-trait)
  "Tipos de mutaciones posibles en el genoma")

(defparameter *trait-categories*
  '(:structural :functional :behavioral :cognitive :adaptive)
  "Categorías principales de rasgos genómicos")

;;;; Estructuras de Datos Principales

(defstruct (genome (:constructor %make-genome))
  "Estructura principal para un genoma simbólico"
  (id nil :type string :read-only t)
  (version +genome-version+ :type string)
  (creation-timestamp (get-universal-time) :read-only t)
  (last-modified (get-universal-time))
  (capabilities nil :type list)
  (traits (make-hash-table :test #'eq) :type hash-table)
  (mutation-rate 0.05 :type float)
  (evolution-history nil :type list))

;;;; Funciones Constructor

(defun make-genome (id &key capabilities traits (mutation-rate 0.05))
  "Crea un nuevo genoma simbólico.
   
   Args:
     id (string): Identificador único para el genoma
     capabilities (lista, opcional): Lista de capacidades iniciales
     traits (lista, opcional): Lista de rasgos iniciales
     mutation-rate (float, opcional): Tasa de mutación inicial (0.0-1.0)
   
   Returns:
     Instancia de genoma inicializada"
  
  ;; Validación
  (when (or (null id) (string= id ""))
    (error "El ID del genoma no puede estar vacío"))
  
  (when (and mutation-rate (or (< mutation-rate 0.0) (> mutation-rate 1.0)))
    (error "La tasa de mutación debe estar entre 0.0 y 1.0"))
  
  ;; Crear el genoma base
  (let ((genome (%make-genome :id id
                             :capabilities (or capabilities '())
                             :mutation-rate (or mutation-rate 0.05))))
    
    ;; Añadir rasgos iniciales si se proporcionan
    (when traits
      (dolist (trait traits)
        (add-trait genome (car trait) (cdr trait))))
    
    ;; Registrar creación en historial
    (push (list :event :creation
                :timestamp (genome-creation-timestamp genome))
          (genome-evolution-history genome))
    
    ;; Devolver el nuevo genoma
    genome))

;;;; Funciones de Manipulación de Rasgos

(defun add-trait (genome category trait-data)
  "Añade un nuevo rasgo al genoma.
   
   Args:
     genome: El genoma a modificar
     category (keyword): Categoría del rasgo (ej: :structural)
     trait-data: Datos asociados al rasgo
   
   Returns:
     T si se añadió correctamente, NIL si ya existe"
  
  ;; Validar categoría
  (unless (member category *trait-categories*)
    (error "Categoría de rasgo inválida: ~A" category))
  
  ;; Obtener los rasgos actuales de esa categoría
  (let ((category-traits (gethash category (genome-traits genome))))
    
    ;; Si la categoría no existe, crear una lista vacía
    (unless category-traits
      (setf category-traits '())
      (setf (gethash category (genome-traits genome)) category-traits))
    
    ;; Verificar si el rasgo ya existe (por nombre)
    (let ((trait-name (getf trait-data :name)))
      (when (and trait-name 
                 (find trait-name category-traits 
                       :key (lambda (x) (getf x :name))
                       :test #'equal))
        (return-from add-trait nil)))
    
    ;; Añadir el nuevo rasgo
    (push trait-data (gethash category (genome-traits genome)))
    
    ;; Actualizar timestamp de modificación
    (setf (genome-last-modified genome) (get-universal-time))
    
    ;; Registrar en historial
    (push (list :event :add-trait
                :category category
                :trait trait-data
                :timestamp (get-universal-time))
          (genome-evolution-history genome))
    
    t))

(defun remove-trait (genome category trait-name)
  "Elimina un rasgo del genoma.
   
   Args:
     genome: El genoma a modificar
     category (keyword): Categoría del rasgo
     trait-name: Nombre del rasgo a eliminar
   
   Returns:
     T si se eliminó correctamente, NIL si no existe"
  
  ;; Validar categoría
  (unless (member category *trait-categories*)
    (error "Categoría de rasgo inválida: ~A" category))
  
  ;; Obtener los rasgos actuales de esa categoría
  (let ((category-traits (gethash category (genome-traits genome))))
    
    ;; Si no hay rasgos en esa categoría, no hay nada que eliminar
    (unless category-traits
      (return-from remove-trait nil))
    
    ;; Buscar el rasgo por nombre
    (let* ((old-traits category-traits)
           (new-traits (remove trait-name old-traits 
                              :key (lambda (x) (getf x :name))
                              :test #'equal)))
      
      ;; Si son iguales, no se encontró el rasgo
      (when (= (length old-traits) (length new-traits))
        (return-from remove-trait nil))
      
      ;; Actualizar la lista de rasgos
      (setf (gethash category (genome-traits genome)) new-traits)
      
      ;; Actualizar timestamp de modificación
      (setf (genome-last-modified genome) (get-universal-time))
      
      ;; Registrar en historial
      (push (list :event :remove-trait
                  :category category
                  :trait-name trait-name
                  :timestamp (get-universal-time))
            (genome-evolution-history genome))
      
      t)))

(defun modify-trait (genome category trait-name modifications)
  "Modifica un rasgo existente del genoma.
   
   Args:
     genome: El genoma a modificar
     category (keyword): Categoría del rasgo
     trait-name: Nombre del rasgo a modificar
     modifications: Plist con modificaciones a aplicar
   
   Returns:
     T si se modificó correctamente, NIL si no existe"
  
  ;; Validar categoría
  (unless (member category *trait-categories*)
    (error "Categoría de rasgo inválida: ~A" category))
  
  ;; Obtener los rasgos actuales de esa categoría
  (let ((category-traits (gethash category (genome-traits genome))))
    
    ;; Si no hay rasgos en esa categoría, no hay nada que modificar
    (unless category-traits
      (return-from modify-trait nil))
    
    ;; Buscar el rasgo por nombre
    (let ((trait (find trait-name category-traits 
                      :key (lambda (x) (getf x :name))
                      :test #'equal)))
      
      ;; Si no se encontró, no se puede modificar
      (unless trait
        (return-from modify-trait nil))
      
      ;; Crear copia del rasgo original para el historial
      (let ((original-trait (copy-list trait)))
        
        ;; Aplicar modificaciones
        (loop for (key value) on modifications by #'cddr
              do (setf (getf trait key) value))
        
        ;; Actualizar timestamp de modificación
        (setf (genome-last-modified genome) (get-universal-time))
        
        ;; Registrar en historial
        (push (list :event :modify-trait
                    :category category
                    :trait-name trait-name
                    :before original-trait
                    :after trait
                    :timestamp (get-universal-time))
              (genome-evolution-history genome))
        
        t))))

(defun extract-trait (genome category trait-name)
  "Extrae un rasgo del genoma sin modificarlo.
   
   Args:
     genome: El genoma fuente
     category (keyword): Categoría del rasgo
     trait-name: Nombre del rasgo a extraer
   
   Returns:
     El rasgo si existe, NIL en caso contrario"
  
  ;; Validar categoría
  (unless (member category *trait-categories*)
    (error "Categoría de rasgo inválida: ~A" category))
  
  ;; Obtener los rasgos de esa categoría
  (let ((category-traits (gethash category (genome-traits genome))))
    
    ;; Si no hay rasgos, no hay nada que extraer
    (unless category-traits
      (return-from extract-trait nil))
    
    ;; Buscar y devolver el rasgo por nombre
    (find trait-name category-traits 
          :key (lambda (x) (getf x :name))
          :test #'equal)))

;;;; Funciones de Evolución Genómica

(defun mutate-genome (genome &key (specific-mutation nil))
  "Aplica una mutación aleatoria (o específica) al genoma.
   
   Args:
     genome: El genoma a mutar
     specific-mutation (opcional): Tipo específico de mutación a aplicar
   
   Returns:
     T si la mutación fue exitosa, NIL en caso contrario"
  
  ;; Seleccionar tipo de mutación
  (let ((mutation-type (or specific-mutation
                          (nth (random (length *mutation-types*))
                               *mutation-types*))))
    
    ;; Aplicar mutación según el tipo
    (case mutation-type
      (:add-trait (mutate-add-trait genome))
      (:remove-trait (mutate-remove-trait genome))
      (:modify-trait (mutate-modify-trait genome))
      (:merge-traits (mutate-merge-traits genome))
      (:split-trait (mutate-split-trait genome))
      (otherwise (error "Tipo de mutación desconocido: ~A" mutation-type)))))

(defun mutate-add-trait (genome)
  "Mutación: Añade un nuevo rasgo aleatorio al genoma.
   
   Args:
     genome: El genoma a mutar
   
   Returns:
     T si la mutación fue exitosa, NIL en caso contrario"
  
  ;; Seleccionar categoría aleatoria
  (let* ((category (nth (random (length *trait-categories*))
                       *trait-categories*))
         ;; Crear rasgo aleatorio
         (trait-name (format nil "trait-~A" (random 1000)))
         (trait-data (list :name trait-name
                          :strength (+ 0.1 (random 1.0))
                          :flexibility (+ 0.1 (random 1.0))
                          :created-by :mutation
                          :creation-timestamp (get-universal-time))))
    
    ;; Añadir el rasgo
    (add-trait genome category trait-data)))

(defun mutate-remove-trait (genome)
  "Mutación: Elimina un rasgo aleatorio del genoma.
   
   Args:
     genome: El genoma a mutar
   
   Returns:
     T si la mutación fue exitosa, NIL en caso contrario"
  
  ;; Obtener lista de categorías con rasgos
  (let ((categories-with-traits
          (loop for category in *trait-categories*
                when (> (length (gethash category (genome-traits genome) nil)) 0)
                collect category)))
    
    ;; Si no hay categorías con rasgos, no se puede eliminar
    (when (null categories-with-traits)
      (return-from mutate-remove-trait nil))
    
    ;; Seleccionar categoría aleatoria con rasgos
    (let* ((category (nth (random (length categories-with-traits))
                         categories-with-traits))
           ;; Obtener rasgos de esa categoría
           (traits (gethash category (genome-traits genome)))
           ;; Seleccionar rasgo aleatorio
           (trait (nth (random (length traits)) traits))
           ;; Obtener nombre del rasgo
           (trait-name (getf trait :name)))
      
      ;; Eliminar el rasgo
      (remove-trait genome category trait-name))))

(defun mutate-modify-trait (genome)
  "Mutación: Modifica un rasgo aleatorio del genoma.
   
   Args:
     genome: El genoma a mutar
   
   Returns:
     T si la mutación fue exitosa, NIL en caso contrario"
  
  ;; Obtener lista de categorías con rasgos
  (let ((categories-with-traits
          (loop for category in *trait-categories*
                when (> (length (gethash category (genome-traits genome) nil)) 0)
                collect category)))
    
    ;; Si no hay categorías con rasgos, no se puede modificar
    (when (null categories-with-traits)
      (return-from mutate-modify-trait nil))
    
    ;; Seleccionar categoría aleatoria con rasgos
    (let* ((category (nth (random (length categories-with-traits))
                         categories-with-traits))
           ;; Obtener rasgos de esa categoría
           (traits (gethash category (genome-traits genome)))
           ;; Seleccionar rasgo aleatorio
           (trait (nth (random (length traits)) traits))
           ;; Obtener nombre del rasgo
           (trait-name (getf trait :name))
           ;; Crear modificaciones aleatorias
           (modifications (list :strength (+ 0.1 (random 1.0))
                               :flexibility (+ 0.1 (random 1.0))
                               :last-modified (get-universal-time))))
      
      ;; Modificar el rasgo
      (modify-trait genome category trait-name modifications))))

(defun mutate-merge-traits (genome)
  "Mutación: Fusiona dos rasgos en uno nuevo.
   
   Args:
     genome: El genoma a mutar
   
   Returns:
     T si la mutación fue exitosa, NIL en caso contrario"
  
  ;; Obtener lista de categorías con al menos dos rasgos
  (let ((categories-with-multiple-traits
          (loop for category in *trait-categories*
                when (>= (length (gethash category (genome-traits genome) nil)) 2)
                collect category)))
    
    ;; Si no hay categorías con múltiples rasgos, no se puede fusionar
    (when (null categories-with-multiple-traits)
      (return-from mutate-merge-traits nil))
    
    ;; Seleccionar categoría aleatoria con múltiples rasgos
    (let* ((category (nth (random (length categories-with-multiple-traits))
                         categories-with-multiple-traits))
           ;; Obtener rasgos de esa categoría
           (traits (gethash category (genome-traits genome)))
           ;; Seleccionar dos rasgos aleatorios diferentes
           (trait1-index (random (length traits)))
           (trait2-index (loop for i = (random (length traits))
                              until (/= i trait1-index)
                              finally (return i)))
           (trait1 (nth trait1-index traits))
           (trait2 (nth trait2-index traits))
           ;; Obtener nombres de los rasgos
           (trait1-name (getf trait1 :name))
           (trait2-name (getf trait2 :name))
           ;; Crear nuevo rasgo fusionado
           (merged-name (format nil "merged-~A-~A" trait1-name trait2-name))
           (merged-trait (list :name merged-name
                              :parents (list trait1-name trait2-name)
                              :strength (/ (+ (getf trait1 :strength 0.5)
                                             (getf trait2 :strength 0.5))
                                          2.0)
                              :flexibility (/ (+ (getf trait1 :flexibility 0.5)
                                                (getf trait2 :flexibility 0.5))
                                             2.0)
                              :created-by :merge-mutation
                              :creation-timestamp (get-universal-time))))
      
      ;; Eliminar los rasgos originales
      (remove-trait genome category trait1-name)
      (remove-trait genome category trait2-name)
      
      ;; Añadir el rasgo fusionado
      (add-trait genome category merged-trait))))

(defun mutate-split-trait (genome)
  "Mutación: Divide un rasgo en dos nuevos.
   
   Args:
     genome: El genoma a mutar
   
   Returns:
     T si la mutación fue exitosa, NIL en caso contrario"
  
  ;; Obtener lista de categorías con rasgos
  (let ((categories-with-traits
          (loop for category in *trait-categories*
                when (> (length (gethash category (genome-traits genome) nil)) 0)
                collect category)))
    
    ;; Si no hay categorías con rasgos, no se puede dividir
    (when (null categories-with-traits)
      (return-from mutate-split-trait nil))
    
    ;; Seleccionar categoría aleatoria con rasgos
    (let* ((category (nth (random (length categories-with-traits))
                         categories-with-traits))
           ;; Obtener rasgos de esa categoría
           (traits (gethash category (genome-traits genome)))
           ;; Seleccionar rasgo aleatorio para dividir
           (trait (nth (random (length traits)) traits))
           ;; Obtener nombre del rasgo
           (trait-name (getf trait :name))
           ;; Crear dos nuevos rasgos derivados
           (trait1-name (format nil "~A-split-1" trait-name))
           (trait2-name (format nil "~A-split-2" trait-name))
           (trait1-data (list :name trait1-name
                             :parent trait-name
                             :strength (max 0.1 (* (getf trait :strength 0.5) 
                                                   (+ 0.8 (random 0.4))))
                             :flexibility (max 0.1 (* (getf trait :flexibility 0.5)
                                                      (+ 0.8 (random 0.4))))
                             :created-by :split-mutation
                             :creation-timestamp (get-universal-time)))
           (trait2-data (list :name trait2-name
                             :parent trait-name
                             :strength (max 0.1 (* (getf trait :strength 0.5)
                                                   (+ 0.8 (random 0.4))))
                             :flexibility (max 0.1 (* (getf trait :flexibility 0.5)
                                                      (+ 0.8 (random 0.4))))
                             :created-by :split-mutation
                             :creation-timestamp (get-universal-time))))
      
      ;; Eliminar el rasgo original
      (remove-trait genome category trait-name)
      
      ;; Añadir los nuevos rasgos
      (add-trait genome category trait1-data)
      (add-trait genome category trait2-data))))

(defun replicate-genome (genome &key (mutate t) (mutation-count 1))
  "Crea una réplica del genoma, opcionalmente con mutaciones.
   
   Args:
     genome: El genoma a replicar
     mutate (boolean, opcional): Si se deben aplicar mutaciones
     mutation-count (integer, opcional): Número de mutaciones a aplicar
   
   Returns:
     Nuevo genoma replicado"
  
  ;; Crear ID para la réplica
  (let ((replica-id (format nil "~A-replica-~A" 
                           (genome-id genome)
                           (get-universal-time))))
    
    ;; Crear réplica base
    (let ((replica (make-genome replica-id
                               :capabilities (copy-list (genome-capabilities genome))
                               :mutation-rate (genome-mutation-rate genome))))
      
      ;; Copiar todos los rasgos
      (maphash (lambda (category traits)
                 (dolist (trait traits)
                   (add-trait replica category (copy-list trait))))
               (genome-traits genome))
      
      ;; Registrar relación con el original
      (push (list :event :replication
                  :parent-id (genome-id genome)
                  :timestamp (get-universal-time))
            (genome-evolution-history replica))
      
      ;; Aplicar mutaciones si se solicita
      (when mutate
        (dotimes (i mutation-count)
          (mutate-genome replica)))
      
      ;; Devolver la réplica
      replica)))

(defun merge-genomes (genome1 genome2 &key (id nil) (mutation-rate nil))
  "Fusiona dos genomas en uno nuevo.
   
   Args:
     genome1: Primer genoma a fusionar
     genome2: Segundo genoma a fusionar
     id (string, opcional): ID para el genoma fusionado
     mutation-rate (float, opcional): Tasa de mutación para el nuevo genoma
   
   Returns:
     Nuevo genoma fusionado"
  
  ;; Crear ID para el genoma fusionado si no se proporciona
  (let ((merged-id (or id 
                      (format nil "merged-~A-~A" 
                             (genome-id genome1)
                             (genome-id genome2)))))
    
    ;; Crear genoma fusionado base
    (let* ((capabilities (union (genome-capabilities genome1)
                               (genome-capabilities genome2)
                               :test #'eq))
           (new-mutation-rate (or mutation-rate
                                 (/ (+ (genome-mutation-rate genome1)
                                       (genome-mutation-rate genome2))
                                    2.0)))
           (merged-genome (make-genome merged-id
                                      :capabilities capabilities
                                      :mutation-rate new-mutation-rate)))
      
      ;; Fusionar rasgos de ambos genomas
      (dolist (category *trait-categories*)
        ;; Obtener rasgos de cada categoría de ambos genomas
        (let ((traits1 (gethash category (genome-traits genome1) nil))
              (traits2 (gethash category (genome-traits genome2) nil)))
          
          ;; Procesar rasgos del primer genoma
          (dolist (trait traits1)
            (let ((trait-name (getf trait :name))
                  (trait-copy (copy-list trait)))
              
              ;; Buscar si existe un rasgo con el mismo nombre en el segundo genoma
              (let ((matching-trait (find trait-name traits2
                                         :key (lambda (x) (getf x :name))
                                         :test #'equal)))
                
                (if matching-trait
                    ;; Si existe, crear un rasgo fusionado
                    (let ((merged-trait 
                            (list :name trait-name
                                 :parents (list (genome-id genome1) 
                                               (genome-id genome2))
                                 :strength (/ (+ (getf trait :strength 0.5)
                                                (getf matching-trait :strength 0.5))
                                             2.0)
                                 :flexibility (/ (+ (getf trait :flexibility 0.5)
                                                   (getf matching-trait :flexibility 0.5))
                                                2.0)
                                 :created-by :genome-merge
                                 :creation-timestamp (get-universal-time))))
                      (add-trait merged-genome category merged-trait))
                    
                    ;; Si no existe, añadir el rasgo del primer genoma
                    (add-trait merged-genome category trait-copy)))))
          
          ;; Procesar rasgos del segundo genoma que no estén en el primero
          (dolist (trait traits2)
            (let ((trait-name (getf trait :name)))
              (unless (find trait-name traits1
                           :key (lambda (x) (getf x :name))
                           :test #'equal)
                (add-trait merged-genome category (copy-list trait)))))))
      
      ;; Registrar fusión en historial
      (push (list :event :genome-merge
                  :parents (list (genome-id genome1) (genome-id genome2))
                  :timestamp (get-universal-time))
            (genome-evolution-history merged-genome))
      
      ;; Devolver el genoma fusionado
      merged-genome)))

;;;; Funciones de Análisis

(defun compatible-p (genome1 genome2 &key (threshold 0.7))
  "Determina si dos genomas son compatibles para fusión.
   
   Args:
     genome1: Primer genoma a comparar
     genome2: Segundo genoma a comparar
     threshold (float, opcional): Umbral de compatibilidad (0.0-1.0)
   
   Returns:
     T si son compatibles, NIL en caso contrario"
  
  (>= (calculate-compatibility genome1 genome2) threshold))

(defun calculate-compatibility (genome1 genome2)
  "Calcula un índice de compatibilidad entre dos genomas.
   
   Args:
     genome1: Primer genoma a comparar
     genome2: Segundo genoma a comparar
   
   Returns:
     Float entre 0.0 (incompatible) y 1.0 (totalmente compatible)"
  
  ;; Compatibilidad de capacidades
  (let* ((capabilities1 (genome-capabilities genome1))
         (capabilities2 (genome-capabilities genome2))
         (shared-capabilities (length (intersection capabilities1 capabilities2 :test #'eq)))
         (total-capabilities (length (union capabilities1 capabilities2 :test #'eq)))
         (capability-compatibility (if (zerop total-capabilities)
                                      1.0
                                      (/ shared-capabilities total-capabilities))))
    
    ;; Compatibilidad de rasgos por categoría
    (let ((trait-compatibility-sum 0.0)
          (category-count 0))
      
      ;; Calcular compatibilidad para cada categoría de rasgos
      (dolist (category *trait-categories*)
        (let ((traits1 (gethash category (genome-traits genome1) nil))
              (traits2 (gethash category (genome-traits genome2) nil)))
          
          ;; Solo considerar categorías que existen en al menos un genoma
          (when (or traits1 traits2)
            (incf category-count)
            
            ;; Si una categoría no existe en un genoma, compatibilidad baja
            (cond
              ((null traits1) 
               (incf trait-compatibility-sum 0.2))
              ((null traits2)
               (incf trait-compatibility-sum 0.2))
              (t
               ;; Calcular compatibilidad entre rasgos
               (let* ((trait-names1 (mapcar (lambda (x) (getf x :name)) traits1))
                      (trait-names2 (mapcar (lambda (x) (getf x :name)) traits2))
                      (shared-traits (length (intersection trait-names1 trait-names2 :test #'equal)))
                      (total-traits (length (union trait-names1 trait-names2 :test #'equal)))
                      (category-compatibility (/ shared-traits total-traits)))
                 
                 (incf trait-compatibility-sum category-compatibility)))))))
      
      ;; Calcular compatibilidad promedio de rasgos
      (let ((trait-compatibility (if (zerop category-count)
                                    1.0
                                    (/ trait-compatibility-sum category-count))))
        
        ;; Combinar compatibilidades (70% rasgos, 30% capacidades)
        (+ (* 0.7 trait-compatibility)
           (* 0.3 capability-compatibility))))))

(defun analyze-genome (genome)
  "Realiza un análisis completo del genoma.
   
   Args:
     genome: El genoma a analizar
   
   Returns:
     Hash-table con resultados del análisis"
  
  (let ((result (make-hash-table :test #'eq)))
    
    ;; Información básica
    (setf (gethash :id result) (genome-id genome))
    (setf (gethash :version result) (genome-version genome))
    (setf (gethash :age result) (- (get-universal-time) 
                                  (genome-creation-timestamp genome)))
    (setf (gethash :mutation-rate result) (genome-mutation-rate genome))
    
    ;; Capacidades
    (setf (gethash :capability-count result) (length (genome-capabilities genome)))
    (setf (gethash :capabilities result) (genome-capabilities genome))
    
    ;; Rasgos
    (let ((total-traits 0)
          (trait-counts (make-hash-table :test #'eq))
          (trait-strengths (make-hash-table :test #'eq)))
      
      ;; Contar rasgos por categoría
      (dolist (category *trait-categories*)
        (let ((traits (gethash category (genome-traits genome) nil)))
          (setf (gethash category trait-counts) (length traits))
          (incf total-traits (length traits))
          
          ;; Calcular fuerza promedio por categoría
          (when traits
            (setf (gethash category trait-strengths)
                  (/ (reduce #'+ traits :key (lambda (x) (getf x :strength 0.5)))
                     (length traits))))))
      
      ;; Almacenar resultados
      (setf (gethash :total-traits result) total-traits)
      (setf (gethash :trait-counts result) trait-counts)
      (setf (gethash :trait-strengths result) trait-strengths))
    
    ;; Evolución
    (setf (gethash :evolution-events result) (length (genome-evolution-history genome)))
    (setf (gethash :last-evolution result) (first (genome-evolution-history genome)))
    
    ;; Devolver resultados
    result))

;;;; Exportar símbolos públicos
(export '(make-genome
          genome
          genome-id
          genome-version
          genome-capabilities
          genome-traits
          genome-mutation-rate
          genome-evolution-history
          mutate-genome
          replicate-genome
          merge-genomes
          compatible-p
          extract-trait
          add-trait
          remove-trait
          modify-trait
          calculate-compatibility
          analyze-genome))
