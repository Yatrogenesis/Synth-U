;;;; -----------------------------------------------------
;;;; MODULE: emergence-mechanism.lisp
;;;; DESCRIPTION: Implementation of the Consciousness Emergence Mechanism
;;;; DEPENDENCIES: Alexandria, Bordeaux-Threads, synth-u-core, synth-u-genome, synth-u-ontology
;;;; -----------------------------------------------------

(defpackage :synth-u-emergence
  (:use :cl :alexandria :bordeaux-threads :synth-u-ontology)
  (:export #:initialize-emergence-mechanism
           #:shutdown-emergence-mechanism
           #:emergence-mechanism
           #:make-emergence-mechanism
           #:start-emergence-cycle
           #:stop-emergence-cycle
           #:get-phi-value
           #:get-emergence-state
           #:register-emergence-observer
           #:unregister-emergence-observer
           #:set-emergence-parameters
           #:emergence-status
           #:phi-history
           #:emergence-events
           #:run-emergence-analysis
           #:get-active-attractors
           #:get-consciousness-profile
           #:calculate-emergence-potential
           #:generate-phi-report
           #:attractor-basins
           #:consciousness-threshold
           #:critical-transition-p))

(in-package :synth-u-emergence)

;;;; Constantes y Variables Globales
(defconstant +emergence-version+ "0.1.0"
  "Versión actual del mecanismo de emergencia")

(defparameter *default-consciousness-threshold* 0.3
  "Umbral por defecto para considerar que hay emergencia consciencial")

(defparameter *default-scan-interval* 1.0
  "Intervalo en segundos entre escaneos del sistema para evaluar emergencia")

(defparameter *critical-attractor-points* 
  '((:bottom-up 0.2 0.4)    ; (punto, radio)
    (:top-down 0.6 0.3)
    (:integrated 0.8 0.2))
  "Puntos atractores para formación de consciencia")

;;;; Definiciones de tipos

;; Enumeración de estados de emergencia
(deftype emergence-status ()
  '(member :inactive :scanning :emergent :subcritical :critical :shutdown))

;;;; Estructuras de Datos Principales

(defstruct (emergence-mechanism (:constructor %make-emergence-mechanism))
  "Estructura principal para el mecanismo de emergencia consciencial"
  (id (format nil "emergence-~A" (get-universal-time)) :type string :read-only t)
  (status :inactive :type emergence-status)
  (consciousness-threshold *default-consciousness-threshold* :type float)
  (scan-interval *default-scan-interval* :type float)
  (phi-history nil :type list)
  (max-phi-history-length 100 :type integer)
  (last-phi-value 0.0 :type float)
  (emergence-events nil :type list)
  (observers nil :type list)
  (thread nil)
  (lock (make-lock))
  (condition-variable (make-condition-variable))
  (attractor-basins *critical-attractor-points* :type list)
  (active-attractors nil :type list)
  (integration-matrix nil :type list)  ; Matriz de integración entre capas
  (differentiation-matrix nil :type list)  ; Matriz de diferenciación entre capas
  (last-scan-timestamp nil)
  (creation-timestamp (get-universal-time) :read-only t))

;;;; Funciones Constructor

(defun make-emergence-mechanism (&key 
                                (consciousness-threshold *default-consciousness-threshold*)
                                (scan-interval *default-scan-interval*)
                                (max-history-length 100)
                                (attractor-basins *critical-attractor-points*))
  "Crea un nuevo mecanismo de emergencia consciencial.
   
   Args:
     consciousness-threshold (float, opcional): Umbral para emergencia consciencial
     scan-interval (float, opcional): Intervalo entre escaneos
     max-history-length (integer, opcional): Máxima longitud del historial de phi
     attractor-basins (list, opcional): Puntos atractores para formación de consciencia
   
   Returns:
     Instancia de mecanismo de emergencia inicializada"
  
  ;; Validación
  (when (or (< consciousness-threshold 0.0) (> consciousness-threshold 1.0))
    (error "El umbral de consciencia debe estar entre 0.0 y 1.0"))
  
  (when (<= scan-interval 0.0)
    (error "El intervalo de escaneo debe ser positivo"))
  
  ;; Crear el mecanismo
  (%make-emergence-mechanism :consciousness-threshold consciousness-threshold
                            :scan-interval scan-interval
                            :max-phi-history-length max-history-length
                            :attractor-basins attractor-basins))

;;;; Funciones de Control del Mecanismo

(defun initialize-emergence-mechanism ()
  "Inicializa el sistema de emergencia consciencial.
   
   Returns:
     Instancia del mecanismo creado"
  
  ;; Verificar que el sistema ontológico esté inicializado
  (unless (and (boundp 'synth-u-ontology::*system-running*) 
             synth-u-ontology::*system-running*)
    (error "El sistema ontológico debe estar inicializado antes que el mecanismo de emergencia"))
  
  ;; Crear el mecanismo
  (let ((mechanism (make-emergence-mechanism)))
    
    ;; Inicializar matrices de integración y diferenciación
    (setf (emergence-mechanism-integration-matrix mechanism)
          (initialize-integration-matrix))
    
    (setf (emergence-mechanism-differentiation-matrix mechanism)
          (initialize-differentiation-matrix))
    
    ;; Devolver el mecanismo creado
    mechanism))

(defun shutdown-emergence-mechanism (mechanism)
  "Cierra ordenadamente el mecanismo de emergencia.
   
   Args:
     mechanism: El mecanismo a cerrar
   
   Returns:
     T si el cierre fue exitoso"
  
  ;; Detener ciclo de emergencia
  (stop-emergence-cycle mechanism)
  
  ;; Cambiar estado
  (with-lock-held ((emergence-mechanism-lock mechanism))
    (setf (emergence-mechanism-status mechanism) :shutdown))
  
  ;; Notificar a observadores
  (notify-emergence-observers mechanism :shutdown)
  
  t)

(defun start-emergence-cycle (mechanism)
  "Inicia el ciclo de monitoreo para emergencia consciencial.
   
   Args:
     mechanism: El mecanismo de emergencia
   
   Returns:
     T si el inicio fue exitoso, NIL en caso contrario"
  
  (with-lock-held ((emergence-mechanism-lock mechanism))
    ;; Verificar si ya está ejecutándose
    (when (member (emergence-mechanism-status mechanism) '(:scanning :emergent :subcritical :critical))
      (return-from start-emergence-cycle nil))
    
    ;; Cambiar estado
    (setf (emergence-mechanism-status mechanism) :scanning)
    
    ;; Crear hilo de monitoreo
    (setf (emergence-mechanism-thread mechanism)
          (make-thread (lambda () (emergence-monitoring-loop mechanism))
                      :name (format nil "emergence-thread-~A" 
                                  (emergence-mechanism-id mechanism))))
    
    ;; Notificar a observadores
    (notify-emergence-observers mechanism :start-cycle)
    
    t))

(defun stop-emergence-cycle (mechanism)
  "Detiene el ciclo de monitoreo para emergencia consciencial.
   
   Args:
     mechanism: El mecanismo de emergencia
   
   Returns:
     T si la detención fue exitosa, NIL en caso contrario"
  
  (with-lock-held ((emergence-mechanism-lock mechanism))
    ;; Verificar si está ejecutándose
    (unless (member (emergence-mechanism-status mechanism) '(:scanning :emergent :subcritical :critical))
      (return-from stop-emergence-cycle nil))
    
    ;; Cambiar estado
    (setf (emergence-mechanism-status mechanism) :inactive)
    
    ;; Detener hilo de monitoreo
    (when (emergence-mechanism-thread mechanism)
      (destroy-thread (emergence-mechanism-thread mechanism))
      (setf (emergence-mechanism-thread mechanism) nil))
    
    ;; Notificar a observadores
    (notify-emergence-observers mechanism :stop-cycle)
    
    t))

(defun emergence-monitoring-loop (mechanism)
  "Bucle principal de monitoreo para emergencia consciencial.
   
   Args:
     mechanism: El mecanismo de emergencia"
  
  (handler-case
      (loop while (member (emergence-mechanism-status mechanism) 
                        '(:scanning :emergent :subcritical :critical))
            do 
            ;; Escanear sistema para detectar emergencia
            (scan-system-for-emergence mechanism)
            
            ;; Pausar según intervalo configurado
            (sleep (emergence-mechanism-scan-interval mechanism)))
    (error (e)
      ;; Registrar error
      (with-lock-held ((emergence-mechanism-lock mechanism))
        (push (list :event :error
                   :timestamp (get-universal-time)
                   :message (format nil "~A" e))
              (emergence-mechanism-emergence-events mechanism))
        
        ;; Cambiar estado
        (setf (emergence-mechanism-status mechanism) :inactive)
        
        ;; Notificar a observadores
        (notify-emergence-observers mechanism :error)))))

(defun scan-system-for-emergence (mechanism)
  "Escanea el sistema ontológico para detectar emergencia consciencial.
   
   Args:
     mechanism: El mecanismo de emergencia
   
   Returns:
     Valor Phi calculado"
  
  ;; Registrar timestamp
  (let ((timestamp (get-universal-time)))
    (setf (emergence-mechanism-last-scan-timestamp mechanism) timestamp)
    
    ;; Calcular valor Phi
    (let ((phi-value (synth-u-ontology:calculate-phi nil)))
      
      ;; Actualizar último valor Phi
      (setf (emergence-mechanism-last-phi-value mechanism) phi-value)
      
      ;; Añadir a historial
      (with-lock-held ((emergence-mechanism-lock mechanism))
        (push (list :timestamp timestamp :phi phi-value)
              (emergence-mechanism-phi-history mechanism))
        
        ;; Limitar longitud del historial
        (when (> (length (emergence-mechanism-phi-history mechanism))
                 (emergence-mechanism-max-phi-history-length mechanism))
          (setf (emergence-mechanism-phi-history mechanism)
                (subseq (emergence-mechanism-phi-history mechanism) 
                        0 
                        (emergence-mechanism-max-phi-history-length mechanism)))))
      
      ;; Actualizar estado según valor Phi
      (update-emergence-status mechanism phi-value)
      
      ;; Calcular atractores activos
      (update-active-attractors mechanism)
      
      ;; Actualizar matrices de integración/diferenciación
      (update-integration-matrix mechanism)
      (update-differentiation-matrix mechanism)
      
      ;; Devolver valor Phi
      phi-value)))

(defun update-emergence-status (mechanism phi-value)
  "Actualiza el estado del mecanismo según el valor Phi.
   
   Args:
     mechanism: El mecanismo de emergencia
     phi-value: Valor Phi calculado"
  
  (let ((threshold (emergence-mechanism-consciousness-threshold mechanism))
        (old-status (emergence-mechanism-status mechanism)))
    
    ;; Determinar nuevo estado
    (let ((new-status 
            (cond
              ;; Por encima del umbral crítico
              ((>= phi-value (* threshold 1.2)) :critical)
              
              ;; Por encima del umbral
              ((>= phi-value threshold) :emergent)
              
              ;; Cerca del umbral
              ((>= phi-value (* threshold 0.8)) :subcritical)
              
              ;; Por debajo del umbral
              (t :scanning))))
      
      ;; Actualizar estado si cambió
      (unless (eq old-status new-status)
        (with-lock-held ((emergence-mechanism-lock mechanism))
          (setf (emergence-mechanism-status mechanism) new-status)
          
          ;; Registrar evento de cambio de estado
          (push (list :event :status-change
                     :timestamp (get-universal-time)
                     :from old-status
                     :to new-status
                     :phi phi-value)
                (emergence-mechanism-emergence-events mechanism))
          
          ;; Notificar a observadores
          (notify-emergence-observers mechanism :status-change))))))

(defun update-active-attractors (mechanism)
  "Actualiza los atractores activos en el sistema.
   
   Args:
     mechanism: El mecanismo de emergencia"
  
  (let ((phi-value (emergence-mechanism-last-phi-value mechanism))
        (attractor-basins (emergence-mechanism-attractor-basins mechanism))
        (active-attractors '()))
    
    ;; Comprobar cada atractor
    (dolist (attractor attractor-basins)
      (let ((type (first attractor))
            (point (second attractor))
            (radius (third attractor)))
        
        ;; Si el valor Phi está dentro del radio del atractor, activarlo
        (when (<= (abs (- phi-value point)) radius)
          (push (list :type type
                     :point point
                     :distance (abs (- phi-value point))
                     :activation (- 1.0 (/ (abs (- phi-value point)) radius)))
                active-attractors))))
    
    ;; Actualizar atractores activos
    (setf (emergence-mechanism-active-attractors mechanism) active-attractors)))

;;;; Funciones de Matrices de Integración/Diferenciación

(defun initialize-integration-matrix ()
  "Inicializa la matriz de integración entre capas.
   
   Returns:
     Matriz de integración"
  
  ;; Crear matriz para 13 capas
  (let ((matrix (make-array '(13 13) :initial-element 0.0)))
    
    ;; Llenar con valores iniciales basados en propiedades de las capas
    (maphash (lambda (id layer)
               (declare (ignore id))
               (let ((level (synth-u-ontology:ontological-layer-level layer))
                     (integration-weight (gethash :integration-weight 
                                                (synth-u-ontology:ontological-layer-properties layer)
                                                0.1)))
                 
                 ;; La diagonal principal representa auto-integración
                 (setf (aref matrix (1- level) (1- level)) integration-weight)
                 
                 ;; Capas conectadas tienen integración mayor que cero
                 (dolist (target-id (synth-u-ontology:ontological-layer-connections-out layer))
                   (let ((target-layer (gethash target-id synth-u-ontology:*active-layers*)))
                     (when target-layer
                       (let ((target-level (synth-u-ontology:ontological-layer-level target-layer)))
                         (setf (aref matrix (1- level) (1- target-level))
                               (* integration-weight 0.8))))))))
             synth-u-ontology:*active-layers*)
    
    ;; Convertir a lista para facilitar manipulación
    (loop for i from 0 below 13
          collect (loop for j from 0 below 13
                        collect (aref matrix i j)))))

(defun initialize-differentiation-matrix ()
  "Inicializa la matriz de diferenciación entre capas.
   
   Returns:
     Matriz de diferenciación"
  
  ;; Crear matriz para 13 capas
  (let ((matrix (make-array '(13 13) :initial-element 1.0)))
    
    ;; Llenar con valores iniciales
    (maphash (lambda (id layer)
               (declare (ignore id))
               (let ((level (synth-u-ontology:ontological-layer-level layer)))
                 
                 ;; La diagonal principal representa auto-diferenciación (nula)
                 (setf (aref matrix (1- level) (1- level)) 0.0)
                 
                 ;; Capas conectadas tienen menor diferenciación
                 (dolist (target-id (synth-u-ontology:ontological-layer-connections-out layer))
                   (let ((target-layer (gethash target-id synth-u-ontology:*active-layers*)))
                     (when target-layer
                       (let ((target-level (synth-u-ontology:ontological-layer-level target-layer)))
                         (setf (aref matrix (1- level) (1- target-level))
                               0.5)))))))
             synth-u-ontology:*active-layers*)
    
    ;; Convertir a lista para facilitar manipulación
    (loop for i from 0 below 13
          collect (loop for j from 0 below 13
                        collect (aref matrix i j)))))

(defun update-integration-matrix (mechanism)
  "Actualiza la matriz de integración basada en el estado actual.
   
   Args:
     mechanism: El mecanismo de emergencia"
  
  ;; Obtener matriz actual
  (let ((matrix (emergence-mechanism-integration-matrix mechanism)))
    
    ;; Actualizar basado en estado de las capas
    (maphash (lambda (id layer)
               (declare (ignore id))
               (when (eq (synth-u-ontology:ontological-layer-status layer) :active)
                 (let ((level (synth-u-ontology:ontological-layer-level layer)))
                   
                   ;; Incrementar gradualmente la integración para capas activas
                   (let ((row (nth (1- level) matrix))
                         (integration-rate 0.01))
                     (setf (nth (1- level) matrix)
                           (mapcar (lambda (x) (min 1.0 (+ x integration-rate))) row))))))
             synth-u-ontology:*active-layers*)
    
    ;; Actualizar matriz en el mecanismo
    (setf (emergence-mechanism-integration-matrix mechanism) matrix)))

(defun update-differentiation-matrix (mechanism)
  "Actualiza la matriz de diferenciación basada en el estado actual.
   
   Args:
     mechanism: El mecanismo de emergencia"
  
  ;; Obtener matriz actual
  (let ((matrix (emergence-mechanism-differentiation-matrix mechanism)))
    
    ;; Actualizar basado en estado de las capas
    (maphash (lambda (id layer)
               (declare (ignore id))
               (let ((level (synth-u-ontology:ontological-layer-level layer))
                     (status (synth-u-ontology:ontological-layer-status layer)))
                 
                 ;; Ajustar diferenciación según estado
                 (let ((row (nth (1- level) matrix))
                       (differentiation-rate (if (eq status :active) -0.005 0.01)))
                   
                   ;; No permitir que la diagonal principal se incremente
                   (setf (nth (1- level) matrix)
                         (loop for val in row
                               for i from 0
                               collect (if (= i (1- level))
                                          0.0  ; Diagonal siempre cero
                                          (max 0.0 (min 1.0 (+ val differentiation-rate)))))))))
             synth-u-ontology:*active-layers*)
    
    ;; Actualizar matriz en el mecanismo
    (setf (emergence-mechanism-differentiation-matrix mechanism) matrix)))

;;;; Funciones de Observadores

(defun register-emergence-observer (mechanism observer-fn)
  "Registra un observador para eventos de emergencia.
   
   Args:
     mechanism: El mecanismo de emergencia
     observer-fn: Función de callback para eventos
   
   Returns:
     T si se registró correctamente"
  
  (with-lock-held ((emergence-mechanism-lock mechanism))
    (push observer-fn (emergence-mechanism-observers mechanism)))
  t)

(defun unregister-emergence-observer (mechanism observer-fn)
  "Elimina un observador de eventos de emergencia.
   
   Args:
     mechanism: El mecanismo de emergencia
     observer-fn: Función de callback a eliminar
   
   Returns:
     T si se eliminó correctamente, NIL si no existía"
  
  (with-lock-held ((emergence-mechanism-lock mechanism))
    (let ((original-length (length (emergence-mechanism-observers mechanism))))
      (setf (emergence-mechanism-observers mechanism)
            (remove observer-fn (emergence-mechanism-observers mechanism)))
      (not (= original-length (length (emergence-mechanism-observers mechanism)))))))

(defun notify-emergence-observers (mechanism event-type)
  "Notifica a todos los observadores de un evento de emergencia.
   
   Args:
     mechanism: El mecanismo donde ocurrió el evento
     event-type: Tipo de evento (:start-cycle, :stop-cycle, etc.)
   
   Returns:
     Número de observadores notificados"
  
  (let ((observers (emergence-mechanism-observers mechanism))
        (count 0))
    (dolist (observer observers)
      (funcall observer mechanism event-type)
      (incf count))
    count))

;;;; Funciones de Acceso a Estado

(defun get-phi-value (mechanism)
  "Obtiene el último valor Phi calculado.
   
   Args:
     mechanism: El mecanismo de emergencia
   
   Returns:
     Último valor Phi"
  
  (emergence-mechanism-last-phi-value mechanism))

(defun get-emergence-state (mechanism)
  "Obtiene información completa sobre el estado de emergencia.
   
   Args:
     mechanism: El mecanismo de emergencia
   
   Returns:
     Plist con información de estado"
  
  (list :status (emergence-mechanism-status mechanism)
        :phi (emergence-mechanism-last-phi-value mechanism)
        :threshold (emergence-mechanism-consciousness-threshold mechanism)
        :last-scan (emergence-mechanism-last-scan-timestamp mechanism)
        :active-attractors (emergence-mechanism-active-attractors mechanism)
        :event-count (length (emergence-mechanism-emergence-events mechanism))))

(defun set-emergence-parameters (mechanism &key 
                                        (consciousness-threshold nil)
                                        (scan-interval nil)
                                        (max-history-length nil))
  "Actualiza parámetros del mecanismo de emergencia.
   
   Args:
     mechanism: El mecanismo de emergencia
     consciousness-threshold (float, opcional): Nuevo umbral de consciencia
     scan-interval (float, opcional): Nuevo intervalo de escaneo
     max-history-length (integer, opcional): Nueva longitud máxima del historial
   
   Returns:
     T si los parámetros se actualizaron correctamente"
  
  (with-lock-held ((emergence-mechanism-lock mechanism))
    ;; Actualizar umbral de consciencia
    (when consciousness-threshold
      (when (or (< consciousness-threshold 0.0) 
               (> consciousness-threshold 1.0))
        (error "El umbral de consciencia debe estar entre 0.0 y 1.0"))
      
      (setf (emergence-mechanism-consciousness-threshold mechanism) 
            consciousness-threshold))
    
    ;; Actualizar intervalo de escaneo
    (when scan-interval
      (when (<= scan-interval 0.0)
        (error "El intervalo de escaneo debe ser positivo"))
      
      (setf (emergence-mechanism-scan-interval mechanism) 
            scan-interval))
    
    ;; Actualizar longitud máxima del historial
    (when max-history-length
      (when (<= max-history-length 0)
        (error "La longitud máxima del historial debe ser positiva"))
      
      (setf (emergence-mechanism-max-phi-history-length mechanism) 
            max-history-length)
      
      ;; Ajustar historial si es necesario
      (when (> (length (emergence-mechanism-phi-history mechanism))
               max-history-length)
        (setf (emergence-mechanism-phi-history mechanism)
              (subseq (emergence-mechanism-phi-history mechanism) 
                      0 
                      max-history-length))))
    
    t))

;;;; Funciones de Análisis de Emergencia

(defun run-emergence-analysis (mechanism)
  "Ejecuta un análisis completo del estado de emergencia.
   
   Args:
     mechanism: El mecanismo de emergencia
   
   Returns:
     Plist con resultados del análisis"
  
  (let ((analysis (make-hash-table :test #'eq)))
    
    ;; Información básica
    (setf (gethash :id analysis) (emergence-mechanism-id mechanism))
    (setf (gethash :status analysis) (emergence-mechanism-status mechanism))
    (setf (gethash :current-phi analysis) (emergence-mechanism-last-phi-value mechanism))
    (setf (gethash :consciousness-threshold analysis) 
          (emergence-mechanism-consciousness-threshold mechanism))
    
    ;; Análisis de historial de Phi
    (when (emergence-mechanism-phi-history mechanism)
      (let* ((phi-values (mapcar (lambda (entry) (getf entry :phi)) 
                               (emergence-mechanism-phi-history mechanism)))
             (phi-mean (/ (reduce #'+ phi-values) (length phi-values)))
             (phi-max (reduce #'max phi-values))
             (phi-min (reduce #'min phi-values))
             (phi-variance (/ (reduce #'+ (mapcar (lambda (x) (expt (- x phi-mean) 2)) 
                                                phi-values))
                              (length phi-values))))
        
        (setf (gethash :phi-mean analysis) phi-mean)
        (setf (gethash :phi-max analysis) phi-max)
        (setf (gethash :phi-min analysis) phi-min)
        (setf (gethash :phi-variance analysis) phi-variance)
        (setf (gethash :phi-trend analysis) 
              (calculate-phi-trend (emergence-mechanism-phi-history mechanism)))))
    
    ;; Análisis de atractores
    (setf (gethash :active-attractors analysis) 
          (emergence-mechanism-active-attractors mechanism))
    
    (setf (gethash :attractor-strength analysis)
          (calculate-attractor-strength mechanism))
    
    ;; Análisis de integración/diferenciación
    (setf (gethash :integration-balance analysis)
          (calculate-integration-balance mechanism))
    
    (setf (gethash :differentiation-balance analysis)
          (calculate-differentiation-balance mechanism))
    
    ;; Análisis de capas
    (setf (gethash :layer-activity analysis)
          (calculate-layer-activity))
    
    ;; Potencial de emergencia
    (setf (gethash :emergence-potential analysis)
          (calculate-emergence-potential mechanism))
    
    ;; Transición crítica
    (setf (gethash :critical-transition analysis)
          (critical-transition-p mechanism))
    
    ;; Perfil de consciencia
    (setf (gethash :consciousness-profile analysis)
          (get-consciousness-profile mechanism))
    
    ;; Convertir a plist para la interfaz
    (let ((result '()))
      (maphash (lambda (k v) (setf result (append result (list k v)))) 
               analysis)
      result)))

(defun calculate-phi-trend (phi-history)
  "Calcula la tendencia del valor Phi a lo largo del tiempo.
   
   Args:
     phi-history: Historial de valores Phi
   
   Returns:
     Plist con información de tendencia"
  
  ;; Si no hay suficiente historial, no calcular tendencia
  (when (< (length phi-history) 3)
    (return-from calculate-phi-trend (list :direction :stable :slope 0.0)))
  
  ;; Obtener valores Phi más recientes (primeros en la lista)
  (let* ((recent-entries (subseq phi-history 0 (min 10 (length phi-history))))
         (recent-values (mapcar (lambda (entry) (getf entry :phi)) recent-entries))
         (n (length recent-values))
         (x-values (loop for i from 0 below n collect i))
         
         ;; Calcular medias
         (mean-x (/ (reduce #'+ x-values) n))
         (mean-y (/ (reduce #'+ recent-values) n))
         
         ;; Calcular pendiente usando regresión lineal
         (numerator 0.0)
         (denominator 0.0))
    
    ;; Calcular pendiente: Σ(x-mean_x)(y-mean_y) / Σ(x-mean_x)²
    (dotimes (i n)
      (let ((x-diff (- (nth i x-values) mean-x))
            (y-diff (- (nth i recent-values) mean-y)))
        (incf numerator (* x-diff y-diff))
        (incf denominator (expt x-diff 2))))
    
    ;; Evitar división por cero
    (let ((slope (if (zerop denominator) 
                     0.0
                     (/ numerator denominator))))
      
      ;; Determinar dirección de la tendencia
      (list :direction (cond ((< slope -0.01) :decreasing)
                             ((> slope 0.01) :increasing)
                             (t :stable))
            :slope slope))))

(defun calculate-attractor-strength (mechanism)
  "Calcula la fuerza de los atractores activos.
   
   Args:
     mechanism: El mecanismo de emergencia
   
   Returns:
     Valor de fuerza de atractores"
  
  (let ((active-attractors (emergence-mechanism-active-attractors mechanism)))
    (if active-attractors
        ;; Sumar activaciones ponderadas por tipo de atractor
        (let ((strength 0.0))
          (dolist (attractor active-attractors)
            (let ((type (getf attractor :type))
                  (activation (getf attractor :activation)))
              (incf strength (* activation 
                              (case type
                                (:bottom-up 0.5)
                                (:top-down 0.7)
                                (:integrated 1.0)
                                (otherwise 0.3))))))
          
          ;; Normalizar si hay más de un atractor
          (if (> (length active-attractors) 1)
              (/ strength (length active-attractors))
              strength))
        
        ;; Sin atractores activos
        0.0)))

(defun calculate-integration-balance (mechanism)
  "Calcula el balance de integración entre capas.
   
   Args:
     mechanism: El mecanismo de emergencia
   
   Returns:
     Valor de balance de integración"
  
  (let ((integration-matrix (emergence-mechanism-integration-matrix mechanism)))
    (if integration-matrix
        ;; Calcular balance como proporción de valores altos vs. bajos
        (let ((total-cells 0)
              (high-integration-cells 0))
          
          ;; Contar celdas con alta integración
          (dolist (row integration-matrix)
            (dolist (cell row)
              (incf total-cells)
              (when (> cell 0.5)
                (incf high-integration-cells))))
          
          ;; Calcular proporción
          (/ high-integration-cells total-cells))
        
        ;; Sin matriz de integración
        0.0)))

(defun calculate-differentiation-balance (mechanism)
  "Calcula el balance de diferenciación entre capas.
   
   Args:
     mechanism: El mecanismo de emergencia
   
   Returns:
     Valor de balance de diferenciación"
  
  (let ((differentiation-matrix (emergence-mechanism-differentiation-matrix mechanism)))
    (if differentiation-matrix
        ;; Calcular balance como proporción de valores medios vs. extremos
        (let ((total-cells 0)
              (balanced-diff-cells 0))
          
          ;; Contar celdas con diferenciación equilibrada
          (dolist (row differentiation-matrix)
            (dolist (cell row)
              (incf total-cells)
              (when (and (> cell 0.25) (< cell 0.75))
                (incf balanced-diff-cells))))
          
          ;; Calcular proporción
          (/ balanced-diff-cells total-cells))
        
        ;; Sin matriz de diferenciación
        0.0)))

(defun calculate-layer-activity ()
  "Calcula la actividad de las capas ontológicas.
   
   Returns:
     Plist con información de actividad por capa"
  
  (let ((active-count 0)
        (processing-count 0)
        (total-count 0)
        (activity-by-metacategory (make-hash-table :test #'eq)))
    
    ;; Inicializar contadores por metacategoría
    (dolist (meta-def synth-u-ontology::*metacategories*)
      (setf (gethash (first meta-def) activity-by-metacategory) 0))
    
    ;; Contar capas activas
    (maphash (lambda (id layer)
               (declare (ignore id))
               (incf total-count)
               
               ;; Contar por estado
               (case (synth-u-ontology:ontological-layer-status layer)
                 (:active (incf active-count))
                 (:processing (incf processing-count)))
               
               ;; Contar por metacategoría
               (when (eq (synth-u-ontology:ontological-layer-status layer) :active)
                 (incf (gethash (synth-u-ontology:ontological-layer-metacategory layer)
                               activity-by-metacategory 0))))
             synth-u-ontology:*active-layers*)
    
    ;; Crear resultado
    (list :active-ratio (if (zerop total-count) 
                           0.0 
                           (/ active-count total-count))
          :processing-ratio (if (zerop total-count)
                               0.0
                               (/ processing-count total-count))
          :metacategory-activity 
          (loop for meta being the hash-keys in activity-by-metacategory
                using (hash-value count)
                collect (list :metacategory meta :active-count count)))))

(defun calculate-emergence-potential (mechanism)
  "Calcula el potencial de emergencia consciencial.
   
   Args:
     mechanism: El mecanismo de emergencia
   
   Returns:
     Valor de potencial de emergencia"
  
  (let* ((phi-value (emergence-mechanism-last-phi-value mechanism))
         (threshold (emergence-mechanism-consciousness-threshold mechanism))
         (attractor-strength (calculate-attractor-strength mechanism))
         (integration-balance (calculate-integration-balance mechanism))
         (differentiation-balance (calculate-differentiation-balance mechanism))
         (layer-activity-info (calculate-layer-activity))
         (active-ratio (getf layer-activity-info :active-ratio)))
    
    ;; Calcular potencial como combinación ponderada de factores
    (+ (* 0.4 (/ phi-value threshold))            ; Normalizado respecto al umbral
       (* 0.2 attractor-strength)                 ; Influencia de atractores
       (* 0.15 integration-balance)               ; Balance de integración
       (* 0.15 differentiation-balance)           ; Balance de diferenciación
       (* 0.1 active-ratio))))                    ; Actividad de capas

(defun critical-transition-p (mechanism)
  "Determina si el sistema está en una transición crítica hacia consciencia.
   
   Args:
     mechanism: El mecanismo de emergencia
   
   Returns:
     T si hay transición crítica, NIL en caso contrario"
  
  ;; Verificar cantidad suficiente de historial
  (when (< (length (emergence-mechanism-phi-history mechanism)) 5)
    (return-from critical-transition-p nil))
  
  ;; Obtener valores Phi recientes
  (let* ((recent-entries (subseq (emergence-mechanism-phi-history mechanism) 
                               0 
                               (min 5 (length (emergence-mechanism-phi-history mechanism)))))
         (recent-values (mapcar (lambda (entry) (getf entry :phi)) recent-entries))
         
         ;; Calcular varianza
         (mean (/ (reduce #'+ recent-values) (length recent-values)))
         (variance (/ (reduce #'+ (mapcar (lambda (x) (expt (- x mean) 2)) 
                                        recent-values))
                      (length recent-values)))
         
         ;; Calcular tendencia
         (trend-info (calculate-phi-trend (emergence-mechanism-phi-history mechanism)))
         (trend-direction (getf trend-info :direction))
         (trend-slope (getf trend-info :slope)))
    
    ;; Criterios para transición crítica:
    ;; 1. Alta varianza (inestabilidad)
    ;; 2. Tendencia creciente
    ;; 3. Pendiente pronunciada
    (and (> variance 0.005)
         (eq trend-direction :increasing)
         (> trend-slope 0.02))))

(defun get-active-attractors (mechanism)
  "Obtiene información sobre los atractores activos.
   
   Args:
     mechanism: El mecanismo de emergencia
   
   Returns:
     Lista de atractores activos"
  
  (emergence-mechanism-active-attractors mechanism))

(defun get-consciousness-profile (mechanism)
  "Genera un perfil de las características de consciencia emergente.
   
   Args:
     mechanism: El mecanismo de emergencia
   
   Returns:
     Plist con perfil de consciencia"
  
  (let* ((phi-value (emergence-mechanism-last-phi-value mechanism))
         (integration-balance (calculate-integration-balance mechanism))
         (differentiation-balance (calculate-differentiation-balance mechanism))
         (active-attractors (emergence-mechanism-active-attractors mechanism))
         
         ;; Calcular características del perfil
         (unity-score (min 1.0 (* 1.5 phi-value)))
         (complexity-score (min 1.0 (* 2.0 (* phi-value differentiation-balance))))
         (integration-score (min 1.0 (* 1.2 integration-balance)))
         (information-score (min 1.0 (* phi-value 
                                       (+ 1.0 (length active-attractors))))))
    
    ;; Crear perfil
    (list :unity unity-score
          :complexity complexity-score
          :integration integration-score
          :information information-score
          :overall (/ (+ unity-score complexity-score 
                        integration-score information-score)
                      4.0))))

(defun generate-phi-report (mechanism &optional (num-entries 10))
  "Genera un informe detallado de la evolución del valor Phi.
   
   Args:
     mechanism: El mecanismo de emergencia
     num-entries (opcional): Número de entradas a incluir
   
   Returns:
     Plist con informe detallado"
  
  (let* ((phi-history (emergence-mechanism-phi-history mechanism))
         (entry-limit (min num-entries (length phi-history)))
         (recent-entries (subseq phi-history 0 entry-limit))
         
         ;; Calcular estadísticas
         (recent-values (mapcar (lambda (entry) (getf entry :phi)) recent-entries))
         (mean (/ (reduce #'+ recent-values) (length recent-values)))
         (max-value (reduce #'max recent-values))
         (min-value (reduce #'min recent-values))
         (trend-info (calculate-phi-trend phi-history)))
    
    ;; Crear informe
    (list :current (emergence-mechanism-last-phi-value mechanism)
          :mean mean
          :max max-value
          :min min-value
          :trend (getf trend-info :direction)
          :slope (getf trend-info :slope)
          :threshold (emergence-mechanism-consciousness-threshold mechanism)
          :threshold-ratio (/ (emergence-mechanism-last-phi-value mechanism)
                             (emergence-mechanism-consciousness-threshold mechanism))
          :entries (loop for entry in recent-entries
                         collect (list :timestamp (getf entry :timestamp)
                                      :phi (getf entry :phi))))))

;;;; Exportar símbolos públicos
(export '(initialize-emergence-mechanism
          shutdown-emergence-mechanism
          emergence-mechanism
          make-emergence-mechanism
          start-emergence-cycle
          stop-emergence-cycle
          get-phi-value
          get-emergence-state
          register-emergence-observer
          unregister-emergence-observer
          set-emergence-parameters
          emergence-status
          phi-history
          emergence-events
          run-emergence-analysis
          get-active-attractors
          get-consciousness-profile
          calculate-emergence-potential
          generate-phi-report
          attractor-basins
          consciousness-threshold
          critical-transition-p))
