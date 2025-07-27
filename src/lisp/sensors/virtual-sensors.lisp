;;;; -----------------------------------------------------
;;;; MODULE: virtual-sensors.lisp
;;;; DESCRIPTION: Implementation of virtual sensors for the Synth-U system
;;;; DEPENDENCIES: Alexandria, Bordeaux-Threads, synth-u-core, synth-u-ontology
;;;; -----------------------------------------------------

(defpackage :synth-u-sensors
  (:use :cl :alexandria :bordeaux-threads :synth-u-ontology)
  (:export #:sensor
           #:make-sensor
           #:sensor-id
           #:sensor-type
           #:sensor-mode
           #:sensor-status
           #:sensor-range
           #:sensor-resolution
           #:sensor-noise-level
           #:sensor-sampling-rate
           #:sensor-buffer
           #:sensor-preprocessors
           #:activate-sensor
           #:deactivate-sensor
           #:read-sensor
           #:configure-sensor
           #:attach-preprocessor
           #:detach-preprocessor
           #:get-sensor-readings
           #:connect-to-layer
           #:disconnect-from-layer
           #:register-sensor-observer
           #:unregister-sensor-observer
           #:discrete-sensor
           #:continuous-sensor
           #:visual-sensor
           #:auditory-sensor
           #:tactile-sensor
           #:proprioceptive-sensor
           #:make-discrete-sensor
           #:make-continuous-sensor
           #:make-visual-sensor
           #:make-auditory-sensor
           #:make-tactile-sensor
           #:make-proprioceptive-sensor
           #:with-sensor-reading))

(in-package :synth-u-sensors)

;;;; Constantes y Variables Globales
(defconstant +sensors-version+ "0.1.0"
  "Versión actual del sistema de sensores virtuales")

(defparameter *active-sensors* (make-hash-table :test #'equal)
  "Tabla hash de todos los sensores activos, indexados por ID")

(defparameter *system-running* nil
  "Estado global del sistema de sensores")

(defparameter *default-preprocessors* (make-hash-table :test #'eq)
  "Preprocesadores por defecto para cada tipo de sensor")

;;;; Definiciones de tipos

;; Enumeración de estados de sensor
(deftype sensor-status ()
  '(member :active :inactive :error :calibrating :overloaded :disconnected))

;; Enumeración de tipos de sensor
(deftype sensor-type ()
  '(member :visual :auditory :tactile :proprioceptive :temperature :pressure
    :chemical :electromagnetic :inertial :proximity :distance :custom))

;; Enumeración de modos de sensor
(deftype sensor-mode ()
  '(member :discrete :continuous :hybrid :threshold :periodic :event-driven))

;;;; Estructuras de Datos Principales

(defstruct (sensor (:constructor %make-sensor))
  "Estructura principal para un sensor virtual"
  (id nil :type string :read-only t)
  (type nil :type sensor-type)
  (mode nil :type sensor-mode)
  (status :inactive :type sensor-status)
  (description "" :type string)
  (range nil :type list)  ; (min max)
  (resolution nil :type float)
  (noise-level 0.0 :type float)
  (sampling-rate 1.0 :type float)  ; Hz
  (buffer nil :type list)
  (buffer-size 100 :type integer)
  (preprocessors nil :type list)
  (connected-layers nil :type list)
  (observers nil :type list)
  (thread nil)
  (lock (make-lock))
  (condition-variable (make-condition-variable))
  (last-reading nil)
  (last-reading-timestamp nil)
  (data-source nil)
  (calibration-data nil)
  (properties (make-hash-table :test #'eq) :type hash-table)
  (creation-timestamp (get-universal-time) :read-only t))

;;;; Especializaciones de Sensores

(defstruct (discrete-sensor (:include sensor)
                          (:constructor %make-discrete-sensor))
  "Sensor con valores discretos/categóricos"
  (states nil :type list)  ; Lista de posibles estados
  (current-state nil)
  (transition-matrix nil))  ; Matriz de probabilidad de transición entre estados

(defstruct (continuous-sensor (:include sensor)
                            (:constructor %make-continuous-sensor))
  "Sensor con valores continuos/numéricos"
  (min-value 0.0 :type float)
  (max-value 1.0 :type float)
  (unit "" :type string)
  (precision 2 :type integer)  ; Número de decimales
  (current-value 0.0 :type float)
  (gradient nil))  ; Función que describe cambio gradual

;; Sensores específicos
(defstruct (visual-sensor (:include continuous-sensor)
                        (:constructor %make-visual-sensor))
  "Sensor visual (cámara virtual)"
  (width 640 :type integer)
  (height 480 :type integer)
  (channels 3 :type integer)  ; RGB = 3, RGBA = 4, Grayscale = 1
  (field-of-view 90.0 :type float)  ; En grados
  (focal-length 1.0 :type float)
  (current-frame nil))

(defstruct (auditory-sensor (:include continuous-sensor)
                          (:constructor %make-auditory-sensor))
  "Sensor auditivo (micrófono virtual)"
  (sample-rate 44100 :type integer)  ; Hz
  (bit-depth 16 :type integer)
  (channels 1 :type integer)  ; Mono = 1, Stereo = 2
  (frequency-range (list 20.0 20000.0) :type list)  ; Hz
  (current-audio-data nil))

(defstruct (tactile-sensor (:include continuous-sensor)
                         (:constructor %make-tactile-sensor))
  "Sensor táctil"
  (pressure-sensitivity 1.0 :type float)
  (contact-area 1.0 :type float)
  (texture-detection nil :type boolean)
  (current-pressure 0.0 :type float))

(defstruct (proprioceptive-sensor (:include continuous-sensor)
                                (:constructor %make-proprioceptive-sensor))
  "Sensor propioceptivo (posición interna)"
  (axes 3 :type integer)  ; Número de ejes (x, y, z)
  (position (list 0.0 0.0 0.0) :type list)
  (orientation (list 0.0 0.0 0.0) :type list)  ; Pitch, yaw, roll
  (velocity (list 0.0 0.0 0.0) :type list))

;;;; Funciones Constructor

(defun make-sensor (id type mode &key 
                      description 
                      range 
                      resolution 
                      noise-level 
                      sampling-rate
                      buffer-size)
  "Crea un nuevo sensor virtual básico.
   
   Args:
     id (string): Identificador único para el sensor
     type (sensor-type): Tipo de sensor
     mode (sensor-mode): Modo de operación del sensor
     description (string, opcional): Descripción del sensor
     range (list, opcional): Rango de valores (min max)
     resolution (float, opcional): Resolución del sensor
     noise-level (float, opcional): Nivel de ruido (0.0-1.0)
     sampling-rate (float, opcional): Tasa de muestreo en Hz
     buffer-size (integer, opcional): Tamaño del buffer de lecturas
   
   Returns:
     Instancia de sensor inicializada"
  
  ;; Validación
  (when (or (null id) (string= id ""))
    (error "El ID del sensor no puede estar vacío"))
  
  (when (gethash id *active-sensors*)
    (error "Ya existe un sensor con ID: ~A" id))
  
  ;; Crear el sensor
  (let ((sensor (%make-sensor :id id
                            :type type
                            :mode mode
                            :description (or description "")
                            :range (or range (list 0.0 1.0))
                            :resolution (or resolution 0.01)
                            :noise-level (or noise-level 0.0)
                            :sampling-rate (or sampling-rate 1.0)
                            :buffer-size (or buffer-size 100))))
    
    ;; Inicializar buffer vacío
    (setf (sensor-buffer sensor) (make-list (sensor-buffer-size sensor) :initial-element nil))
    
    ;; Añadir preprocesadores por defecto según tipo
    (when-let ((default-preprocessors (gethash type *default-preprocessors*)))
      (setf (sensor-preprocessors sensor) (copy-list default-preprocessors)))
    
    ;; Registrar en el sistema global
    (setf (gethash id *active-sensors*) sensor)
    
    ;; Devolver el sensor creado
    sensor))

;;;; Funciones Constructoras Especializadas

(defun make-discrete-sensor (id &key 
                               description 
                               states 
                               current-state
                               noise-level 
                               sampling-rate
                               buffer-size
                               transition-matrix)
  "Crea un nuevo sensor discreto (con estados).
   
   Args:
     id (string): Identificador único para el sensor
     description (string, opcional): Descripción del sensor
     states (list, opcional): Lista de posibles estados
     current-state (opcional): Estado inicial
     noise-level (float, opcional): Nivel de ruido (0.0-1.0)
     sampling-rate (float, opcional): Tasa de muestreo en Hz
     buffer-size (integer, opcional): Tamaño del buffer de lecturas
     transition-matrix (opcional): Matriz de probabilidad de transición
   
   Returns:
     Instancia de sensor discreto inicializada"
  
  ;; Crear sensor base
  (let ((sensor (make-sensor id :custom :discrete
                           :description description
                           :noise-level noise-level
                           :sampling-rate sampling-rate
                           :buffer-size buffer-size)))
    
    ;; Convertir a sensor discreto y configurar
    (let ((discrete-sensor (%make-discrete-sensor
                            :id (sensor-id sensor)
                            :type (sensor-type sensor)
                            :mode (sensor-mode sensor)
                            :status (sensor-status sensor)
                            :description (sensor-description sensor)
                            :range (sensor-range sensor)
                            :resolution (sensor-resolution sensor)
                            :noise-level (sensor-noise-level sensor)
                            :sampling-rate (sensor-sampling-rate sensor)
                            :buffer (sensor-buffer sensor)
                            :buffer-size (sensor-buffer-size sensor)
                            :preprocessors (sensor-preprocessors sensor)
                            :connected-layers (sensor-connected-layers sensor)
                            :observers (sensor-observers sensor)
                            :thread (sensor-thread sensor)
                            :lock (sensor-lock sensor)
                            :condition-variable (sensor-condition-variable sensor)
                            :last-reading (sensor-last-reading sensor)
                            :last-reading-timestamp (sensor-last-reading-timestamp sensor)
                            :data-source (sensor-data-source sensor)
                            :calibration-data (sensor-calibration-data sensor)
                            :properties (sensor-properties sensor)
                            :creation-timestamp (sensor-creation-timestamp sensor)
                            :states (or states '(:on :off))
                            :current-state (or current-state (first (or states '(:on :off))))
                            :transition-matrix transition-matrix)))
      
      ;; Actualizar en tabla de sensores activos
      (setf (gethash id *active-sensors*) discrete-sensor)
      
      ;; Devolver el sensor discreto
      discrete-sensor)))

(defun make-continuous-sensor (id &key 
                                description 
                                min-value 
                                max-value
                                unit
                                precision
                                current-value
                                noise-level 
                                sampling-rate
                                buffer-size
                                gradient)
  "Crea un nuevo sensor continuo (con valores numéricos).
   
   Args:
     id (string): Identificador único para el sensor
     description (string, opcional): Descripción del sensor
     min-value (float, opcional): Valor mínimo
     max-value (float, opcional): Valor máximo
     unit (string, opcional): Unidad de medida
     precision (integer, opcional): Precisión decimal
     current-value (float, opcional): Valor inicial
     noise-level (float, opcional): Nivel de ruido (0.0-1.0)
     sampling-rate (float, opcional): Tasa de muestreo en Hz
     buffer-size (integer, opcional): Tamaño del buffer de lecturas
     gradient (opcional): Función de cambio gradual
   
   Returns:
     Instancia de sensor continuo inicializada"
  
  ;; Valores por defecto
  (let ((min-val (or min-value 0.0))
        (max-val (or max-value 1.0)))
    
    ;; Crear sensor base
    (let ((sensor (make-sensor id :custom :continuous
                             :description description
                             :range (list min-val max-val)
                             :noise-level noise-level
                             :sampling-rate sampling-rate
                             :buffer-size buffer-size)))
      
      ;; Convertir a sensor continuo y configurar
      (let ((continuous-sensor (%make-continuous-sensor
                               :id (sensor-id sensor)
                               :type (sensor-type sensor)
                               :mode (sensor-mode sensor)
                               :status (sensor-status sensor)
                               :description (sensor-description sensor)
                               :range (sensor-range sensor)
                               :resolution (sensor-resolution sensor)
                               :noise-level (sensor-noise-level sensor)
                               :sampling-rate (sensor-sampling-rate sensor)
                               :buffer (sensor-buffer sensor)
                               :buffer-size (sensor-buffer-size sensor)
                               :preprocessors (sensor-preprocessors sensor)
                               :connected-layers (sensor-connected-layers sensor)
                               :observers (sensor-observers sensor)
                               :thread (sensor-thread sensor)
                               :lock (sensor-lock sensor)
                               :condition-variable (sensor-condition-variable sensor)
                               :last-reading (sensor-last-reading sensor)
                               :last-reading-timestamp (sensor-last-reading-timestamp sensor)
                               :data-source (sensor-data-source sensor)
                               :calibration-data (sensor-calibration-data sensor)
                               :properties (sensor-properties sensor)
                               :creation-timestamp (sensor-creation-timestamp sensor)
                               :min-value min-val
                               :max-value max-val
                               :unit (or unit "")
                               :precision (or precision 2)
                               :current-value (or current-value min-val)
                               :gradient gradient)))
        
        ;; Actualizar en tabla de sensores activos
        (setf (gethash id *active-sensors*) continuous-sensor)
        
        ;; Devolver el sensor continuo
        continuous-sensor))))

;; Constructores específicos para tipos de sensores

(defun make-visual-sensor (id &key 
                            description 
                            width 
                            height
                            channels
                            field-of-view
                            focal-length
                            noise-level 
                            sampling-rate
                            buffer-size)
  "Crea un nuevo sensor visual (cámara virtual).
   
   Args:
     id (string): Identificador único para el sensor
     description (string, opcional): Descripción del sensor
     width (integer, opcional): Ancho de la imagen en píxeles
     height (integer, opcional): Alto de la imagen en píxeles
     channels (integer, opcional): Canales de color (RGB=3, RGBA=4, Grayscale=1)
     field-of-view (float, opcional): Campo de visión en grados
     focal-length (float, opcional): Longitud focal
     noise-level (float, opcional): Nivel de ruido (0.0-1.0)
     sampling-rate (float, opcional): Tasa de muestreo en Hz (FPS)
     buffer-size (integer, opcional): Tamaño del buffer de lecturas
   
   Returns:
     Instancia de sensor visual inicializada"
  
  ;; Crear sensor continuo base
  (let ((sensor (make-continuous-sensor id
                                      :description description
                                      :min-value 0.0
                                      :max-value 1.0
                                      :unit "intensity"
                                      :noise-level noise-level
                                      :sampling-rate sampling-rate
                                      :buffer-size buffer-size)))
    
    ;; Convertir a sensor visual y configurar
    (let ((visual-sensor (%make-visual-sensor
                         :id (sensor-id sensor)
                         :type :visual
                         :mode (sensor-mode sensor)
                         :status (sensor-status sensor)
                         :description (sensor-description sensor)
                         :range (sensor-range sensor)
                         :resolution (sensor-resolution sensor)
                         :noise-level (sensor-noise-level sensor)
                         :sampling-rate (sensor-sampling-rate sensor)
                         :buffer (sensor-buffer sensor)
                         :buffer-size (sensor-buffer-size sensor)
                         :preprocessors (sensor-preprocessors sensor)
                         :connected-layers (sensor-connected-layers sensor)
                         :observers (sensor-observers sensor)
                         :thread (sensor-thread sensor)
                         :lock (sensor-lock sensor)
                         :condition-variable (sensor-condition-variable sensor)
                         :last-reading (sensor-last-reading sensor)
                         :last-reading-timestamp (sensor-last-reading-timestamp sensor)
                         :data-source (sensor-data-source sensor)
                         :calibration-data (sensor-calibration-data sensor)
                         :properties (sensor-properties sensor)
                         :creation-timestamp (sensor-creation-timestamp sensor)
                         :min-value (continuous-sensor-min-value sensor)
                         :max-value (continuous-sensor-max-value sensor)
                         :unit (continuous-sensor-unit sensor)
                         :precision (continuous-sensor-precision sensor)
                         :current-value (continuous-sensor-current-value sensor)
                         :gradient (continuous-sensor-gradient sensor)
                         :width (or width 640)
                         :height (or height 480)
                         :channels (or channels 3)
                         :field-of-view (or field-of-view 90.0)
                         :focal-length (or focal-length 1.0))))
      
      ;; Actualizar en tabla de sensores activos
      (setf (gethash id *active-sensors*) visual-sensor)
      
      ;; Devolver el sensor visual
      visual-sensor)))

(defun make-auditory-sensor (id &key 
                              description 
                              sample-rate
                              bit-depth
                              channels
                              frequency-range
                              noise-level 
                              sampling-rate
                              buffer-size)
  "Crea un nuevo sensor auditivo (micrófono virtual).
   
   Args:
     id (string): Identificador único para el sensor
     description (string, opcional): Descripción del sensor
     sample-rate (integer, opcional): Tasa de muestreo de audio en Hz
     bit-depth (integer, opcional): Profundidad de bits
     channels (integer, opcional): Número de canales (Mono=1, Stereo=2)
     frequency-range (list, opcional): Rango de frecuencias en Hz (min max)
     noise-level (float, opcional): Nivel de ruido (0.0-1.0)
     sampling-rate (float, opcional): Tasa de muestreo del sensor en Hz
     buffer-size (integer, opcional): Tamaño del buffer de lecturas
   
   Returns:
     Instancia de sensor auditivo inicializada"
  
  ;; Crear sensor continuo base
  (let ((sensor (make-continuous-sensor id
                                      :description description
                                      :min-value -1.0
                                      :max-value 1.0
                                      :unit "amplitude"
                                      :noise-level noise-level
                                      :sampling-rate sampling-rate
                                      :buffer-size buffer-size)))
    
    ;; Convertir a sensor auditivo y configurar
    (let ((auditory-sensor (%make-auditory-sensor
                           :id (sensor-id sensor)
                           :type :auditory
                           :mode (sensor-mode sensor)
                           :status (sensor-status sensor)
                           :description (sensor-description sensor)
                           :range (sensor-range sensor)
                           :resolution (sensor-resolution sensor)
                           :noise-level (sensor-noise-level sensor)
                           :sampling-rate (sensor-sampling-rate sensor)
                           :buffer (sensor-buffer sensor)
                           :buffer-size (sensor-buffer-size sensor)
                           :preprocessors (sensor-preprocessors sensor)
                           :connected-layers (sensor-connected-layers sensor)
                           :observers (sensor-observers sensor)
                           :thread (sensor-thread sensor)
                           :lock (sensor-lock sensor)
                           :condition-variable (sensor-condition-variable sensor)
                           :last-reading (sensor-last-reading sensor)
                           :last-reading-timestamp (sensor-last-reading-timestamp sensor)
                           :data-source (sensor-data-source sensor)
                           :calibration-data (sensor-calibration-data sensor)
                           :properties (sensor-properties sensor)
                           :creation-timestamp (sensor-creation-timestamp sensor)
                           :min-value (continuous-sensor-min-value sensor)
                           :max-value (continuous-sensor-max-value sensor)
                           :unit (continuous-sensor-unit sensor)
                           :precision (continuous-sensor-precision sensor)
                           :current-value (continuous-sensor-current-value sensor)
                           :gradient (continuous-sensor-gradient sensor)
                           :sample-rate (or sample-rate 44100)
                           :bit-depth (or bit-depth 16)
                           :channels (or channels 1)
                           :frequency-range (or frequency-range (list 20.0 20000.0)))))
      
      ;; Actualizar en tabla de sensores activos
      (setf (gethash id *active-sensors*) auditory-sensor)
      
      ;; Devolver el sensor auditivo
      auditory-sensor)))

(defun make-tactile-sensor (id &key 
                             description 
                             pressure-sensitivity
                             contact-area
                             texture-detection
                             noise-level 
                             sampling-rate
                             buffer-size)
  "Crea un nuevo sensor táctil.
   
   Args:
     id (string): Identificador único para el sensor
     description (string, opcional): Descripción del sensor
     pressure-sensitivity (float, opcional): Sensibilidad a la presión
     contact-area (float, opcional): Área de contacto
     texture-detection (boolean, opcional): Detección de texturas
     noise-level (float, opcional): Nivel de ruido (0.0-1.0)
     sampling-rate (float, opcional): Tasa de muestreo en Hz
     buffer-size (integer, opcional): Tamaño del buffer de lecturas
   
   Returns:
     Instancia de sensor táctil inicializada"
  
  ;; Crear sensor continuo base
  (let ((sensor (make-continuous-sensor id
                                      :description description
                                      :min-value 0.0
                                      :max-value 10.0
                                      :unit "pressure"
                                      :noise-level noise-level
                                      :sampling-rate sampling-rate
                                      :buffer-size buffer-size)))
    
    ;; Convertir a sensor táctil y configurar
    (let ((tactile-sensor (%make-tactile-sensor
                          :id (sensor-id sensor)
                          :type :tactile
                          :mode (sensor-mode sensor)
                          :status (sensor-status sensor)
                          :description (sensor-description sensor)
                          :range (sensor-range sensor)
                          :resolution (sensor-resolution sensor)
                          :noise-level (sensor-noise-level sensor)
                          :sampling-rate (sensor-sampling-rate sensor)
                          :buffer (sensor-buffer sensor)
                          :buffer-size (sensor-buffer-size sensor)
                          :preprocessors (sensor-preprocessors sensor)
                          :connected-layers (sensor-connected-layers sensor)
                          :observers (sensor-observers sensor)
                          :thread (sensor-thread sensor)
                          :lock (sensor-lock sensor)
                          :condition-variable (sensor-condition-variable sensor)
                          :last-reading (sensor-last-reading sensor)
                          :last-reading-timestamp (sensor-last-reading-timestamp sensor)
                          :data-source (sensor-data-source sensor)
                          :calibration-data (sensor-calibration-data sensor)
                          :properties (sensor-properties sensor)
                          :creation-timestamp (sensor-creation-timestamp sensor)
                          :min-value (continuous-sensor-min-value sensor)
                          :max-value (continuous-sensor-max-value sensor)
                          :unit (continuous-sensor-unit sensor)
                          :precision (continuous-sensor-precision sensor)
                          :current-value (continuous-sensor-current-value sensor)
                          :gradient (continuous-sensor-gradient sensor)
                          :pressure-sensitivity (or pressure-sensitivity 1.0)
                          :contact-area (or contact-area 1.0)
                          :texture-detection (or texture-detection nil))))
      
      ;; Actualizar en tabla de sensores activos
      (setf (gethash id *active-sensors*) tactile-sensor)
      
      ;; Devolver el sensor táctil
      tactile-sensor)))

(defun make-proprioceptive-sensor (id &key 
                                    description 
                                    axes
                                    position
                                    orientation
                                    velocity
                                    noise-level 
                                    sampling-rate
                                    buffer-size)
  "Crea un nuevo sensor propioceptivo.
   
   Args:
     id (string): Identificador único para el sensor
     description (string, opcional): Descripción del sensor
     axes (integer, opcional): Número de ejes (normalmente 3 para x, y, z)
     position (list, opcional): Posición inicial (x, y, z)
     orientation (list, opcional): Orientación inicial (pitch, yaw, roll)
     velocity (list, opcional): Velocidad inicial (vx, vy, vz)
     noise-level (float, opcional): Nivel de ruido (0.0-1.0)
     sampling-rate (float, opcional): Tasa de muestreo en Hz
     buffer-size (integer, opcional): Tamaño del buffer de lecturas
   
   Returns:
     Instancia de sensor propioceptivo inicializada"
  
  ;; Crear sensor continuo base
  (let ((sensor (make-continuous-sensor id
                                      :description description
                                      :min-value -1000.0
                                      :max-value 1000.0
                                      :unit "position"
                                      :noise-level noise-level
                                      :sampling-rate sampling-rate
                                      :buffer-size buffer-size)))
    
    ;; Configurar axes y valores por defecto
    (let* ((num-axes (or axes 3))
           (default-zero-vector (make-list num-axes :initial-element 0.0))
           (pos (or position default-zero-vector))
           (orient (or orientation default-zero-vector))
           (vel (or velocity default-zero-vector)))
      
      ;; Convertir a sensor propioceptivo y configurar
      (let ((proprioceptive-sensor (%make-proprioceptive-sensor
                                  :id (sensor-id sensor)
                                  :type :proprioceptive
                                  :mode (sensor-mode sensor)
                                  :status (sensor-status sensor)
                                  :description (sensor-description sensor)
                                  :range (sensor-range sensor)
                                  :resolution (sensor-resolution sensor)
                                  :noise-level (sensor-noise-level sensor)
                                  :sampling-rate (sensor-sampling-rate sensor)
                                  :buffer (sensor-buffer sensor)
                                  :buffer-size (sensor-buffer-size sensor)
                                  :preprocessors (sensor-preprocessors sensor)
                                  :connected-layers (sensor-connected-layers sensor)
                                  :observers (sensor-observers sensor)
                                  :thread (sensor-thread sensor)
                                  :lock (sensor-lock sensor)
                                  :condition-variable (sensor-condition-variable sensor)
                                  :last-reading (sensor-last-reading sensor)
                                  :last-reading-timestamp (sensor-last-reading-timestamp sensor)
                                  :data-source (sensor-data-source sensor)
                                  :calibration-data (sensor-calibration-data sensor)
                                  :properties (sensor-properties sensor)
                                  :creation-timestamp (sensor-creation-timestamp sensor)
                                  :min-value (continuous-sensor-min-value sensor)
                                  :max-value (continuous-sensor-max-value sensor)
                                  :unit (continuous-sensor-unit sensor)
                                  :precision (continuous-sensor-precision sensor)
                                  :current-value (continuous-sensor-current-value sensor)
                                  :gradient (continuous-sensor-gradient sensor)
                                  :axes num-axes
                                  :position pos
                                  :orientation orient
                                  :velocity vel)))
        
        ;; Actualizar en tabla de sensores activos
        (setf (gethash id *active-sensors*) proprioceptive-sensor)
        
        ;; Devolver el sensor propioceptivo
        proprioceptive-sensor))))

;;;; Funciones de Control de Sensores

(defun activate-sensor (sensor)
  "Activa un sensor y comienza su monitoreo.
   
   Args:
     sensor: El sensor a activar
   
   Returns:
     T si la activación fue exitosa, NIL en caso contrario"
  
  (with-lock-held ((sensor-lock sensor))
    (when (eq (sensor-status sensor) :active)
      (return-from activate-sensor nil))
    
    ;; Cambiar estado
    (setf (sensor-status sensor) :active)
    
    ;; Iniciar hilo de monitoreo para sensores periódicos
    (when (eq (sensor-mode sensor) :periodic)
      (setf (sensor-thread sensor)
            (make-thread (lambda () (sensor-monitoring-loop sensor))
                        :name (format nil "sensor-thread-~A" (sensor-id sensor)))))
    
    ;; Notificar a observadores
    (notify-sensor-observers sensor :activation)
    
    t))

(defun deactivate-sensor (sensor)
  "Desactiva un sensor y detiene su monitoreo.
   
   Args:
     sensor: El sensor a desactivar
   
   Returns:
     T si la desactivación fue exitosa, NIL en caso contrario"
  
  (with-lock-held ((sensor-lock sensor))
    (unless (eq (sensor-status sensor) :active)
      (return-from deactivate-sensor nil))
    
    ;; Cambiar estado
    (setf (sensor-status sensor) :inactive)
    
    ;; Detener hilo de monitoreo
    (when (sensor-thread sensor)
      (destroy-thread (sensor-thread sensor))
      (setf (sensor-thread sensor) nil))
    
    ;; Notificar a observadores
    (notify-sensor-observers sensor :deactivation)
    
    t))

(defun sensor-monitoring-loop (sensor)
  "Bucle principal de monitoreo para sensores periódicos.
   
   Args:
     sensor: El sensor a monitorear"
  
  (handler-case
      (loop while (eq (sensor-status sensor) :active)
            do
            ;; Realizar lectura del sensor
            (let ((reading (read-sensor sensor)))
              
              ;; Si hay lectura, procesarla
              (when reading
                ;; Aplicar preprocesadores
                (let ((processed-reading (apply-preprocessors sensor reading)))
                  
                  ;; Almacenar en buffer
                  (with-lock-held ((sensor-lock sensor))
                    (push-to-sensor-buffer sensor processed-reading)
                    
                    ;; Notificar a observadores
                    (notify-sensor-observers sensor :new-reading)))))
            
            ;; Pausar según tasa de muestreo
            (sleep (/ 1.0 (sensor-sampling-rate sensor))))
    (error (e)
      ;; Registrar error
      (with-lock-held ((sensor-lock sensor))
        (setf (sensor-status sensor) :error)
        (setf (gethash :last-error (sensor-properties sensor))
              (format nil "~A" e))
        
        ;; Notificar a observadores
        (notify-sensor-observers sensor :error)))))

(defun read-sensor (sensor)
  "Lee el valor actual del sensor.
   
   Args:
     sensor: El sensor a leer
   
   Returns:
     Lectura del sensor, o NIL si no está activo"
  
  (with-lock-held ((sensor-lock sensor))
    (unless (eq (sensor-status sensor) :active)
      (return-from read-sensor nil))
    
    ;; Realizar lectura según tipo específico de sensor
    (let ((reading (generate-sensor-reading sensor)))
      
      ;; Almacenar última lectura
      (setf (sensor-last-reading sensor) reading)
      (setf (sensor-last-reading-timestamp sensor) (get-universal-time))
      
      ;; Devolver la lectura
      reading)))

(defun generate-sensor-reading (sensor)
  "Genera una lectura para el sensor según su tipo específico.
   
   Args:
     sensor: El sensor para generar la lectura
   
   Returns:
     Lectura generada para el sensor"
  
  ;; Generar lectura según el tipo de sensor
  (typecase sensor
    (discrete-sensor
     ;; Para sensores discretos, devolver el estado actual (posiblemente con ruido)
     (let ((current-state (discrete-sensor-current-state sensor)))
       (if (< (random 1.0) (sensor-noise-level sensor))
           ;; Con ruido, seleccionar un estado aleatorio
           (let ((states (discrete-sensor-states sensor)))
             (nth (random (length states)) states))
           ;; Sin ruido, devolver estado actual
           current-state)))
    
    (continuous-sensor
     ;; Para sensores continuos, devolver valor actual (posiblemente con ruido)
     (let* ((current-value (continuous-sensor-current-value sensor))
            (noise-level (sensor-noise-level sensor))
            (noise-amount (* (- (continuous-sensor-max-value sensor)
                               (continuous-sensor-min-value sensor))
                            noise-level
                            (- (random 2.0) 1.0))))
       (+ current-value noise-amount)))
    
    (visual-sensor
     ;; Para sensores visuales, devolver frame actual
     (let ((current-frame (visual-sensor-current-frame sensor)))
       (if current-frame
           current-frame
           ;; Si no hay frame, generar uno de prueba
           (generate-test-frame sensor))))
    
    (auditory-sensor
     ;; Para sensores auditivos, devolver datos de audio actuales
     (let ((current-audio (auditory-sensor-current-audio-data sensor)))
       (if current-audio
           current-audio
           ;; Si no hay audio, generar datos de prueba
           (generate-test-audio sensor))))
    
    (tactile-sensor
     ;; Para sensores táctiles, devolver presión actual
     (tactile-sensor-current-pressure sensor))
    
    (proprioceptive-sensor
     ;; Para sensores propioceptivos, devolver posición y orientación actuales
     (list :position (proprioceptive-sensor-position sensor)
           :orientation (proprioceptive-sensor-orientation sensor)
           :velocity (proprioceptive-sensor-velocity sensor)))
    
    (otherwise
     ;; Para otros tipos, lectura genérica
     (let* ((range (sensor-range sensor))
            (min-val (first range))
            (max-val (second range))
            (range-size (- max-val min-val)))
       (+ min-val (random range-size))))))

(defun generate-test-frame (sensor)
  "Genera un frame de prueba para un sensor visual.
   
   Args:
     sensor: El sensor visual
   
   Returns:
     Frame de prueba generado"
  
  (let* ((width (visual-sensor-width sensor))
         (height (visual-sensor-height sensor))
         (channels (visual-sensor-channels sensor))
         (frame (make-array (list height width channels) :initial-element 0)))
    
    ;; Generar patrón de prueba (gradiente simple)
    (dotimes (y height)
      (dotimes (x width)
        (let ((r (/ x (float width)))
              (g (/ y (float height)))
              (b (/ (+ x y) (float (+ width height)))))
          
          ;; Establecer valores RGB
          (setf (aref frame y x 0) r)
          (when (>= channels 2)
            (setf (aref frame y x 1) g))
          (when (>= channels 3)
            (setf (aref frame y x 2) b))
          (when (>= channels 4)
            (setf (aref frame y x 3) 1.0)))))  ; Alpha siempre 1.0
    
    ;; Almacenar como frame actual
    (setf (visual-sensor-current-frame sensor) frame)
    
    frame))

(defun generate-test-audio (sensor)
  "Genera datos de audio de prueba para un sensor auditivo.
   
   Args:
     sensor: El sensor auditivo
   
   Returns:
     Datos de audio de prueba generados"
  
  (let* ((sample-rate (auditory-sensor-sample-rate sensor))
         (duration 0.1)  ; 100ms de audio
         (num-samples (round (* sample-rate duration)))
         (audio-data (make-array num-samples :initial-element 0.0)))
    
    ;; Generar tono sinusoidal simple (440Hz = La)
    (let ((frequency 440.0)
          (amplitude 0.5))
      (dotimes (i num-samples)
        (let ((time (/ i (float sample-rate))))
          (setf (aref audio-data i)
                (* amplitude (sin (* 2 pi frequency time)))))))
    
    ;; Almacenar como audio actual
    (setf (auditory-sensor-current-audio-data sensor) audio-data)
    
    audio-data))

(defun push-to-sensor-buffer (sensor reading)
  "Añade una lectura al buffer del sensor, manteniendo el tamaño máximo.
   
   Args:
     sensor: El sensor cuyo buffer se actualizará
     reading: La lectura a añadir
   
   Returns:
     Buffer actualizado"
  
  ;; Añadir al inicio del buffer (más reciente primero)
  (push reading (sensor-buffer sensor))
  
  ;; Limitar tamaño del buffer
  (when (> (length (sensor-buffer sensor)) (sensor-buffer-size sensor))
    (setf (sensor-buffer sensor)
          (subseq (sensor-buffer sensor) 0 (sensor-buffer-size sensor))))
  
  (sensor-buffer sensor))

(defun apply-preprocessors (sensor reading)
  "Aplica los preprocesadores configurados a una lectura del sensor.
   
   Args:
     sensor: El sensor con los preprocesadores
     reading: La lectura a procesar
   
   Returns:
     Lectura procesada"
  
  ;; Si no hay preprocesadores, devolver lectura original
  (if (null (sensor-preprocessors sensor))
      reading
      ;; Aplicar cada preprocesador en secuencia
      (let ((processed-reading reading))
        (dolist (preprocessor (sensor-preprocessors sensor))
          (setf processed-reading (funcall preprocessor processed-reading sensor)))
        processed-reading)))

(defun configure-sensor (sensor &rest properties)
  "Configura propiedades del sensor.
   
   Args:
     sensor: El sensor a configurar
     properties: Plist con propiedades a configurar
   
   Returns:
     T si la configuración fue exitosa"
  
  (with-lock-held ((sensor-lock sensor))
    ;; Configurar propiedades básicas del sensor
    (loop for (key value) on properties by #'cddr
          do (case key
               (:noise-level (setf (sensor-noise-level sensor) value))
               (:sampling-rate (setf (sensor-sampling-rate sensor) value))
               (:resolution (setf (sensor-resolution sensor) value))
               (:buffer-size (setf (sensor-buffer-size sensor) value))
               (:range (setf (sensor-range sensor) value))
               (otherwise 
                ;; Otras propiedades se almacenan en la tabla hash
                (setf (gethash key (sensor-properties sensor)) value))))
    
    ;; Configurar propiedades específicas según tipo de sensor
    (typecase sensor
      (discrete-sensor
       (loop for (key value) on properties by #'cddr
             do (case key
                  (:states (setf (discrete-sensor-states sensor) value))
                  (:current-state (setf (discrete-sensor-current-state sensor) value))
                  (:transition-matrix (setf (discrete-sensor-transition-matrix sensor) value))
                  (otherwise nil))))
      
      (continuous-sensor
       (loop for (key value) on properties by #'cddr
             do (case key
                  (:min-value (setf (continuous-sensor-min-value sensor) value))
                  (:max-value (setf (continuous-sensor-max-value sensor) value))
                  (:unit (setf (continuous-sensor-unit sensor) value))
                  (:precision (setf (continuous-sensor-precision sensor) value))
                  (:current-value (setf (continuous-sensor-current-value sensor) value))
                  (:gradient (setf (continuous-sensor-gradient sensor) value))
                  (otherwise nil))))
      
      (visual-sensor
       (loop for (key value) on properties by #'cddr
             do (case key
                  (:width (setf (visual-sensor-width sensor) value))
                  (:height (setf (visual-sensor-height sensor) value))
                  (:channels (setf (visual-sensor-channels sensor) value))
                  (:field-of-view (setf (visual-sensor-field-of-view sensor) value))
                  (:focal-length (setf (visual-sensor-focal-length sensor) value))
                  (:current-frame (setf (visual-sensor-current-frame sensor) value))
                  (otherwise nil))))
      
      (auditory-sensor
       (loop for (key value) on properties by #'cddr
             do (case key
                  (:sample-rate (setf (auditory-sensor-sample-rate sensor) value))
                  (:bit-depth (setf (auditory-sensor-bit-depth sensor) value))
                  (:channels (setf (auditory-sensor-channels sensor) value))
                  (:frequency-range (setf (auditory-sensor-frequency-range sensor) value))
                  (:current-audio-data (setf (auditory-sensor-current-audio-data sensor) value))
                  (otherwise nil))))
      
      (tactile-sensor
       (loop for (key value) on properties by #'cddr
             do (case key
                  (:pressure-sensitivity (setf (tactile-sensor-pressure-sensitivity sensor) value))
                  (:contact-area (setf (tactile-sensor-contact-area sensor) value))
                  (:texture-detection (setf (tactile-sensor-texture-detection sensor) value))
                  (:current-pressure (setf (tactile-sensor-current-pressure sensor) value))
                  (otherwise nil))))
      
      (proprioceptive-sensor
       (loop for (key value) on properties by #'cddr
             do (case key
                  (:axes (setf (proprioceptive-sensor-axes sensor) value))
                  (:position (setf (proprioceptive-sensor-position sensor) value))
                  (:orientation (setf (proprioceptive-sensor-orientation sensor) value))
                  (:velocity (setf (proprioceptive-sensor-velocity sensor) value))
                  (otherwise nil)))))
    
    ;; Notificar a observadores
    (notify-sensor-observers sensor :configuration-changed)
    
    t))

;;;; Funciones de Preprocesadores

(defun attach-preprocessor (sensor preprocessor)
  "Añade un preprocesador al sensor.
   
   Args:
     sensor: El sensor al que añadir el preprocesador
     preprocessor: Función de preprocesamiento
   
   Returns:
     T si se añadió correctamente"
  
  (with-lock-held ((sensor-lock sensor))
    (push preprocessor (sensor-preprocessors sensor))
    
    ;; Notificar a observadores
    (notify-sensor-observers sensor :preprocessor-added)
    
    t))

(defun detach-preprocessor (sensor preprocessor)
  "Elimina un preprocesador del sensor.
   
   Args:
     sensor: El sensor del que eliminar el preprocesador
     preprocessor: Función de preprocesamiento a eliminar
   
   Returns:
     T si se eliminó correctamente, NIL si no existía"
  
  (with-lock-held ((sensor-lock sensor))
    (let ((original-length (length (sensor-preprocessors sensor))))
      (setf (sensor-preprocessors sensor)
            (remove preprocessor (sensor-preprocessors sensor)))
      
      (let ((removed (not (= original-length (length (sensor-preprocessors sensor))))))
        (when removed
          ;; Notificar a observadores
          (notify-sensor-observers sensor :preprocessor-removed))
        
        removed))))

;;;; Funciones de Integración con Capas Ontológicas

(defun connect-to-layer (sensor layer)
  "Conecta un sensor a una capa ontológica.
   
   Args:
     sensor: El sensor a conectar
     layer: La capa ontológica de destino
   
   Returns:
     T si la conexión fue exitosa, NIL en caso contrario"
  
  ;; Verificar si ya está conectado
  (when (member (synth-u-ontology:ontological-layer-id layer) 
              (sensor-connected-layers sensor) 
              :test #'string=)
    (return-from connect-to-layer nil))
  
  ;; Establecer conexión
  (push (synth-u-ontology:ontological-layer-id layer) 
        (sensor-connected-layers sensor))
  
  ;; Notificar a observadores
  (notify-sensor-observers sensor :layer-connected)
  
  t)

(defun disconnect-from-layer (sensor layer)
  "Desconecta un sensor de una capa ontológica.
   
   Args:
     sensor: El sensor a desconectar
     layer: La capa ontológica de origen
   
   Returns:
     T si la desconexión fue exitosa, NIL en caso contrario"
  
  ;; Verificar si está conectado
  (unless (member (synth-u-ontology:ontological-layer-id layer) 
                (sensor-connected-layers sensor) 
                :test #'string=)
    (return-from disconnect-from-layer nil))
  
  ;; Eliminar conexión
  (setf (sensor-connected-layers sensor)
        (remove (synth-u-ontology:ontological-layer-id layer) 
               (sensor-connected-layers sensor) 
               :test #'string=))
  
  ;; Notificar a observadores
  (notify-sensor-observers sensor :layer-disconnected)
  
  t)

;;;; Funciones de Acceso a Lecturas

(defun get-sensor-readings (sensor &key (count nil) (average nil))
  "Obtiene las lecturas más recientes del sensor.
   
   Args:
     sensor: El sensor del que obtener lecturas
     count (opcional): Número de lecturas a obtener
     average (opcional): Si se debe devolver el promedio de las lecturas
   
   Returns:
     Lista de lecturas, o promedio si se solicitó"
  
  (with-lock-held ((sensor-lock sensor))
    (let* ((buffer (sensor-buffer sensor))
           (readings (if count
                        (subseq buffer 0 (min count (length buffer)))
                        buffer)))
      
      (if average
          ;; Calcular promedio según tipo de sensor
          (typecase sensor
            (discrete-sensor
             ;; Para sensores discretos, devolver el estado más frecuente
             (let ((state-counts (make-hash-table :test #'equal)))
               (dolist (reading readings)
                 (incf (gethash reading state-counts 0)))
               
               ;; Encontrar el estado más frecuente
               (let ((max-count 0)
                     (max-state nil))
                 (maphash (lambda (state count)
                            (when (> count max-count)
                              (setf max-count count)
                              (setf max-state state)))
                          state-counts)
                 max-state)))
            
            (continuous-sensor
             ;; Para sensores continuos, calcular media aritmética
             (if (null readings)
                 nil
                 (/ (reduce #'+ readings)
                    (length readings))))
            
            (otherwise
             ;; Para otros sensores, devolver la lectura más reciente
             (first readings)))
          
          ;; Devolver las lecturas sin promediar
          readings))))

;;;; Funciones de Observadores

(defun register-sensor-observer (sensor observer-fn)
  "Registra un observador para eventos del sensor.
   
   Args:
     sensor: El sensor a observar
     observer-fn: Función de callback para eventos
   
   Returns:
     T si se registró correctamente"
  
  (with-lock-held ((sensor-lock sensor))
    (push observer-fn (sensor-observers sensor)))
  t)

(defun unregister-sensor-observer (sensor observer-fn)
  "Elimina un observador de eventos del sensor.
   
   Args:
     sensor: El sensor observado
     observer-fn: Función de callback a eliminar
   
   Returns:
     T si se eliminó correctamente, NIL si no existía"
  
  (with-lock-held ((sensor-lock sensor))
    (let ((original-length (length (sensor-observers sensor))))
      (setf (sensor-observers sensor)
            (remove observer-fn (sensor-observers sensor)))
      (not (= original-length (length (sensor-observers sensor)))))))

(defun notify-sensor-observers (sensor event-type)
  "Notifica a todos los observadores de un evento en el sensor.
   
   Args:
     sensor: El sensor donde ocurrió el evento
     event-type: Tipo de evento (:activation, :deactivation, etc.)
   
   Returns:
     Número de observadores notificados"
  
  (let ((observers (sensor-observers sensor))
        (count 0))
    (dolist (observer observers)
      (funcall observer sensor event-type)
      (incf count))
    count))

;;;; Macros Utilitarias

(defmacro with-sensor-reading ((var sensor) &body body)
  "Macro para leer un sensor y ejecutar código con la lectura.
   
   Args:
     var: Variable que recibirá la lectura
     sensor: Sensor a leer
     body: Código a ejecutar con la lectura"
  
  `(let ((,var (read-sensor ,sensor)))
     (when ,var
       ,@body)))

;;;; Exportar símbolos públicos
(export '(sensor
          make-sensor
          sensor-id
          sensor-type
          sensor-mode
          sensor-status
          sensor-range
          sensor-resolution
          sensor-noise-level
          sensor-sampling-rate
          sensor-buffer
          sensor-preprocessors
          activate-sensor
          deactivate-sensor
          read-sensor
          configure-sensor
          attach-preprocessor
          detach-preprocessor
          get-sensor-readings
          connect-to-layer
          disconnect-from-layer
          register-sensor-observer
          unregister-sensor-observer
          discrete-sensor
          continuous-sensor
          visual-sensor
          auditory-sensor
          tactile-sensor
          proprioceptive-sensor
          make-discrete-sensor
          make-continuous-sensor
          make-visual-sensor
          make-auditory-sensor
          make-tactile-sensor
          make-proprioceptive-sensor
          with-sensor-reading))
