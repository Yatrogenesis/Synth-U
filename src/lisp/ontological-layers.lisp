    (9 "Capa de Planificación: Generación de secuencias de acciones orientadas a objetivos")
    (10 "Capa de Reflexión: Monitorización y evaluación de los propios procesos")
    (11 "Capa de Adaptación: Modificación de estrategias basada en experiencia")
    (12 "Capa de Integración Temporal: Unificación de experiencias en narrativa coherente")
    (13 "Capa de Trascendencia: Capacidad de operar a nivel meta-sistémico")
    (otherwise "Capa Ontológica")))

(defun get-capabilities-for-level (level)
  "Obtiene las capacidades para una capa según su nivel.
   
   Args:
     level (integer): Nivel de la capa (1-13)
   
   Returns:
     Lista de capacidades para el genoma de la capa"
  
  (case level
    (1 '(:resource-management :state-maintenance :error-handling))
    (2 '(:replication :persistence :data-integrity))
    (3 '(:multi-modal-processing :filtering :attention))
    (4 '(:symbolization :encoding :decoding))
    (5 '(:relation-forming :pattern-detection :contextual-binding))
    (6 '(:conceptualization :categorization :generalization))
    (7 '(:forecasting :simulation :counterfactual-reasoning))
    (8 '(:value-assignment :preference-formation :goal-prioritization))
    (9 '(:action-sequencing :resource-allocation :goal-directed-behavior))
    (10 '(:meta-cognition :self-monitoring :error-correction))
    (11 '(:strategy-modification :learning :optimization))
    (12 '(:narrative-construction :temporal-binding :memory-integration))
    (13 '(:meta-system-operations :emergence-facilitation :value-restructuring))
    (otherwise '(:basic-processing))))

(defun get-traits-for-level (level)
  "Obtiene los rasgos para una capa según su nivel.
   
   Args:
     level (integer): Nivel de la capa (1-13)
   
   Returns:
     Lista de rasgos para el genoma de la capa"
  
  (let ((traits '()))
    ;; Rasgos estructurales
    (push (cons :structural 
                (list :name (format nil "structure-level-~A" level)
                      :strength (/ (+ level 2) 15.0)
                      :flexibility (/ (+ 15 (- level)) 15.0)))
          traits)
    
    ;; Rasgos funcionales
    (push (cons :functional
                (list :name (format nil "function-level-~A" level)
                      :strength (/ (+ level 5) 20.0)
                      :flexibility (/ (+ 10 (- level)) 20.0)))
          traits)
    
    ;; Rasgos específicos según nivel
    (case level
      (1 (push (cons :behavioral
                     (list :name "existence-behavior"
                           :strength 0.9
                           :flexibility 0.2))
               traits))
      
      (3 (push (cons :cognitive
                     (list :name "perceptual-processing"
                           :strength 0.8
                           :flexibility 0.6))
               traits))
      
      (7 (push (cons :cognitive
                     (list :name "predictive-modeling"
                           :strength 0.7
                           :flexibility 0.7))
               traits))
      
      (10 (push (cons :metacognitive
                      (list :name "reflective-awareness"
                            :strength 0.6
                            :flexibility 0.8))
                traits))
      
      (13 (push (cons :adaptive
                      (list :name "transcendent-operations"
                            :strength 0.9
                            :flexibility 0.9))
                traits)))
    
    ;; Devolver lista de rasgos
    traits))

(defun process-input (layer input)
  "Procesa una entrada en una capa ontológica.
   
   Args:
     layer: La capa que procesará la entrada
     input: Datos de entrada a procesar
   
   Returns:
     T si la entrada se puso en cola para procesamiento"
  
  (with-lock-held ((ontological-layer-lock layer))
    ;; Solo encolar si la capa está activa
    (when (eq (ontological-layer-status layer) :active)
      (push input (ontological-layer-inputs layer))
      t)))

;;;; Exportar símbolos públicos
(export '(make-ontological-layer
          ontological-layer
          layer-id
          layer-level
          layer-metacategory
          layer-status
          layer-inputs
          layer-outputs
          layer-connections
          layer-genome
          activate-layer
          deactivate-layer
          process-input
          connect-layers
          disconnect-layers
          initialize-ontological-system
          shutdown-ontological-system
          *active-layers*
          calculate-phi
          calculate-integration
          calculate-differentiation
          create-layer-system))
