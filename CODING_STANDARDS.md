# CODING STANDARDS - SYNTH-U PROJECT
## Estándares de Codificación Multi-Lenguaje

**Versión**: 1.0  
**Fecha**: [ACTUAL]  
**Aplicable a**: Todo el código del proyecto Synth-U

---

## 1. PRINCIPIOS GENERALES

### 1.1 Principios Fundamentales
- **Legibilidad sobre Brevedad**: El código debe ser auto-explicativo
- **Consistencia Absoluta**: Mismo estilo en todo el proyecto
- **Documentación Viva**: Comentarios que explican el "por qué", no el "qué"
- **Modularidad**: Componentes pequeños, cohesivos y débilmente acoplados
- **Testabilidad**: Todo código debe ser testeable

### 1.2 Filosofía del Código
- **Self-Documenting Code**: Nombres descriptivos y estructura clara
- **Defensive Programming**: Validar entradas y manejar errores explícitamente
- **Performance Conscious**: Optimizar cuando sea necesario, pero priorizar claridad
- **Future-Proof**: Escribir código que sea fácil de modificar y extender

---

## 2. COMMON LISP STANDARDS

### 2.1 Naming Conventions

#### Funciones:
```lisp
;; ✅ CORRECTO: kebab-case con verbos descriptivos
(defun calculate-phi-value (layer-states)
  "Calcula el valor Phi según la Teoría de Información Integrada")

(defun register-microkernel (id role genome)
  "Registra un nuevo microkernel en el sistema")

;; ❌ INCORRECTO: nombres ambiguos o no descriptivos
(defun calc (x) ...)
(defun process (data) ...)
```

#### Variables:
```lisp
;; ✅ CORRECTO: nombres descriptivos con contexto
(defparameter *active-kernels* (make-hash-table))
(defvar *consciousness-threshold* 0.7)
(let ((emergence-score 0.0)
      (integration-measure 1.2))
  ...)

;; ❌ INCORRECTO: nombres crípticos
(defparameter *ak* (make-hash-table))
(let ((x 0.0) (y 1.2)) ...)
```

#### Estructuras y Clases:
```lisp
;; ✅ CORRECTO: nombres claros en singular
(defstruct (microkernel (:constructor make-microkernel (id role genome)))
  id role genome memory status)

(defclass ontological-layer ()
  ((id :initarg :id :reader layer-id)
   (level :initarg :level :reader layer-level)))

;; Package names: kebab-case con prefijo del proyecto
(defpackage :syhman-core (:use :cl :uiop))
(defpackage :syhman-emergence (:use :cl :syhman-ontology))
```

### 2.2 Code Structure

#### Archivo de Módulo Estándar:
```lisp
;;;; -----------------------------------------------------
;;;; MODULE: nombre_modulo.lisp
;;;; DESCRIPTION: Descripción clara del propósito del módulo
;;;; DEPENDENCIES: Lista de dependencias
;;;; -----------------------------------------------------

(defpackage :syhman-nombre-modulo
  (:use :cl :uiop :dependencia1 :dependencia2)
  (:export #:funcion-publica-1
           #:funcion-publica-2
           #:*variable-publica*))

(in-package :syhman-nombre-modulo)

;;;; Constantes y Variables Globales
(defconstant +max-iterations+ 1000
  "Número máximo de iteraciones permitidas")

(defparameter *default-threshold* 0.5
  "Umbral por defecto para emergencia")

;;;; Estructuras de Datos
(defstruct (nombre-estructura (:constructor make-nombre-estructura (param1 param2)))
  "Documentación de la estructura"
  param1 param2 campo-opcional)

;;;; Funciones Principales
(defun funcion-principal (parametros)
  "Documentación completa de la función.
   
   Args:
     parametros: Descripción de parámetros
   
   Returns:
     Descripción del valor de retorno
   
   Raises:
     Condiciones que puede lanzar"
  (body))

;;;; Funciones Auxiliares (al final del archivo)
(defun funcion-auxiliar (params)
  "Función auxiliar interna"
  (body))
```

#### Comentarios y Documentación:
```lisp
;; Comentario de línea para explicaciones breves

;;; Comentario de sección para agrupar código relacionado

;;;; Comentario de archivo/módulo para headers principales

;; TODO: Descripción de tarea pendiente
;; FIXME: Descripción de problema a corregir
;; NOTE: Información importante para desarrolladores
;; HACK: Solución temporal que necesita refactoring

(defun ejemplo-documentacion (param1 param2 &key opcional)
  "Documentación principal de la función.
   
   Esta función demuestra cómo documentar apropiadamente.
   
   Args:
     param1 (symbol): Primer parámetro requerido
     param2 (number): Segundo parámetro requerido
     opcional (boolean): Parámetro opcional, default NIL
   
   Returns:
     (list): Lista con resultados procesados
   
   Raises:
     invalid-parameter-error: Si param1 no es un símbolo válido
   
   Example:
     (ejemplo-documentacion 'test 42 :opcional t)
     => (:result test 42)"
  ;; Implementación aquí
  )
```

### 2.3 Error Handling
```lisp
;; ✅ CORRECTO: Manejo explícito de errores
(defun safe-calculation (input)
  "Realiza cálculo con manejo seguro de errores"
  (check-type input number)
  (when (zerop input)
    (error 'division-by-zero :operation 'safe-calculation))
  
  (handler-case
      (/ 1.0 input)
    (division-by-zero ()
      (warn "División por cero detectada")
      0.0)
    (error (e)
      (error "Error inesperado en safe-calculation: ~A" e))))

;; ✅ CORRECTO: Definir condiciones específicas
(define-condition synth-u-error (error)
  ((module :initarg :module :reader error-module)
   (operation :initarg :operation :reader error-operation))
  (:report (lambda (condition stream)
             (format stream "Error en módulo ~A durante operación ~A"
                     (error-module condition)
                     (error-operation condition)))))
```

---

## 3. RUST STANDARDS

### 3.1 Naming Conventions
```rust
// ✅ CORRECTO: snake_case para funciones y variables
fn calculate_phi_value(layer_states: &[LayerState]) -> f64 {
    // implementación
}

let emergence_score = 0.0;
let integration_measure = calculate_integration(&states);

// ✅ CORRECTO: PascalCase para tipos
struct MicroKernel {
    id: String,
    role: KernelRole,
    genome: SymbolicGenome,
}

enum KernelRole {
    VisionCore,
    LinguisticInit,
    NetworkCore,
}

// ✅ CORRECTO: SCREAMING_SNAKE_CASE para constantes
const MAX_KERNEL_COUNT: usize = 100;
const DEFAULT_PHI_THRESHOLD: f64 = 0.3;
```

### 3.2 Code Structure
```rust
//! Module documentation
//! 
//! This module implements the core microkernel functionality
//! for the Synth-U operating system.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// Public API exports
pub use microkernel::MicroKernel;
pub use role::KernelRole;

/// Module-level constants
const MODULE_VERSION: &str = "1.0.0";

/// Primary structure with full documentation
#[derive(Debug, Clone)]
pub struct MicroKernel {
    /// Unique identifier for this kernel
    pub id: String,
    /// Current functional role
    pub role: KernelRole,
    /// Symbolic genome for self-modification
    genome: SymbolicGenome,
}

impl MicroKernel {
    /// Creates a new microkernel with specified parameters
    /// 
    /// # Arguments
    /// 
    /// * `id` - Unique identifier for the kernel
    /// * `role` - Initial functional role
    /// * `genome` - Symbolic genome for evolution
    /// 
    /// # Returns
    /// 
    /// A new MicroKernel instance
    /// 
    /// # Examples
    /// 
    /// ```
    /// let kernel = MicroKernel::new("k1", KernelRole::VisionCore, genome);
    /// ```
    pub fn new(id: String, role: KernelRole, genome: SymbolicGenome) -> Self {
        Self { id, role, genome }
    }
    
    /// Reassigns the functional role of this kernel
    /// 
    /// # Arguments
    /// 
    /// * `new_role` - The new role to assign
    /// 
    /// # Returns
    /// 
    /// Result indicating success or failure
    pub fn reassign_role(&mut self, new_role: KernelRole) -> Result<(), KernelError> {
        // Validation logic
        if !self.can_assume_role(&new_role) {
            return Err(KernelError::InvalidRoleTransition);
        }
        
        self.role = new_role;
        Ok(())
    }
}
```

### 3.3 Error Handling
```rust
use thiserror::Error;

/// Comprehensive error types for the module
#[derive(Error, Debug)]
pub enum KernelError {
    #[error("Invalid role transition from {from:?} to {to:?}")]
    InvalidRoleTransition { from: KernelRole, to: KernelRole },
    
    #[error("Kernel {id} not found")]
    KernelNotFound { id: String },
    
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

// ✅ CORRECTO: Uso de Result para manejo de errores
fn register_kernel(id: String, role: KernelRole) -> Result<MicroKernel, KernelError> {
    // Validation
    if id.is_empty() {
        return Err(KernelError::InvalidInput("ID cannot be empty".into()));
    }
    
    // Implementation
    Ok(MicroKernel::new(id, role, SymbolicGenome::default()))
}
```

---

## 4. PYTHON STANDARDS

### 4.1 Naming Conventions (PEP 8)
```python
# ✅ CORRECTO: snake_case para funciones y variables
def calculate_phi_value(layer_states: List[LayerState]) -> float:
    """Calculate Phi value according to IIT theory."""
    pass

emergence_score = 0.0
integration_measure = calculate_integration(states)

# ✅ CORRECTO: PascalCase para clases
class MicroKernel:
    """Represents a microkernel in the Synth-U system."""
    
    def __init__(self, kernel_id: str, role: str, genome: SymbolicGenome):
        self.id = kernel_id
        self.role = role
        self.genome = genome

# ✅ CORRECTO: UPPER_SNAKE_CASE para constantes
MAX_KERNEL_COUNT = 100
DEFAULT_PHI_THRESHOLD = 0.3
```

### 4.2 Code Structure
```python
"""Module for microkernel functionality.

This module implements the core microkernel system for Synth-U,
providing dynamic role assignment and symbolic evolution capabilities.
"""

from typing import Dict, List, Optional, Union
from dataclasses import dataclass
from abc import ABC, abstractmethod
import logging

# Module-level constants
MODULE_VERSION = "1.0.0"
logger = logging.getLogger(__name__)

@dataclass
class KernelConfig:
    """Configuration for microkernel initialization."""
    max_memory: int = 1024
    enable_logging: bool = True
    phi_threshold: float = 0.3

class MicroKernel:
    """Core microkernel implementation.
    
    A microkernel represents a functional unit in the Synth-U system
    that can dynamically change its role and evolve its capabilities.
    
    Attributes:
        id: Unique identifier for this kernel
        role: Current functional role
        genome: Symbolic genome for self-modification
        
    Example:
        >>> kernel = MicroKernel("k1", "vision_core", genome)
        >>> kernel.reassign_role("linguistic_init")
        True
    """
    
    def __init__(self, kernel_id: str, role: str, genome: 'SymbolicGenome') -> None:
        """Initialize a new microkernel.
        
        Args:
            kernel_id: Unique identifier for the kernel
            role: Initial functional role
            genome: Symbolic genome for evolution
            
        Raises:
            ValueError: If kernel_id is empty or role is invalid
        """
        if not kernel_id:
            raise ValueError("Kernel ID cannot be empty")
            
        self.id = kernel_id
        self.role = role
        self.genome = genome
        self._status = "active"
        
        logger.info(f"Initialized kernel {kernel_id} with role {role}")
    
    def reassign_role(self, new_role: str) -> bool:
        """Reassign the functional role of this kernel.
        
        Args:
            new_role: The new role to assign
            
        Returns:
            True if reassignment was successful, False otherwise
            
        Raises:
            InvalidRoleError: If the role transition is not allowed
        """
        if not self._can_assume_role(new_role):
            raise InvalidRoleError(f"Cannot transition from {self.role} to {new_role}")
        
        old_role = self.role
        self.role = new_role
        
        logger.info(f"Kernel {self.id} role changed from {old_role} to {new_role}")
        return True
```

### 4.3 Error Handling
```python
class SynthUError(Exception):
    """Base exception for Synth-U system."""
    pass

class KernelError(SynthUError):
    """Exception raised for kernel-related errors."""
    
    def __init__(self, message: str, kernel_id: Optional[str] = None):
        super().__init__(message)
        self.kernel_id = kernel_id

class InvalidRoleError(KernelError):
    """Exception raised for invalid role transitions."""
    pass

# ✅ CORRECTO: Uso de excepciones específicas
def register_kernel(kernel_id: str, role: str) -> MicroKernel:
    """Register a new kernel in the system.
    
    Args:
        kernel_id: Unique identifier
        role: Initial role
        
    Returns:
        Newly created MicroKernel instance
        
    Raises:
        KernelError: If registration fails
        ValueError: If parameters are invalid
    """
    try:
        if not kernel_id:
            raise ValueError("Kernel ID is required")
            
        kernel = MicroKernel(kernel_id, role, SymbolicGenome())
        _active_kernels[kernel_id] = kernel
        
        return kernel
        
    except Exception as e:
        logger.error(f"Failed to register kernel {kernel_id}: {e}")
        raise KernelError(f"Registration failed: {e}", kernel_id)
```

---

## 5. STANDARDS INTER-LENGUAJES

### 5.1 Convenciones de Interfaz
```lisp
;; Common Lisp - Función que será llamada desde otros lenguajes
(defun lisp-calculate-emergence (layer-data)
  "Calcula emergencia consciencial - interfaz para FFI"
  (declare (optimize (safety 3) (debug 3)))
  ;; Validation de entrada
  (check-type layer-data list)
  ;; Cálculo principal
  (calculate-phi-value layer-data))
```

```rust
// Rust - FFI interface para Common Lisp
#[no_mangle]
pub extern "C" fn rust_optimize_memory(data_ptr: *const f64, len: usize) -> f64 {
    // Safety checks
    if data_ptr.is_null() || len == 0 {
        return 0.0;
    }
    
    // Safe conversion
    let data = unsafe { std::slice::from_raw_parts(data_ptr, len) };
    
    // Implementation
    optimize_memory_usage(data)
}
```

```python
# Python - Interfaz con Common Lisp vía JSON/ZeroMQ
import json
import zmq

def send_to_lisp_core(operation: str, data: Dict) -> Dict:
    """Send command to Lisp core and receive response."""
    message = {
        "operation": operation,
        "data": data,
        "timestamp": time.time()
    }
    
    socket.send_json(message)
    response = socket.recv_json()
    
    return response
```

### 5.2 Protocolos de Comunicación
```json
// Formato estándar de mensaje inter-lenguajes
{
  "source": "python-ai-module",
  "target": "lisp-core",
  "operation": "calculate_phi",
  "data": {
    "layer_states": [...],
    "parameters": {...}
  },
  "message_id": "uuid-string",
  "timestamp": "2024-01-01T00:00:00Z",
  "priority": "normal"
}
```

---

## 6. TESTING STANDARDS

### 6.1 Common Lisp Testing
```lisp
(defpackage :syhman-core-test
  (:use :cl :fiveam :syhman-core))

(in-package :syhman-core-test)

(def-suite core-tests
  :description "Test suite for core microkernel functionality")

(in-suite core-tests)

(test test-kernel-creation
  "Test microkernel creation and initialization"
  (let ((kernel (make-microkernel "test-1" :vision-core (make-hash-table))))
    (is (string= (microkernel-id kernel) "test-1"))
    (is (eq (microkernel-role kernel) :vision-core))
    (is (hash-table-p (microkernel-genome kernel)))))

(test test-role-reassignment
  "Test dynamic role reassignment"
  (let ((kernel (register-kernel "test-2" :vision-core)))
    (reassign-role "test-2" :linguistic-init)
    (is (eq (microkernel-role (get-kernel "test-2")) :linguistic-init))))
```

### 6.2 Rust Testing
```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_kernel_creation() {
        let kernel = MicroKernel::new(
            "test-1".to_string(),
            KernelRole::VisionCore,
            SymbolicGenome::default()
        );
        
        assert_eq!(kernel.id, "test-1");
        assert_eq!(kernel.role, KernelRole::VisionCore);
    }
    
    #[test]
    fn test_role_reassignment() {
        let mut kernel = MicroKernel::new(
            "test-2".to_string(),
            KernelRole::VisionCore,
            SymbolicGenome::default()
        );
        
        let result = kernel.reassign_role(KernelRole::LinguisticInit);
        assert!(result.is_ok());
        assert_eq!(kernel.role, KernelRole::LinguisticInit);
    }
    
    #[test]
    #[should_panic(expected = "Invalid role transition")]
    fn test_invalid_role_transition() {
        // Test that invalid transitions are rejected
        let mut kernel = MicroKernel::new(
            "test-3".to_string(),
            KernelRole::VisionCore,
            SymbolicGenome::default()
        );
        
        kernel.reassign_role(KernelRole::InvalidRole).unwrap();
    }
}
```

### 6.3 Python Testing
```python
import unittest
from unittest.mock import Mock, patch
import pytest

class TestMicroKernel(unittest.TestCase):
    """Test cases for MicroKernel class."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.genome = Mock(spec=SymbolicGenome)
        self.kernel = MicroKernel("test-1", "vision_core", self.genome)
    
    def test_kernel_creation(self):
        """Test microkernel creation and initialization."""
        self.assertEqual(self.kernel.id, "test-1")
        self.assertEqual(self.kernel.role, "vision_core")
        self.assertEqual(self.kernel.genome, self.genome)
    
    def test_role_reassignment(self):
        """Test dynamic role reassignment."""
        result = self.kernel.reassign_role("linguistic_init")
        self.assertTrue(result)
        self.assertEqual(self.kernel.role, "linguistic_init")
    
    def test_invalid_role_transition(self):
        """Test that invalid role transitions raise appropriate errors."""
        with self.assertRaises(InvalidRoleError):
            self.kernel.reassign_role("invalid_role")
    
    @patch('syhman.core.logger')
    def test_logging(self, mock_logger):
        """Test that appropriate logging occurs."""
        self.kernel.reassign_role("network_core")
        mock_logger.info.assert_called()

# Pytest style tests
def test_kernel_with_empty_id():
    """Test that empty ID raises ValueError."""
    with pytest.raises(ValueError, match="Kernel ID cannot be empty"):
        MicroKernel("", "vision_core", Mock())

@pytest.mark.parametrize("role,expected", [
    ("vision_core", True),
    ("linguistic_init", True),
    ("invalid_role", False),
])
def test_role_validation(role, expected):
    """Test role validation with various inputs."""
    # Implementation of role validation test
    pass
```

---

## 7. DOCUMENTATION STANDARDS

### 7.1 Inline Documentation
- **Cada función pública**: Documentación completa con ejemplos
- **Parámetros**: Tipo y descripción clara
- **Valores de retorno**: Tipo y significado
- **Excepciones**: Condiciones que las causan
- **Ejemplos**: Al menos uno por función compleja

### 7.2 Module Documentation
- **Header**: Propósito del módulo y dependencias
- **API Overview**: Resumen de funciones públicas principales
- **Usage Examples**: Ejemplos de uso común
- **Integration Notes**: Cómo se integra con otros módulos

---

## 8. VERSION CONTROL STANDARDS

### 8.1 Commit Messages
```
tipo(scope): descripción breve

Descripción más detallada si es necesaria.

- Cambio específico 1
- Cambio específico 2

Refs: #123
```

Tipos permitidos:
- `feat`: Nueva funcionalidad
- `fix`: Corrección de bug
- `docs`: Solo documentación
- `style`: Formateo, sin cambios funcionales
- `refactor`: Refactoring sin cambios funcionales
- `test`: Agregar o modificar tests
- `chore`: Tareas de mantenimiento

### 8.2 Branch Naming
- `feature/nombre-feature`: Nuevas funcionalidades
- `bugfix/descripcion-bug`: Corrección de bugs
- `hotfix/descripcion-urgente`: Fixes urgentes
- `refactor/nombre-refactor`: Refactoring
- `docs/nombre-documentacion`: Solo documentación

---

## 9. PERFORMANCE STANDARDS

### 9.1 Common Lisp
```lisp
;; ✅ CORRECTO: Declaraciones de optimización apropiadas
(defun performance-critical-function (data)
  "Función crítica de performance"
  (declare (optimize (speed 3) (safety 1) (debug 0))
           (type list data))
  ;; Implementation
  )

;; ✅ CORRECTO: Type declarations para performance
(defun calculate-phi (integration differentiation)
  "Calcula phi con tipos optimizados"
  (declare (type single-float integration differentiation)
           (optimize (speed 3) (safety 1)))
  (/ (* integration differentiation)
     (+ integration differentiation)))
```

### 9.2 Rust Performance
```rust
// ✅ CORRECTO: Uso de iteradores para performance
fn process_large_dataset(data: &[f64]) -> Vec<f64> {
    data.iter()
        .filter(|&&x| x > 0.0)
        .map(|&x| x * 2.0)
        .collect()
}

// ✅ CORRECTO: Evitar allocaciones innecesarias
fn calculate_in_place(data: &mut [f64]) {
    for value in data.iter_mut() {
        *value = value.sqrt();
    }
}
```

---

## 10. ENFORCEMENT

### 10.1 Automated Checks
- **Linting**: Ejecutar linters en CI/CD
- **Formatting**: Auto-formateo en pre-commit hooks
- **Documentation**: Verificar coverage de documentación
- **Tests**: Cobertura mínima 85%

### 10.2 Code Review Checklist
- [ ] Sigue convenciones de naming
- [ ] Documentación completa
- [ ] Tests incluidos
- [ ] Manejo de errores apropiado
- [ ] Performance considerada
- [ ] Consistente con arquitectura general

---

**NOTA**: Estos estándares son obligatorios y serán verificados en cada checkpoint. El incumplimiento bloqueará el avance del proyecto.
