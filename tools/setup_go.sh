# Script para instalar y configurar Go
#!/bin/bash
set -e

echo "=== Configurando Go ==="

# Verificar si Go ya está instalado
if command -v go &> /dev/null; then
    echo "Go ya está instalado. Versión:"
    go version
else
    echo "Go no está instalado. Instalando..."
    
    # Instalar Go según el sistema operativo
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        wget https://go.dev/dl/go1.21.1.linux-amd64.tar.gz
        sudo tar -C /usr/local -xzf go1.21.1.linux-amd64.tar.gz
        rm go1.21.1.linux-amd64.tar.gz
        
        # Añadir a PATH si no está
        if ! grep -q "export PATH=\$PATH:/usr/local/go/bin" ~/.bashrc; then
            echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.bashrc
        fi
        
        # Cargar PATH inmediatamente
        export PATH=$PATH:/usr/local/go/bin
        
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        brew install go
    else
        echo "Sistema operativo no soportado para instalación automática."
        echo "Por favor, instale Go manualmente desde: https://golang.org/dl/"
        exit 1
    fi
    
    echo "Go instalado correctamente."
fi

# Verificar instalación
go version

# Configurar estructura de proyecto Go
echo "Configurando estructura de proyecto Go..."

# Verificar si estamos en la raíz del proyecto
if [ -d "src/go" ]; then
    cd src/go
    
    # Inicializar módulo Go si no existe
    if [ ! -f "go.mod" ]; then
        echo "Inicializando módulo Go..."
        go mod init github.com/synth-u/synth-u
        
        # Instalar dependencias comunes
        echo "Instalando dependencias Go..."
        go get -u google.golang.org/grpc
        go get -u github.com/go-redis/redis/v8
        go get -u github.com/neo4j/neo4j-go-driver/v5
        go get -u github.com/spf13/cobra
        go get -u github.com/stretchr/testify
        
        # Crear estructura básica
        mkdir -p cmd/synth-u
        mkdir -p internal/orchestration
        mkdir -p internal/monitoring
        mkdir -p internal/microkernel
        mkdir -p internal/communication
        mkdir -p pkg/common
        mkdir -p api/proto
        
        # Crear archivo principal
        cat > cmd/synth-u/main.go << EOF
package main

import (
	"fmt"
	"log"
	"os"
	"os/signal"
	"syscall"

	"github.com/synth-u/synth-u/internal/orchestration"
)

func main() {
	fmt.Println("Synth-U Orchestration System v0.1.0")
	fmt.Println("Initializing...")

	// Setup signal handling
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	// Initialize orchestration
	orchestrator, err := orchestration.NewOrchestrator()
	if err != nil {
		log.Fatalf("Failed to initialize orchestrator: %v", err)
	}

	// Start the orchestrator
	err = orchestrator.Start()
	if err != nil {
		log.Fatalf("Failed to start orchestrator: %v", err)
	}

	fmt.Println("Orchestrator running. Press Ctrl+C to exit.")

	// Wait for termination signal
	<-sigChan
	fmt.Println("\nShutdown signal received.")

	// Graceful shutdown
	err = orchestrator.Shutdown()
	if err != nil {
		log.Printf("Error during shutdown: %v", err)
	}

	fmt.Println("Shutdown complete.")
}
EOF

        # Crear módulo de orquestación
        cat > internal/orchestration/orchestrator.go << EOF
package orchestration

import (
	"errors"
	"fmt"
	"log"
	"sync"
	"time"

	"github.com/synth-u/synth-u/internal/microkernel"
)

// Orchestrator manages multiple microkernels
type Orchestrator struct {
	kernels     map[string]*microkernel.MicroKernel
	mutex       sync.RWMutex
	isRunning   bool
	stopChan    chan struct{}
	kernelWaitGroup sync.WaitGroup
}

// NewOrchestrator creates a new orchestrator
func NewOrchestrator() (*Orchestrator, error) {
	return &Orchestrator{
		kernels:  make(map[string]*microkernel.MicroKernel),
		stopChan: make(chan struct{}),
	}, nil
}

// RegisterKernel registers a microkernel with the orchestrator
func (o *Orchestrator) RegisterKernel(kernel *microkernel.MicroKernel) error {
	o.mutex.Lock()
	defer o.mutex.Unlock()

	if kernel == nil {
		return errors.New("cannot register nil kernel")
	}

	if _, exists := o.kernels[kernel.ID]; exists {
		return fmt.Errorf("kernel with ID %s already registered", kernel.ID)
	}

	o.kernels[kernel.ID] = kernel
	log.Printf("Registered kernel: %s with role: %s", kernel.ID, kernel.Role)
	return nil
}

// Start starts the orchestrator and all registered kernels
func (o *Orchestrator) Start() error {
	o.mutex.Lock()
	defer o.mutex.Unlock()

	if o.isRunning {
		return errors.New("orchestrator already running")
	}

	log.Printf("Starting orchestrator with %d kernels", len(o.kernels))
	o.isRunning = true

	// Start monitoring routine
	go o.monitor()

	// Start all kernels
	for _, kernel := range o.kernels {
		o.kernelWaitGroup.Add(1)
		go func(k *microkernel.MicroKernel) {
			defer o.kernelWaitGroup.Done()
			if err := k.Start(); err != nil {
				log.Printf("Error starting kernel %s: %v", k.ID, err)
			}
		}(kernel)
	}

	return nil
}

// Shutdown stops the orchestrator and all kernels
func (o *Orchestrator) Shutdown() error {
	o.mutex.Lock()
	if !o.isRunning {
		o.mutex.Unlock()
		return errors.New("orchestrator not running")
	}

	log.Println("Shutting down orchestrator...")
	close(o.stopChan)
	o.isRunning = false
	o.mutex.Unlock()

	// Stop all kernels
	for _, kernel := range o.kernels {
		if err := kernel.Stop(); err != nil {
			log.Printf("Error stopping kernel %s: %v", kernel.ID, err)
		}
	}

	// Wait for all kernels to stop
	o.kernelWaitGroup.Wait()
	log.Println("All kernels stopped")

	return nil
}

// monitor periodically checks the health of all kernels
func (o *Orchestrator) monitor() {
	ticker := time.NewTicker(5 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ticker.C:
			o.checkKernelHealth()
		case <-o.stopChan:
			log.Println("Stopping monitoring routine")
			return
		}
	}
}

// checkKernelHealth checks the health of all registered kernels
func (o *Orchestrator) checkKernelHealth() {
	o.mutex.RLock()
	defer o.mutex.RUnlock()

	for id, kernel := range o.kernels {
		status := kernel.Status()
		if status != microkernel.StatusRunning {
			log.Printf("Kernel %s has status: %s", id, status)
			
			// Here would be recovery logic for non-running kernels
			// For now, just log the issue
		}
	}
}
EOF

        # Crear módulo de microkernel
        cat > internal/microkernel/microkernel.go << EOF
package microkernel

import (
	"errors"
	"log"
	"sync"
	"time"
)

// Status represents the current status of a microkernel
type Status string

const (
	StatusIdle     Status = "IDLE"
	StatusRunning  Status = "RUNNING"
	StatusError    Status = "ERROR"
	StatusStopping Status = "STOPPING"
)

// Role represents the functional role of a microkernel
type Role string

const (
	RoleVision       Role = "VISION"
	RoleLinguistic   Role = "LINGUISTIC"
	RoleNetwork      Role = "NETWORK"
	RoleOrchestration Role = "ORCHESTRATION"
)

// MicroKernel represents a single microkernel in the system
type MicroKernel struct {
	ID        string
	Role      Role
	status    Status
	mutex     sync.RWMutex
	stopChan  chan struct{}
	lastError error
}

// NewMicroKernel creates a new microkernel with the given ID and role
func NewMicroKernel(id string, role Role) *MicroKernel {
	return &MicroKernel{
		ID:       id,
		Role:     role,
		status:   StatusIdle,
		stopChan: make(chan struct{}),
	}
}

// Start begins the microkernel's operation
func (m *MicroKernel) Start() error {
	m.mutex.Lock()
	defer m.mutex.Unlock()

	if m.status == StatusRunning {
		return errors.New("kernel already running")
	}

	log.Printf("Starting kernel %s with role %s", m.ID, m.Role)
	m.status = StatusRunning
	m.lastError = nil

	// Start the kernel's main loop
	go m.run()

	return nil
}

// Stop halts the microkernel's operation
func (m *MicroKernel) Stop() error {
	m.mutex.Lock()
	if m.status != StatusRunning {
		m.mutex.Unlock()
		return errors.New("kernel not running")
	}

	log.Printf("Stopping kernel %s", m.ID)
	m.status = StatusStopping
	close(m.stopChan)
	m.mutex.Unlock()

	// Give the kernel time to clean up
	time.Sleep(100 * time.Millisecond)

	return nil
}

// Status returns the current status of the microkernel
func (m *MicroKernel) Status() Status {
	m.mutex.RLock()
	defer m.mutex.RUnlock()
	return m.status
}

// LastError returns the last error encountered by the microkernel
func (m *MicroKernel) LastError() error {
	m.mutex.RLock()
	defer m.mutex.RUnlock()
	return m.lastError
}

// run is the main loop of the microkernel
func (m *MicroKernel) run() {
	ticker := time.NewTicker(1 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ticker.C:
			// Perform the kernel's main function
			// For now, just a placeholder
			m.performFunction()
		case <-m.stopChan:
			log.Printf("Kernel %s received stop signal", m.ID)
			m.mutex.Lock()
			m.status = StatusIdle
			m.mutex.Unlock()
			return
		}
	}
}

// performFunction executes the kernel's main functionality
func (m *MicroKernel) performFunction() {
	// This would contain the actual implementation of the kernel's function
	// For now, just a placeholder
	m.mutex.RLock()
	role := m.Role
	m.mutex.RUnlock()

	switch role {
	case RoleVision:
		// Vision processing logic
	case RoleLinguistic:
		// Linguistic processing logic
	case RoleNetwork:
		// Network communication logic
	case RoleOrchestration:
		// Orchestration logic
	default:
		// Generic processing
	}
}
EOF

        # Crear test para el orquestador
        mkdir -p internal/orchestration/test
        cat > internal/orchestration/orchestrator_test.go << EOF
package orchestration

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/synth-u/synth-u/internal/microkernel"
)

func TestNewOrchestrator(t *testing.T) {
	orch, err := NewOrchestrator()
	assert.NoError(t, err)
	assert.NotNil(t, orch)
	assert.NotNil(t, orch.kernels)
	assert.False(t, orch.isRunning)
}

func TestRegisterKernel(t *testing.T) {
	orch, _ := NewOrchestrator()
	
	// Create a test kernel
	kernel := microkernel.NewMicroKernel("test-1", microkernel.RoleVision)
	
	// Register the kernel
	err := orch.RegisterKernel(kernel)
	assert.NoError(t, err)
	
	// Verify kernel is registered
	assert.Len(t, orch.kernels, 1)
	assert.Equal(t, kernel, orch.kernels["test-1"])
	
	// Try to register the same kernel again
	err = orch.RegisterKernel(kernel)
	assert.Error(t, err)
	
	// Try to register nil
	err = orch.RegisterKernel(nil)
	assert.Error(t, err)
}

func TestStartAndShutdown(t *testing.T) {
	orch, _ := NewOrchestrator()
	
	// Register a few test kernels
	kernel1 := microkernel.NewMicroKernel("test-1", microkernel.RoleVision)
	kernel2 := microkernel.NewMicroKernel("test-2", microkernel.RoleLinguistic)
	
	orch.RegisterKernel(kernel1)
	orch.RegisterKernel(kernel2)
	
	// Start the orchestrator
	err := orch.Start()
	assert.NoError(t, err)
	assert.True(t, orch.isRunning)
	
	// Give it a moment to start
	time.Sleep(100 * time.Millisecond)
	
	// Check kernels are running
	assert.Equal(t, microkernel.StatusRunning, kernel1.Status())
	assert.Equal(t, microkernel.StatusRunning, kernel2.Status())
	
	// Try to start again
	err = orch.Start()
	assert.Error(t, err)
	
	// Shutdown
	err = orch.Shutdown()
	assert.NoError(t, err)
	assert.False(t, orch.isRunning)
	
	// Give it a moment to stop
	time.Sleep(200 * time.Millisecond)
	
	// Check kernels are stopped
	assert.Equal(t, microkernel.StatusIdle, kernel1.Status())
	assert.Equal(t, microkernel.StatusIdle, kernel2.Status())
	
	// Try to shutdown again
	err = orch.Shutdown()
	assert.Error(t, err)
}
EOF

        echo "Proyecto Go inicializado correctamente."
    else
        echo "El proyecto Go ya está inicializado."
    fi
else
    echo "Error: No se encontró el directorio src/go."
    echo "Asegúrese de ejecutar este script desde la raíz del proyecto Synth-U."
    exit 1
fi

echo "Configuración de Go completada."
