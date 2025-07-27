# Script para configurar entorno de desarrollo C
#!/bin/bash
set -e

echo "=== Configurando C ==="

# Verificar si gcc está instalado
if command -v gcc &> /dev/null; then
    echo "GCC ya está instalado. Versión:"
    gcc --version
else
    echo "Instalando herramientas de desarrollo C..."
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        sudo apt-get update
        sudo apt-get install -y build-essential cmake ninja-build
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        brew install gcc cmake ninja
    else
        echo "Sistema operativo no soportado para instalación automática."
        echo "Por favor, instale herramientas de desarrollo C manualmente."
        exit 1
    fi
fi

# Verificar si clang está instalado
if command -v clang &> /dev/null; then
    echo "Clang ya está instalado. Versión:"
    clang --version
else
    echo "Instalando Clang..."
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        sudo apt-get update
        sudo apt-get install -y clang clang-format clang-tidy
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        brew install llvm
    else
        echo "Sistema operativo no soportado para instalación automática."
        exit 1
    fi
fi

# Verificar si cmake está instalado
if command -v cmake &> /dev/null; then
    echo "CMake ya está instalado. Versión:"
    cmake --version
else
    echo "CMake es necesario para construir el proyecto. Por favor, instálelo."
    exit 1
fi

# Configurar estructura de proyecto C
echo "Configurando estructura de proyecto C..."

# Verificar si estamos en la raíz del proyecto
if [ -d "src/c" ]; then
    cd src/c
    
    # Crear estructura básica
    mkdir -p hardware_drivers
    mkdir -p system_calls
    mkdir -p optimization
    mkdir -p include
    
    # Crear archivo principal de CMake si no existe
    if [ ! -f "CMakeLists.txt" ]; then
        echo "Creando archivos de configuración C..."
        
        # Crear CMakeLists.txt principal
        cat > CMakeLists.txt << EOF
cmake_minimum_required(VERSION 3.20)
project(SynthUCCore VERSION 0.1.0 LANGUAGES C)

# Opciones
option(BUILD_TESTS "Build the tests" ON)
option(ENABLE_ASAN "Enable Address Sanitizer" OFF)

# Flags de compilación
set(CMAKE_C_STANDARD 11)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(ENABLE_ASAN)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fsanitize=address -fno-omit-frame-pointer")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fsanitize=address")
endif()

# Dependencias externas
find_package(Threads REQUIRED)

# Directorios de include
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
)

# Biblioteca principal
add_library(synth_c_core STATIC
    hardware_drivers/memory_manager.c
    hardware_drivers/hardware_interface.c
    system_calls/system_interface.c
    optimization/performance.c
)

target_link_libraries(synth_c_core PRIVATE
    Threads::Threads
)

# Aplicación de prueba
add_executable(synth_c_test
    main.c
)

target_link_libraries(synth_c_test PRIVATE
    synth_c_core
)

# Tests
if(BUILD_TESTS)
    enable_testing()
    
    add_executable(hardware_test
        tests/hardware_test.c
    )
    
    target_link_libraries(hardware_test PRIVATE
        synth_c_core
    )
    
    add_test(NAME HardwareTest COMMAND hardware_test)
endif()

# Instalación
install(TARGETS synth_c_core
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
    RUNTIME DESTINATION bin
)

install(DIRECTORY include/
    DESTINATION include/synth_u
)
EOF
        
        # Crear archivo de cabecera principal
        mkdir -p include/synth_u
        cat > include/synth_u/core.h << EOF
/**
 * @file core.h
 * @brief Core header file for Synth-U C components
 *
 * This file contains the primary interfaces for the Synth-U
 * C core components, including hardware access, memory management,
 * and system calls.
 */

#ifndef SYNTH_U_CORE_H
#define SYNTH_U_CORE_H

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

/**
 * @brief Version information
 */
#define SYNTH_U_VERSION_MAJOR 0
#define SYNTH_U_VERSION_MINOR 1
#define SYNTH_U_VERSION_PATCH 0

/**
 * @brief Status codes for API functions
 */
typedef enum {
    SYNTH_U_SUCCESS = 0,
    SYNTH_U_ERROR_INVALID_ARGUMENT = -1,
    SYNTH_U_ERROR_OUT_OF_MEMORY = -2,
    SYNTH_U_ERROR_IO = -3,
    SYNTH_U_ERROR_NOT_IMPLEMENTED = -4,
    SYNTH_U_ERROR_PERMISSION_DENIED = -5,
    SYNTH_U_ERROR_TIMEOUT = -6,
    SYNTH_U_ERROR_UNKNOWN = -100
} synth_u_status_t;

/**
 * @brief Initialization options
 */
typedef struct {
    uint32_t flags;
    size_t memory_limit;
    bool enable_logging;
    const char* log_file;
} synth_u_init_options_t;

/**
 * @brief Initialize the Synth-U C core
 *
 * This function must be called before any other function in the library.
 *
 * @param options Initialization options, or NULL for defaults
 * @return Status code
 */
synth_u_status_t synth_u_initialize(const synth_u_init_options_t* options);

/**
 * @brief Shutdown the Synth-U C core
 *
 * This function should be called before application exit to release resources.
 *
 * @return Status code
 */
synth_u_status_t synth_u_shutdown(void);

/**
 * @brief Get the version string
 *
 * @return Version string in format "major.minor.patch"
 */
const char* synth_u_version(void);

#endif /* SYNTH_U_CORE_H */
EOF
        
        # Crear implementación del memory manager
        mkdir -p hardware_drivers
        cat > hardware_drivers/memory_manager.c << EOF
/**
 * @file memory_manager.c
 * @brief Implementation of the memory management system
 */

#include <synth_u/core.h>
#include <string.h>
#include <stdio.h>

// Private structure for memory block
typedef struct memory_block {
    void* address;
    size_t size;
    bool in_use;
    struct memory_block* next;
} memory_block_t;

// Global memory manager state
static struct {
    memory_block_t* blocks;
    size_t total_allocated;
    size_t memory_limit;
    bool initialized;
} memory_manager = {0};

/**
 * Initialize the memory manager
 */
synth_u_status_t memory_manager_init(size_t memory_limit) {
    if (memory_manager.initialized) {
        return SYNTH_U_SUCCESS;
    }
    
    memory_manager.blocks = NULL;
    memory_manager.total_allocated = 0;
    memory_manager.memory_limit = memory_limit;
    memory_manager.initialized = true;
    
    return SYNTH_U_SUCCESS;
}

/**
 * Allocate memory
 */
void* memory_manager_alloc(size_t size) {
    if (!memory_manager.initialized) {
        return NULL;
    }
    
    // Check if we would exceed memory limit
    if (memory_manager.memory_limit > 0 && 
        memory_manager.total_allocated + size > memory_manager.memory_limit) {
        return NULL;
    }
    
    // Allocate the memory
    void* mem = malloc(size);
    if (!mem) {
        return NULL;
    }
    
    // Create a new block
    memory_block_t* block = (memory_block_t*)malloc(sizeof(memory_block_t));
    if (!block) {
        free(mem);
        return NULL;
    }
    
    // Initialize the block
    block->address = mem;
    block->size = size;
    block->in_use = true;
    block->next = memory_manager.blocks;
    memory_manager.blocks = block;
    
    // Update stats
    memory_manager.total_allocated += size;
    
    return mem;
}

/**
 * Free memory
 */
void memory_manager_free(void* ptr) {
    if (!memory_manager.initialized || !ptr) {
        return;
    }
    
    memory_block_t* prev = NULL;
    memory_block_t* current = memory_manager.blocks;
    
    while (current) {
        if (current->address == ptr) {
            // Found the block
            if (current->in_use) {
                current->in_use = false;
                memory_manager.total_allocated -= current->size;
                free(ptr);
                
                // Remove from list
                if (prev) {
                    prev->next = current->next;
                } else {
                    memory_manager.blocks = current->next;
                }
                
                free(current);
            }
            return;
        }
        
        prev = current;
        current = current->next;
    }
    
    // Block not found, this might be a double free or invalid pointer
    fprintf(stderr, "Warning: Attempting to free untracked memory at %p\n", ptr);
}

/**
 * Get memory statistics
 */
void memory_manager_stats(size_t* total_allocated, size_t* memory_limit) {
    if (total_allocated) {
        *total_allocated = memory_manager.total_allocated;
    }
    
    if (memory_limit) {
        *memory_limit = memory_manager.memory_limit;
    }
}

/**
 * Shutdown the memory manager
 */
synth_u_status_t memory_manager_shutdown(void) {
    if (!memory_manager.initialized) {
        return SYNTH_U_SUCCESS;
    }
    
    // Free all allocated blocks
    memory_block_t* current = memory_manager.blocks;
    while (current) {
        memory_block_t* next = current->next;
        
        if (current->in_use) {
            free(current->address);
        }
        
        free(current);
        current = next;
    }
    
    // Reset state
    memory_manager.blocks = NULL;
    memory_manager.total_allocated = 0;
    memory_manager.initialized = false;
    
    return SYNTH_U_SUCCESS;
}
EOF
        
        # Crear archivo de implementación principal
        cat > main.c << EOF
/**
 * @file main.c
 * @brief Main entry point for Synth-U C core test application
 */

#include <synth_u/core.h>
#include <stdio.h>

int main(int argc, char** argv) {
    printf("Synth-U C Core Test Application\n");
    printf("Version: %s\n", synth_u_version());
    
    // Initialize with default options
    synth_u_status_t status = synth_u_initialize(NULL);
    if (status != SYNTH_U_SUCCESS) {
        printf("Failed to initialize Synth-U C Core: %d\n", status);
        return 1;
    }
    
    printf("Synth-U C Core initialized successfully.\n");
    
    // TODO: Add more functionality
    
    // Shutdown
    status = synth_u_shutdown();
    if (status != SYNTH_U_SUCCESS) {
        printf("Failed to shutdown Synth-U C Core: %d\n", status);
        return 1;
    }
    
    printf("Synth-U C Core shutdown successfully.\n");
    
    return 0;
}
EOF
        
        # Crear archivo de test
        mkdir -p tests
        cat > tests/hardware_test.c << EOF
/**
 * @file hardware_test.c
 * @brief Tests for hardware interfaces
 */

#include <synth_u/core.h>
#include <stdio.h>
#include <assert.h>

int main(int argc, char** argv) {
    printf("Running hardware tests...\n");
    
    // Initialize with default options
    synth_u_status_t status = synth_u_initialize(NULL);
    assert(status == SYNTH_U_SUCCESS);
    
    // TODO: Add actual tests
    
    // Shutdown
    status = synth_u_shutdown();
    assert(status == SYNTH_U_SUCCESS);
    
    printf("All tests passed.\n");
    return 0;
}
EOF
        
        echo "Proyecto C inicializado correctamente."
    else
        echo "El proyecto C ya está inicializado."
    fi
else
    echo "Error: No se encontró el directorio src/c."
    echo "Asegúrese de ejecutar este script desde la raíz del proyecto Synth-U."
    exit 1
fi

echo "Configuración de C completada."
