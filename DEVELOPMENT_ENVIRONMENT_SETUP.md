# ENVIRONMENT SETUP - SYNTH-U PROJECT
## Configuración del Entorno de Desarrollo

**Versión**: 1.0  
**Fecha**: 2025-07-26  
**Estado**: Documento Fundacional

---

## 1. VISIÓN GENERAL

Este documento describe la configuración completa del entorno de desarrollo para el proyecto Synth-U. Dado que el proyecto utiliza múltiples lenguajes (Common Lisp, Rust, Python, C y Go) e integra diversas tecnologías, es crucial mantener un entorno de desarrollo consistente y reproducible.

### 1.1 Principios de Configuración
- **Reproducibilidad**: Cualquier desarrollador debe poder recrear el entorno exacto
- **Aislamiento**: Evitar conflictos entre dependencias y versiones
- **Automatización**: Minimizar la configuración manual
- **Verificación**: Validar que el entorno está correctamente configurado

---

## 2. REQUISITOS DE SISTEMA

### 2.1 Hardware Recomendado
- **CPU**: 8+ núcleos, preferiblemente con soporte para virtualización
- **RAM**: 32GB+ (16GB mínimo)
- **Almacenamiento**: 100GB+ de espacio libre, preferiblemente SSD
- **GPU**: CUDA compatible para módulos de IA (opcional pero recomendado)

### 2.2 Sistemas Operativos Soportados
- **Principal**: Linux (Ubuntu 24.04 LTS o similar)
- **Secundario**: Windows 11 con WSL2
- **Alternativo**: macOS 14+ (Sonoma o superior)

---

## 3. ENTORNO CONTAINERIZADO

### 3.1 Docker / Podman Setup
El proyecto utiliza contenedores para garantizar la consistencia del entorno de desarrollo.

```bash
# Instalar Docker
sudo apt-get update
sudo apt-get install -y docker.io docker-compose

# Configuración de permisos
sudo usermod -aG docker $USER
newgrp docker

# Verificar instalación
docker --version
docker-compose --version
```

### 3.2 Contenedor Principal de Desarrollo

```dockerfile
# Dockerfile.dev
FROM ubuntu:24.04

# Evitar interacciones durante la instalación
ENV DEBIAN_FRONTEND=noninteractive

# Instalar dependencias comunes
RUN apt-get update && apt-get install -y \
    build-essential \
    git \
    curl \
    wget \
    vim \
    htop \
    python3 \
    python3-pip \
    python3-dev \
    libssl-dev \
    pkg-config \
    cmake \
    ninja-build

# Setup para cada lenguaje específico
# 1. Common Lisp (SBCL)
RUN apt-get install -y sbcl rlwrap
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit

# 2. Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"
RUN rustup default stable && \
    rustup component add rust-src rustfmt clippy

# 3. Python
RUN pip3 install --upgrade pip && \
    pip3 install numpy scipy torch pandas matplotlib scikit-learn pytest black flake8

# 4. Go
RUN wget https://go.dev/dl/go1.21.1.linux-amd64.tar.gz && \
    tar -C /usr/local -xzf go1.21.1.linux-amd64.tar.gz && \
    rm go1.21.1.linux-amd64.tar.gz
ENV PATH="/usr/local/go/bin:${PATH}"

# 5. C/C++ toolchain
RUN apt-get install -y clang clang-format clang-tidy

# Herramientas de desarrollo adicionales
RUN apt-get install -y valgrind gdb lldb

# Limpieza
RUN apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Directorio de trabajo
WORKDIR /synth-u

# Volumen para el código
VOLUME ["/synth-u"]

# Comando por defecto
CMD ["/bin/bash"]
```

### 3.3 Docker Compose para Multi-Contenedor

```yaml
# docker-compose.yml
version: '3.8'

services:
  dev:
    build:
      context: .
      dockerfile: Dockerfile.dev
    volumes:
      - .:/synth-u
      - ~/.ssh:/root/.ssh:ro
      - ~/.gitconfig:/root/.gitconfig:ro
    ports:
      - "8000:8000"  # Para debugging/web services
    environment:
      - DISPLAY=${DISPLAY}  # Para aplicaciones GUI si es necesario
    command: /bin/bash
    tty: true
    stdin_open: true
    
  neo4j:
    image: neo4j:5.9.0
    ports:
      - "7474:7474"  # HTTP
      - "7687:7687"  # Bolt
    environment:
      - NEO4J_AUTH=neo4j/synth-u-dev
    volumes:
      - neo4j-data:/data
      
  redis:
    image: redis:7.2
    ports:
      - "6379:6379"
    volumes:
      - redis-data:/data

volumes:
  neo4j-data:
  redis-data:
```

### 3.4 Uso del Entorno Containerizado

```bash
# Construir y levantar los contenedores
docker-compose build
docker-compose up -d

# Entrar al contenedor de desarrollo
docker-compose exec dev bash

# Verificar que todos los lenguajes estén disponibles
sbcl --version
rustc --version
python3 --version
go version
clang --version

# Detener los contenedores cuando termine
docker-compose down
```

---

## 4. CONFIGURACIÓN POR LENGUAJE

### 4.1 Common Lisp (SBCL)

#### 4.1.1 Instalación Directa (sin Docker)
```bash
# Ubuntu/Debian
sudo apt-get install sbcl rlwrap

# Instalar Quicklisp (gestor de paquetes)
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit
```

#### 4.1.2 Dependencias de Quicklisp
```lisp
;; En REPL de SBCL
(ql:quickload :alexandria)      ; Utilidades generales
(ql:quickload :cl-ppcre)        ; Regex
(ql:quickload :bordeaux-threads) ; Multithreading
(ql:quickload :usocket)         ; Sockets
(ql:quickload :cl-json)         ; JSON 
(ql:quickload :cffi)            ; Foreign Function Interface
(ql:quickload :cl-async)        ; Async programming
(ql:quickload :prove)           ; Testing framework
```

#### 4.1.3 Configuración del Editor (Emacs/SLIME)
```elisp
;; Configuración para Emacs
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)
(package-install 'slime)
(package-install 'paredit)
(package-install 'rainbow-delimiters)

;; Configurar SLIME
(setq inferior-lisp-program "sbcl")
(require 'slime)
(slime-setup '(slime-fancy slime-asdf))
```

### 4.2 Rust

#### 4.2.1 Instalación Directa (sin Docker)
```bash
# Instalar rustup
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source $HOME/.cargo/env

# Componentes adicionales
rustup component add rust-src rustfmt clippy
```

#### 4.2.2 Dependencias de Cargo (Cargo.toml)
```toml
[dependencies]
tokio = { version = "1.28", features = ["full"] }  # Async runtime
serde = { version = "1.0", features = ["derive"] } # Serialization
serde_json = "1.0"                                # JSON
zeromq = "0.3"                                    # ZeroMQ bindings
neo4rs = "0.6"                                    # Neo4j client
thiserror = "1.0"                                 # Error handling
log = "0.4"                                       # Logging
env_logger = "0.10"                               # Logging
clap = { version = "4.2", features = ["derive"] } # CLI args

[dev-dependencies]
criterion = "0.5"                                 # Benchmarking
mockall = "0.11"                                  # Mocking
test-case = "3.1"                                 # Test utilities
```

#### 4.2.3 Herramientas de Desarrollo
```bash
# Instalar herramientas adicionales
cargo install cargo-edit      # Editar dependencias
cargo install cargo-watch     # Auto-recompilación
cargo install cargo-audit     # Auditoría de seguridad
cargo install cargo-expand    # Ver código expandido
cargo install cargo-flamegraph # Profiling
```

### 4.3 Python

#### 4.3.1 Instalación Directa (sin Docker)
```bash
# Instalar Python y pip
sudo apt-get install python3 python3-pip python3-dev

# Crear entorno virtual
python3 -m venv venv
source venv/bin/activate
```

#### 4.3.2 Dependencias (requirements.txt)
```
# AI/ML
numpy==1.26.0
scipy==1.12.0
scikit-learn==1.3.1
torch==2.1.0
tensorflow==2.15.0
transformers==4.35.0
pillow==10.0.1

# Data Processing
pandas==2.1.1
matplotlib==3.8.0
seaborn==0.13.0

# Networking/Comms
zeromq==0.0.0
pyzmq==25.1.1
grpcio==1.58.0
protobuf==4.24.3

# Database
neo4j==5.11.0
redis==5.0.0

# Dev Tools
pytest==7.4.2
black==23.9.1
flake8==6.1.0
mypy==1.5.1
```

#### 4.3.3 Configuración de Jupyter (opcional)
```bash
# Instalar Jupyter
pip install jupyter jupyterlab

# Crear kernel para el proyecto
python -m ipykernel install --user --name=synth-u

# Iniciar Jupyter
jupyter lab
```

### 4.4 Go

#### 4.4.1 Instalación Directa (sin Docker)
```bash
# Descargar e instalar Go
wget https://go.dev/dl/go1.21.1.linux-amd64.tar.gz
sudo tar -C /usr/local -xzf go1.21.1.linux-amd64.tar.gz
rm go1.21.1.linux-amd64.tar.gz

# Añadir a PATH
echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.bashrc
source ~/.bashrc
```

#### 4.4.2 Configuración del Proyecto Go
```bash
# Inicializar módulo
mkdir -p src/go
cd src/go
go mod init github.com/your-username/synth-u

# Instalar dependencias comunes
go get -u google.golang.org/grpc
go get -u github.com/go-redis/redis/v8
go get -u github.com/neo4j/neo4j-go-driver/v5
go get -u github.com/zeromq/goczmq
go get -u github.com/spf13/cobra
go get -u github.com/stretchr/testify
```

### 4.5 C / C++

#### 4.5.1 Instalación Directa (sin Docker)
```bash
# Instalar herramientas de C/C++
sudo apt-get install build-essential cmake ninja-build
sudo apt-get install clang clang-format clang-tidy
sudo apt-get install valgrind gdb
```

#### 4.5.2 Estructura de CMake
```cmake
# CMakeLists.txt
cmake_minimum_required(VERSION 3.20)
project(SynthU VERSION 0.1.0 LANGUAGES C CXX)

# Opciones
option(BUILD_TESTS "Build the tests" ON)
option(ENABLE_ASAN "Enable Address Sanitizer" OFF)

# Flags de compilación
set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(ENABLE_ASAN)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fsanitize=address -fno-omit-frame-pointer")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address -fno-omit-frame-pointer")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fsanitize=address")
endif()

# Dependencias externas
find_package(ZLIB REQUIRED)
find_package(OpenSSL REQUIRED)

# Biblioteca principal
add_library(synth_c_core STATIC
    src/c/memory_manager.c
    src/c/hardware_interface.c
    src/c/system_calls.c
)

target_include_directories(synth_c_core PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/include
)

target_link_libraries(synth_c_core PUBLIC
    ZLIB::ZLIB
    OpenSSL::SSL
    OpenSSL::Crypto
)

# Tests
if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests/c)
endif()
```

---

## 5. CONFIGURACIÓN DE IDE

### 5.1 Visual Studio Code

#### 5.1.1 Extensiones Recomendadas
```json
{
    "recommendations": [
        "ms-vscode-remote.remote-containers",
        "ms-vscode.cpptools",
        "rust-lang.rust-analyzer",
        "ms-python.python",
        "golang.go",
        "vlad-sheiderman.cl-ide",
        "yzhang.markdown-all-in-one",
        "streetsidesoftware.code-spell-checker",
        "eamodio.gitlens",
        "ms-azuretools.vscode-docker"
    ]
}
```

#### 5.1.2 Configuración del Workspace
```json
{
    "folders": [
        {
            "path": "."
        }
    ],
    "settings": {
        "editor.formatOnSave": true,
        "editor.rulers": [100],
        "files.trimTrailingWhitespace": true,
        "files.insertFinalNewline": true,
        
        // C/C++
        "C_Cpp.clang_format_style": "file",
        
        // Rust
        "rust-analyzer.checkOnSave.command": "clippy",
        
        // Python
        "python.formatting.provider": "black",
        "python.linting.enabled": true,
        "python.linting.flake8Enabled": true,
        
        // Go
        "go.formatTool": "goimports",
        "go.lintTool": "golangci-lint"
    },
    "launch": {
        "configurations": [
            {
                "name": "Debug C++ Program",
                "type": "cppdbg",
                "request": "launch",
                "program": "${workspaceFolder}/build/debug/program",
                "args": [],
                "stopAtEntry": false,
                "cwd": "${workspaceFolder}",
                "environment": [],
                "externalConsole": false,
                "MIMode": "gdb"
            },
            {
                "name": "Debug Rust Program",
                "type": "lldb",
                "request": "launch",
                "program": "${workspaceFolder}/target/debug/program",
                "args": [],
                "cwd": "${workspaceFolder}"
            }
        ]
    }
}
```

### 5.2 Emacs (para Common Lisp)

```elisp
;; Añadir a .emacs o init.el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Instalar paquetes si no existen
(dolist (package '(slime paredit rainbow-delimiters company))
  (unless (package-installed-p package)
    (package-install package)))

;; Configuración SLIME
(setq inferior-lisp-program "sbcl")
(require 'slime)
(slime-setup '(slime-fancy slime-asdf slime-indentation))

;; Paredit para edición estructural
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook       #'enable-paredit-mode)

;; Rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
```

### 5.3 JetBrains IDEs (opcional)

#### 5.3.1 PyCharm (Python)
- Instalar PyCharm Professional
- Configurar intérprete de Python al entorno virtual
- Habilitar formateo automático con Black
- Configurar mypy y flake8 como herramientas de análisis estático

#### 5.3.2 CLion (C/C++ y Rust)
- Instalar CLion
- Configurar toolchains para C/C++
- Instalar plugin de Rust
- Configurar CMake

#### 5.3.3 GoLand (Go)
- Instalar GoLand
- Configurar GOPATH y GOROOT
- Habilitar goimports para formateo automático

---

## 6. CONFIGURACIÓN DE VERSION CONTROL

### 6.1 Git

#### 6.1.1 Configuración Global
```bash
# Configuración básica
git config --global user.name "Tu Nombre"
git config --global user.email "tu.email@example.com"

# Configuración de editor
git config --global core.editor "code --wait"

# Alias útiles
git config --global alias.st status
git config --global alias.co checkout
git config --global alias.br branch
git config --global alias.ci commit
```

#### 6.1.2 Git Hooks
Crear en `.git/hooks/pre-commit`:
```bash
#!/bin/bash
set -e

# Verificar estilo de código
echo "Verificando estilo de código..."
if [ -d "src/rust" ]; then
    cargo fmt --all -- --check
fi

if [ -d "src/python" ]; then
    black --check src/python
    flake8 src/python
fi

if [ -d "src/go" ]; then
    go fmt ./...
fi

# Ejecutar tests unitarios rápidos
echo "Ejecutando tests unitarios..."
if [ -d "src/rust" ]; then
    cargo test --quiet
fi

if [ -d "src/python" ]; then
    pytest -xvs src/python
fi

if [ -d "src/go" ]; then
    go test ./...
fi

echo "Pre-commit hooks ejecutados exitosamente"
```

```bash
# Hacer el hook ejecutable
chmod +x .git/hooks/pre-commit
```

### 6.2 GitHub / GitLab

#### 6.2.1 Plantilla de Pull Request
Crear en `.github/pull_request_template.md`:
```markdown
## Descripción
[Descripción detallada de los cambios]

## Tipo de Cambio
- [ ] Corrección de Bug (fix)
- [ ] Nueva Funcionalidad (feat)
- [ ] Cambio que rompe compatibilidad (breaking)
- [ ] Documentación (docs)
- [ ] Refactorización (refactor)
- [ ] Optimización de Rendimiento (perf)
- [ ] Tests (test)

## ¿Cómo se ha probado?
[Descripción de las pruebas realizadas]

## Checklist
- [ ] Mi código sigue los estándares del proyecto
- [ ] He realizado una auto-revisión de mi código
- [ ] He comentado mi código, especialmente en áreas complejas
- [ ] He actualizado la documentación correspondiente
- [ ] Mis cambios no generan nuevas advertencias
- [ ] He añadido tests que prueban que mi corrección es efectiva o que mi nueva funcionalidad funciona
- [ ] Los tests unitarios nuevos y existentes pasan localmente con mis cambios
```

#### 6.2.2 GitHub Actions / GitLab CI
Crear en `.github/workflows/ci.yml`:
```yaml
name: CI

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]

jobs:
  build-test:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Set up Docker
      uses: docker/setup-buildx-action@v2
    
    - name: Build development container
      run: docker-compose build
    
    - name: Run Rust tests
      run: docker-compose run --rm dev bash -c "cd /synth-u && cargo test"
    
    - name: Run Python tests
      run: docker-compose run --rm dev bash -c "cd /synth-u && python -m pytest src/python"
    
    - name: Run Go tests
      run: docker-compose run --rm dev bash -c "cd /synth-u && cd src/go && go test ./..."
    
    - name: Lint code
      run: |
        docker-compose run --rm dev bash -c "
          cd /synth-u && 
          cargo clippy -- -D warnings &&
          black --check src/python &&
          flake8 src/python &&
          cd src/go && go vet ./...
        "
```

---

## 7. CONFIGURACIÓN DE CI/CD

### 7.1 Jenkins (Opcional)

#### 7.1.1 Jenkinsfile
```groovy
pipeline {
    agent {
        dockerfile {
            filename 'Dockerfile.dev'
            args '-v $HOME/.cargo:/var/jenkins_home/.cargo'
        }
    }
    
    stages {
        stage('Build') {
            steps {
                sh 'cargo build --workspace'
                sh 'cd src/go && go build ./...'
                sh 'cd src/python && python -m pip install -e .'
            }
        }
        
        stage('Test') {
            steps {
                sh 'cargo test --workspace'
                sh 'cd src/go && go test ./...'
                sh 'cd src/python && python -m pytest'
            }
        }
        
        stage('Lint') {
            steps {
                sh 'cargo clippy -- -D warnings'
                sh 'cd src/python && flake8 && black --check .'
                sh 'cd src/go && golangci-lint run'
            }
        }
        
        stage('Package') {
            steps {
                sh 'cargo build --release'
                sh 'cd src/go && go build -o ../../bin/go-components ./cmd/...'
                sh 'cd build && cmake --build . --target package'
            }
        }
    }
    
    post {
        always {
            junit '**/test-results/*.xml'
            archiveArtifacts artifacts: 'target/release/*, bin/*, build/package/*', fingerprint: true
        }
    }
}
```

### 7.2 GitHub Actions

#### 7.2.1 Release Workflow
Crear en `.github/workflows/release.yml`:
```yaml
name: Release

on:
  push:
    tags:
      - 'v*'

jobs:
  create-release:
    runs-on: ubuntu-latest
    outputs:
      upload_url: ${{ steps.create_release.outputs.upload_url }}
    
    steps:
    - name: Create Release
      id: create_release
      uses: actions/create-release@v1
      env:
        GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
      with:
        tag_name: ${{ github.ref }}
        release_name: Release ${{ github.ref }}
        draft: false
        prerelease: false
  
  build-binaries:
    needs: create-release
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Set up Docker
      uses: docker/setup-buildx-action@v2
    
    - name: Build release container
      run: docker-compose -f docker-compose.release.yml build
    
    - name: Build release binaries
      run: docker-compose -f docker-compose.release.yml run --rm release
    
    - name: Upload Release Assets
      uses: actions/upload-release-asset@v1
      env:
        GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
      with:
        upload_url: ${{ needs.create-release.outputs.upload_url }}
        asset_path: ./dist/synth-u-${{ github.ref }}.tar.gz
        asset_name: synth-u-${{ github.ref }}.tar.gz
        asset_content_type: application/gzip
```

---

## 8. VERIFICACIÓN DEL ENTORNO

### 8.1 Script de Verificación
Crear `verify_environment.sh`:
```bash
#!/bin/bash
set -e

echo "=== VERIFICACIÓN DEL ENTORNO DE DESARROLLO ==="
echo "Proyecto: Synth-U"
echo "Fecha: $(date)"
echo "---------------------------------------------"

# Verificar Docker
echo -n "Docker: "
if command -v docker &> /dev/null; then
    docker --version
    echo "✅ Instalado"
else
    echo "❌ No instalado"
    exit 1
fi

# Verificar lenguajes de programación
echo -n "Common Lisp (SBCL): "
if command -v sbcl &> /dev/null; then
    sbcl --version
    echo "✅ Instalado"
else
    echo "❌ No instalado"
    exit 1
fi

echo -n "Rust: "
if command -v rustc &> /dev/null; then
    rustc --version
    echo "✅ Instalado"
else
    echo "❌ No instalado"
    exit 1
fi

echo -n "Python: "
if command -v python3 &> /dev/null; then
    python3 --version
    echo "✅ Instalado"
else
    echo "❌ No instalado"
    exit 1
fi

echo -n "Go: "
if command -v go &> /dev/null; then
    go version
    echo "✅ Instalado"
else
    echo "❌ No instalado"
    exit 1
fi

echo -n "C/C++ (Clang): "
if command -v clang &> /dev/null; then
    clang --version
    echo "✅ Instalado"
else
    echo "❌ No instalado"
    exit 1
fi

# Verificar herramientas de desarrollo
echo -n "Git: "
if command -v git &> /dev/null; then
    git --version
    echo "✅ Instalado"
else
    echo "❌ No instalado"
    exit 1
fi

echo -n "CMake: "
if command -v cmake &> /dev/null; then
    cmake --version
    echo "✅ Instalado"
else
    echo "❌ No instalado"
    exit 1
fi

# Verificar frameworks de testing
echo -n "Pytest: "
if python3 -c "import pytest" &> /dev/null; then
    python3 -c "import pytest; print(pytest.__version__)"
    echo "✅ Instalado"
else
    echo "❌ No instalado"
    exit 1
fi

# Verificar bases de datos
echo -n "Neo4j: "
if command -v docker &> /dev/null && docker ps | grep -q neo4j; then
    echo "✅ Ejecutándose en Docker"
else
    echo "❌ No detectado"
fi

echo -n "Redis: "
if command -v docker &> /dev/null && docker ps | grep -q redis; then
    echo "✅ Ejecutándose en Docker"
else
    echo "❌ No detectado"
fi

echo "---------------------------------------------"
echo "✅ VERIFICACIÓN COMPLETA: Entorno configurado correctamente"
```

### 8.2 Instrucciones de Ejecución
```bash
# Hacer el script ejecutable
chmod +x verify_environment.sh

# Ejecutar verificación
./verify_environment.sh
```

---

## 9. TROUBLESHOOTING

### 9.1 Problemas Comunes y Soluciones

#### 9.1.1 Docker
- **Problema**: Permisos denegados al ejecutar Docker
  - **Solución**: `sudo usermod -aG docker $USER` y reiniciar sesión

- **Problema**: No se puede conectar a Docker daemon
  - **Solución**: `sudo systemctl start docker`

#### 9.1.2 Common Lisp
- **Problema**: Quicklisp no encuentra paquetes
  - **Solución**: `(ql:update-all-dists)` en REPL

- **Problema**: Error al compilar con SBCL
  - **Solución**: Verificar que todas las dependencias estén instaladas con `(ql:quickload :nombre-paquete)`

#### 9.1.3 Rust
- **Problema**: Error de compilación por versiones incompatibles
  - **Solución**: Verificar Cargo.lock o usar `cargo update`

- **Problema**: Error de linking al compilar
  - **Solución**: `sudo apt-get install build-essential`

#### 9.1.4 Python
- **Problema**: Conflictos de dependencias
  - **Solución**: Usar entorno virtual `python -m venv venv`

- **Problema**: Módulos no encontrados
  - **Solución**: Verificar PYTHONPATH o instalar con `pip install -e .`

### 9.2 Registro de Logs
Crear `logs/development.log` para registrar problemas:
```bash
mkdir -p logs
touch logs/development.log
```

---

## 10. ONBOARDING DE NUEVOS DESARROLLADORES

### 10.1 Lista de Verificación para Nuevos Desarrolladores
1. Clonar repositorio: `git clone [URL_REPO] synth-u`
2. Cambiar al directorio: `cd synth-u`
3. Construir entorno Docker: `docker-compose build`
4. Iniciar contenedores: `docker-compose up -d`
5. Ejecutar verificación: `./verify_environment.sh`
6. Revisar documentación en `docs/`
7. Ejecutar tests: `docker-compose exec dev bash -c "cd /synth-u && ./run_all_tests.sh"`

### 10.2 Recursos Adicionales
- [Documentación oficial de Docker](https://docs.docker.com/)
- [Documentación de SBCL](http://www.sbcl.org/manual/)
- [Documentación de Rust](https://doc.rust-lang.org/book/)
- [Documentación de Python](https://docs.python.org/3/)
- [Documentación de Go](https://golang.org/doc/)

---

**NOTA**: Este documento debe actualizarse regularmente a medida que evolucione el entorno de desarrollo. La última actualización fue realizada el 2025-07-26.
