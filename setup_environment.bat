@echo off
setlocal

echo ==========================================
echo   CONFIGURACIÓN DEL ENTORNO SYNTH-U      
echo ==========================================
echo.

:: Verificar que estamos en la raíz del proyecto
if not exist "README.md" (
    echo Error: Este script debe ejecutarse desde la raíz del proyecto Synth-U.
    echo Por favor, cambie al directorio raíz e inténtelo de nuevo.
    exit /b 1
)

:: Crear directorio de logs si no existe
if not exist "logs" mkdir logs

:: Función para registrar en el log
call :log "Iniciando configuración del entorno de desarrollo..."

:: Paso 1: Construir el contenedor Docker
call :log "Paso 1: Construyendo contenedor Docker..."
cd build
docker-compose build
if %ERRORLEVEL% neq 0 (
    call :log "Error: No se pudo construir el contenedor Docker."
    exit /b 1
)
call :log "Contenedor Docker construido correctamente."

:: Paso 2: Iniciar los servicios
call :log "Paso 2: Iniciando servicios Docker..."
docker-compose up -d
if %ERRORLEVEL% neq 0 (
    call :log "Error: No se pudieron iniciar los servicios Docker."
    exit /b 1
)
call :log "Servicios Docker iniciados correctamente."

:: Paso 3: Configurar Common Lisp
call :log "Paso 3: Configurando Common Lisp..."
docker-compose exec -T dev bash -c "cd /synth-u && bash tools/setup_lisp.sh"
if %ERRORLEVEL% neq 0 (
    call :log "Error: No se pudo configurar Common Lisp."
    exit /b 1
)
call :log "Common Lisp configurado correctamente."

:: Paso 4: Configurar Rust
call :log "Paso 4: Configurando Rust..."
docker-compose exec -T dev bash -c "cd /synth-u && bash tools/setup_rust.sh"
if %ERRORLEVEL% neq 0 (
    call :log "Error: No se pudo configurar Rust."
    exit /b 1
)
call :log "Rust configurado correctamente."

:: Paso 5: Configurar Python
call :log "Paso 5: Configurando Python..."
docker-compose exec -T dev bash -c "cd /synth-u && bash tools/setup_python.sh"
if %ERRORLEVEL% neq 0 (
    call :log "Error: No se pudo configurar Python."
    exit /b 1
)
call :log "Python configurado correctamente."

:: Paso 6: Configurar Go
call :log "Paso 6: Configurando Go..."
docker-compose exec -T dev bash -c "cd /synth-u && bash tools/setup_go.sh"
if %ERRORLEVEL% neq 0 (
    call :log "Error: No se pudo configurar Go."
    exit /b 1
)
call :log "Go configurado correctamente."

:: Paso 7: Configurar C
call :log "Paso 7: Configurando C..."
docker-compose exec -T dev bash -c "cd /synth-u && bash tools/setup_c.sh"
if %ERRORLEVEL% neq 0 (
    call :log "Error: No se pudo configurar C."
    exit /b 1
)
call :log "C configurado correctamente."

:: Paso 8: Configurar Git
call :log "Paso 8: Configurando Git..."
docker-compose exec -T dev bash -c "cd /synth-u && git init && git config --global user.name 'Synth-U Developer' && git config --global user.email 'dev@synth-u.example.com'"
if %ERRORLEVEL% neq 0 (
    call :log "Error: No se pudo configurar Git."
    exit /b 1
)
call :log "Git configurado correctamente."

:: Paso 9: Crear hook de pre-commit
call :log "Paso 9: Configurando git hooks..."
docker-compose exec -T dev bash -c "cd /synth-u && mkdir -p .git/hooks && cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash
set -e

echo 'Ejecutando pre-commit hooks...'

# Verificar estilo de código
if [ -d 'src/rust' ]; then
    echo 'Verificando código Rust...'
    cargo fmt --all -- --check
fi

if [ -d 'src/python' ]; then
    echo 'Verificando código Python...'
    black --check src/python
    flake8 src/python
fi

if [ -d 'src/go' ]; then
    echo 'Verificando código Go...'
    go fmt ./...
fi

# Ejecutar tests unitarios rápidos
echo 'Ejecutando tests unitarios...'
if [ -d 'src/rust' ]; then
    cargo test --quiet
fi

if [ -d 'src/python' ]; then
    pytest -xvs src/python
fi

if [ -d 'src/go' ]; then
    go test ./...
fi

echo 'Pre-commit hooks ejecutados exitosamente'
EOF
chmod +x .git/hooks/pre-commit"
if %ERRORLEVEL% neq 0 (
    call :log "Error: No se pudieron configurar los git hooks."
    exit /b 1
)
call :log "Git hooks configurados correctamente."

:: Paso 10: Verificar el entorno completo
call :log "Paso 10: Verificando entorno completo..."
docker-compose exec -T dev bash -c "cd /synth-u && bash tools/verify_environment.sh"
if %ERRORLEVEL% neq 0 (
    call :log "Error: La verificación del entorno falló."
    exit /b 1
)
call :log "Verificación del entorno completada correctamente."

:: Paso 11: Iniciar primer commit
call :log "Paso 11: Inicializando repositorio Git..."
docker-compose exec -T dev bash -c "cd /synth-u && git add . && git commit -m 'Initial setup: Project structure and environment'"
if %ERRORLEVEL% neq 0 (
    call :log "Advertencia: No se pudo crear el commit inicial. Esto es normal si el repositorio ya estaba inicializado."
)
call :log "Repositorio Git inicializado."

call :log "¡Configuración del entorno completada con éxito!"
echo.
echo ==========================================
echo   ENTORNO SYNTH-U CONFIGURADO EXITOSAMENTE
echo ==========================================
echo.
echo Para entrar al contenedor de desarrollo:
echo   cd build ^&^& docker-compose exec dev bash
echo.
echo Para verificar el estado del proyecto:
echo   type project_management\STATUS_DASHBOARD.md
echo.
echo Para programar el Checkpoint 1:
echo   Edite project_management\STATUS_DASHBOARD.md
echo.
echo Los servicios Neo4j y Redis están disponibles en:
echo   Neo4j: http://localhost:7474 (neo4j/synth-u-dev)
echo   Redis: localhost:6379
echo.

goto :eof

:log
echo [%DATE% %TIME%] %~1
echo [%DATE% %TIME%] %~1 >> logs\setup.log
goto :eof
