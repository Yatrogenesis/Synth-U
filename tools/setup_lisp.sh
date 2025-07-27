# Script para instalar y configurar SBCL y Quicklisp
#!/bin/bash
set -e

echo "=== Configurando Common Lisp (SBCL) ==="

# Verificar si ya está instalado
if command -v sbcl &> /dev/null; then
    echo "SBCL ya está instalado. Versión:"
    sbcl --version
else
    echo "Instalando SBCL..."
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        sudo apt-get update
        sudo apt-get install -y sbcl rlwrap
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        brew install sbcl rlwrap
    else
        echo "Sistema operativo no soportado para instalación automática."
        echo "Por favor, instale SBCL manualmente desde: http://www.sbcl.org/platform-table.html"
        exit 1
    fi
fi

# Instalar Quicklisp si no está instalado
if [ ! -d "$HOME/quicklisp" ]; then
    echo "Instalando Quicklisp..."
    curl -O https://beta.quicklisp.org/quicklisp.lisp
    sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit
    rm quicklisp.lisp
    echo "Quicklisp instalado correctamente."
else
    echo "Quicklisp ya está instalado."
fi

# Instalar dependencias comunes
echo "Instalando dependencias de Common Lisp..."
sbcl --eval '(ql:quickload :alexandria)' \
     --eval '(ql:quickload :cl-ppcre)' \
     --eval '(ql:quickload :bordeaux-threads)' \
     --eval '(ql:quickload :usocket)' \
     --eval '(ql:quickload :cl-json)' \
     --eval '(ql:quickload :cffi)' \
     --eval '(ql:quickload :cl-async)' \
     --eval '(ql:quickload :prove)' \
     --eval '(ql:quickload :fiveam)' \
     --quit

echo "Configuración de Common Lisp completada."
