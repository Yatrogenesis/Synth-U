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
