@echo off
echo === SUBIENDO PROYECTO A GITHUB ===

cd D:\Synth-U

echo 1. Limpiando repositorio Git anterior (si existe)...
if exist .git rmdir /s /q .git

echo 2. Inicializando repositorio Git...
git init

echo 3. Configurando rama principal...
git branch -m main

echo 4. Añadiendo todos los archivos...
git add .

echo 5. Creando commit inicial...
git commit -m "Version inicial del sistema Synth-U"

echo 6. Conectando con GitHub...
git remote add origin https://github.com/Yatrogenesis/Synth-U.git

echo 7. Subiendo a GitHub (puede pedir credenciales)...
git push -u origin main --force

echo === PROCESO COMPLETADO ===
echo Si hubo algún error, verifica tu conexión a internet o tus credenciales de GitHub.
pause