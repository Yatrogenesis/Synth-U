# GIT WORKFLOW - SYNTH-U PROJECT
## Estrategia de Control de Versiones y Colaboración

**Versión**: 1.0  
**Fecha**: 2025-07-26  
**Estado**: Documento Fundacional

---

## 1. VISIÓN GENERAL

Este documento define la estrategia de control de versiones y el flujo de trabajo de Git para el proyecto Synth-U. Debido a la naturaleza compleja y multi-lenguaje del proyecto, un enfoque estructurado y disciplinado para el control de versiones es esencial para mantener la integridad del código y facilitar la colaboración.

### 1.1 Principios Fundamentales
- **Trunk-Based Development**: Enfoque principal con ramas de corta duración
- **Integración Continua**: Integración frecuente a la rama principal
- **Revisión de Código**: Todo código debe ser revisado antes de integrarse
- **Historias Atómicas**: Commits lógicos y atómicos con mensajes descriptivos
- **Trazabilidad**: Conexión clara entre código y tickets/requerimientos

---

## 2. ESTRUCTURA DE REPOSITORIO

### 2.1 Repositorio Principal
- **URL**: `https://github.com/[ORGANIZATION]/synth-u`
- **Descripción**: Repositorio monolítico que contiene todo el código del proyecto
- **Visibilidad**: Privado con acceso controlado

### 2.2 Estructura de Ramas

#### 2.2.1 Ramas Permanentes
- **`main`**: Rama principal, siempre estable y desplegable
- **`develop`**: Rama de integración para desarrollo continuo

#### 2.2.2 Ramas Temporales
- **`feature/[nombre-feature]`**: Para nuevas funcionalidades
- **`bugfix/[nombre-bug]`**: Para corrección de bugs
- **`hotfix/[nombre-hotfix]`**: Para correcciones urgentes en producción
- **`release/v[X.Y.Z]`**: Para preparación de releases
- **`experiment/[nombre-experimento]`**: Para código experimental

### 2.3 Convenciones de Nomenclatura
- Todas las ramas en inglés
- Usar kebab-case para nombres (ej: `feature/symbolic-genome`)
- Incluir identificador de ticket cuando sea aplicable (ej: `feature/SYN-123-symbolic-genome`)

---

## 3. FLUJO DE TRABAJO

### 3.1 Creación de Ramas

```bash
# Asegurarse de tener la última versión de la rama principal
git checkout develop
git pull origin develop

# Crear nueva rama feature
git checkout -b feature/nombre-feature

# Alternativamente, para bugs
git checkout -b bugfix/descripcion-bug
```

### 3.2 Desarrollo y Commits

#### 3.2.1 Convención de Mensajes de Commit
Formato:
```
tipo(alcance): descripción breve

Descripción detallada (opcional)
```

Tipos permitidos:
- `feat`: Nueva funcionalidad
- `fix`: Corrección de bug
- `docs`: Cambios en documentación
- `style`: Cambios que no afectan el significado del código (espacios, formato, etc)
- `refactor`: Cambio de código que no corrige un bug ni añade una característica
- `perf`: Cambio que mejora el rendimiento
- `test`: Añadir o corregir tests
- `chore`: Cambios en el proceso de build, herramientas, etc.

Ejemplos:
```
feat(ontology): implementar capa 3 de percepción

fix(microkernel): corregir memory leak en asignación de roles

docs(architecture): actualizar diagrama de comunicación entre capas
```

#### 3.2.2 Buenas Prácticas para Commits
- Hacer commits pequeños y frecuentes
- Cada commit debe representar un cambio lógico y coherente
- Asegurar que el código compila y pasa los tests antes de commit
- No incluir archivos temporales o generados
- No mezclar cambios no relacionados en un mismo commit

### 3.3 Integración a Develop

#### 3.3.1 Pull Requests
Todo código debe integrarse a través de Pull Requests:

1. Empujar cambios a la rama remota:
   ```bash
   git push origin feature/nombre-feature
   ```

2. Crear Pull Request en GitHub con:
   - Título descriptivo
   - Descripción detallada
   - Referencia a tickets/issues
   - Asignar revisores

3. Verificar que CI/CD pasa correctamente
   - Tests automáticos
   - Análisis estático
   - Métricas de calidad

4. Incorporar feedback de revisores

5. Una vez aprobado, merge a develop:
   - Preferir "Squash and Merge" para features completas
   - Preferir "Merge Commit" para integraciones complejas

#### 3.3.2 Revisión de Código
Criterios para aprobar PR:
- Código sigue estándares definidos en `CODING_STANDARDS.md`
- Tests adecuados incluidos
- Documentación actualizada
- Performance considerada
- Sin vulnerabilidades de seguridad
- Compatibilidad con arquitectura general

### 3.4 Releases

1. Crear rama de release:
   ```bash
   git checkout develop
   git checkout -b release/v1.0.0
   ```

2. Preparación final:
   - Actualizar versión en archivos
   - Generar CHANGELOG
   - Realizar tests exhaustivos

3. Merge a main y tagging:
   ```bash
   git checkout main
   git merge release/v1.0.0
   git tag -a v1.0.0 -m "Release v1.0.0"
   git push origin main --tags
   ```

4. Merge back a develop:
   ```bash
   git checkout develop
   git merge main
   git push origin develop
   ```

---

## 4. GESTIÓN DE DEPENDENCIAS

### 4.1 Dependencias de Código
- Todas las dependencias externas deben ser aprobadas
- Dependencias fijadas a versiones específicas
- Documentación de propósito y justificación
- Auditoría regular de dependencias

### 4.2 Submodulos y Vendoring
- Preferir vendoring para dependencias críticas
- Uso juicioso de submodulos cuando sea necesario
- Documentación clara de todos los submodulos

---

## 5. BACKUP Y SEGURIDAD

### 5.1 Estrategia de Backup
- Repositorio clonado en múltiples ubicaciones
- Backups automatizados diarios
- Retention policy: 30 días mínimo

### 5.2 Seguridad
- Acceso por SSH con 2FA
- Firmas GPG para commits críticos
- No secrets en repositorio (usar env vars o gestores de secretos)
- Scans de seguridad regulares

---

## 6. HERRAMIENTAS Y CONFIGURACIÓN

### 6.1 Configuración Global Recomendada
```bash
git config --global user.name "Nombre Apellido"
git config --global user.email "email@example.com"
git config --global core.editor "code --wait"
git config --global pull.rebase true
```

### 6.2 Git Hooks
- **pre-commit**: Linting y formateo automático
- **pre-push**: Tests unitarios rápidos
- **commit-msg**: Validación de formato de mensaje

### 6.3 GitHub Actions / CI
- Ejecución automática en cada PR
- Tests por lenguaje
- Análisis estático
- Métricas de calidad
- Verificación de estilo

---

## 7. RESOLUCIÓN DE CONFLICTOS

### 7.1 Prevención de Conflictos
- Integración frecuente desde develop
- Comunicación clara sobre áreas de trabajo
- Modularización efectiva del código

### 7.2 Manejo de Conflictos
```bash
# Actualizar rama base
git checkout develop
git pull origin develop

# Rebase de feature branch
git checkout feature/mi-feature
git rebase develop

# Resolver conflictos para cada commit
# Editar archivos para resolver conflictos
git add [archivos resueltos]
git rebase --continue

# En caso de problemas complejos
git rebase --abort
```

---

## 8. MÉTRICAS Y MONITOREO

### 8.1 Métricas de Repositorio
- Frecuencia de commits
- Tiempo de integración
- Tasa de builds exitosos
- Velocidad de resolución de PR
- Cobertura de código

### 8.2 Dashboards
- GitHub Insights
- PR metrics
- CI/CD performance

---

## 9. ONBOARDING DE DESARROLLADORES

### 9.1 Primeros Pasos para Nuevos Desarrolladores
1. Clonar repositorio:
   ```bash
   git clone https://github.com/[ORGANIZATION]/synth-u.git
   cd synth-u
   ```

2. Configurar entorno:
   ```bash
   # Instalar hooks
   cp -r .github/hooks/* .git/hooks/
   chmod +x .git/hooks/*
   
   # Verificar configuración
   git config --list
   ```

3. Familiarizarse con el workflow:
   - Leer este documento completamente
   - Revisar PRs recientes para entender el proceso
   - Crear una rama experimental para pruebas

---

**NOTA**: Este workflow es obligatorio para todos los desarrolladores del proyecto. Cualquier excepción debe ser explícitamente aprobada.
