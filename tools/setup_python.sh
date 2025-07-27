# Script para instalar y configurar Python
#!/bin/bash
set -e

echo "=== Configurando Python ==="

# Verificar si Python ya está instalado
if command -v python3 &> /dev/null; then
    echo "Python ya está instalado. Versión:"
    python3 --version
else
    echo "Error: Python 3 no está instalado."
    echo "Por favor, instale Python 3 antes de continuar."
    exit 1
fi

# Verificar si pip está instalado
if command -v pip3 &> /dev/null; then
    echo "pip ya está instalado. Versión:"
    pip3 --version
else
    echo "Instalando pip..."
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        sudo apt-get update
        sudo apt-get install -y python3-pip
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        brew install python3
    else
        echo "Sistema operativo no soportado para instalación automática."
        echo "Por favor, instale pip manualmente."
        exit 1
    fi
fi

# Crear entorno virtual si no existe
if [ ! -d "venv" ]; then
    echo "Creando entorno virtual..."
    python3 -m venv venv
    echo "Entorno virtual creado."
else
    echo "El entorno virtual ya existe."
fi

# Activar entorno virtual
echo "Activando entorno virtual..."
source venv/bin/activate

# Actualizar pip
echo "Actualizando pip..."
pip install --upgrade pip

# Instalar dependencias
echo "Instalando dependencias de Python..."
pip install numpy scipy scikit-learn torch tensorflow transformers pandas matplotlib seaborn pyzmq grpcio protobuf neo4j redis pytest black flake8 mypy jupyter jupyterlab

# Configurar estructura de proyecto Python
echo "Configurando estructura de proyecto Python..."

# Verificar si estamos en la raíz del proyecto
if [ -d "src/python" ]; then
    cd src/python
    
    # Crear estructura básica si no existe
    if [ ! -f "setup.py" ]; then
        echo "Inicializando proyecto Python..."
        
        # Crear setup.py
        cat > setup.py << EOF
from setuptools import setup, find_packages

setup(
    name="synth_u_ai",
    version="0.1.0",
    packages=find_packages(),
    install_requires=[
        "numpy",
        "scipy",
        "scikit-learn",
        "torch",
        "tensorflow",
        "transformers",
        "pandas",
        "matplotlib",
        "seaborn",
        "pyzmq",
        "grpcio",
        "protobuf",
        "neo4j",
        "redis",
    ],
    author="Synth-U Team",
    author_email="example@example.com",
    description="AI and ML components for Synth-U OS",
)
EOF
        
        # Crear requirements.txt
        cat > requirements.txt << EOF
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
EOF

        # Crear estructura de módulos
        mkdir -p synth_u_ai
        mkdir -p synth_u_ai/neural_networks
        mkdir -p synth_u_ai/computer_vision
        mkdir -p synth_u_ai/natural_language
        mkdir -p synth_u_ai/integration
        mkdir -p tests
        
        # Crear __init__.py principales
        touch synth_u_ai/__init__.py
        touch synth_u_ai/neural_networks/__init__.py
        touch synth_u_ai/computer_vision/__init__.py
        touch synth_u_ai/natural_language/__init__.py
        touch synth_u_ai/integration/__init__.py
        
        # Crear módulo principal
        cat > synth_u_ai/__init__.py << EOF
"""
Synth-U AI Module
=================

This package contains the AI and ML components for the Synth-U
neuroplastic operating system.
"""

__version__ = "0.1.0"
EOF

        # Crear módulo base de redes neuronales
        cat > synth_u_ai/neural_networks/base.py << EOF
"""
Base Neural Network Components
=============================

Core neural network infrastructure for Synth-U.
"""

import torch
import torch.nn as nn
import numpy as np
from typing import Dict, List, Tuple, Optional, Union


class SynthUNeuralBase(nn.Module):
    """Base class for all Synth-U neural networks."""
    
    def __init__(self, input_dim: int, hidden_dim: int, output_dim: int):
        """
        Initialize the neural network.
        
        Args:
            input_dim: Dimension of input
            hidden_dim: Dimension of hidden layer
            output_dim: Dimension of output
        """
        super().__init__()
        self.input_dim = input_dim
        self.hidden_dim = hidden_dim
        self.output_dim = output_dim
        
        # Basic network structure
        self.network = nn.Sequential(
            nn.Linear(input_dim, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, output_dim)
        )
        
        self.activation_history = []
        self.learning_rate = 0.001
    
    def forward(self, x: torch.Tensor) -> torch.Tensor:
        """
        Forward pass through the network.
        
        Args:
            x: Input tensor
            
        Returns:
            Output tensor
        """
        activations = self.network(x)
        
        # Store activation history for analysis
        if self.training:
            self.activation_history.append(activations.detach().mean().item())
            
            # Limit history length
            if len(self.activation_history) > 100:
                self.activation_history = self.activation_history[-100:]
        
        return activations
    
    def get_activation_stats(self) -> Dict[str, float]:
        """
        Get statistics about network activations.
        
        Returns:
            Dictionary with activation statistics
        """
        if not self.activation_history:
            return {"mean": 0.0, "std": 0.0, "min": 0.0, "max": 0.0}
        
        history = np.array(self.activation_history)
        return {
            "mean": float(np.mean(history)),
            "std": float(np.std(history)),
            "min": float(np.min(history)),
            "max": float(np.max(history))
        }
    
    def adapt_learning_rate(self, performance: float) -> None:
        """
        Adapt learning rate based on performance.
        
        Args:
            performance: Performance metric between 0 and 1
        """
        # Simple adaptive learning rate
        if performance > 0.8:
            self.learning_rate *= 0.9  # Reduce if performing well
        else:
            self.learning_rate *= 1.1  # Increase if performing poorly
            
        # Ensure learning rate stays in reasonable bounds
        self.learning_rate = max(0.0001, min(0.01, self.learning_rate))
EOF

        # Crear test básico
        mkdir -p tests
        cat > tests/test_neural_base.py << EOF
"""
Tests for the base neural network components
"""

import pytest
import torch
from synth_u_ai.neural_networks.base import SynthUNeuralBase


def test_neural_base_initialization():
    """Test that the base neural network initializes correctly."""
    net = SynthUNeuralBase(10, 20, 5)
    
    assert net.input_dim == 10
    assert net.hidden_dim == 20
    assert net.output_dim == 5
    assert len(list(net.parameters())) > 0
    assert net.activation_history == []


def test_neural_base_forward():
    """Test forward pass through the network."""
    net = SynthUNeuralBase(10, 20, 5)
    
    # Create random input
    x = torch.rand(32, 10)  # Batch of 32 inputs
    
    # Forward pass
    output = net(x)
    
    assert output.shape == (32, 5)
    
    # When in training mode, should record activations
    net.train()
    _ = net(x)
    assert len(net.activation_history) == 1
    
    # Generate more activations and check limit
    for _ in range(105):
        _ = net(x)
    
    assert len(net.activation_history) == 100  # Should be limited to 100


def test_get_activation_stats():
    """Test retrieval of activation statistics."""
    net = SynthUNeuralBase(10, 20, 5)
    
    # Empty history should return zeros
    stats = net.get_activation_stats()
    assert stats["mean"] == 0.0
    
    # Generate some activations
    net.train()
    x = torch.rand(32, 10)
    for _ in range(10):
        _ = net(x)
    
    # Should have stats now
    stats = net.get_activation_stats()
    assert "mean" in stats
    assert "std" in stats
    assert "min" in stats
    assert "max" in stats
    assert stats["min"] <= stats["mean"] <= stats["max"]


def test_adapt_learning_rate():
    """Test adaptation of learning rate."""
    net = SynthUNeuralBase(10, 20, 5)
    
    initial_lr = net.learning_rate
    
    # High performance should decrease learning rate
    net.adapt_learning_rate(0.9)
    assert net.learning_rate < initial_lr
    
    # Low performance should increase learning rate
    net.adapt_learning_rate(0.5)
    assert net.learning_rate > 0.9 * initial_lr
EOF

        echo "Proyecto Python inicializado correctamente."
    else
        echo "El proyecto Python ya está inicializado."
    fi
else
    echo "Error: No se encontró el directorio src/python."
    echo "Asegúrese de ejecutar este script desde la raíz del proyecto Synth-U."
    exit 1
fi

# Desactivar entorno virtual
deactivate

echo "Configuración de Python completada."
