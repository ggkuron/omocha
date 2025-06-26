# CLAUDE.md

日本語で回答してください。
This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Astrata is a 3D card-based battle game written in Haskell using functional reactive programming (FRP). The game simulates tactical card battles on a grid-based battlefield between two sides (Own vs Rival) with origin cards and battler cards.

## Build and Development Commands

```bash
# Build the project
cabal build

# Run the game
cabal run astrata

# Run tests
cabal test

# Build and run tests with verbose output
cabal test --test-show-details=direct

# Clean build artifacts
cabal clean

# Install dependencies
cabal build --dependencies-only
```

## Architecture Overview

### Core Technologies
- **Graphics.GPipe**: Type-safe 3D graphics pipeline
- **FRP.Elerea**: Functional reactive programming for game state
- **GLFW**: Window management and input handling
- **JuicyPixels**: Image loading and processing
- **Aeson**: JSON parsing for game data files

### Key Architectural Patterns

**Graphics Pipeline**: Multi-pass rendering with specialized shaders:
- `boardShader`: Textured 3D models with lighting
- `monoShader`/`phongShader`: Solid-color models with Phong lighting  
- `textShader`: 2D text overlay rendering
- `gridShader`: Grid line rendering

**FRP Game Loop**: Signal-based state management using FRP.Elerea with:
- Event-driven input processing with edge detection
- Time-based animations with delta time calculations
- Reactive camera system following player movement

**Monadic Context**: GameCtx monad wrapping graphics context with resource management using ResourceT for safe cleanup.

### Module Organization

**Core Game (`src/Astrata/`)**:
- `Game.hs`: Main game loop, FRP network setup, rendering coordination
- `Context.hs`: Monadic context wrapper around GPipe's ContextT
- `UserInput.hs`: Input handling with keyboard mapping and edge detection

**Graphics and Rendering**:
- `Shader.hs`: Shader definitions, lighting calculations, projection matrices
- `Scene.hs`: Scene graph data structures (SceneObject, Mesh)
- `Uniform.hs`: Uniform buffer management for GPU data
- `Bitmap.hs`, `Font.hs`, `Text.hs`: Text and image rendering

**Game World and Data**:
- `Map.hs`: Game world parsing, 3D object placement from JSON
- `MapFile.hs`: Data types for battlefield, cards, battlers, game state
- `Shape.hs`: Primitive 3D geometry generation
- `Mesh.hs`: 3D mesh data structures

**Asset Loading**:
- `Gltf.hs`: 3D model loading from GLTF/GLB files
- `Resource.hs`: Resource management and cleanup

### Key Data Structures

**Game State**: `BattleField` containing `Own` and `Rival` fields with origin and satellite card slots (frontL, frontR, backL, backR).

**Graphics State**: `ApplicationUniforms` for global/per-object uniform buffers, `TextureStorage` for cached font textures, and scene objects mapped to card positions.

**FRP Signals**: Input signals with change detection, position signals with physics integration, camera signals with smooth following, and time signals for frame-rate independence.

## Project Structure

- `src/Astrata/`: Main game modules
- `test/`: Hspec test suite with QuickCheck properties
- `static/`: Game assets (images, maps, 3D models)
- `static/maps/`: JSON game data files for battlefield layouts
- `GPipe-Core/`: Custom GPipe graphics library fork
- `JuicyPixels-util/`: Custom image utilities

## Testing

The project uses Hspec with QuickCheck for property-based testing. Test files mirror the source structure under `test/Astrata/`. The test suite includes:
- Property tests for mathematical functions (transpose, matrix operations)
- Unit tests for game logic components
- Spec tests for map file parsing and validation

## Development Notes

- Uses GHC 9.4.8 with GHC2021 language extensions
- Strict data types and no implicit prelude by default
- Record dot syntax and overloaded strings enabled
- Wall warnings enabled with specific suppressions for name shadowing
