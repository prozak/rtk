# RTK Build Optimizations

This directory contains scripts and tools to optimize RTK build performance.

## Quick Start

### Option 1: Use Optimized Makefile (Recommended)

```bash
# Use the optimized makefile
make -f makefile.optimized test-java

# Or replace your existing makefile
cp makefile.optimized makefile
make test-java
```

### Option 2: Compare Performance

```bash
# Run both original and optimized builds
./compare-builds.sh
```

## Optimizations Included

### 1. Parallel Builds (2-4x speedup)
- Enabled with `MAKEFLAGS += -j4`
- Compiles multiple modules simultaneously
- Scales with CPU cores

### 2. Smart Caching (5-10x for unchanged grammars)
- Tracks grammar file changes via MD5 checksums
- Skips regeneration when grammar hasn't changed
- Stores cache in `.build-cache/`

### 3. Compiler Optimization Flags

**Development mode (default):**
```make
GHC_FLAGS = -O0 -j4  # Fast compilation, no optimization
```

**Production mode:**
```make
make BUILD_MODE=prod test-java
GHC_FLAGS = -O2 -j4  # Optimized runtime performance
```

### 4. Happy/Alex Optimization Flags

```make
HAPPY_FLAGS = --strict --array --ghc
ALEX_FLAGS = --ghc
```

## Benchmarking

### Run Build Performance Benchmark

```bash
./benchmark-java-grammar.sh
```

Measures:
- RTK grammar generation time
- Happy parser compilation time
- Alex lexer compilation time
- GHC compilation time
- Runtime parsing performance

### Profile RTK for Bottlenecks

```bash
./profile-rtk.sh
```

Generates:
- `rtk.prof` - Time and allocation profile
- `rtk.hp` - Heap profile
- Identifies hot functions for optimization

## Expected Performance Improvements

| Scenario | Original | Optimized | Speedup |
|----------|----------|-----------|---------|
| Clean build | ~10s | ~4s | 2.5x |
| Incremental (no changes) | ~10s | ~1s | 10x |
| Incremental (grammar change) | ~10s | ~5s | 2x |

## Build Modes

### Development Mode (Fast Compilation)
```bash
make BUILD_MODE=dev test-java
```
- GHC -O0 (no optimizations)
- Fast iteration
- Slower runtime

### Production Mode (Fast Runtime)
```bash
make BUILD_MODE=prod test-java
```
- GHC -O2 (full optimizations)
- Slower compilation
- Fast runtime

## Troubleshooting

### Cache Issues

If you see stale results:
```bash
# Clear build cache
rm -rf .build-cache
make clean
```

### Profiling Fails

Ensure profiling libraries are installed:
```bash
# For GHC via ghcup
ghcup install ghc --set 9.x.x

# For system packages (Ubuntu/Debian)
sudo apt-get install ghc-prof
```

## Advanced Optimizations

See `docs/java-grammar-build-performance.md` for:
- Grammar conflict reduction strategies
- Module splitting techniques
- Lexer/parser runtime optimizations
- Grammar modularization approaches

## Monitoring Performance

Add to your CI pipeline:
```bash
# Run benchmark and save results
./benchmark-java-grammar.sh > current-benchmark.txt

# Compare against baseline
diff baseline-benchmark.txt current-benchmark.txt
```

## Next Steps

1. ✅ Apply quick wins (use optimized makefile)
2. Profile RTK to find hotspots
3. Reduce grammar conflicts further (currently 149)
4. Implement module splitting for large grammars
5. Add continuous performance monitoring
