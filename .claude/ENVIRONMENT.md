# Claude Code Environment Configuration

**IMPORTANT: Read this file at the start of each session to understand environment limitations!**

## Container Environment: CLI (Ubuntu 24.04 Container)

### ❌ NOT Available in This Environment

**Haskell Toolchain:**
- GHC (Glasgow Haskell Compiler) - NOT INSTALLED
- Cabal - NOT INSTALLED
- Stack - NOT INSTALLED
- Alex, Happy - NOT INSTALLED

**Why:** Network restrictions prevent installing via ghcup or apt-get

**DO NOT ATTEMPT TO:**
- Check if `ghc`, `cabal`, or `stack` are installed
- Try to install Haskell tools via ghcup, apt, or any other method
- Run `make test`, `make test-grammar`, `make test-all-java`, `make test-java-qq` (require Haskell)
- Run any commands that depend on Haskell compilation

### ✅ What IS Available

**Pre-installed Languages:**
- Node.js v22.21.0 (npm 10.9.4)
- Java OpenJDK 21.0.8 (Maven 3.9.11, Gradle 8.14.3)
- Python 3.x (pip3)
- Go
- Rust (rustc, cargo)
- Ruby 3.1/3.2/3.3
- C/C++ (gcc, g++)

**Build Tools:**
- Git (all operations work)
- Make (for non-Haskell targets)
- Standard Unix tools

**What You CAN Do:**
- Read, analyze, and edit all source files
- Git operations (commit, push, branch, merge, etc.)
- Create documentation and configuration files
- Analyze code structure and provide guidance
- Write/modify JavaScript, Java, Python, Go, Rust code
- Design and plan features
- Review code and suggest improvements

### 🧪 Testing Strategy

**For Haskell tests:** Use GitHub Actions CI (`.github/workflows/ci.yml`)
- The CI environment has full network access
- Installs GHC 9.6.4 and Cabal 3.10.2.0 via haskell-actions/setup@v2
- Caches dependencies for faster runs

**Workflow:**
1. Make code changes in CLI environment
2. Commit and push to feature branch
3. Let GitHub Actions run the tests
4. Review CI results for failures

### 📋 Session Start Checklist

When starting a new session, Claude should:
1. ✅ Read this file first to understand limitations
2. ✅ Check git status and recent commits
3. ✅ Focus on tasks that don't require Haskell compilation
4. ❌ Skip checking for Haskell tools
5. ❌ Skip attempting Haskell installation

### 🔧 Configuration Notes

- Repository files persist between sessions
- System packages DO NOT persist - container resets each session
- Network access is heavily restricted (403 on most external URLs)

---

**Last Updated:** 2025-11-01
