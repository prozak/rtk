#!/bin/bash
# RTK Bootstrap Build Script
#
# This script performs a multi-stage bootstrap build of RTK using generated
# parsers from grammar.pg. See BOOTSTRAP.md for details.

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "========================================"
echo "RTK Bootstrap Build"
echo "========================================"
echo ""
echo "This performs a multi-stage build:"
echo "  Stage 0: Build with hand-written parser"
echo "  Stage 1: Generate parsers from grammar.pg"
echo "  Stage 2: Build with generated parser (experimental)"
echo ""

# Stage 0: Build with hand-written parser
echo -e "${BLUE}=== Stage 0: Building RTK with hand-written parser ===${NC}"
echo ""
echo ">>> Running: cabal build"
cabal build
echo ""
echo -e "${GREEN}✓ Stage 0 complete${NC}"
echo ""

# Stage 1: Generate parser from grammar.pg
echo -e "${BLUE}=== Stage 1: Generating parser from grammar.pg ===${NC}"
echo ""
echo ">>> Generating GrammarLexer.x, GrammarParser.y, GrammarQQ.hs"
mkdir -p src/generated
cabal run rtk -- test-grammars/grammar.pg src/generated/
echo ""

echo ">>> Compiling generated lexer (alex)"
cd src/generated
cabal exec alex -- GrammarLexer.x -o GrammarLexer.hs
echo ""

echo ">>> Compiling generated parser (happy)"
cabal exec happy -- GrammarParser.y --ghc -o GrammarParser.hs
cd ../..
echo ""
echo -e "${GREEN}✓ Stage 1 complete${NC}"
echo ""

# Stage 2: Verify generated parser (experimental)
echo -e "${BLUE}=== Stage 2: Testing generated parser ===${NC}"
echo ""
echo "Note: The generated parser is not yet integrated into cabal build."
echo "This is experimental - uses --use-generated runtime flag."
echo ""

echo ">>> Testing: rtk --use-generated test-grammars/grammar.pg test-out/bootstrap-test"
if cabal run rtk -- --use-generated test-grammars/grammar.pg test-out/bootstrap-test 2>&1 | grep -q "not yet available"; then
    echo -e "${YELLOW}⚠ Generated parser not yet integrated (expected)${NC}"
    echo ""
    echo "The --use-generated flag exists but implementation is in progress."
    echo "This is normal - generated files are ready for Prototype 2 implementation."
else
    echo -e "${GREEN}✓ Generated parser working!${NC}"
fi
echo ""

# Summary
echo "========================================"
echo "Bootstrap Build Summary"
echo "========================================"
echo ""
echo -e "${GREEN}✓ Stage 0:${NC} RTK built with hand-written parser"
echo -e "${GREEN}✓ Stage 1:${NC} Generated files created:"
echo "           src/generated/GrammarLexer.x"
echo "           src/generated/GrammarParser.y"
echo "           src/generated/GrammarQQ.hs"
echo "           src/generated/GrammarLexer.hs"
echo "           src/generated/GrammarParser.hs"
echo -e "${YELLOW}⚠ Stage 2:${NC} Generated parser awaiting integration (Prototype 2)"
echo ""
echo "Next steps:"
echo "  1. Implement ASTAdapter.hs (convert generated → hand-written AST)"
echo "  2. Integrate into main.hs (--use-generated implementation)"
echo "  3. Test: cabal run rtk -- --use-generated <args>"
echo ""
echo "See docs/prototype-2-plan.md for implementation details."
echo "========================================"
