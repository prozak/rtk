# Phase 1 Hybrid Testing - Blocker

## Problem

Cannot combine manual lexer with RTK-generated parser due to token naming incompatibility.

## Details

### Token Name Mismatch

The RTK-generated parser expects quasi-quotation tokens with specific names:
```haskell
Tk__qq_Java
Tk__qq_AdditiveOp
Tk__qq_AnnotationList
Tk__qq_Expression
... etc
```

The manual lexer exports tokens with different names:
```haskell
Tk__qq_Java0
Tk__qq_AdditiveOp220
Tk__qq_AnnotationList40
Tk__qq_Expression188
... etc
```

### Root Cause

The manual lexer (`JavaLexer-manual.x`) was created independently as a workaround for RTK's comment handling issues. It has its own token naming scheme that doesn't match RTK's automatically-generated token names.

### Why This Blocks Phase 1

Phase 1 goal: Test manual lexer + generated parser to isolate parser issues from lexer bugs.

The incompatibility means we cannot combine:
- Manual lexer (works, handles comments correctly)
- Generated parser (unknown quality, what we want to test)

## Alternatives

### Option A: Fix Token Names in Manual Lexer
**Effort:** High
**Approach:** Manually update all token names in `JavaLexer-manual.x` to match RTK numbering scheme

**Pros:**
- Would enable true hybrid testing
- Clean separation of lexer vs parser issues

**Cons:**
- Requires understanding RTK's token numbering algorithm
- Error-prone (100+ token names to update)
- Manual lexer would need ongoing maintenance to stay in sync

### Option B: Test Comment-Free Files
**Effort:** Medium
**Approach:** Pre-process Apache Commons files to remove all comments, then test with full RTK lexer+parser

**Pros:**
- Bypasses comment tokenization issue
- Tests parser directly
- Uses existing RTK infrastructure

**Cons:**
- Doesn't test real-world files
- Comment removal might change semantics
- Still affected by char literal bug

### Option C: Fix RTK Lexer Generation (Phase 2 Early)
**Effort:** High
**Approach:** Fix RTK to generate proper comment handling, then test on real files

**Pros:**
- Solves root cause
- Tests complete system

**Cons:**
- Skips Phase 1 entirely
- Doesn't isolate parser vs lexer issues

### Option D: Manual Sample Testing
**Effort:** Low
**Approach:** Manually select 10-20 Apache Commons files, remove comments, test with RTK lexer+parser

**Pros:**
- Quick validation
- Identifies parser issues in real-world code
- Lower effort than Options A or B

**Cons:**
- Not comprehensive
- Manual work required

## Recommendation

**Option D - Manual Sample Testing**

Rationale:
1. Quickest path to get parser quality data
2. Can be done in parallel with Phase 2 (fixing RTK lexer)
3. Will reveal if parser has fundamental issues
4. If parser works on samples, validates that lexer is the only blocker

Process:
1. Select 20 diverse Apache Commons files
2. Create comment-free versions (`*.nocomments.java`)
3. Test with RTK lexer+parser
4. Analyze results
5. Document parser pass rate estimate

## Status

- ❌ Hybrid testing blocked by token incompatibility
- ⏳ Awaiting decision on alternative approach
