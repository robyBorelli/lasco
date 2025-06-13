# LASCO – LAS → ASP Compiler

**LASCO** (Learning from Answer Sets Compiler) is a tool that translates LAS tasks into ASP encodings.  
It supports optional solving via Clingo or DLV and provides multiple encoding strategies.

---

## 🚀 Download binaries

Precompiled binaries are available in the [GitHub Releases](https://github.com/robyBorelli/lasco/releases/latest):

- [lasco-linux](https://github.com/robyBorelli/lasco/releases/latest/download/lasco-linux)
- [lasco-macos](https://github.com/robyBorelli/lasco/releases/latest/download/lasco-macos)
- [lasco-windows.exe](https://github.com/robyBorelli/lasco/releases/latest/download/lasco-windows.exe)

---

## 🔨 Build from source

To build LASCO from source, you need:

- [GHC](https://www.haskell.org/ghc/)
- [Cabal](https://www.haskell.org/cabal/)
- [alex](https://hackage.haskell.org/package/alex)
- [happy](https://hackage.haskell.org/package/happy)

1. Clone the repository:

```bash
git clone https://github.com/robyBorelli/lasco.git
cd lasco
```

2. Build using Cabal:

```bash
cd code
cabal build
```

3. The binary will be located inside `dist-newstyle/build/.../lasco`.

4. Test the compiled binary:
   
```bash
cabal run lasco -- --help
```

---


## ⚙️ Command-line options

```text
Usage: lasco [options]

Available options:
  -i,--input IFILE         Specifies the input file containing the learning task
                           to solve.
  -o,--output OFILE        Specifies the output file which will contain the
                           output encoding.
  -e,--encoder ENCODER     Specifies the encoding type:
                             - 'exponential'
                             - 'disjunctive' (default)
  --solve SOLVER           Solves the encoding using:
                             - 'clingo'
                             - 'dlv'
  --solve-mode MODE        Solving mode:
                             - 'first', 'all', or 'optimum'
                             - an integer (e.g. 3) for number of solutions
  -v,--verbose             Prints intermediate steps and debug information.
  --no-comments            Disables comments in the generated encoding.
  --show-hypos [i1,i2,...] Prints hypotheses of specified indexes and exits.
  -h,--help                Show this help text.
```

### Examples

```bash
# Basic usage
lasco --input examples/ex1_normal_rules.las

# Solves at optimum using clingo
lasco --input examples/ex1_normal_rules.las --solve clingo --solve-mode optimum

# Shows the first two rules in the hypothesis space
lasco --input examples/ex1_normal_rules.las --show-hypos [1,2]

# Basic usage with exponential encoder
lasco --input examples/ex1_normal_rules.las --encoder exponential
```

---


## 📥 Input Syntax

Input files must use **Answer Set Programming (ASP)** syntax with some extensions to define the hypothesis and examples.


### 1. Background Knowledge

The **background** is a set of standard **ASP rules**, including:

- **Facts** (e.g., `edge(a,b).`)
- **Normal rules** (e.g., `reachable(X,Y) :- edge(X,Y).`)
- **Constraints** (e.g., `:- not reachable(a,c).`)
- **Arithmetic** (e.g., `X = Y + 1.`)
- **Comparison predicates** (e.g., `X < Y.`)

**Syntax rules:**

- **Variables** start with an uppercase letter: `X`, `Node`, `Time`.
- **Constants** start with a lowercase letter: `a`, `node1`, `zero`.
- Every word containing `LASCO` or `lasco` as a substring, is a reserved word.

### 2. Hypotheses

A rule in the hypothesis space is defined as:

```prolog
weight ~ rule.
```
where:
- `weight` is a positive integer.
- `rule` is an ASP rule.

---

### 3. Examples

A standard example is defined as:
```prolog
#type(inclusion_set, exclusion_set).
```
where:
- `type` is either  `pos` (positive example) or  `neg` (negative example).
- `inclusion_set` is a set of ground atoms. (e.g., `{p, q, reachable(a), edge(a,b)}`)
- `exclusion_set` is a set of ground atoms.

A context example is defined as:
```prolog
#type(inclusion_set, exclusion_set, {context}).
```
where:
- `context` is an ASP program.


### 4. Full example

```prolog
p :- not q.

1 ~ q.
2 ~ q :- not p.

#pos({p},{q}).
#neg({q},{p}).
```

---



