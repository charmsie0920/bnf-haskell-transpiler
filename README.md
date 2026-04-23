# BNF to Haskell Transpiler

A high-performance transpiler implemented in **Haskell** designed to convert Context-Free Grammars defined in **Backus-Naur Form (BNF)** into executable Haskell source code. The system architecturally transforms grammar specifications into a custom **Abstract Data Type (ADT)** before generating corresponding type definitions and recursive descent parsers.

## Key Features

* **Abstract Syntax Tree (AST) Generation**: Efficiently parses raw BNF strings into a structured Intermediary Representation (IR) using robust Algebraic Data Types.
* **Haskell Code Synthesis**: Automatically generates formatted Haskell source code, including `data` and `newtype` declarations with proper indentation and numbered constructors.
* **Advanced Grammar Modifiers**: Full support for regex-inspired modifiers including `tok` (whitespace handling), `*` (many), `+` (some), and `?` (optional).
* **Parameterised Rules**: Implements higher-order grammar rules with unique parameters, enabling the generation of generic Haskell functions with polymorphic type variables.
* **Iterative BNF Validation**: A multi-step validation engine that detects and reports **Undefined Nonterminals**, **Duplicate Rules**, and **Left Recursion** to ensure the stability of the generated parsers.

## Technical Architecture

This project leverages fundamental and advanced functional programming patterns to ensure type safety and modularity:

* **Parser Combinators**: The parsing engine is constructed using `Functor`, `Applicative`, and `Monad` typeclasses, allowing for the composition of complex logic from small, testable primitives.
* **Declarative Logic**: Emphasizes pure functions, immutable data structures, and function composition to maintain a clear separation between parsing and code generation.
* **Full-Stack WebSocket Integration**: Features a **Haskell backend server** that communicates in real-time with a **TypeScript/Next.js frontend** via HTML-based WebSockets.

[Image of an abstract syntax tree diagram for a programming language]

## Getting Started

### Prerequisites
* [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
* [Node.js & npm](https://nodejs.org/)

### Build and Test
To compile the transpiler and run the automated test suite:
```bash
stack build
stack test