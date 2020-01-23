# TAPL in Rust

## Introduction

This project contains implementation of languages introduced in [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) in Rust. 

## Contents

* [Chapter 4: Arithmetic Expressions](src/arith.rs)
* [Chapter 7: Untyped Lambda Calculus](src/untyped.rs)

To be added...

## Usage

This project is just for personal study, so user-friendliness is not in consideration. Since lexing and parsing are not the main interests of this book, only terms and evaluation functions for them are implemented, while lexers and parsers are not. This means the only way to construct the AST is to code it by hand. Reading from an external text file is not supported.

Each module (except `util`) in this crate contains a language. Terms are defined as `enum Term`, and the evaluation rules for them are defined as `fn eval(t: &Rc<Term>) -> Rc<Term>`. 

Each module contains a test function which demonstrates the construction of AST of this languages, which could serve as a reference.

## Details

This project implement interpreters that are usually done with pure functional languages, like OCaml or Haskell. The implementation details on how to map these features to Rust are discussed in this section.

### Memory Management

Typically, functional languages support garbage collection to prevent programmers from caring about memory management issues. However, this is not the case for Rust, which enforces strict disciplines on the lifetime of objects. `Rc`, the reference counting facility provided by the standard library is used throughout the project. Parent AST nodes hold `Rc<Term>`s to children, and evaluation function `eval` receives `&Rc<Term>` as parameter and returns `Rc` as result. Forwarding existing `Term`s and creating new instances of `Term`s all work without pain. An abbreviated `rc` is provided in `util` to save typing the longer `Rc::new`.

### Pattern Matching

It is fortunate that Rust supports algebraic data type `enum` and `match` expression, which are commonly seen in functional languages. This makes pattern matching, a commonly seen feature in language interpreters, convenient. When matching `enum`s, their associated values can be extracted to perform further evaluation. However, as `Rc` is used as pointer to child nodes, nested `enum` matching is not possible, so nested `match` expressions for associated values are used. Besides, predicate on associated values (like `when` in OCaml) is not supported in Rust, so `if` expressions should be used. This could lead to code duplication on the `else` branch. To solve this, closures that encapsulate expressions can be used to alleviate repetition.
