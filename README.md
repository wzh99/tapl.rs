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

## Details

