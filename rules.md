

# Rules

[TOC]



## Lambda Calculus

### Evaluation

* E-App1
    $$
    \tt\frac{t_1\longrightarrow t_2}{t_1t_2\longrightarrow t'_1t_2}
    $$
  
* E-App2
    $$
    \tt\frac{t_2\longrightarrow t'_2}{v_1t_2\longrightarrow v_1t'_2}
    $$

* E-AppAbs
    $$
    \tt(\lambda x:T_{11}.t_{12})v_2\longrightarrow [x\mapsto v_2]t_{12}
    $$

### Typing

* T-Var
    $$
    \tt\frac{x:T\in\Gamma}{\Gamma\vdash x:T}
    $$

* T-Abs
    $$
    \tt\frac{\Gamma,x:T_1\vdash t_2:T_2}{\Gamma\vdash\lambda x\!:\!T_1.t_2:T_1\!\to\! T_2}
    $$
    
* T-App
    $$
    \tt\frac{\Gamma,t_1\vdash T_{11}\!\to\!T_{12}\quad\Gamma\vdash t_2:T_{11}}{\Gamma\vdash t_1\,t_2:T_{12}}
    $$

### Subtyping

* S-Arrow
    $$
    \tt\frac{T_1<:S_1\quad S_2<:T_2}{S_1\!\to\!S_2<:T_1\!\to\!T_2}
    $$

## Conditional

### Evaluation

* E-IfTrue
    $$
    \tt if\;true\;then\;t_2\;else\;t_3\longrightarrow t_2
    $$

* E-IfFalse
    $$
    \tt if\;false\;then\;t_2\;else\;t_3\longrightarrow t_3
    $$

* E-If
    $$
    \tt\frac{t_1\longrightarrow t'_1}{if\;t_1\;then\;t_2\;else\;t_3\longrightarrow if\;t'_1\;then\;t_2\;else\;t_3}
    $$
    
### Typing

* T-True
    $$
    \tt\Gamma\vdash true:Bool
    $$

* T-False
    $$
    \tt\Gamma\vdash false:Bool
    $$

* T-If
    $$
    \tt\frac{\Gamma\vdash t_1:Bool\quad\Gamma\vdash t_2:T\quad\Gamma\vdash t_3:T}{\Gamma\vdash if\;t_1\;then\;t_2\;else\;t_3:T}
    $$

* TA-If
    $$
    \tt\frac{\Gamma\vdash t_1:Bool\quad\Gamma\vdash t_2:T_2\quad\Gamma\vdash t_3:T_3\quad T_2\or T_3=T}{\Gamma\vdash if\;t_1\;then\;t_2\;else\;t_3:T}
    $$

## Numeric

### Evaluation

* E-Succ
    $$
    \tt\frac{t_1\longrightarrow t'_1}{succ\;t_1\longrightarrow succ\;t'_1}
    $$

* E-PredZero
    $$
    \tt pred\;0\longrightarrow0
    $$

* E-PredSucc
    $$
    \tt pred\;(succ\;nv_1)\longrightarrow nv_1
    $$

* E-Pred
    $$
    \tt\frac{t_1\longrightarrow t'_1}{pred\;t_1\longrightarrow pred\;t'_1}
    $$

* E-IsZeroZero
    $$
    \tt iszero\;0\longrightarrow true
    $$

* E-IsZeroSucc
    $$
    \tt iszero\;(succ\;nv_1)\longrightarrow false
    $$

* E-IsZero
    $$
    \tt\frac{t_1\longrightarrow t'_1}{iszero\;t_1\longrightarrow iszero\;t'_1}
    $$

### Typing

* T-Succ
    $$
    \tt\frac{\Gamma\vdash t_1:Nat}{\Gamma\vdash succ\;t_1:Nat}
    $$

* T-Pred
    $$
    \tt\frac{\Gamma\vdash t_1:Nat}{\Gamma\vdash pred\;t_1:Nat}
    $$

* T-IsZero
    $$
    \tt\frac{\Gamma\vdash t_1:Nat}{\Gamma\vdash iszero\;t_1:Bool}
    $$

## Unit

### Typing

* T-Unit
    $$
    \tt\Gamma\vdash unit:Unit
    $$

### Derived Form

$$
\tt t_1;t_2\overset{def}{=}(\lambda x\!:\!Unit.t_2)\,t_1\;\mathrm{where}\;x\notin \mathrm{FV}(t_2)
$$

## Ascription

### Evaluation

* E-Ascribe
    $$
    \tt v_1\;as\;T\longrightarrow v_1
    $$

* E-Ascribe1
    $$
    \tt\frac{t_1\longrightarrow t'_1}{t_1\;as\;T\longrightarrow t'_1\;as\;T}
    $$

### Typing

* T-Ascribe
    $$
    \tt\frac{\Gamma\vdash t_1:T}{\Gamma\vdash t_1\;as\;T:T}
    $$

## Let Binding

### Evaluation

* E-LetV
    $$
    \tt let\;x=v_1\;in\;t_2\longrightarrow[x\mapsto v_1]t_2
    $$

* E-Let
    $$
    \tt\frac{t_1\longrightarrow t'_1}{let\;x=t_1\;in\;t_2\longrightarrow let\;x=\,t'_1\;in\;t_2}
    $$
    

### Typing

* T-Let
    $$
    \tt\frac{\Gamma\vdash t_1:T_1\quad\Gamma,x\!:\!T_1\vdash t_2:T_2}{\Gamma\vdash let\;x=t_1\;in\;t_2:T_2}
    $$

## Tuple

### Evaluation

* E-ProjTuple
    $$
    \tt\{v_i^{i\in1..n}\}.j\longrightarrow v_j
    $$

* E-Proj
    $$
    \tt\frac{t_1\longrightarrow t'_1}{t_1.i\longrightarrow t'_1.i}
    $$

* E-Tuple
    $$
    \tt\frac{t_j\longrightarrow t'_j}{\{v_i^{i\in1..j-1},t_j,t_k^{k\in j+1..n}\}\longrightarrow \{v_i^{i\in 1..j-1},t'_j,t_k^{k\in j+1..n}\}}
    $$

### Typing

* T-Tuple
    $$
    \tt\frac{\forall i\in1..n\ \Gamma\vdash t_i:T_i}{\Gamma\vdash\{t_i^{i\in1..n}\}:\{T_i^{i\in1..n}\}}
    $$

* T-Proj
    $$
    \tt\frac{\Gamma\vdash t_1:\{T_i^{i\in1..n}\}}{\Gamma\vdash t_1.j:T_j}
    $$

## Record

### Evaluation

* E-ProjRcd
    $$
    \tt\{l_i=v_i^{i\in1..n}\}.l_j\longrightarrow v_j
    $$

* E-Proj
    $$
    \tt\frac{t_1\longrightarrow t'_1}{t_1.l\longrightarrow t'_1.l}
    $$

* E-Rcd
    $$
    \tt\frac{t_j\longrightarrow t'_j}{\{l_i=v_i^{i\in1..j-1},l_j=t_j,l_k=t_k^{k\in j+1..n}\}\longrightarrow \{l_i=v_i^{i\in1..j-1},l_j=t'_j,l_k=t_k^{k\in j+1..n}\}}
    $$

### Typing

* T-Rcd
    $$
    \tt\frac{\forall i\in1..n\ \Gamma\vdash t_i:T_i}{\Gamma\vdash\{l_i=t_i^{i\in1..n}\}:\{l_i\!:\!T_i^{i\in1..n}\}}
    $$

* T-Proj
    $$
    \tt\frac{\Gamma\vdash t_1:\{l_i\!:\!T_i^{i\in1..n}\}}{\Gamma\vdash t_1.l_j:T_j}
    $$

### Subtyping

* S-RcdWidth
    $$
    \tt\{l_i\!:\!T_i^{i\in{1..n+k}}\}<:\{l_i\!:\!T_i^{i\in1..n}\}
    $$

* S-RcdDepth
    $$
    \tt\frac{\forall i\ S_i<:T_i}{\{l_i\!:\!S_i^{i\in1..n}\}<:\{l_i\!:\!T_i^{i\in1..n}\}}
    $$

* S-RcdPerm
    $$
    \tt\frac{\{k_j\!:\!S_j^{j\in1..n}\}\textrm{ perm. of }\{l_i\!:\!T_i^{i\in1..n}\}}{\{k_j\!:\!S_j^{j\in1..n}\}<:\{l_i\!:\!T_i^{i\in1..n}\}}
    $$
    
* S-Rcd
  $$
  \tt\frac{\{l_i^{i\in1..n}\}\subseteq\{k_j^{j\in1..m}\}\quad k_j=l_i\implies S_j<:T_i}{\{k_j\!:\!S_j^{j\in1..m}\}<:\{l_i\!:\!T_i^{i\in1..m}\}}
  $$

##  Variant

### Evaluation

* E-CaseVariant
    $$
    \tt case\;(<\!l_j\!=\!v_j\!>as\;T)\;of<\!l_i\!=\!x_i\!>\,\Rightarrow t_i^{i\in1..n}\longrightarrow [x_j\mapsto v_j]t_j
    $$

* E-Case
    $$
    \tt\frac{t_0\longrightarrow t'_0}{case\;t_0\;of<\!l_i\!=\!x_i\!>\,\Rightarrow t_i^{i\in1..n}\longrightarrow case\;t'_0\;of<\!l_i\!=\!x_i\!>\;\Rightarrow t_i^{i\in1..n}}
    $$

* E-Variant
    $$
    \tt\frac{t_i\longrightarrow t'_i}{<\!l_i\!=\!t_i\!>as\;T\longrightarrow\;<\!l_i\!=\!t'_i\!>as\;T}
    $$

### Typing

* T-Variant
    $$
    \tt\frac{\Gamma\vdash t_j:T_j}{\Gamma\vdash\,<\!l_j\!=\!t_j\!>as<\!l_i\!:\!T_i\!>\,:\,<\!l_i\!:\!T_i^{i\in1..n}\!>}
    $$

* T-Case
    $$
    \tt\frac{\Gamma\vdash t_0:\,<\!l_i\!:\!T_i^{i\in1..n}\!>\quad\forall i\in1..n\ \Gamma,x\!:\!T_i\vdash t_i:T}{\Gamma\vdash case\;t_0\;of<\!l_i\!=\!x_i\!>\;\Rightarrow t_i^{i\in1..n}:T}
    $$
    

## General Recursion

### Evaluation

* E-FixBeta
    $$
    \tt fix\;(\lambda x\!:\!T_1.t_2)\longrightarrow[x\mapsto(fix\;(\lambda x\!:\!T_1.t_2))]t_2
    $$

* E-Fix
    $$
    \tt\frac{t_1\longrightarrow t'_1}{fix\;t_1\longrightarrow fix\;t'_1}
    $$

### Typing

* T-Fix
    $$
    \tt\frac{\Gamma\vdash t_1:T_1\!\to\!T_1}{\Gamma\vdash fix\;t_1:T_1}
    $$

### Derived Form

$$
\tt letrec\;x\!:\!T_1=t_1\;in\;t_2\overset{def}{=}let\;x=fix\;(\lambda x\!:\!T_1.t_1)\;in\;t_2
$$

## Subtyping

### Subtyping

* S-Refl
    $$
    \tt S<:S
    $$

* S-Trans
    $$
    \tt\frac{S<:U\quad U<:T}{S<:T}
    $$

* S-Top
    $$
    \tt S<:Top
    $$

### Typing

* T-Sub
    $$
    \tt\frac{\Gamma\vdash t:S\quad S<:T}{\Gamma\vdash t:T}
    $$
    

