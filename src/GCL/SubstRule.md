<!-- markdownlint-disable MD028 MD024 -->
# Substitution semantics

## 1. Notations

- $e_1 \xrightarrow[s]{}\; e_2 \equiv \mathsf{Subst}\; e_1\; s\; e_2$

- $e1 \xrightarrow{s} e2$

  - $\mathsf{subst}\; s\; e1 \equiv e2$
  - $s\; ::\; \mathsf{Map}\; \mathsf{Name}\; \mathsf{Expr}$

- $e1 \xRightarrow{s} e2$

  - $\mathsf{subst}\; s\; e1 \equiv e2$
  - $e_2 \neq e_{21}\; \xrightarrow[s_{e_2}]{}\; e_{22}$
  - $s :: \mathsf{Map}\; \mathsf{Name}\; \mathsf{Expr}$

- $\displaystyle a\; \rightarrow_\beta b$

## 2. Var

- ### subst

  - $\displaystyle \frac{s,\; \mathsf{Var}\; n\;\dashv\; (n,\; \mathsf{Let Binding}\; v)\; \in\; s,\;\; v\; \xrightarrow{\,s\; //\; n\, }\; v'}
{\mathsf{Var}\; n\; \xrightarrow[s]{}\; v'}$

  - $\displaystyle \frac{s,\; \mathsf{Var}\; n\; \dashv\; (n,\; \mathsf{Other Binding}\; v)\; \in\; s,\;\; v\; \xrightarrow{\, s\; //\; n\, }\; v'}
{v'}$

  - $\displaystyle \frac{s,\; \mathsf{Var}\; n\; \dashv}{\mathsf{Var}\; n}$

- ### beta reduction

  - $\displaystyle \frac{\mathsf{Var}\; n\; \dashv}{\mathsf{Var}\; n}$

## 3. Const

- Same as Var

## 4. Op

- ### subst

  - $\displaystyle \frac{s,\; Op\; op\; \dashv}{Op\; op}$

- ### beta reduction

  - $\displaystyle \frac{\mathsf{Op}\; op\; \dashv}{\mathsf{Op}\; op}$

## 5. Chain

- ### subst

  - $\displaystyle \frac{s,\; a\; \bowtie\; b\; \dashv\; a\; \xrightarrow{s}\; a',\; b\; \xrightarrow{s}\; b'}{a'\; \bowtie\; b'}$

- ### beta reduction

  - $\displaystyle \frac{a\;\bowtie\;b\; \dashv\;
  a\; \rightarrow_\beta\; a',\;
  b\; \rightarrow_\beta\; b'}
  {a'\; \bowtie\; b'}$

## 6. App

<!-- - $\displaystyle \frac{s,\; \mathsf{App\; (\lambda\, x.\;e)\; a}\; \dashv\;
  (\lambda\, x.\;e)\; \xrightarrow{s}\; (\lambda\, x.\;e),\;
  a\; \xrightarrow{s}\; a,\;
  e\; \xRightarrow{\{(x,\; a)\}}\; e'}
  {\mathsf{App}\; (\lambda\, x.\; e)\; a\;
  \xrightarrow[\{(x,\; \mathsf{BetaBinding}\; a)\}]{}\; e'}$

- $\displaystyle \frac{s,\; \mathsf{App}\; (\lambda\, x.\;e)\; a\; \dashv\; (\lambda\, x.\; e)\; \xrightarrow{s}\; (\lambda\, x.\; e'),\;
a\; \xrightarrow{s}\; a',\;
e'\; \xRightarrow{\{(x, a')\}}\; e''}
{(\mathsf{App}\;(\lambda\, x,\; e)\; a\;
  \xrightarrow[s]{}\; \mathsf{App}\; (\lambda\, x.\; e')\; a')\;
\xrightarrow[\{(x,\; \mathsf{BetaBinding}\; a'\}]{}\; e''}$

- $\displaystyle \frac{s,\; \mathsf{App}\; (b\; \xrightarrow[sb]{}\; \lambda\,x.\; e)\; a\; \dashv\;
  (b\; \xrightarrow[sb]{}\; \lambda\, x.\; e)\; \xrightarrow{s} (b\; \xrightarrow[sb]{}\; \lambda\, x.\; e),\;
  a\; \xrightarrow{s}\; a,\;
  e\; \xRightarrow{\{(x,\; a)\}}\; e'}
  {\mathsf{App}\; (b\; \xrightarrow[sb]{}\; \lambda\, x.\; e)\; a\; \xrightarrow[\{(x,\; \mathsf{BetaBinding}\; a\}]{}\; e'}$ -->

<!-- - $\displaystyle \frac{s,\; \mathsf{App}\; (b\; \xrightarrow[sb]{}\; \lambda\,x.\; e)\; a\; \dashv\;
  (b\; \xrightarrow[sb]{}\; \lambda\, x.\; e)\; \xrightarrow{s} (b\; \xrightarrow[sb]{}\; \lambda\, x.\; e'),\;
  a\; \xrightarrow{s}\; a',\;
  e'\; \xRightarrow{\{(x,\; a')\}}\; e''}
  {(\mathsf{App}\; (b\; \xrightarrow[sb]{}\; \lambda\, x.\; e)\;a\;
  \xrightarrow[s]{}\; \mathsf{App}\; (b\; \xrightarrow[sb]{}\; \lambda\, x.\; e')\; a')\; \xrightarrow[\{(x,\; \mathsf{BetaBinding}\; a')\}]{}\; e''}$ -->

- ### subst
  
  - $\displaystyle \frac{s,\; \mathsf{App\; f\; a}\; \dashv\;
  f\; \xrightarrow{s}\; f,\; a\; \xrightarrow{s}\; a}
  {\mathsf{App}\; f\; a}$

  - $\displaystyle \frac{s,\; \mathsf{App\; (Op\; op)\;} a\; \dashv\;
  a\; \xrightarrow{s}\; a'}
  {\mathsf{App\; (Op\; op)\; } a'}$

  - $\displaystyle \frac{s,\; \mathsf{App}\; f\; a\; \dashv\;
  \forall\; (n,\;v)\; \in\; s,\; \exists\; v',\; v \equiv (\mathsf{LetBinding}\; v'),\;
  f\; \xrightarrow{s}\; f',\;
  a\; \xrightarrow{s}\; a'}
  {\mathsf{App}\; f'\; a'}$

  - $\displaystyle \frac{s,\; \mathsf{App}\; f\; a\; \dashv\;
  f\; \xrightarrow{s}\; f',\;
  a\; \xrightarrow{s}\; a',\;}
  {\mathsf{App}\; f\; a\; \xrightarrow[s]{}\; \mathsf{App}\; f'\; a'}$

- ### beta reduction

  - $\displaystyle \frac{\mathsf{App}\; (\lambda\, x.\; e)\; a\; \dashv\;
  a\; \rightarrow_\beta\; a',\;
  e\; \xrightarrow{\{(x,\; \mathsf{BetaBinding}\; a')\}}\; e' }
  {\mathsf{App}\; (\lambda\, x.\; e)\; a'\; \xrightarrow[\{(x,\; \mathsf{BetaBinding}\; a')\}]{}\; e'}$

  - $\displaystyle \frac{\mathsf{App}\; f\; a\; \dashv\;
  f\; \rightarrow_\beta\; f',\;
  a\; \rightarrow_\beta\; a'}
  {\mathsf{App}\; f'\; a'}$

## 7. Lam

- ### subst

  - $\displaystyle \frac{s,\; \lambda\, x.\; e\; \dashv\; e\; \xrightarrow{s\; //\; x}\; e',\; e\; \neq\; e' }
{\lambda\, x.\; e\; \xrightarrow[s]{}\; \lambda\, x.\; e'}$

  - $\displaystyle \frac{s,\; \lambda\, x.\; e\; \dashv\; e\; \xrightarrow{s\; //\; x}\; e',\; e\; =\; e' }
{\lambda\, x.\; e}$

- ### beta reduction

  - $\displaystyle \frac{\lambda\, x.\; e\; \dashv\;
  e\; \rightarrow_\beta\; e'}
  {\lambda\, x.\; e'}$

## 8. Hole

- ### subst

  - $\displaystyle \frac{s,\; \_\; \dashv}{ \_ }$

- ### beta reduction

  - $\displaystyle \frac{\_\; \dashv}{\_}$

## 9. Quant

- ### subst

  - $\displaystyle \frac{s,\; \langle op\; args : rng : e\rangle\; \dashv\;
op\; \xrightarrow{s\; //\; args}\; op',\; rng\; \xrightarrow{s\; //\; args}\; rng',\; e\; \xrightarrow{s\; //\; args}\; e',\;}
{\langle op'\; args : rng' : e' \rangle}$

- ### beta reduction

  - $\displaystyle \frac{\langle op\; args : rng : e\rangle\; \dashv\;
  op\; \rightarrow_\beta\; op',\;
  rng\; \rightarrow_\beta\; rng',\;
  e\; \rightarrow_\beta\; e'}
  {\langle op'\; args : rng' : e'\rangle}$

## 10. Subst

<!-- - $\displaystyle \frac{s,\; \mathsf{App}\; a_1\; a_2\; \xrightarrow[\{(\_,\; \mathsf{BetaBinding}\; \_)\}]{}\; b\; \dashv\;
\mathsf{App}\; a_1 \; a_2\; \xrightarrow{s}\; a,\;}{}$ -->

- ### subst

  - $\displaystyle \frac{s,\; a\; \xrightarrow[s_1]{}\; b\; \dashv\;
  b\; \xrightarrow{s}\; b}
  {a\; \xrightarrow[s_1]{}\; b}$

  - $\displaystyle \frac{s,\; a\; \xrightarrow[s_1]{}\; b\; \dashv\;
  b\; \xrightarrow{s}\; c}
  {(a\; \xrightarrow[s_1]{}\; b)\; \xrightarrow[s]{}\; c}$

- ### beta reduction **(NotSure)**

  - $\displaystyle \frac{a\; \xrightarrow[s]{}\; \mathsf{App}\; (\lambda\, x.\; e)\; b\; \dashv\;
  a\; \rightarrow_\beta\; a',\;
  e\; \xrightarrow{\{(x,\; \mathsf{BetaBinding}\; b)\}}\; e',\;
  b\; \rightarrow_\beta\; b'}
  {(a'\; \xrightarrow[s]{}\; \mathsf{App}\; (\lambda\, x.\; e)\; b')\; \xrightarrow[\{(x,\; \mathsf{BetaBinding}\; b')\}]{}\; e' }$

  - $\displaystyle \frac{a\; \xrightarrow[s]{}\; b\; \dashv\;
  a\; \rightarrow_\beta\; a',\;
  b\; \rightarrow_\beta\; b'}
  {a'\; \rightarrow_\beta\; b'}$
  