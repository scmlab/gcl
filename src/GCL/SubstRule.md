<!-- markdownlint-disable MD028 MD024 -->
# Substitution semantics

## 1. Notations

- $e_1\; [s]\; e_2 \equiv \mathsf{Subst}\; e_1\; s\; e_2$

- $e_1 \xrightarrow{s} e_2$

  - $\mathsf{subst}\; s\; e_1 \equiv e_2$
  - $s\; ::\; \mathsf{Map}\; \mathsf{Name}\; \mathsf{Bindings}$

- $\displaystyle a\; \rightarrow_\beta b$

- $\displaystyle AssignBinding\; s$
  - $\forall\; (x,\; v) \in s,\;\exists\; v',\; v=\mathsf{Assign Binding}\; v'$

- $\displaystyle LetBinding\; s$
  - $\forall\; (x,\; v) \in s,\;\exists\; v',\; v=\mathsf{Let Binding}\; v'$

## 2. Var

- ### subst

  - $\displaystyle \frac{(n,\; \mathsf{Let Binding}\; v)\; \in\; s}
{\mathsf{Var}\; n\; \xrightarrow{s}\; \mathsf{Var}\; n\; [s]\; v}$

  - $\displaystyle \frac{(n,\; \mathsf{Other Binding}\; v)\; \in\; s,\;\; v\; \xrightarrow{\, s\; //\; n\, }\; v'}
{\mathsf{Var}\; n\; \xrightarrow{s}\; v'}$

  - $\displaystyle \frac{(n,\; \_)\; \notin\; s}
  {\mathsf{Var}\; n\; \xrightarrow{s}\; \mathsf{Var}\; n}$

- ### beta reduction

  - $\displaystyle \frac{}{\mathsf{Var}\; n\;\rightarrow_\beta\mathsf{Var}\; n}$

## 3. Const

- Same as Var

## 4. Op

- ### subst

  - $\displaystyle \frac{}
  {\mathsf{Op}\; op\;\xrightarrow{s}\;\mathsf{Op}\; op}$

- ### beta reduction

  - $\displaystyle \frac{}
  {\mathsf{Op}\; op\; \rightarrow_\beta\; \mathsf{Op}\; op}$

## 5. Chain

- ### subst

  - $\displaystyle \frac{a\; \xrightarrow{s}\; a',\; b\; \xrightarrow{s}\; b'}{a\; \bowtie\; b\; \xrightarrow{s}\; a'\; \bowtie\; b'}$

- ### beta reduction

  - $\displaystyle \frac{a\; \rightarrow_\beta\; a',\;
  b\; \rightarrow_\beta\; b'}
  {a\;\bowtie\;b\; \rightarrow_\beta\; a'\; \bowtie\; b'}$

## 6. App

- ### subst
  
  - $\displaystyle \frac{
    a\; \xrightarrow{s}\; a'
  } {
    (Op\; op)\; a\; \xrightarrow{s}\;
    (Op\; op)\; a'
  }$

  - $\displaystyle \frac{
    a\; \xrightarrow{s}\; a',\;
    b\; \xrightarrow{s}\; b'
  } {
    ((\mathsf{Op}\; op)\; a)\;
    b\; \xrightarrow{s}\;
    ((\mathsf{Op}\; op)\; a')\;
    b'
  }$

  - $\displaystyle \frac{
    f\; \xrightarrow{s}\; f',\;
    a\; \xrightarrow{s}\; a',\;
    f\; =\; f',\;
    a\; =\; a'
  }
  {
    f\; a\; \xrightarrow{s}\;
    f\; a
  }$

  - $\displaystyle \frac{
    AssignBinding\; s,\;
    f\; \xrightarrow{s}\; f',\;
    a\; \xrightarrow{s}\; a',\;
  }
  {
    f\; a\; \xrightarrow{s}\;
    f\; a\; [s]\; f'\; a'
  }$

  - $\displaystyle \frac{
    f\; \xrightarrow{s}\; f',\;
    a\; \xrightarrow{s}\; a'
  }
  {
    f\; a\; \xrightarrow{s}\;
    f'\; a'
  }$

- ### beta reduction

  - $\displaystyle \frac{
    a\; \rightarrow_\beta\; a',\;
    e\; \xrightarrow{\{(x,\; \mathsf{BetaBinding}\; a')\}}\; e',\;
    e'\; \rightarrow_\beta\; e'' }
  {
    (\lambda\, x.\; e)\; a\;
    \rightarrow_\beta e''
  }$

  - $\displaystyle \frac{
      a\; \rightarrow_\beta a',\;
      e\; \xrightarrow{\{(x,\; \mathsf{BetaBinding\; a'})\}}\; e',\;
      e'\; \rightarrow_\beta\; e''
    } {
      (f\; [s_1]\; (\lambda\, x.\; e))\; a\;
      \rightarrow_\beta
      (f\; [s_1]\; (\lambda\, x.\; e))\; a\; [s_1]\; e''
    }$

  - $\displaystyle \frac{
    f\; \rightarrow_\beta\; \lambda\, x.\; e,\;
    a\; \rightarrow_\beta\; a',\;
    (\lambda\, x.\; e)\; a'\; \rightarrow_\beta e'
  }
  {
    f\; a\;
    \rightarrow_\beta e'}$

  - $\displaystyle \frac{
    f\; \rightarrow_\beta f'\;[s]\;\lambda\, x.\; e,\;
    a\; \rightarrow_\beta\; a',\;
    (f'\; [s]\; \lambda\, x.\; e)\; a'\; \rightarrow_\beta e'
  }
  {
    f\; a\;
    \rightarrow_\beta e'}$

  - $\displaystyle \frac{
    f\; \rightarrow_\beta\; f',\;
    a\; \rightarrow_\beta\; a'}
  {
    f\; a\;
    \rightarrow_\beta f'\; a'}$

## 7. Lam

- ### subst

  - $\displaystyle \frac{
    e\; \xrightarrow{s\; //\; x}\; e'
  }
  {
    \lambda\, x.\; e\; \xrightarrow{s}\;
    \lambda\, x.\; e'
  }$

- ### beta reduction

  - $\displaystyle \frac{
    e\; \rightarrow_\beta\; e'}
  {
    \lambda\, x.\; e\; \rightarrow_\beta
    \lambda\, x.\; e'
  }$

## 8. Hole

- ### subst

  - $\displaystyle \frac{}{ \_\; \xrightarrow{s}\;\_ }$

- ### beta reduction

  - $\displaystyle \frac{}{\_\; \rightarrow_\beta\; \_}$

## 9. Quant

- ### subst

  - $\displaystyle \frac{
    op\; \xrightarrow{s\; //\; args}\; op',\;
    rng\; \xrightarrow{s\; //\; args}\; rng',\;
    e\; \xrightarrow{s\; //\; args}\; e',\;
  }
  {
    \langle op\; args : rng : e\rangle\;\xrightarrow{s}\;
    \langle op'\; args : rng' : e' \rangle
  }$

- ### beta reduction

  - $\displaystyle \frac{
    op\; \rightarrow_\beta\; op',\;
    rng\; \rightarrow_\beta\; rng',\;
    e\; \rightarrow_\beta\; e'
  } {
    \langle op\; args : rng : e\rangle\;\rightarrow_\beta\;
    \langle op'\; args : rng' : e'\rangle
  }$

## 10. Subst

<!-- - $\displaystyle \frac{s,\; \mathsf{App}\; a_1\; a_2\; \xrightarrow[\{(\_,\; \mathsf{BetaBinding}\; \_)\}]{}\; b\; \dashv\;
\mathsf{App}\; a_1 \; a_2\; \xrightarrow{s}\; a,\;}{}$ -->

- ### subst

  - $\displaystyle \frac{
      LetBinding\; s_1,\;
      \lambda\, x.\; e\; \xrightarrow{s}\; \lambda\, x.\; e'\;
    } {
      a\; [s_1]\; \lambda\, x.\;e\; \xrightarrow{s}\;
      a\; [s]\; \lambda\, x.\;e'
    }$

  - $\displaystyle \frac{
      \lambda\, x.\; e\; \xrightarrow{s}\; \lambda\, x.\; e'\;
    } {
      a\; [s_1]\; \lambda\, x.\;e\; \xrightarrow{s}\;
      a\; [s_1]\; \lambda\, x.\;e\; [s]\; \lambda\, x.\;e'
    }$

  - $\displaystyle \frac{
      b_1\; \xrightarrow{s}\; b_1',\;
      b_2\; \xrightarrow{s}\; b_2',\;
      b_1 = b_1',\;
      b_2 = b_2'
    }{
      a\;[s_1]\; b1\; b2\; \xrightarrow{s}\;
      a\;[s_1]\; b1\; b2
    }$

  - $\displaystyle \frac{
      AssignBinding\; s,\;
      b_1\; \xrightarrow{s}\; b_1',\;
      b_2\; \xrightarrow{s}\; b_2'
    }{
      a\;[s_1]\; b1\; b2\; \xrightarrow{s}\;
      a\;[s_1]\; b1\; b2\; [s]\; b1'\; b2'
    }$

  - $\displaystyle \frac{
      b_1\; \xrightarrow{s}\; b_1',\;
      b_2\; \xrightarrow{s}\; b_2'
    }{
      a\;[s_1]\; b1\; b2\; \xrightarrow{s}\;
      a\;[s_1]\; b1'\; b2'
    }$

  - $\displaystyle \frac{
    b\; \xrightarrow{s}\; c,\;
    b\; =\; c
  } {
    a\; [s_1]\; b\; \xrightarrow{s}\;
    a\; [s_1]\; b
  }$

  - $\displaystyle \frac{
    AssignBinding\; s,\;
    b\; \xrightarrow{s}\; c
  } {
    a\; [s_1]\; b\; \xrightarrow{s}\;
    a\; [s_1]\; b\; [s]\; c
  }$

  - $\displaystyle \frac{
    b\; \xrightarrow{s}\; c
  } {
    a\; [s_1]\; b\; \xrightarrow{s}\;
    a\; [s_1]\; c
  }$

- ### beta reduction

  - $\displaystyle \frac{
    c\; \rightarrow_\beta\; c',\;
    e\; \xrightarrow{\{(x,\; \mathsf{BetaBinding}\; c')\}}\; e',\;
    e'\; \rightarrow_\beta\; d,\;
  } {
    a\; [s_1]\;
    (b_1\; [s_2]\; \lambda\, x.\; e)\; c
    \rightarrow_\beta
    (a\; [s_1]\; (b_1\; [s_2]\; \lambda\, x.\; e)\; c)\; [\{(x,\; \mathsf{BetaBinding}\; c')\}]\; d
  }$

  - $\displaystyle \frac {
    b\; \rightarrow_\beta\; b'
  } {
    a\; [s]\; b\; \rightarrow_\beta
    a\; [s]\; b'
  }$
  