<!-- markdownlint-disable MD028 MD024 -->
## Judgements 

There are 5 kinds of judgments 

- Reduction `reduce`: $\displaystyle a\; \rightarrow_\beta b$
- Substitution `subst`: 
  - $a \xrightarrow{[y/x]} b$ with the mapping of subsitution 
  - $a \rightarrow b$ with mapping of subsitution omitted 
- Renaming `rename`: $\displaystyle a\; \rightarrow_\alpha b$
- Definition: 
  - $a\;\textsf{is defined as}\;b$
  - $a\;\textsf{is not defined}$

## Syntax for Expressions (object language)

Mostly the same as in Haskell, with a few exceptions, we write:

* $f\;x$ instead of `App f x`
* $\lambda n . x$ instead of `Lam n x`
* $(before \Rightarrow after)$ instead of `Expand before after`

## Rules for `reduce`

-  $\displaystyle \frac
      { a \; x \rightarrow_\beta b 
          \qquad 
        (\lambda n . body) \; x \rightarrow_\beta c 
      }{
        (a \Rightarrow \lambda n . body) \; x \rightarrow_\beta b \Rightarrow  c 
      }$ [reduce-App-Expand-Lam]

- $\displaystyle \frac
    { body \xrightarrow{[x/n]} b 
    }{
      (\lambda n . body) \; x \rightarrow_\beta b
    }$ [reduce-App-Lam] 

- $\displaystyle \frac
    { 
    }{
      other \; constructs \rightarrow_\beta other \; constructs
    }$ [reduce-Others] 

## Rules for `subst`

- `Lit` 
  - $\displaystyle \frac
        { 
        }{
          Lit \; a \rightarrow Lit \; a'
        }$ [subst-Lit]

- `Var`
  
  - $\displaystyle \frac
        { 
          a \rightarrow_\beta a' 
        }{
          Var \; x \xrightarrow{[a/x]} a'
        }$ [subst-Var-substituted]

  - $\displaystyle \frac
        { 
          x\;\textsf{is defined as}\;a
            \qquad
          a \rightarrow a'
        }{
          Var x \rightarrow (Var x \Rightarrow a')
        }$ [subst-Var-defined]

  - $\displaystyle \frac
        { 
          x\;\textsf{is not defined}
        }{
          Var x \rightarrow Var x
        }$ [subst-Var-not-defined]


- `Const`: same as rules for `Var`

- `Op` 
  - $\displaystyle \frac
        { 
        }{
          Op \; a \rightarrow Op \; a'
        }$ [subst-Op]

- `Chain` 
  - $\displaystyle \frac
        { 
          a \rightarrow a'
            \qquad
          b \rightarrow b'
        }{
          Chain \; a \; op \; b \rightarrow Chain \; a' \; op \; b'
        }$ [subst-Chain]

- `App` 
  - $\displaystyle \frac
        { 
          f \rightarrow f'
            \qquad
          x \rightarrow x'
            \qquad
          f \; x \rightarrow_\beta y
        }{
          f \; x \rightarrow y
        }$ [subst-App]

- `Lam` 
  - $\displaystyle \frac
        { 
          n \rightarrow_\alpha n'
            \qquad
          body \rightarrow body'
        }{
          \lambda n . body \rightarrow \lambda n' . body'
        }$ [subst-Lam]

- `Quant` 
  - $\displaystyle \frac
        { 
          ns \rightarrow_\alpha ns'
            \qquad
          a \rightarrow a'
            \qquad
          b \rightarrow b'
        }{
          Quant \; op \; ns \; a \; b \rightarrow Quant \; op \; ns' \; a \; b'
        }$ [subst-Quant]

- `Expand` 
  - $\displaystyle \frac
        { a \rightarrow a'
            \qquad
          b \rightarrow b'
        }{
          (a \Rightarrow b) \rightarrow (a' \Rightarrow b')
        }$ [subst-Expand]

- `ArrIdx` 
  - $\displaystyle \frac
        { a \rightarrow a'
            \qquad
          b \rightarrow b'
        }{
          ArrIdx \; a' \; b'
        }$ [subst-ArrIdx]


- `ArrUpd` 
  - $\displaystyle \frac
        { a \rightarrow a'
            \qquad
          b \rightarrow b'
            \qquad
          c \rightarrow c'
        }{
          ArrUpd \; a' \; b' \; c'
        }$ [subst-ArrUpd]
