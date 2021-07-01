# Substitution Rule

## Var

1. $\frac{s,\; \mathcal{Var}\; n\;\dashv\; (n,\; Let Binding\; v)\; \in\; s,\;\; v\; =(s\; //\; n)\Rightarrow\; v'}
{\mathcal{Var}\; n\; -[s]\rightarrow\; v'}$

2. $\frac{s,\; \mathcal{Var}\; n\; \dashv\; (n,\; other Binding\; v)\; \in\; s,\;\; v\; =(s\; //\; n)\Rightarrow\; v'}
{v'}$

## Const

- same as Var

## Op

- $\frac{s,\; Op\; op\; \dashv}{Op\; op}$

## Chain

- $\frac{s,\; a\; \bowtie\; b\; \dashv\; a\; --subst\; s-\rightarrow\; a',\; b\; --subst \; s-\rightarrow\; b'}{a'\; \bowtie\; b'}$

## App

## Lam

- $\frac{s,\; \lambda\, x.\; e\; \dashv\; e\; -- subst\; [s//x]-\rightarrow\; e',\; e\; \neq\; e' }
{\lambda\, x.\; e\; -[s]\rightarrow\; \lambda\, x.\; e'}$

- $\frac{s,\; \lambda\, x.\; e\; \dashv\; e\; -- subst\; [s//x]-\rightarrow\; e',\; e\; =\; e' }
{\lambda\, x.\; e}$

## Hole

- $\frac{s,\; \_\; \dashv}{ \_ }$

## Quant

## Subst
