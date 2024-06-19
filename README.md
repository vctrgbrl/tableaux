## Trabalho Linguagens de Programação

Victor Sant'Ana

### Descrição
> Um tableau é uma árvore com raiz rotulada por uma fórmula na Lógica Clássica Proposicional cujos nós dessa árvore são subfórmulas da raiz e são rotulados como "verdadeiro" ou "falso". A partir da raiz novas fórmulas são criadas de acordo com a aplicação de regras especificadas que geral uma ou duas novas folhas. Trata-se de uma prova por refutação, então supõe-se que a fórmula de entrada é falsa; caso haja contradições (i.e. uma mesma fórmula rotulada como verdadeira e como falsa em nós diferentes de um mesmo ramo) em todos os ramos da árvore, então a fórmula da raiz é uma tautologia (i.e. a partir de toda tentativa possível de torná-la falsa obteve-se alguma contradição). Caso todas as regras possíveis tenham sido aplicadas e haja um ou mais ramos sem contradições, então a fórmula da raiz é falsificável (um ramo é um caminho da folha até a raiz).


### Executando

A utilização do programa espera um arquivo que servirá como entrada

```sh
./Main "teste_0.txt"
```
O programa responderá se a expressão é verdade ou não e gerará um arquivo de saída de nome **output.txt** contendo
a árvore de dedução gerada. 

### Backus-Naur Form do arquivo de entrada

\<expressoes> ::= \<expressao> | \<expressao> **'\n'** \<expressoes>

\<expressao> ::= **(** \<variavel> **)** | **(** \<expressao> \<infix_operando> \<expressao> **)** | \<prefix_operando> \<expressao>

\<infix_operando> ::= **→** | **∧** | **∨**

\<prefix_operando> ::= **¬**

\<variavel> ::= \<char> | \<char> \<variavel>

\<char> ::= // qualquer coisa diferente de **¬ → ∧ ∨ ( )**

#### Exemplos: 
```
(p∨(q∧r))→((p∨q)∧(p∨r))
```
```
(a)
(a→b)
(b)
```
A última expressão, sendo as expressões dividias pela quebra de linha, será interpretada como conclusão, sendo assim, negada.

### Compilando

Para compilar é necessário ter o GHC instalado e executar o comando

```sh
ghc Main.hs
```

### Arquivo de saída

O arquivo de saída contém uma representação da árvore de refutação aonde 0 representa um ramo a "esquerda" e 1 um ramo a "direita"