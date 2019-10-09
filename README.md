# Projeto de Prolog - Paradigmas de Programação (MC346)

[![Documentação: GitHub Pages](https://img.shields.io/badge/Documenta%C3%A7%C3%A3o-GitHub%20Pages-blueviolet "Documentação")](https://tiagodepalves.github.io/MC346-prolog/)

## Alunos

- [Tiago de Paula Alves](mailto:t187679@dac.unicamp.br "t187679@dac.unicamp.br") (187679)
- [João Pedro de Amorim](mailto:j176131@dac.unicamp.br "j176131@dac.unicamp.br") (176131)

## Enunciado do Projeto

Leia um arquivo pelo stdin no formato

```prolog
[ circ(N,X1,Y1,R) , quad(M, X2, Y2,L), ... ].
```

onde

- X1, Y1 são as coordenadas do centro de um circulo de raio R, chamando N
- X2, Y2 são as coordenadas do centro de um quadrado com lado L, chamado M. O quadrado é paralelo aos eixos.
- e assim por diante.

dado

```prolog
quad(a,1,2,10)
```

então os vertices do quadrado estão em

```raw
-4,  7
 6,  7
 6, -3
-4, -3
```

Note que o arquivo de entrada contem apenas um termo em Prolog (terminado por .) que é uma lista de estruturas.

Imprima no stdout:

- a quantidade de pares de figuras geométricas que tem intercessão não vazia (tanto círculos como quadrados)
- e nas linha seguintes os nomes dos pares das figuras com interseção nao vazia, um por linha.

Cuidado que se a figura A tem intercessao com o B isso só conta uma vez (ja que possivelmente o programa pode considerar que intercessão de A com B é diferente de B com A).

Também não se preocupe com regiões onde mais de 2 figuras fazer intercessão. Se 3 circulos (A e B e C) tem uma intercessão, o seu programa precisa contar apenas 3 (A com B), (A com C) e (B com C). O seu programa rodará da seguinte forma

```bash
swipl -q -f seu-prog.pr -t topo  < arqtestes.in
```

O que é importante é que o predicado topo (sem nenhum argumento) será o único predicado a ser chamado.

### Dica

o central é definir quando 2 figuras tem alguma interceção, e há 3 definições: quadrado-quadrado, circulo-circulo e quadrado-circulo. Uma vez que voce definiu quando 2 figuras tem intercessão, obtenha todas as soluções possíveis para a intercessão.

A intercessão de 2 circulos ou de 2 quadrados são mais faceis. A de um quadrado com um circulo um pouco mais trabalhosa.
