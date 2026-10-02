# Interpretador Haskell

Este guia rápido explica como preparar o ambiente e rodar o projeto localmente, garantindo que os imports funcionem corretamente tanto no terminal quanto no VS Code.

## 1. Instalando o GHC, GHCi e Cabal
A maneira padrão e recomendada de instalar o ecossistema Haskell é através do [**GHCup**](https://www.haskell.org/ghcup/). 


## 2. Configurando o VSCode
Para ter funções como autocomplete e linter de haskell, baixe as extensões oficiais na IDE.

## 3. Instalando as dependências
O interpretador tem dependências que não são nativas na linguagem, para instalá-las e permitir que o VSCode não indique erro nos imports, use o comando

```
cabal install --lib --package-env . silently HUnit containers
```

> **Observação:** Este comando cria um arquivo de ambiente oculto (`.ghc.environment.*`) na sua máquina. Ele já está no `.gitignore`, não se preocupe.

## 4. Como rodar
Para rodar o interpretador, execute no terminal
```
ghci interpretador.hs
```