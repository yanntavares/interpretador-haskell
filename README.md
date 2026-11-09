# Interpretador em Haskell

Projeto da 1ª VA da disciplina de Paradigmas de Programação (2026.2).
O projeto estende o interpretador-base fornecido pelo professor (`interpretador-21-09-2026-2.hs`). Todo o código está em `interpretador.hs`.

**Equipe 5:** Yann Tavares de Santana, Tomaz Bruno Aureliano de Santanna Vieira, Lucas Gabriel da Silva Andrade e João Vítor de Souza Santos.

## 1. Como instalar e executar

### 1.1 Instalar o GHC, GHCi e Cabal

Use o [GHCup](https://www.haskell.org/ghcup/), que instala o GHC, o GHCi e o Cabal.

### 1.2 Executar apenas com o GHCi (sem instalar nada além do GHC)

Este é o comando recomendado no enunciado e funciona se as bibliotecas `silently`, `containers` e `HUnit` estiverem disponíveis para o GHC:

```
ghci --package silently --package containers --package HUnit interpretador.hs
```

### 1.3 Instalar as dependências e usar o VS Code (opcional)

O interpretador usa bibliotecas que não vêm com o GHC (`silently` e `HUnit`). Para instalá-las e evitar erros nos `import` do editor:

```
cabal install --lib --package-env . silently HUnit containers
```

> Este comando cria um arquivo oculto `.ghc.environment.*` na máquina. Ele já está no `.gitignore`.

Depois, basta abrir o interpretador:

```
ghci interpretador.hs
```

### 1.4 Rodar os testes

Dentro do GHCi, com o arquivo carregado:

```
ghci> main
```

`main` executa a suíte HUnit (`suite`) e encerra o GHCi ao final. Para rodar sem sair do GHCi, use:

```
ghci> runTestTT suite
```

## 2. O que foi implementado

Todo código novo foi escrito com TDD (primeiro o teste, depois a implementação), com um commit por conjunto de testes que passa.

| Item | O que foi feito | Commit |
|------|-----------------|--------|
| 1.a / 1.b | Tipo `Float` (`TFloat`, `FLit`, `VFloat`). Em operações com `Int` e `Float`, o `Int` é promovido a `Float` (função auxiliar `convert`). `Div` usa `div` para dois inteiros e `/` quando há `Float`. | `af0ab2e` |
| 1.c | Arrays unidimensionais de inteiros, booleanos e floats (`TArray Type`, `ArrayLit`, `VArray`), com impressão no formato `[1, 2, 3]`. | `868f6f7` |
| 2.a | Teste da ordem de avaliação dos argumentos da esquerda para a direita (`testOrdemArgumentos`). Ver observação na seção 5. | `a3951cf` |
| 2.b | Funções passam a enxergar as variáveis globais. Parâmetros e variáveis locais têm prioridade sobre globais de mesmo nome. | `14a43cd` |
| 3.a | Verificação de tipo na declaração (`compativel`) e na atribuição. | `b397415` |
| 3.b | Conversão implícita na atribuição, como em C: `int` recebendo `float` guarda só a parte inteira; `float` recebendo `int` guarda o valor como float (`converterPara`). | `7d3f833` |
| 4 | Limitação identificada: o interpretador não tinha texto. Foi adicionado o tipo `String` (declaração, atribuição, impressão e concatenação com `Add`). | `96bf2e2` |

## 3. Mudanças nos tipos das funções existentes

Nenhuma função existente teve a assinatura alterada. `evalExpr`, `evalBinOp`, `execStmt`, `execBlock` e `showValue` mantêm os tipos do código-base.

Foram apenas **acrescentados construtores** aos tipos de dados existentes, o que é necessário para suportar os novos tipos:

- `Type`: `TFloat`, `TArray Type`, `TString`
- `Expr`: `FLit Float`, `ArrayLit [Expr]`, `SLit String`
- `Value`: `VFloat Float`, `VArray [Value]`, `VString String`

Também foram criadas funções auxiliares novas: `convert`, `compativel`, `mesmoTipo`, `converterPara` e, nos testes, `assertErro`.

## 4. Sintaxes e recursos não vistos na disciplina

- **`Map.union`**: une dois mapas e, em caso de chave repetida, mantém o valor do **primeiro** mapa. É assim que os parâmetros têm prioridade sobre as globais.
- **`intercalate`** (`Data.List`): junta uma lista de textos com um separador, usado para imprimir arrays como `[1, 2, 3]`.
- **`all`**: verifica se todos os elementos de uma lista satisfazem uma condição, usado para checar os elementos de um array.
- **`try`, `evaluate` e `ErrorCall`** (`Control.Exception`, que já vem no GHC): usados em `assertErro` para testar que um programa falha com `error`. `evaluate` força a execução, `try` captura a exceção e a anotação `:: IO (Either ErrorCall Value)` indica o tipo do resultado.
- 
## 5. Limitações conhecidas

- **Item 2.a:** o `mapM` em IO já avalia os argumentos da esquerda para a direita, então o teste passou sem mudança de código. Porém, o ambiente alterado durante a avaliação de um argumento (por exemplo, uma global atualizada) **não é repassado** ao argumento seguinte, porque `evalExpr` devolve apenas o valor e não o ambiente.
- **Declaração:** `Decl` não faz conversão implícita. `float y = 2` gera erro (coberto por `testDeclIntEmFloat`). A conversão implícita vale apenas na atribuição, conforme o item 3.b.
- **Atribuição em arrays:** qualquer array é aceito no lugar de outro array, sem checar o tipo dos elementos.

## 6. Uso de IA

- A IA foi usada para: tirar dúvidas sobre sintaxe e conceitos de Haskell.
- A IA **não** foi usada para gerar código do interpretador.
- Material de apresentação: os [slides](https://github.com/user-attachments/files/34578879/Interpretador.em.Haskell.Projeto.da.1.VA.pdf) foram montados com apoio do Claude a partir do código e dos commits do repositório; o conteúdo foi revisado pela equipe.
