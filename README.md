# AutoCAD LISP

Coleção de rotinas AutoLISP para AutoCAD.

## Estrutura

```
autocad-lisp/
├── carregar.lsp      # carrega todas as rotinas da pasta src/
├── src/              # rotinas (.lsp), uma por arquivo
└── templates/        # modelo para criar novas rotinas
```

## Rotinas

### Lotes e quadras

| Comando | Arquivo | Descrição |
|---------|---------|-----------|
| `1NUMERARLOTES` | [1NUMERARLOTES.lsp](src/1NUMERARLOTES.lsp) | Numera os lotes de uma quadra pela distância ao longo do perímetro, a partir do vértice de origem e do sentido clicados. O lote 1 usa o vértice inicial do lote 2; os demais, o próprio vértice final. |
| `2NUMERARLOTES` | [2NUMERARLOTES.lsp](src/2NUMERARLOTES.lsp) | Mesma regra do `1NUMERARLOTES`, corrigindo lotes que atravessam o ponto de origem da quadra. |
| `SQ` | [NUMERAR_LOTES_ESCOLHER_ PRIMEIRO_NUMERO.lsp](src/NUMERAR_LOTES_ESCOLHER_%20PRIMEIRO_NUMERO.lsp) | Numera os lotes da quadra em sequência (1, 2, 3…) a partir de um número inicial informado. |
| `SELED` / `SELEDLIMPAR` / `SELEDCAM` | [SELED_4.lsp](src/SELED_4.lsp) | Selagem por edificação: acrescenta `-E1`, `-E2`… ao texto do lote conforme as edificações dentro dele (maior área = E1). `SELEDLIMPAR` desfaz; `SELEDCAM` lista as camadas reconhecidas. |

### Edificações

| Comando | Arquivo | Descrição |
|---------|---------|-----------|
| `4BEIRAL` | [4beiral.lsp](src/4beiral.lsp) | Gera o beiral para dentro: offset interno de 0,6 na edificação, sem recuo nos lados colados na divisa do lote. |
| `3` | [3ret.lsp](src/3ret.lsp) | Desenha um retângulo a 90° a partir de dois pontos da base e um terceiro que define a altura. |

### Limpeza e conferência de geometria

| Comando | Arquivo | Descrição |
|---------|---------|-----------|
| `SY2` | [SyncAndClean_sy2.lsp](src/SyncAndClean_sy2.lsp) | Sincroniza e limpa muitas LWPOLYLINEs fechadas: apaga polígonos degenerados, zera a elevação, cria/ajusta vértices nas divisas vizinhas e remove vértices desnecessários. |
| `COLINEAR` / `LIMPACOLINEAR` | [colinear.lsp](src/colinear.lsp) | Marca com círculo os vértices colineares das polilinhas, sem marcar os que coincidem com vértice de outra polilinha. `LIMPACOLINEAR` apaga as marcações. |
| `ESCANERGRIP` | [ESCANERGRIP.LSP](src/ESCANERGRIP.LSP) | Procura vértices quase encostados em outra polilinha (micro-erros até a tolerância informada, padrão 5 cm), marca e dá zoom em cada um para corrigir. |

### Textos, pontos e importação

| Comando | Arquivo | Descrição |
|---------|---------|-----------|
| `RA` | [rota_auto.lsp](src/rota_auto.lsp) | Gira os textos selecionados para ficarem alinhados à polilinha mais próxima, sem ficar de cabeça para baixo. |
| `TOTULARSOLO` | [totularsolo2.lsp](src/totularsolo2.lsp) | Rotula pontos (POINT) com número sequencial por camada, cota Z formatada e nome da camada. |
| `5COLA` | [5COLA.lsp](src/5COLA.lsp) | Lê `Desktop\dxf\Exportado(DXF).dxf` e recria as polilinhas no desenho atual, na coordenada original. |
| `SOMACOMP` | [SOMACOMP.lsp](src/SOMACOMP.lsp) | Soma o comprimento de linhas, polilinhas, arcos, círculos, elipses e splines. |

## Instalação

1. Clone o repositório:
   ```
   git clone https://github.com/edugcs-gif/autocad-lisp.git
   ```
2. No AutoCAD, abra **OPTIONS → Files** e adicione a pasta do repositório em:
   - **Support File Search Path**
   - **Trusted Locations** (evita o aviso de segurança ao carregar)
3. Carregue as rotinas:
   - **Uma vez:** digite `APPLOAD` e escolha `carregar.lsp`.
   - **Sempre que abrir um desenho:** em `APPLOAD`, clique em **Startup Suite → Contents** e adicione `carregar.lsp`.

Para carregar só uma rotina, use `APPLOAD` direto no arquivo dentro de `src/`.

## Criando uma nova rotina

1. Copie `templates/MODELO.lsp` para `src/NOMEDOCOMANDO.lsp`.
2. Troque `NOME` pelo nome do comando e escreva o código.
3. Adicione uma linha na tabela de rotinas acima.
4. Faça o commit:
   ```
   git add .
   git commit -m "Adiciona rotina NOMEDOCOMANDO"
   git push
   ```

## Convenções

- Rotinas novas: de preferência um comando por arquivo, com o nome do arquivo igual ao do comando.
- Variáveis locais sempre declaradas depois da `/` no `defun`.
- Funções auxiliares com prefixo do arquivo (ex.: `somacomp:comprimento`) para não colidir com outras rotinas.
- Arquivos salvos em UTF-8. Se os acentos das mensagens aparecerem trocados no AutoCAD, confira se a variável `LISPSYS` está em `1` (AutoCAD 2021 ou mais novo).
- Arquivos compilados (`.fas`, `.vlx`) vão na pasta `build/`, que não é versionada. Para distribuir, use as *Releases* do GitHub.

## Licença

[MIT](LICENSE)
