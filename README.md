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

| Comando    | Arquivo                               | Descrição                                                                      |
|------------|---------------------------------------|--------------------------------------------------------------------------------|
| `SOMACOMP` | [src/SOMACOMP.lsp](src/SOMACOMP.lsp)  | Soma o comprimento de linhas, polilinhas, arcos, círculos, elipses e splines.  |

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

- Um comando por arquivo, com o nome do arquivo igual ao do comando.
- Variáveis locais sempre declaradas depois da `/` no `defun`.
- Funções auxiliares com prefixo do arquivo (ex.: `somacomp:comprimento`) para não colidir com outras rotinas.
- **Sem acentos dentro dos arquivos `.lsp`**: dependendo da versão do AutoCAD (e da variável `LISPSYS`), arquivos UTF-8 com acentos aparecem com caracteres errados.
- Arquivos compilados (`.fas`, `.vlx`) vão na pasta `build/`, que não é versionada. Para distribuir, use as *Releases* do GitHub.

## Licença

[MIT](LICENSE)
