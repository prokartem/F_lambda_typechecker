# Type checker for polymorphic lambda calculus

## Syntax

### Types
`T` — type
- Variable `X`
- Arrow `T -> T`
- Universal `Forall X. T`

### Terms
`t` — term 
- Variable `x`
- Abstraction `lambda x : T. t`
- Application `t $ t`
- Type abstraction `Lambda X. t`
- Type application (should be space after `[`!) `t $ [ T ]`

### Additional
- Comments `{- some text -}`
- Term separator (if placed in the end of file, parser raises error) `;`

### Correct input
```
{- self-application -}
lambda x : Forall A. A -> A.
    x $ [ Forall A. A -> A ] $ x;

{- succesor -}
lambda n : Forall X. (X -> X) -> X -> X.
    Lambda X.
        lambda s : X -> X.
            lambda z : X.
                s $ (n $ [ X ] $ s $ z)
```

## Install
```
git clone https://github.com/prokartem/simple_lambda_typechecker.git
cd simple_lambda_typechecker
curl -sSL https://get.haskellstack.org/ | sh -s - -f
stack build
```

## Usage
Available options:
 - Input file `-f`, `--file FILENAME`
 - Read from stdin `--stdin`                     
 - Show this help text `-h`, `--help`

`stack exec -- lambda-calculus-exe ((-f|--file FILENAME) | --stdin)`

## Test
`stack test`

