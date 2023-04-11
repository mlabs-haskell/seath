# Mini Dev Wiki

- [Mini Dev Wiki](#mini-dev-wiki)
  - [To develop on-chain](#to-develop-on-chain)
  - [To develop off-chain](#to-develop-off-chain)
    - [To run off-chain tests](#to-run-off-chain-tests)
    - [To start default CTL runtime](#to-start-default-ctl-runtime)
    - [To start serving PureScript docs](#to-start-serving-purescript-docs)
  - [To serialize and export scripts](#to-serialize-and-export-scripts)


## To develop on-chain

Haskell environment with HLS and Hoogle

```shell
nix develop .#on-chain
cd on-chain
```

## To develop off-chain

```shell
nix develop .#off-chain
cd off-chain
```

### To run off-chain tests
```shell
make
```


### To start default CTL runtime

```shell
nix run .#default-ctl-runtime
```

### To start serving PureScript docs

```shell
nix run .#docs
```

or

```shell
nix run .#ctl-docs
```

## To serialize and export scripts

```shell
nix run .#script-export
```

or from on-chain shell

```shell
cabal run script-export
```
