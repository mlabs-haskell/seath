# Milestone 2 demo

It is strongly recommended to use Nix to run this demo and following description will use Nix setup.

Main entry point of the demo is [Seath.Test.Main module](./test/Main.purs).

## Running on local private network

Local network capabilities are integrated into current setup. Local network provided by [Plutip](https://github.com/mlabs-haskell/plutip). Network is started from scratch on each run. Slot length is is to 1 second - see [config :: PlutipConfig](test/PlutipRunner.purs).

To run the demo with local network do the following.

From the root of the repository start Nix development shell:

```shell
nix develop --extra-experimental-features 'flakes nix-command' .#off-chain
```

Note, that features `flakes` and `nix-command` are required.

When Nix shell is ready, switch to `off-chain` directory and start the execution:

```shell
cd off-chain
spago run -m Seath.Test.Main -b plutip
```

Local network will be started automatically and as soon as all required wallets will be funded, execution of Seath demo will begin. Execution trace will be printed to the terminal.

## Running on preprod testnet

For preprod testnet [cardano transaction library runtime](https://github.com/Plutonomicon/cardano-transaction-lib/blob/v5.0.0/doc/runtime.md) is required. It includes Cardano node, Ogmios and Kupo.

Demo setup has Nix run command to launch everything. Only requirement is `Docker` installed and running.

To start required services from the root of the repo execute

```shell
nix run --extra-experimental-features 'flakes nix-command' .#preprod-ctl-runtime
```

Then wait till node and rest of the services will sync with the network.

To check node sync process following command can be used

```shell
docker exec -ti [container_id/container_name] sh -c "CARDANO_NODE_SOCKET_PATH=/ipc/node.socket cardano-cli query tip --testnet-magic=1"
```

When you see `"syncProgress": "100.00"` it means that node is fully synced and you can start the demo.

E.g.:

```shell
{
    "block": 753435,
    "epoch": 59,
    "era": "Babbage",
    "hash": "19d39cc47e46b814ccc34a97b84370f72a1e9f65d6408c66d18b54805f3fdf36",
    "slot": 23879120,
    "syncProgress": "100.00"
}
```

Usually, if node is synced, demo can be started. The process of starting the demo is the same as with [local private network](#running-on-local-private-network), but with different argument.

From the root of the repository start Nix development shell:

```shell
nix develop --extra-experimental-features 'flakes nix-command' .#off-chain
```

When Nix shell is ready, switch to `off-chain` directory and start the execution:

```shell
cd off-chain
spago run -m Seath.Test.Main -b preprod
```

Execution trace will be printed to the terminal.
