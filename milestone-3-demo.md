# Seath framework and Milestone 3 demo

- [Seath framework and Milestone 3 demo](#seath-framework-and-milestone-3-demo)
  - [Demo protocol](#demo-protocol)
  - [Repository structure](#repository-structure)
    - [On-chain](#on-chain)
    - [Off-chain](#off-chain)
      - [Developing with real network](#developing-with-real-network)
  - [Seath technical overview](#seath-technical-overview)
  - [Addition protocol demo](#addition-protocol-demo)
    - [Core functionality](#core-functionality)
    - [User and Leader logic](#user-and-leader-logic)
  - [Users demo setup](#users-demo-setup)
  - [Running the demo](#running-the-demo)

This documents describes simple stateful protocol that can be run on-chain and the way how to integrate and run it with Seath framework.

## Demo protocol

Demo protocol is called `Additional protocol` and is pretty simple:

- State of the protocol is represented by single UTXO with single number in the Datum
- Users of the protocol perform "addition action" to increment current state by desired amount. Thats it, users build transactions to spend current script UTXO and produce new one where number in the Datum is incremented by desired amount. Validator script checks that amount to be added is `>= 0` and that Datum was properly updated.

Although this protocol is really simple, it suffers from the UTXO contention problem - if 2 or more users will build and submit transactions at the same time, they will use same single UTXO with current state fro validator script address. But only 1st transaction accepted by the blockchain will succeed. It will spend current state UTXO and produce new one. Other transactions will fail, because input state UTXO will be spent already.

More details and explanations of UTXO contention problem can be found in the introductory parts of the following demo recordings:

- [Milestone 2 demo records](https://drive.google.com/drive/folders/1uBvU1d5iAWRd7IvStLEiip83yS8kh5i5?usp=sharing)
- Milestone 3 demo record - TODO: add link

On-chain part of the protocol is represented by validator script written in PlutusTx. It can be found in `on-chain` directory of the repo inside [AdditionValidator.hs](./on-chain/src/AdditionValidator.hs) Haskell module.

Transaction building and submitting is handled by the Seath framework in such a way, that users of the protocol can perform their step on the protocol avoiding UTXO contention problem.

## Repository structure

Repository is divided into two main parts represented by `on-chain` and `off-chain` directories. For fast and easy setup repository provides `flake.nix` file - Nix with flake feature is recommended way of running code in this repo and all following examples will use Nix capabilities.

### On-chain

As was mentioned before, `on-chain` directory contains validation script written in PlutusTx. Also directory contains tools written in Haskell to compile and serialize the script into CBOR format. Serialized script can then be integrated into `off-chain` part to build transactions.

If you want to change validator script, it is possible to start nix environment with Haskell dependencies required for PlutusTx - from the root of the repository run

```shell
nix develop .#on-chain
# when Nix shell is up and ready
cd on-chain
# now you have environment with cabal and HLS
```

After changes are made and ready to go, run this command **from the root of the repo**:

```shell
nix run .#script-export
```

It will compile the script, serialize it to CBOR and insert in the correct place inside `off-chain` directory, from where Seath framework will be able to pick it up and use in transactions.

### Off-chain

`Off-chain` part contains Seath framework logic. It is built on top of [Cardano Transaction Library](https://github.com/Plutonomicon/cardano-transaction-lib) written in PureScript and have all required capabilities for Cardano blockchain.

To get into development shell with `spago` from the **root** of the repository run

```shell
nix develop .#off-chain
# when Nix shell is up and ready
cd off-chain
```

From here you can run tests with

```shell
spago run -m Seath.Test.Main
```

or run automated end-to-end test for addition protocol with disposable local cluster with (see details on [testing with Plutip tool](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutip-testing.md))

```shell
spago run -m Seath.Test.Main -b addition-e2e-plutip
```

Or run demo on preproduction testnet.

#### Developing with real network

To be able to communicate with blockchain Seath (CTL under the hood) requires some additional runtime - Kupo and Ogmios (see [more details in CTL documentation](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/runtime.md)). `flake.nix` in current repo provides command to start all required services. The only requirement is - installed and set up Docker. To start runtime, from the **root** of the repo run (to stop runtime - `Ctrl/Cmd+C` it the same terminal)

```shell
 nix run .#preprod-ctl-runtime
```

Then wait till node will sync to 100%. You can check current sync progress:

```shell
# find id or name of thhe node container with command
docker ps
# then use id or name to execute command in the node container
docker exec -ti [id_or_name] sh -c "CARDANO_NODE_SOCKET_PATH=/ipc/node.socket cardano-cli query tip --testnet-magic=1"
```

After node is synced, you can start developing or running demo with preproduction network:

-> automated end-to-end test with single leader node and 4 users submitting actions simultaneously

```shell
spago run -m Seath.Test.Main -b preprod -b auto-e2e-test 
```

-> start full Seath node that can accept user requests via IPv4

```shell
spago run -m Seath.Test.Main -b preprod -b start-leader 
```

-> start separate scenario, where 4 users will send their action simultaneously (currently users are set to send requests to the node started with `start-leader` option shown above)

```shell
spago run -m Seath.Test.Main -b preprod -b start-users 
```

## Seath technical overview

The main working unit of the Seath framework is `Seath node`. `Seath node` can be both leader and user.

Under the hood `Seath node` runs following components (note: `actions` will be explained further):

- `LeaderNode` - process, that responsible for running leader logic: accepting `actions` from users, translating them into chain of transactions, handling signing process, submitting chain of transactions to blockchain. `UserNode` uses `core functionality` of Seath framework, that need to be extended by the users of Seath according to the specifics of particular protocol (more on that below).
- `UserNode` - process, that is responsible for running user logic: creating `actions` and sending them to the leader, monitoring current action status and reacting to status change: inspect nad sign transaction when needed (or refuse to sign), detect if transaction was submitted successfully or failed.
- web-server - provides REST API for the `LeaderNode` to accept user requests. `UserNode` under the good uses this known REST API of the leader to enable communication between `Seath nodes` over the network. Currently, there is no requests from leader to users - leader changes status of the `action` submitted by the user, and by monitoring `action` status user sends required information to the leader, or query the leader.

In terms of architecture, Seath framework devided into 3 main parts (see [src dir](./off-chain/src/)):

- `Core` - `core functionality` of Seath
- `Network` - logic of `LeaderNode` and `UserNode`
- `HTTP` - logic of web-server part

Dependency graph: `Core` <- `Network` <- `HTTP`

All this parts will be explained in more details below in parallel with demo setup explanation.

## Addition protocol demo

Modules required to run the demo located in [Demo directory of Addition example](./off-chain/test/Examples/Addition/Demo/).


`FullLeaderNode.purs` contains all necessary setup to start full `Seath node` to handle Addition protocol - lets start form here going layer by layer - first core functionality, then user and leader logic, then web-server.

### Core functionality

`Core functionality` is central part of the Seath framework. Here users of Seath framework will integrate off-chain logic into Seath to enable transaction chaining which helps to solve UTXO contention problem.

Code requeued to integrate Addition protocol into Seath located in [Addition dir inside tests](./off-chain/test/Examples/Addition/). Most important modules are `Actions.purs` and `Types.purs`

To make `Seath node` in `FullLeaderNode.purs` we obtain `CoreConfiguration` bu running CTL Contract:

```haskell
  coreConfig <- runContractInEnv env $ withKeyWallet leader $
    Addition.buildAdditionCoreConfig
```

(for more details on running Contracts in CTL see [related docs](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/getting-started.md#setting-up-a-new-project))

Result of this call will provide us `CoreConfiguration` data from [Seath.Core.Types](./off-chain/test/Examples/Addition/Types.purs) module. It contains necessary "parts" that user of Seath framework will need to define to integrate particular protocol into Seath. Lets look closely what is done to integrate Addition protocol.

We'll need total 4 pieces for `CoreConfiguration`:

- Public key hash of the leader - `leader` field
- Hash of the validator script which will hold state UTXOs of the protocol - `stateValidatorHash` field
- Function, that will be able to interpret `action`, that user performs, into lookups and constraints - `actionHandler`. This function will enable transaction chaining.
- CTL Contract that can query state of the protocol from blockchain - `queryBlockchainState`. To build new chain we need to figure out current state of the protocol. After that we can chan transaction passing changes of the initial state from one transaction in chain to another. Then when chain will be submitted, Seth will use this function again to start building new chain with current state acquired from  the blockchain.

`Action` - is the step on the protocol that user wants to perform expressed via some type. For Addition protocol users want to increment current state represented by number by some amount. For that Addition protocol example uses following type from `Types.purs` module of Addition example

```haskell
newtype AdditionAction = AddAmount BigInt
```

So here is our `action` - add some amount. This type need to be JSON serializable, so nodes can send data over the network.

Now we need to define how to apply this action to the current protocol state. Usually we will make contract that will build transaction which will get current state, modify it and create new output with updated state. In case Seath, we don't need to build full transaction, we need to define how current `action` should change the state on-chain, make corresponding transaction constraints and lookups (CTL contracts were inspired by `plutus-apps` contracts) and describe new state. In case of Addition protocol state is described by `AdditionState` type from `Types.purs` and is simply a number, that reflects state representation on-chain:

```haskell
type AdditionState = BigInt
```

We need to provide function with the following type

```haskell
handleAction
  :: UserAction AdditionAction
  -> AdditionState
  -> Contract UtxoMap
  -> Contract
       ( Seath.Core.Types.StateReturn AdditionValidator AdditionDatum
           AdditionRedeemer
           AdditionState
       )
handleAction userAction lockedValue getScriptUtxos = ...
```

`userAction` - is our `AdditionAction` wrapped in `UserAction`. `UserAction` is part of internal Seath machinery and not need to be constructed by the user of framework.

`lockedValue` - is our representation of on-chain state - `AdditionState` type. It passed buy the State framework under the hood during chaining.

`getScriptUtxos` - this function is provided (injected) by Seath framework and gives access to the current or predicted outputs at the validator script address (depending on whether the `action` is first in the chain or not).

You can find full code in [Seath.Test.Examples.Addition.Actions.handleAction](./off-chain/test/Examples/Addition/Actions.purs). Here we extract addition amount from `AddAmount` constructor of the `action`, get validator script hash, get current (or predicted during chaining) UTXOs from validator script which holds protocol state and build required parts for future transaction: Datum, Redeemer, constraints and lookups. If you familiar with [state machines from Plutus Pioneer Program](https://plutus-pioneer-program.readthedocs.io/en/latest/week7.html#code-example-2) you can see here closely related idea - describe how to perform single step on some state depending on input without building transaction explicitly by hand.

With `handleAction` we have one pice for `CoreConfiguration` ready.

Next piece is also located in `Seath.Test.Examples.Addition.Actions` - `queryBlockchainState`. In case of Addition protocol it is simple CTL contract that can query script validator of the protocol. Script validator was compiled and serialized from `on-chain` part and injected as CBOR into [Seath.Test.Examples.Addition.Validator](./off-chain/test/Examples/Addition/Validator.purs). Using several helper functions `queryBlockchainState` can use CBOR from `Seath.Test.Examples.Addition.Validator` to build CTL contract that can query UTXOs from the script and extract current Datum that represents protocol state.

The last two pieces we need are much simpler - they are hash of validator script and hash of leader public key. You can check out how they obtained in case of Addition example in [Seath.Test.Examples.Addition.ContractUtils.buildAdditionCoreConfig](./off-chain/test/Examples/Addition/ContractUtils.purs). There is nothing special with getting script hash - we just use helper functions to deserialize CBOR and get hash of the script. In case of the leader key we are getting it by running another CTL Contract using `KeyWallet` feature provided by CTL. Depending on environment, it can return either hash of the key generated for preproduction testnet, or hash of the key generated by plutip tool when Seath runs on private local cluster. In case of the current demo, we run Seath on preproduction testnet and required keys are located in [keys dir](./off-chain/test/keys/seath_keys/).

Thats it for `Core` part. Having `CoreConfiguration` Seath node will be able to build transaction to run Addition protocol.

### User and Leader logic







To start the full Seath node we'll need to provide some configurations data.
  - IPv4 network layer options
    - Port for web-server layer
    - Leader url - internal user node will use this URL to make requests to the leader. At the moment, there is no functionality to choose or leader dynamically, but it can be implemented in a pretty straight forawrd way with existing architecture
  - Transaction chaining options
    - Number of user actions in leader's "mailbox" that will trigger chain building and submission. When leader receives request from user to submit his action to chain, the action is added to leader's queue first. When number of actions reqches configureble treshold, leader pulls action from this queue and starts processing them
    - Timeout before leader start processing user actions. Even if there is not enough actions in the leader's mailbox queue, after configureble period of time leader will start processing anyway
    - Time that leader will wait for signatures. After leader started processing user actions, it will build chain of transactions from them accrdong to provided core configuration (more on this below) and then after chain is ready, it will wait for users to sign their transactions. OFter this timeout (in milliseconds) leader will check signen transactions it received from users and submit them. If transaction was not signed on time or user rejected to sign transaction, it will be discarded from processing pipeline, and if this transaction breaks the chain, proper measure will be applied (more on this in the following sections)
  - Contract runner and Core options
    - Running both leader and user logic requires interactions with blockhain. At the moment, Seath uses `Contract` functionality provided by CTL (which was inspired by IOG PAB). Some steps that require creation of user actions, signing, building chains of transactions and chain submission are made as CTL contracts. So node need a way to execute those contracts, but contract execution depends on various environments that CTL capable to provide. For the demo we are using `KeyWallet` wallets and CTL runtime to execute contracts and function that will execute CTL contracts is built to support that (more options can be found in CTL documentation)
    - Seath `CoreConfiguration`. Core configuration contains core logic for on-chain protocol state machine. Here user of Seath framework should describe what it means to perform a step on state machine and how to query initial script state. Config contains following fields:
      - `stateValidatorHash` - hash of the validator that holds state UTXOs
      - `queryBlockchainState` - CTL `Contract` that describes how to query current state of the state machine form blockchain. This call is requered to build chain of transactions, so whole chain will perform actions starting from the latest valid state of protocol's state machine
      - `actionHandler` - crucial part of the framework. It is Contract defined by framework user that describes how to perform step on the state machine of the protocol. Type of `actionHandler` defines the function, which provides to the user of framework  access to the current action that is performed on the protocol, current protocol state and handler that can query UTXOs from validator script of the protocol state machine. With this in hand user able build transaction that can execute step on the protocol, but in case of the Seath insted of full transaction, user need to provide lookups, constrants and new expected state (which will be submitted with new Datum of the output that will create new state UTXO). Real transaction will be built by Seath from this data. In the demo there is only one action to perform on protocol - `AddAmount` that describes our intention to add some number to the current state. We are getting amount from `AddAmount`, building Redeemer and Datum and use them to build lookups and constraints. Then get new amount from new Datum to return it as new "current" state . It is important to note, that function that get UTXOs from the script in `actionHandler` returns real UTXOs from the cblockhain only for first transaction in chain. For further transactions it would return pre-calulculated UTXOs as if we would submit transactions one by one and wait each of them to be confirmed. So for most of the actions in chain this fucntion can't be the source of the real UTXOs that are currently on-chain. In other words, it returns UTXOs that script will have after previous transaction in chain.
      - `leader` - hash of the public key that leader node uses. Currently, Seath doesn't support dynamic leader change, but this can be improved in further development.

It is advised to use framework provided functions to build configs for leader and user nodes: `mkLeaderConfig` and `mkUserConfig`. Nodes configs are pretty involved and this functions simplify creation of configs by making some required wiring under the hood in way that is 100% correct.

## Users demo setup

Unlike Leader setup, nodes that will play role of users are not started as full Seath nodes in the demo. This is made only for convinisence - this way output of all users can be observed in one terminal and there will be no distracting logs from the processes executed by the leader node inside full Seath node. Also, this way it is easer to automate and run demo scenarios.

We are using framowrk function `mkUserConfig` to make config same way as we did in `Leader Setup`. Besides of already metnioned url of the leader server and contract runner, user config also accepts the function that can check chained transaction. After leader finish building, balancing and chainig of transactions, it marks them as ready to be signed. When user detects this status, it pulls transaction with corresponding seath-id from the leader and signs it. But before signing, it runs transaction check function. If this function returns an error, user node will refuse to sign transaction and notify the leader about that. Users of the seath framework can implement any checking function that conforms the type in config. Whole transaction built by the leader is supplied as an argument to this function.

After config is ready, we can call `startUserNode` with it and launch all processes required to act as a user. This is exact same procedure that full Seath node perfors during launch. So this "internal" user node can be used alone as fully functional unit for user interactions. And this is exactly how things arranged for the demo.

-- TODO
- add links to modules
- add code and command snippets
- extend `Running the demo` section

## Running the demo
Execution options:
- Auto test: `auto-e2e-test` - full seath node launched for leader, "internal" user nodes launched to simulate users. Each user submit single action. Test runs for 30 seconds, then shut down all nodes and exit.
- Manual test: `start-server` - to start full seath node that will be used as leader, `start-users` - to start "internal" user nodes and perform scenario.