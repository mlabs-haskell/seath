# Seath framework and Milestone 3 demo

- [Seath framework and Milestone 3 demo](#seath-framework-and-milestone-3-demo)
  - [Demo protocol](#demo-protocol)
  - [Repository structure](#repository-structure)
    - [On-chain](#on-chain)
    - [Off-chain](#off-chain)
      - [Developing with real network](#developing-with-real-network)
  - [Seath technical overview](#seath-technical-overview)
    - [How transaction chaining is handled](#how-transaction-chaining-is-handled)
  - [Addition protocol demo](#addition-protocol-demo)
    - [Core functionality](#core-functionality)
    - [User and Leader logic](#user-and-leader-logic)
      - [Contract runner](#contract-runner)
      - [Leader node config](#leader-node-config)
      - [User node config](#user-node-config)
    - [Web-server config](#web-server-config)
    - [Users demo scenario](#users-demo-scenario)
  - [Some limitations](#some-limitations)

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

### How transaction chaining is handled

Seath chains transactions in "rounds":

- Accumulates some number of incoming requests to perform an `action` from users
- Takes batch of requests and
  - Translates `actions` into chain of transactions using `Core` functionality
  - Marks chained transactions as ready to be signed by the users
  - Waits till users sign transactions. If some users didn't respond or explicitly refused to sign transaction, leader figures out where chain is broken and what transactions can be submitted anyway - this transactions proceed to submission phase. Not-signed transactions are discarded from further processing. Transactions, that were signed but can't be submitted due to chain breakage, are put into special priority queue and will be processed first during next "round" - transactions will be re-built from `action`s from scratch. Users of such transactions will see from the status response that their transaction in priority queue for the next "round"
  - Submits chain of signed transaction. If any transaction fail then submission is aborted. Already submitted transactions are marked as processed successfully. Failed transaction is discarded - user gets notification that transaction failed. Rest transactions in chain can't be submitted anymore, so they will be put into special priority queue and will be processed first during next "round" - transactions will be re-built from `action`s from scratch. Users of such transactions will see from the status response that their transaction in priority queue for the next "round"
- After batch is processed leader waits till submitted chain will be confirmed on the blockchain
- After chain is confirmed leader starts next "round" repeating whole process

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

If we look further into `FullLeaderNode.purs` we'll see

```haskell
    mkRunner :: KeyWallet -> RunContract
    mkRunner kw = RunContract (\c -> runContractInEnv env $ withKeyWallet kw c)

    leaderNodeConfig =
      Seath.mkLeaderConfig
        3000 -- timeout for building chain
        4 -- number of pending actions in queue
        3000 -- timeout for signatures awaiting
        coreConfig
        (mkRunner leader)

    userNodeConfig = Seath.mkUserConfig leaderUrl (mkRunner leader)
      (pure <<< Right)
```

#### Contract runner

First thing to note here is creation of `RunContract` which is further used in making leader and user configs. Both leader and user logic uses CTL Contracts to interact with blockchain, thus it need a way to run Contracts. CTL can execute contracts in several environments, using cardano-cli generated keys or integration with light wallets. Here we are using cardano-cli generated keys and `KeyWallet` CTL feature to run contracts using them. And we construct function that can use `KeyWallet` and current CTL environment (see docs for CTL `withContractEnv`) to build the function that will be able to execute Contract using provided key inside `Seath node`.

#### Leader node config

`Seath.mkLeaderConfig` will build configuration for internal `LeaderNode` that `Seath node` will run. It si strongly recommended to use this framework provided function to build config. Nodes configs are pretty involved and this functions simplify creation of config by making some required wiring under the hood in way that is 100% correct.

Current configurable options are:

- Timeout before leader start processing user actions - milliseconds. Even if there is not enough actions in the leader's mailbox queue, after this period of time leader will start processing anyway

- Number of user `actions` in leader's "mailbox" that will trigger chain building and submission. When leader receives request from user to submit his `action` to chain, the action is added to leader's queue first. When number of actions reaches this threshold, leader pulls action from the queue and starts processing them

- Time that leader waits for signatures - milliseconds. After leader started processing user actions, it will build chain of transactions from them according to provided `CoreConfiguration` and then after chain is ready, it will wait for users to sign their transactions. After this timeout leader will check signed transactions it received from users and submit them. If transaction was not signed on time or user rejected to sign transaction, it will be discarded from processing pipeline, and if this transaction breaks the chain, proper measure will be applied (more on this in the following sections)

And in addition we need to provide `CoreConfiguration` and contract runner to get config for `UserNode`.

#### User node config

To make user config we use `Seath.mkUserConfig`. It is also strongly advised to use this framework provided function instead of building config by hand using constructor.

`UserNode` config accepts:

- Leader url - `mkUserConfig` will create necessary handlers to enable network communication required by user logic
- Contract runner function - we use here leader key again, as we are creating full `Seath node` that can act both as leader and user, but it will play the role of leader for the demo
- Function to check transaction - this function should be defined by the user of Seath framework. When leader will build chain of transactions it mark them as ready to be signed. User will pull his transaction from the leader, but before signing it, user can examine whole transaction, and if something doesn't look correct, refuse to sign it by returning error (`Left`) from this function. Node will notify the leader that user refused to sign transaction and it will be excluded from further processing.

### Web-server config

Further in `FullLeaderNode.purs` we see last required part

```haskell
    serverConf :: SeathServerConfig
    serverConf = { port: leaderPort }
```

To start web-server we need only port. Web-server wil serve REST API that will enable node that acts as leader to receive requests from user nodes.

With all that prepared we can start `Seath node`

```haskell
seathNode <- SeathNode.start serverConf leaderNodeConfig userNodeConfig
```

To stop the node

```haskell
SeathNode.stop seathNode
```

In the demo we leave leader node to run forever until interruption.

As a reminder, to start leader node from CLI

```shell
```shell
nix develop .#off-chain
# when Nix shell is up and ready
cd off-chain
spago run -m Seath.Test.Main -b preprod -b start-leader 
```

(CTL runtime should be up and running)

### Users demo scenario

[Test.Examples.Addition.Demo.SeathUsers](./off-chain/test/Examples/Addition/Demo/SeathUsers.purs)

Unlike Leader setup, nodes that will play role of users in the demo are not started as full `Seath nodes`. This is made only for convenience - this way output of all users can be observed in one terminal and there will be no distracting logs from the processes executed by the leader logic. Also, this way it is easer to automate and run demo scenarios.

We are using framework function `mkUserConfig` to make config same way as we did in `Leader Setup` - `mkUserConfig` will install all required network handlers.

`mkUserConfig` is wrapped into helper function `mkNumeratedUserNodes` that accepts `refuser` flag

```haskell
  let
    refuser = Just 2

  numeratedNodes <- mkNumeratedUserNodes leaderUrl mkRunner refuser setup
```

Current demo setup for preprod network submits actions from 4 users. By setting `refuser = Just 2` we force user `2` to refuse to sign transaction and cause chain break and recovery. To make all users sign their transactions set flag to `Nothing`.

`mkNumeratedUserNodes` will start 4 `UserNode`s that are still capable to send actions to the leader and track status of transaction. They were chosen to run demo scenarios instead of full `Seath node`s to make scenarios a bit simpler to handle and easier to tweak when needed.

`mkNumeratedUserNodes` returns `Array` of already running `UserNode`'s together with their indexes. From here we can submit `action` directly to the node and node will process `action`, send request to the leader and start to monitor status of transaction:

```haskell
  for_ numeratedNodes $ \(ix /\ node) -> do
    log $ ixName ix <> ": preform include action request"
    node `Users.performAction` (AddAmount $ BigInt.fromInt ix)
```

After this point process will run until interruption. After interruption scenario will output results for submitted `actions` for each user. It could be an error or info about successfully submitted transaction.

## Some limitations

There are some limitations in current version. They are not fundamental or unsolvable and can be eliminated during further development.

- User can't send another `action` till previous action is still in progress. When user submits his action, data that sent to the leader includes user's public key hash. This way leader can detect that there is already an `action` in process from this user. The reason for that is: when user submits action, it also submits inputs from his address to balance transaction. At the moment algorithm just grabs all UTXOS from user address. Sending same UTXOs twice will cause 2nd transaction from same user in chain to fail. So problem can be solved by smarter way of picking input UTXOs on user side
- Leader must be known upfront. With current state we need to know leader url and public key upfront before starting the node. This makes impossible to change the leader dynamically w/o node restart. This may be improved during further development - the current architecture does not introduce any blockers for this.
- Whn user refuses to sign transaction, rest transactions in chain a moved to priority queue for the next round which can potentially slow down `action`'s submission. During further development we could try to continue the chain from breakage point by re-building and re-signing such transactions right away, w/o putting them in the queue for the next round.
