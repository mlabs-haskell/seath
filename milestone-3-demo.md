# Seath framework and Milestone 3 demo

- [Seath framework and Milestone 3 demo](#seath-framework-and-milestone-3-demo)
  - [Demo protocol](#demo-protocol)
  - [Repository structure](#repository-structure)
    - [On-chain](#on-chain)
    - [Off-chain](#off-chain)
      - [Developing with real network](#developing-with-real-network)
  - [Seath technical overview](#seath-technical-overview)
    - [How transactions chaining is handled](#how-transactions-chaining-is-handled)
  - [Addition protocol demo](#addition-protocol-demo)
    - [Core functionality](#core-functionality)
    - [User and Leader logic](#user-and-leader-logic)
      - [Contract runner](#contract-runner)
      - [Leader node config](#leader-node-config)
      - [User node config](#user-node-config)
    - [Web server config](#web-server-config)
    - [Users demo scenario](#users-demo-scenario)
  - [Some limitations](#some-limitations)

This documents describes simple stateful protocol that can be run on-chain and the way how to integrate and run it with Seath framework.

## Demo protocol

The demo protocol is called "Additional Protocol" and is quite simple:

- The state of the protocol is represented by a single UTXO with a single number in the Datum.
- Users of the protocol perform an "addition action" to increment the current state by a desired amount. That's it. Users build transactions to spend the current script UTXO and produce a new one where the number in the datum is incremented by the desired amount. The validator script checks that the amount to be added is greater than or equal to zero and that the datum was properly updated.

Although this protocol is really simple, it suffers from the UTXO contention problem. If two or more users build and submit transactions at the same time, they will use the same single UTXO with the current state from the validator script address. But only the first transaction accepted by the blockchain will succeed. It will spend the current state UTXO and produce a new one. Other transactions will fail because the input state UTXO will have already been spent.

You can find more details and explanations of the UTXO contention problem in the introductory sections of the following demo recordings:

- [Milestone 2 demo records](https://drive.google.com/drive/folders/1uBvU1d5iAWRd7IvStLEiip83yS8kh5i5?usp=sharing)
- Milestone 3 demo record - TODO: add links

Seath aims to help developers mitigate issues by enabling transaction chaining. With Seth, it is possible to run a network of nodes where one node acts as the leader and listen users' requests. Users send their requests to the leader, stating the actions they want to perform on the protocol. The leader then builds a chain of non-conflicting transactions and submits it to the blockchain. Transactions are chained by the Seath in a way that allows users of the protocol to perform their step without the fear of encountering UTXO contention problems.

The on-chain part of the protocol is represented by a validator script written in PlutusTx, which can be found in the `on-chain` directory of the repository inside the [AdditionValidator.hs](./on-chain/src/AdditionValidator.hs) Haskell module.

## Repository structure

The repository is divided into two main parts, represented by the `on-chain` and `off-chain` directories. To facilitate a fast and easy setup, the repository provides a `flake.nix` file. It is recommended to use Nix with flake feature to run the code in this repository, and all following examples will use Nix capabilities.

### On-chain

As mentioned previously, the `on-chain` directory contains a validation script written in PlutusTx. Additionally, the directory contains tools written in Haskell to compile and serialize the script into CBOR format. The serialized script can then be integrated into the `off-chain` part to build transactions.

To change the validator script, you can start a Nix environment with the Haskell dependencies required for PlutusTx. From the root of the repository, run:

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

The script will be compiled, serialized to CBOR, and inserted into the correct location within the `off-chain` directory. From there, the Seath framework will be able to pick it up and use it in transactions.

### Off-chain

The Off-chain part contains the Seath framework logic, which is built on top of the [Cardano Transaction Library](https://github.com/Plutonomicon/cardano-transaction-lib) written in PureScript. It has all the required capabilities for the Cardano blockchain.

To access the development shell with `spago`, navigate to the **root** of the repository and run:

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

To communicate with the blockchain, Seath (which is implemented using CTL) requires additional runtime components, namely Kupo and Ogmios (more details can be found in the [CTL documentation](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/runtime.md)). The `flake.nix` file in the current repository provides a command to start all necessary services. The only requirement is that Docker is installed and running. To start the runtime, navigate to the root of the repository and run the following command (to stop the runtime, press `Ctrl/Cmd+C` in the same terminal):

```shell
 nix run .#preprod-ctl-runtime
```

Next, wait until the node has synced to 100%. You can check the current sync progress using the `docker` command:

```shell
# find id or name of the node container with command
docker ps
# then use id or name to execute command in the node container
docker exec -ti [id_or_name] sh -c "CARDANO_NODE_SOCKET_PATH=/ipc/node.socket cardano-cli query tip --testnet-magic=1"
```

Once the node is synced, you can launch Seath tests and components designed for execution on the pre-production network:

-> an automated end-to-end test with a single leader node and four users submitting actions simultaneously

```shell
spago run -m Seath.Test.Main -b preprod -b auto-e2e-test 
```

-> start full Seath node that can accept user requests via IPv4 network

```shell
spago run -m Seath.Test.Main -b preprod -b start-leader 
```

-> start scenario in which 4 users send their action simultaneously (currently users are set to send requests to the node started with `start-leader` option shown above)

```shell
spago run -m Seath.Test.Main -b preprod -b start-users 
```

## Seath technical overview

The primary unit of the Seath framework is the `Seath node`. A `Seath node` can function as both a leader and a user.

Under the hood, a `Seath node` runs the following components (note: `actions` will be further explained):

- `LeaderNode`: the process responsible for running the leader logic. It accepts `actions` from users, translates them into a chain of transactions, handles the signing process, and submits the chain of transactions to the blockchain. The `LeaderNode` uses the `Core` functionality of the Seath framework, which needs to be extended by framework users according to the specifics of the particular protocol (more on that below).
- `UserNode`: the process responsible for running the user logic. It creates `actions` and sends them to the leader, monitors the current action status, and reacts to status changes by inspecting and signing transactions when necessary (or refusing to sign). It also detects if a transaction was submitted successfully or has failed.
- Web server: provides a REST API for the `LeaderNode` to accept user requests. The `UserNode` uses this known REST API of the leader to enable communication between `Seath nodes` over the network. Currently, there are no requests from the leader to users. Instead, the leader changes the status of the `action` submitted by the user, and by monitoring `action` status, the user sends the required information to the leader or queries the leader.

In terms of architecture, Seath framework divided into 3 main parts (see [src dir](./off-chain/src/)):

- `Core` - `core functionality` of Seath
- `Network` - logic of `LeaderNode` and `UserNode`
- `HTTP` - logic of web-server part

Dependency graph: `Core` <- `Network` <- `HTTP`

All of these parts will be explained in more detail below, along with an explanation of the demo setup.

### How transactions chaining is handled

Seath chains transactions in "rounds":

- First, it accumulates a number of incoming requests to perform an `action` from users.
- It then takes a batch of requests and:
  - Translates the `actions` into a chain of transactions using the `Core` functionality.
  - Marks the chained transactions as ready to be signed by the users.
  - Waits for users to sign the transactions. If some users don't respond or explicitly refuse to sign the transaction, the leader figures out where the chain is broken and which transactions can still be submitted. Not-signed transactions are discarded from further processing. Transactions that were signed but can't be submitted due to chain breakage are put into a special priority queue and will be processed first during the next "round." Such transactions will be rebuilt from `actions` from scratch. Users of such transactions will see in the status response that their transaction is in a priority queue for the next "round."
  - Submits the chain of signed transactions. If any transaction fails, further submission is aborted. Already submitted transactions are marked as processed successfully. Failed transactions are discarded, and the user receives a notification that the transaction failed. The remaining transactions in the chain can't be submitted anymore, so they will be put into a special priority queue and will be processed first during the next "round." Such transactions will be rebuilt from `actions` from scratch. Users of such transactions will see in the status response that their transaction is in a priority queue for the next "round."
- After the batch is processed, the leader waits until the submitted chain is confirmed on the blockchain.
- After the chain is confirmed, the leader starts the next "round," repeating the whole process.

## Addition protocol demo

The modules required to run the demo are located in the [Demo directory of Addition example](./off-chain/test/Examples/Addition/Demo/).

 `FullLeaderNode.purs` module contains all the necessary setup to start a full Seath node and handle the Addition protocol. Let's start from here and go layer by layer: first, the core functionality, then user and leader logic, and finally the web server.

### Core functionality

The `Core functionality` is the central part of the Seath framework. Users of the Seath framework need to integrate their own off-chain logic into Seath core to enable transaction chaining.

The code required to integrate the Addition protocol into Seath core can be found in the [Addition directory inside tests](./off-chain/test/Examples/Addition/). Most important modules are `Actions.purs` and `Types.purs`

To create a Seath node in `FullLeaderNode.purs`, we first obtain the `CoreConfiguration` by running the CTL Contract.:

```haskell
  coreConfig <- runContractInEnv env $ withKeyWallet leader $
    Addition.buildAdditionCoreConfig
```

(for more details on running Contracts in CTL see [related docs](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/getting-started.md#setting-up-a-new-project), and there will be also some explanations below)

The result of this call will provide us with `CoreConfiguration` data from the [Seath.Core.Types](./off-chain/test/Examples/Addition/Types.purs) module. It contains the necessary "parts" that a user of the Seath framework will need to define in order to integrate a particular protocol into Seath. Let's take a closer look at what is done to integrate the Addition protocol.

To produce `CoreConfiguration`, you will need a total of 4 pieces:

- The public key hash of the leader - `leader` field.
- The hash of the validator script that will hold state UTXOs of the protocol - `stateValidatorHash` field.
- A function that can interpret the user's `actions` into lookups and constraints - `actionHandler`. This function enables transaction chaining.
- A CTL Contract that can query the state of the protocol from the blockchain - `queryBlockchainState`. To build a new chain, Seath need to determine the current state of the protocol. Once it have that information, it can pass changes to the initial state from one transaction in the chain to another. When the chain is submitted, Seth will use this function again to start building a new chain with the current state acquired from the blockchain.

`Action` refers to a step in the protocol that the user wishes to perform, expressed via a certain type. In the Addition protocol, users want to increment the current state, represented by a number, by a certain amount. To accomplish this, the following type from the `Types.purs` module of the Addition example is used.

```haskell
newtype AdditionAction = AddAmount BigInt
```

So there is our `action` - add some amount. This type need to be JSON serializable, so nodes can send data over the network.

Now we need to define how to apply this `action` to the current protocol state. Usually, we will create a contract that builds a transaction which gets the current state, modifies it, and creates a new output with the updated state. In the case of Seath, we don't need to build a full transaction. Instead, we need to define how the current `action` should change the state on-chain, make corresponding transaction constraints and lookups (CTL contracts were inspired by `plutus-apps` contracts), and describe the new state.

In the case of the Addition protocol, the state is described by the `AdditionState` type from `Types.purs` and is simply a number that reflects the state representation on-chain.

```haskell
type AdditionState = BigInt
```

For `actionHandler` we need to provide function with the following type:

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

`userAction` is `AdditionAction` wrapped in `UserAction`. `UserAction` is part of the internal Seath machinery and does not need to be constructed by the user of the framework.

`lockedValue` is representation of on-chain state - `AdditionState` type. It is passed by the State framework under the hood during chaining.

`getScriptUtxos` is a function provided (injected) by the Seath framework. It gives access to the current or predicted outputs at the validator script address, depending on whether the `action` is first in the chain or not.

You can find full code in [Seath.Test.Examples.Addition.Actions.handleAction](./off-chain/test/Examples/Addition/Actions.purs). Here we extract the addition amount from the `AddAmount` constructor of the `action`. We then get the validator script hash and current (or predicted during chaining) UTXOs from the validator script that holds the protocol state. Finally, we build the required parts for a future transaction: Datum, Redeemer, constraints, and lookups. If you familiar with [state machines from Plutus Pioneer Program](https://plutus-pioneer-program.readthedocs.io/en/latest/week7.html#code-example-2), you may see a closely related idea here. We describe how to perform a single step on some state depending on input without building the transaction explicitly by hand.

With `handleAction` done, we have one pice for `CoreConfiguration` ready.

The next required piece of code is also located in `Seath.Test.Examples.Addition.Actions` and is named `queryBlockchainState`. In the case of the Addition protocol, it is a simple CTL Contract that can query the script validator of the protocol. The script validator was compiled and serialized from the `on-chain` part and injected as CBOR into [Seath.Test.Examples.Addition.Validator](./off-chain/test/Examples/Addition/Validator.purs). Using several helper functions `queryBlockchainState` can use CBOR from `Seath.Test.Examples.Addition.Validator` to build the CTL Contract that can query UTXOs from the script and extract the current Datum, which represents the protocol state.

The last two pieces needed are much simpler - the hash of the validator script and the hash of the leader public key. You can check out how to obtain them in the case of the Addition example in  [Seath.Test.Examples.Addition.ContractUtils.buildAdditionCoreConfig](./off-chain/test/Examples/Addition/ContractUtils.purs). Getting the script hash is straightforward - we just use helper functions to deserialize CBOR and obtain the script's hash. For the leader key, we get it by running another CTL Contract using the `KeyWallet` feature provided by CTL. Depending on the environment, it can return either the hash of the key generated for pre-production testnet or the hash of the key generated by the plutip tool when Seath runs on a private local cluster. In the current demo, we run Seath on pre-production testnet, and the required keys are located in the [keys dir](./off-chain/test/keys/seath_keys/).

That's it for the `Core` part. With `CoreConfiguration`, the Seath node will be able to build transactions and chain them to run the Addition protocol.

### User and Leader logic

If we further examine `FullLeaderNode.purs` we'll see:

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

Lets start from the top.

#### Contract runner

The first thing to note is the creation of the `RunContract`, which is used then to create leader and user configs. Both the leader and user logic use CTL contracts to interact with the blockchain, so they need a way to run Contracts. CTL can execute contracts in several environments, including using cardano-cli-generated keys or integrating with light wallets. In this case, we're using cardano-cli-generated keys and the `KeyWallet` CTL feature to run Contracts using them.

We construct a function that can use `KeyWallet` and the current CTL environment (see docs for CTL's `withContractEnv`) to build a function that can execute a contract using the provided key inside the `Seath node`.

#### Leader node config

The `Seath.mkLeaderConfig` function builds the configuration for the internal `LeaderNode` that the `Seath node` will run. It is strongly recommended to use this framework-provided function to build the config. Node configs are quite involved, but this function simplifies their creation by making some required wiring under the hood in a way that is 100% correct.

The current configurable options are:

- Timeout before the leader starts processing user actions - in milliseconds. No matter how many `actions` currently in the leader's mailbox queue (see next option), after this period of time, the leader will start processing them anyway.
- Number of user `actions` in the leader's "mailbox" that will trigger chain building and submission. When the leader receives a request from the user to submit their `action` to the chain, the action is added to the leader's queue first. When the number of `actions` reaches this threshold, the leader pulls the actions from the queue and starts processing them.
- Time that the leader waits for signatures - in milliseconds. After the leader starts processing user actions, it builds a chain of transactions from them according to the provided `CoreConfiguration`. Once the chain is ready, the leader waits for users to sign their transactions. After this timeout, the leader checks the signed transactions it received from users and submits them. If a transaction was not signed on time or the user rejected signing it, it will be discarded from the processing pipeline.

In addition to options above, we need to provide already explained `CoreConfiguration` and contract runner to get the config for the `LeaderNode`.

#### User node config

To create config for `UserNode`, use the `Seath.mkUserConfig` function. It is strongly advised to use this framework-provided function instead of manually building the config using a constructor.

The `UserNode` config accepts the following parameters:

- Leader URL: `mkUserConfig` creates the necessary handlers to enable network communication required by user logic under the hood.
- Contract runner function: We use the leader key again, as we are creating a full `Seath node` that can act as both leader and user. However, it will play the role of leader for the demo.
- Function to check transactions: This function should be defined by the user of the Seath framework. When the leader builds a chain of transactions, it marks them as ready to be signed. The user pulls their transaction from the leader, but before signing it, the user can examine the entire transaction. If something looks incorrect, the user can refuse to sign it by returning an error (`Left`) from this function. The node will notify the leader that the user refused to sign the transaction, and it will be excluded from further processing.

### Web server config

Further in the `FullLeaderNode.purs` we see the last required part:

```haskell
    serverConf :: SeathServerConfig
    serverConf = { port: leaderPort }
```

To start the web server, you only need to specify the port number. The server will serve a REST API that allows the node acting as leader to receive requests from user nodes.

With all that prepared we can start `Seath node`:

```haskell
seathNode <- SeathNode.start serverConf leaderNodeConfig userNodeConfig
```

To stop the node:

```haskell
SeathNode.stop seathNode
```

In the demo we leave leader node to run forever until interruption.

As a reminder, to start leader node from terminal (CTL runtime should be up and running):

```shell
nix develop .#off-chain
# when Nix shell is up and ready
cd off-chain
spago run -m Seath.Test.Main -b preprod -b start-leader 
```

### Users demo scenario

[Test.Examples.Addition.Demo.SeathUsers](./off-chain/test/Examples/Addition/Demo/SeathUsers.purs)

Unlike the leader setup, nodes that will act as users in the demo are not started as full `Seath nodes`. This is done for convenience, so that the output of all users can be observed in one terminal and there will be no distracting logs from the processes executed by the leader logic. Additionally, it is easier to automate and run demo scenarios this way.

We use the framework function `mkUserConfig` to configure the nodes in the same way as we did in the leader setup. `mkUserConfig` will install all required network handlers.

`mkUserConfig` is wrapped in the helper function `mkNumeratedUserNodes`, which accepts the `refuser` flag.

```haskell
  let
    refuser = Just 2

  numeratedNodes <- mkNumeratedUserNodes leaderUrl mkRunner refuser setup
```

The current demo setup for the preproduction network involves actions from four users. By setting `refuser = Just 2`, we force user `2` to refuse to sign the transaction, causing a chain break and recovery. To make all users sign their transactions, set the flag to `Nothing`.

`mkNumeratedUserNodes` will start four `UserNode`s that are still capable of sending actions to the leader and tracking the status of the transaction. They were chosen to run demo scenarios instead of full `Seath node`s to make scenarios a bit simpler to handle and easier to tweak when needed.

`mkNumeratedUserNodes` returns an `Array` of already running `UserNode`'s together with their indexes. From here, we can submit an `action` directly to the node, and the node will process the `action`, send a request to the leader, start monitoring the status of the transaction and sign it when needed (or refuse signing).

```haskell
  for_ numeratedNodes $ \(ix /\ node) -> do
    log $ ixName ix <> ": preform include action request"
    node `Users.performAction` (AddAmount $ BigInt.fromInt ix)
```

After this, process will continue running until it is interrupted. After interruption, the scenario will output the results for each user's submitted actions. This output can include information about successfully submitted transactions or errors encountered during the process.

## Some limitations

There are some limitations in the current version, but they are not fundamental or unsolvable and can be eliminated during further development. These limitations include:

- Users cannot send another `action` until the previous action is still in progress. When a user submits an action, the data that is sent to the leader includes the user's public key hash. This way, the leader can detect that there is already an `action` in process from this user. The reason for this is that when a user submits an action, they also submit inputs from their address to balance the transaction. At the moment, the algorithm just grabs all UTXOs from the user's address. Sending the same UTXOs twice will cause the 2nd transaction from the same user in the chain to fail. So the problem can be solved by a smarter way of picking input UTXOs on the user side.
- The leader must be known upfront. With the current state, we need to know the leader URL and public key upfront before starting the node. This makes it impossible to change the leader dynamically without a node restart. However, this may be improved during further development, and the current architecture does not introduce any blockers for this.
- When a user refuses to sign a transaction, the rest of the transactions in the chain are moved to a priority queue for the next round, which can potentially slow down the submission of `action`s. During further development, we could try to continue the chain from the breakage point by re-building and re-signing such transactions right away, without putting them in the queue for the next round.
