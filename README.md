# Seath

Seath is a novel solution to the UTXO contention problem experienced on Cardano and
similar platforms that make use of the UTXO ledger model.

## Problem

Most protocols in Cardano are essentially state machines.
Given a UTXO with an identifying/authenticating token, it is locked with a script that represents the logic of the state machine. To consume it you must _continue_ the state machine, i.e. consume existing UTXO and make a new output with the same token and the new state (and often carry over the value (e.g. ADA) too).

Given such a protocol and a particular state, if multiple parties want to perform a
step on it, they each craft a transaction that performs their step, but these transactions
are in fact mutually exclusive. This is a fundamental difference between the UTXO ledger model
and the account ledger model. The transactions refer to the particular _state_ and **not** the
state machine. Naive solutions to this involve a level of indirection, where the steps themselves are represented as UTXOs, such that several such UTXO steps can all be performed in a single _batching_ transaction.
This is however not much better.

We judge a solution by three criteria:
- Best-case performance
- (Worst-case) performance under adversarity
- Trustlessness

Batching isn't any different from no batching in all three respects.
Proof: Best-case performance is the same when all participants cooperate to create
a _chain_ of transactions (_transaction chaining_). Worst-case performance is no different, since each user can submit the batching transaction right after their own step (a batch of one step). This is in fact the _rational_ behavior if you prioritize latency, thus resulting in the behavior that worst-case is _average_ with rational actors. Trustlessness is no different and is trivially obvious.

We seek a solution to this problem that doesn't have any assumptions beyond:
- Most actors are rational
- Most actors are honest

Seath is an attempt at finding such a solution.

## Solution

Seath attempts to solve this by using a _leader_ who will chain transactions of other participants. Such a leader is a central actor, and thus the protocol is inherently _less decentralized_ (but not necessarily fully centralized or trustful).

Given such a leader, other participants do not build and submit their transactions directly, but instead they will send an _action_ they need to perform to _continue_ the state machine and describe how this action can be converted into transaction. Leader then assembles a chain of transactions to fully utilize the throughput of L1. For performed chaining leader can be rewarded through fees.

If the leader is dishonest, throughput will heavily reduce, and a new leader should be selected.
Leader can be selected e.g. using a stake-weighted mechanism a la Cardano, or information about current leader could be provided through some oracle. Leader selection is still an open question and first of all we are aiming to provide reliable chaining solution to dApp developers, but with possibility to add leader selection on top of it.

### Modification to target state machine script necessary

Given a UTXO that represents a state machine, its script can support integration with Seath by simply requiring that any consuming transaction is signed by the leader. To mitigate the risk of dishonest leader preventing valid transaction to pass, every N time, there could be a timeslot M big where this constraint is no longer checked and user can submit transaction directly.

### Off-chain

Using Seath should ideally be a simple as possible. We want to provide an off-chain framework based on [Cardano Transaction Library](https://github.com/Plutonomicon/cardano-transaction-lib) that will let dApp developers to describe state transitions in terms of _actions_ that can be then translated to Cardano transactions. Given that, Seath framework will be able to chain transactions and submit them on chain in such away, that each transaction in chain will _continue_ the state machine in sequence, but from users' perspective it will look like they all submitted transactions simultaneously, without any contention. Of course users won't blindly trust the leader to build and submit transactions. The way how the _action_ should be translated into transaction will be also defined by Seath framework user, and there full control on inputs and outputs will be provided. Then, to submit chain of transactions, each transaction will have to be signed by the user's key, and at this stage user will be able to inspect whole transaction and reject signing if something looks wrong. In this case Seath will rebuild chain of transaction excluding rejected ones.

Networking in this case is a complicated topic, but for the MVP,
it will be assumed that the leader has an openly accessible IPv4 address/port.
The data will be sent over IPv4 to the specified address/port, and can be encrypted using
the leader's public key.

There are two problems with this solution:
- It assumes the leader has an IPv4 address and can open a port.
- It assumes the user can access the full internet.

The former is unfortunately not common and constrains who can be a leader,
the latter is unlikely to be an issue, but optimally, if you can interact
with Cardano, it should be possible to interact with Seath.
You theoretically don't need an IPv4 connection to the open internet to use Cardano.

A simple solution to this is to make use of [Pinecone](https://github.com/matrix-org/pinecone),
an overlay routing protocol which should solve both of the aforementioned issues.
As long as the leader and user have a common (transitive) peer,
they can communicate.
This is not part of the MVP, and is likely to be a fallback mechanism, as raw IPv4
should be more efficient.
