# Seath

Seath is a novel solution to the UTXO contention problem experienced on Cardano and
similar platforms that make use of the UTXO ledger model.

## Problem

Most protocols in Cardano are essentially state machines.
Given a UTXO with an identifying/authenticating token, it is
locked with a script that represents the logic of the state machine.
To consume it you must _continue_ the state machine, i.e. make a new output
with the same token and the new state (and often carry over the value (e.g. ADA) too).

Given such a protocol and a particular state, if multiple parties want to perform a
step on it, they each craft a transaction that performs their step, but these transactions
are in fact mutually exclusive. This is a fundamental difference between the UTXO ledger model
and the account ledger model. The transactions refer to the particular _state_ and **not** the
state machine. Naive solutions to this involve a level of indirection, where the steps themselves
are represented as UTXOs, such that several such UTXO steps can all be performed in a single
_batching_ transaction.
This is however not much better.

We judge a solution by three criteria:
- Best-case performance
- (Worst-case) performance under adversarity
- Trustlessness

Batching isn't any different from no batching in all three respects.
Proof:
Best-case performance is the same when all participants cooperate to create
a _chain_ of transactions (_transaction chaining_).
Worst-case performance is no different, since each user can submit the batching transaction
right after their own step (a batch of one step). This is in fact the _rational_ behaviour
if you prioritise latency, thus resulting in the behaviour that worst-case is _average_
with rational actors.
Trustlessness is no different and is trivially obvious.

We seek a solution to this problem that doesn't have any assumptions beyond:
- Most actors are rational
- Most actors are honest

Seath is an attempt at finding such a solution.

## Solution

Seath attempts to solve this by electing a _leader_. Such a leader
is a central actor, and thus the protocol is inherently _less decentralised_
(but not necessarily fully centralised or trustful).

Given such a leader, Seath essentially mimicks Ouroboros (on top of Ouroboros)
to assemble a chain of transactions to fully utilise the throughput of L1.
The leader is incentivised to do what is correct through fees.
If the leader is dishonest, throughput will heavily reduce, and a new leader should be elected.
It is possible to elect the Seath leader using a stake-weighted mechanism a la Cardano,
assuming that stake is available in the protocol to be optimised.

One core difference from Cardano in its use of Ouroboros, is that Cardano can not suffer
from a majority malicious stake. Even if a large stake pool is malicious, the honest part
of the network can simply ignore their stake. This is not possible in the case of Seath,
because not just the Seath participants, but _all_ Cardano participants would have to agree
to ignore the malicious Seath leader's stake.

To avoid this problem, it is imperative that a fallback mechanism exists to bypass
Seath entirely.

### Modification to target state machine script necessary

Given a UTXO that represents a state machine, its script can support
integration with Seath by simply requiring that any consuming transaction
is signed by the leader. Every N time, there is a timeslot M big where
this constraint is no longer checked.

### Leader election for multi-UTXO protocols

For some protocols, it is unnecessary to have a full leader election
(and subsequently staking and VRFs) mechanism, as a pseudo-election already happens
through choosing a UTXO.
For example, some protocols support making "pools" which then experience contention
with a high number of users. As users are free to choose the pool, pools themselves
experience an election. Thus, having a single leader per pool is not a problem.
The creator of the pool can choose the leader (who is likely to be themselves),
and the leader can reap some fees as part of the protocol.

### Leader election for single-UTXO protocols

Some protocols have a single UTXO that experiences contention.
The distinction between this category and the former isn't binary, but can be
seen as a consequence of _how important_ throughput is for that UTXO (which
represents a state machine). In the above example with pools, a dead leader
means users must make use of the fallback mechanism to recover their funds.
If this problem is to be reduced, i.e. the chance of a dead leader is problematic,
then _leader election_ must happen.

This mimicks Ouroboros, and as such, there must also be a way to _measure stake_.

#### Differences from Cardano

Unlike Cardano, it is highly unlikely there is anything resembling a UTXO
inside the state machine. It is thus not useful to make use of multiple
concurrent blockchains a la Ouroboros Leios.

The networking is simplified as explained in the [Off-chain](#off-chain) section.

Stake delegation is notably crucial.

The fee system depends on the protocol in question, some protocols may be able
to go without any stake rewards entirely, besides the leader fee.

Rollbacks, competing chains, etc. are also not a worry, as those are guaranteed
by the underlying blockchain (Cardano in our case).

Most other parts are heavily simplified if possible or not included at all.

#### Coin flip protocol

(Fill out from https://eprint.iacr.org/2017/573.pdf)

### Off-chain

Using Seath should ideally be a simple as possible.
Given an off-chain framework (e.g. CTL, Lucid), rather than
submitting a transaction upstream to perform the step, a
transaction to lock the funds for the step must be submitted to the
leader using a custodial script (as used for batching).

Networking in this case is a complicated topic, but for the MVP,
it will be assumed that the leader has an openly accessible IPv4 address/port.
The data will be sent over IPv4 to the specified address/port, encrypted using
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
