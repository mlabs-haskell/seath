#!/usr/bin/env bash

# Query UTXOs at address using runtime container  

set -euo pipefail

CONTAINER_ID=$1
ADDRESS=$2

docker exec -ti ${CONTAINER_ID} sh -c "CARDANO_NODE_SOCKET_PATH=/ipc/node.socket cardano-cli query utxo --testnet-magic=1 --out-file=/dev/stdout --address ${ADDRESS}"