#!/usr/bin/env bash

# Query UTXOs at address using node runtime container id or name
# E.g.: ./utxos_at.sh e55a855a479d addr_test1wrq4307pceuulya9xzyflpfg8pngwhlx33wmyvla8gvc3us63un8l 

set -euo pipefail

CONTAINER_ID=$1
ADDRESS=$2

docker exec -ti ${CONTAINER_ID} sh -c "CARDANO_NODE_SOCKET_PATH=/ipc/node.socket cardano-cli query utxo --testnet-magic=1 --out-file=/dev/stdout --address ${ADDRESS}"