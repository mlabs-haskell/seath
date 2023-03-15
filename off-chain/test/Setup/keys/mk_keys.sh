#!/usr/bin/env bash

# Make payment and staking keys
# e.g.: ./mk_keys.sh --testnet-magic=1 ./seath_keys/5    

set -euo pipefail

MAGIC=$1
OUT_DIR=$2

cardano-cli address key-gen \
--verification-key-file ${OUT_DIR}/payment.vkey \
--signing-key-file ${OUT_DIR}/payment.skey

cardano-cli stake-address key-gen \
--verification-key-file ${OUT_DIR}/stake.vkey \
--signing-key-file ${OUT_DIR}/stake.skey

cardano-cli address build \
--payment-verification-key-file ${OUT_DIR}/payment.vkey \
--stake-verification-key-file ${OUT_DIR}/stake.vkey \
--out-file ${OUT_DIR}/address.addr \
${MAGIC}