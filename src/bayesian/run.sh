#!/usr/bin/env bash
set -euo pipefail

SCRIPT="${1:-10_mcmc.R}"
SCRIPT_TRIMMED="${SCRIPT:3:${#SCRIPT}-5}"
MODEL_NAME="${2:-v0.01a}"
LOG_FILE="logs/${MODEL_NAME}_${SCRIPT_TRIMMED}.log"

mkdir -p logs

nohup Rscript "$SCRIPT" "$MODEL_NAME" > "$LOG_FILE" 2>&1 &

echo "Started model '$MODEL_NAME' in background"
echo "PID: $!"
echo "Log file: $LOG_FILE"