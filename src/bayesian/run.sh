#!/usr/bin/env bash
set -euo pipefail

SCRIPT="10_mcmc.R"
MODEL_NAME="${1:-0_base}"
LOG_FILE="logs/${MODEL_NAME}.log"

mkdir -p logs

nohup Rscript "$SCRIPT" "$MODEL_NAME" > "$LOG_FILE" 2>&1 &

echo "Started model '$MODEL_NAME' in background"
echo "PID: $!"
echo "Log file: $LOG_FILE"