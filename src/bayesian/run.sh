#!/usr/bin/env bash
set -euo pipefail

SCRIPT="${1:-10_mcmc.R}"
SCRIPT_TRIMMED="${SCRIPT:3:${#SCRIPT}-5}"
MODEL_NAME="${2:-v0.01a}"
REFERENCE_DATE="${3:-}"

if [[ -n "$REFERENCE_DATE" ]]; then
  LOG_FILE="logs/${MODEL_NAME}_${REFERENCE_DATE}_${SCRIPT_TRIMMED}.log"
else
  LOG_FILE="logs/${MODEL_NAME}_${SCRIPT_TRIMMED}.log"
fi

mkdir -p logs

if [[ -n "$REFERENCE_DATE" ]]; then
  nohup Rscript "$SCRIPT" "$MODEL_NAME" "$REFERENCE_DATE" > "$LOG_FILE" 2>&1 &
else
  nohup Rscript "$SCRIPT" "$MODEL_NAME" > "$LOG_FILE" 2>&1 &
fi

echo "Started model '$MODEL_NAME' in background"
echo "PID: $!"
echo "Log file: $LOG_FILE"
