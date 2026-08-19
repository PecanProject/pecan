#!/bin/bash
# Submit a command to the site scheduler, or run it in this shell if none.
#
# Product scripts have no #SBATCH / #PBS / #$ headers. This is the one place
# that talks to Slurm (sbatch) or Grid Engine / PBS (qsub). Tile vs statewide
# stays an env flag on the command (--tile / DEMO_TILE / unset).
#
# Usage:
#   source setup_env.sh   # sets CCMMF_SUBMIT to this file
#   "$CCMMF_SUBMIT" [options] -- command [args...]
#
# Options:
#   -n NAME     job name (default: ccmmf)
#   -c CPUS     CPUs (default: 4)
#   -m MEM      memory, scheduler native form (default: 16G)
#   -t TIME     wall time as HH:MM:SS or hours (default: 12:00:00)
#   --local     run in the foreground here (ignore scheduler)
#
# Scheduler (first match):
#   CCMMF_SCHEDULER=slurm|sge|pbs|local
#   else sbatch+squeue on PATH -> slurm
#   else qsub on PATH -> sge if SGE_ROOT is set, else pbs
#   else local
#
# Extra native flags (account, queue, partition): CCMMF_SUBMIT_EXTRA
# SGE parallel environment name: CCMMF_SGE_PE (default: omp)
# SGE memory resource:          CCMMF_SGE_MEM_RES (default: mem_free)
#
# Activate pecan-all and source setup_env.sh before submitting so the job
# inherits PATH and CCMMF_* vars (sbatch --export=ALL; qsub -V).
#
# Examples:
#   "$CCMMF_SUBMIT" -n hls-earthdata -t 48:00:00 -- \
#     "$CCMMF_CODE/hls/download_hls_earthdata.sh"
#   "$CCMMF_SUBMIT" -n ndti -- \
#     "$TILLAGE_ROOT/run_ndti.sh" "$PRIOR_YEAR" "$TARGET_YEAR"

set -euo pipefail

usage() {
  sed -n '2,34p' "$0" | sed 's/^# \{0,1\}//'
  exit "${1:-0}"
}

NAME="ccmmf"
CPUS="4"
MEM="16G"
TIME_RAW="12:00:00"
FORCE_LOCAL=0
CMD=()

normalize_time() {
  local t="$1"
  if [[ "$t" =~ ^[0-9]+$ ]]; then
    printf '%s:00:00' "$t"
  elif [[ "$t" =~ ^[0-9]+:[0-9]+$ ]]; then
    printf '%s:00' "$t"
  else
    printf '%s' "$t"
  fi
}

detect_scheduler() {
  local s
  s="$(tr '[:upper:]' '[:lower:]' <<< "${CCMMF_SCHEDULER:-}")"
  if [[ -n "$s" ]]; then
    printf '%s' "$s"
    return
  fi
  if command -v sbatch >/dev/null 2>&1 && command -v squeue >/dev/null 2>&1; then
    printf 'slurm'
    return
  fi
  if command -v qsub >/dev/null 2>&1; then
    if [[ -n "${SGE_ROOT:-}" ]]; then
      printf 'sge'
    else
      printf 'pbs'
    fi
    return
  fi
  printf 'local'
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help) usage 0 ;;
    -n) shift; [[ $# -gt 0 ]] || { echo "ERROR: -n needs a name" >&2; exit 1; }; NAME="$1" ;;
    -c) shift; [[ $# -gt 0 ]] || { echo "ERROR: -c needs a CPU count" >&2; exit 1; }; CPUS="$1" ;;
    -m) shift; [[ $# -gt 0 ]] || { echo "ERROR: -m needs a memory size" >&2; exit 1; }; MEM="$1" ;;
    -t) shift; [[ $# -gt 0 ]] || { echo "ERROR: -t needs a wall time" >&2; exit 1; }; TIME_RAW="$1" ;;
    --local) FORCE_LOCAL=1 ;;
    --) shift; CMD=("$@"); break ;;
    -*) echo "ERROR: unknown option $1 (try --help)" >&2; exit 1 ;;
    *) CMD=("$@"); break ;;
  esac
  shift
done

if ((${#CMD[@]} == 0)); then
  usage 1
fi

TIME="$(normalize_time "$TIME_RAW")"
SCHEDULER="$(detect_scheduler)"
if (( FORCE_LOCAL )); then
  SCHEDULER="local"
fi

SUBMIT_DIR="$(pwd)"

EXTRA=()
if [[ -n "${CCMMF_SUBMIT_EXTRA:-}" ]]; then
  # shellcheck disable=SC2206
  EXTRA=( ${CCMMF_SUBMIT_EXTRA} )
fi

if [[ "$SCHEDULER" == "local" ]]; then
  echo "submit_job: local name=$NAME cpus=$CPUS"
  echo "submit_job: $(printf '%q ' "${CMD[@]}")"
  exec "${CMD[@]}"
fi

JOBDIR="${CCMMF_SUBMIT_LOGDIR:-$SUBMIT_DIR}/.ccmmf-jobs"
mkdir -p "$JOBDIR"
RUNNER="$(mktemp "$JOBDIR/${NAME}.XXXXXX.sh")"

{
  echo '#!/bin/bash'
  echo '#$ -S /bin/bash'
  echo 'set -euo pipefail'
  printf 'cd %q\n' "$SUBMIT_DIR"
  echo 'if [[ -n "${PBS_O_WORKDIR:-}" ]]; then cd "$PBS_O_WORKDIR"; fi'
  printf 'export CCMMF_JOB_CPUS=%q\n' "$CPUS"
  printf 'exec'
  for a in "${CMD[@]}"; do printf ' %q' "$a"; done
  echo
} > "$RUNNER"
chmod +x "$RUNNER"

echo "submit_job: scheduler=$SCHEDULER name=$NAME cpus=$CPUS mem=$MEM time=$TIME"
echo "submit_job: $(printf '%q ' "${CMD[@]}")"

case "$SCHEDULER" in
  slurm)
    sbatch \
      --job-name="$NAME" \
      --nodes=1 \
      --ntasks=1 \
      --cpus-per-task="$CPUS" \
      --mem="$MEM" \
      --time="$TIME" \
      --export=ALL \
      --output="${SUBMIT_DIR}/${NAME}-%j.out" \
      --error="${SUBMIT_DIR}/${NAME}-%j.err" \
      "${EXTRA[@]}" \
      "$RUNNER"
    ;;
  sge)
    PE="${CCMMF_SGE_PE:-omp}"
    MEMRES="${CCMMF_SGE_MEM_RES:-mem_free}"
    qsub \
      -N "$NAME" \
      -cwd \
      -V \
      -S /bin/bash \
      -pe "$PE" "$CPUS" \
      -l "h_rt=${TIME}" \
      -l "${MEMRES}=${MEM}" \
      -o "${SUBMIT_DIR}/${NAME}.\$JOB_ID.out" \
      -e "${SUBMIT_DIR}/${NAME}.\$JOB_ID.err" \
      "${EXTRA[@]}" \
      "$RUNNER"
    ;;
  pbs)
    qsub \
      -N "$NAME" \
      -V \
      -l "select=1:ncpus=${CPUS}:mem=${MEM}" \
      -l "walltime=${TIME}" \
      -o "${SUBMIT_DIR}/${NAME}.o" \
      -e "${SUBMIT_DIR}/${NAME}.e" \
      "${EXTRA[@]}" \
      "$RUNNER"
    ;;
  *)
    echo "ERROR: unknown CCMMF_SCHEDULER=$SCHEDULER (use slurm|sge|pbs|local)" >&2
    exit 1
    ;;
esac
