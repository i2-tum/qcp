#!/usr/bin/env bash

# change to the directory where this script is located in independent where it was called from
cd $(dirname "$0")
echo "[Info] Current working directory is now $PWD"

echo "[Info] Activate python environment, if an error occurs check setup instructions"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
pyenv activate env3.10 2>/dev/null
echo "[Info] Python version is now $(python --version) (should be 3.10.10)"

# look for configuration file
if [ -f "bench.yaml" ]; then
    echo "[Info]: Configuration file found: $(yq '.name' bench.yaml)"
else
    echo "[Error]: No configuration file 'bench.yaml' found."
    exit -1
fi

# read data from config file
export ROOT_DIR=$(yq '.root-dir' bench.yaml)
echo "[Info]: Root directory set to $ROOT_DIR"
export TIMEOUT=$(yq '.timeout' bench.yaml)
echo "[Info]: Timeout set to ${TIMEOUT}s"

# create paths to log files
LOG_FILE="$ROOT_DIR/logs/$(date +'%Y-%m-%d_%H-%M-%S').log.yaml"
echo "[Info]: Log will be written to $LOG_FILE"
ERR_FILE="$ROOT_DIR/logs/$(date +'%Y-%m-%d_%H-%M-%S').err.ansi"
echo "[Info]: Errors will be written to $ERR_FILE"
export LOG_DIR="$ROOT_DIR/logs/$(date +'%Y-%m-%d_%H-%M-%S')"

# start a log file
mkdir -p "${LOG_FILE%/*}"
echo "jobs:" > "$LOG_FILE"
mkdir "$LOG_DIR"

# check whether benchmark circuits are already downloaded
GEN_BENCH=false
if [ -d "$ROOT_DIR/orig" ]; then
    while true; do
        read -n1 -p "Benchmark circuits detected, do you want to generate them anyways? (existing files will be deleted) [Y,n]: " yn
        case $yn in
            [Yy] )
                rm -rf "$ROOT_DIR/orig/*"
                GEN_BENCH=true
                echo ""
                break;;
            [Nn] )
                echo ""
                break;;
            * )
                echo -e "\nType y or n.";;
        esac
    done
else
    mkdir "$ROOT_DIR/orig"
    GEN_BENCH=true
fi

# check whether there are already evaluated circuits, ask for permission to overwrite them
EVAL_BENCH=false
if [ "$(find "$ROOT_DIR" -mindepth 1 -maxdepth 1 -type d -not -name "orig" -not -name "logs" -not -name ".*")" ] ; then
    while true; do
        read -n1 -p "Processed circuits detected, do you want to re-evaluate them? (existing files will be deleted) [Y,n]: " yn
        case $yn in
            [Yy] )
                find "$ROOT_DIR" -mindepth 1 -maxdepth 1 -type d -not -name "orig" -not -name "logs" -not -name ".*" -exec rm -rf -- {} +
                EVAL_BENCH=true
                echo ""
                break;;
            [Nn] )
                echo ""
                break;;
            * )
                echo -e "\nType y or n.";;
        esac
    done
else
    EVAL_BENCH=true
fi

# count and output number of test cases
echo "[Info]: $(yq '.jobs | length' bench.yaml) test cases found"

# ask on how many cores the process should be distributed
if "$EVAL_BENCH"; then
    while true; do
        read -p "How many parallel processes should be used? Type a for all. [a,1,2,...]: " N_PROCS
        case $N_PROCS in
            [Aa] )
                N_PROCS="$(nproc --all)"
                echo -e "[Info]: Use $N_PROCS processes.";
                break;;
            1 )
                echo -e "[Info]: Use $N_PROCS process.";
                break;;
            [2-9]|[1-9][0-9] )
                echo -e "[Info]: Use $N_PROCS processes.";
                break;;
            * )
                echo -e "Type a or any number.";;
        esac
    done
fi

# generate benchmark circuits
if "$GEN_BENCH"; then
    echo "[Info]: Download benchmark circuits in $ROOT_DIR/orig"
    if ! {
        curl 'https://www.cda.cit.tum.de/mqtbench/download' \
        -X 'POST' -H 'Referer: https://www.cda.cit.tum.de/mqtbench/' \
        -H 'Origin: https://www.cda.cit.tum.de' \
        -H 'Content-Type: application/x-www-form-urlencoded' \
        -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' \
        -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.3 Safari/605.1.15' \
        --data 'all_benchmarks=true&selectBench_1=Amplitude+Estimation+%28AE%29&selectBench_2=Deutsch-Jozsa&selectBench_3=Graph+State&selectBench_4=GHZ+State&selectBench_5=Grover%27s+%28no+ancilla%29&selectBench_6=Grover%27s+%28v-chain%29&selectBench_7=Portfolio+Optimization+with+QAOA&selectBench_8=Portfolio+Optimization+with+VQE&selectBench_9=Quantum+Approximation+Optimization+Algorithm+%28QAOA%29&selectBench_10=Quantum+Fourier+Transformation+%28QFT%29&selectBench_11=QFT+Entangled&selectBench_12=Quantum+Generative+Adversarial+Network&selectBench_13=Quantum+Phase+Estimation+%28QPE%29+exact&selectBench_14=Quantum+Phase+Estimation+%28QPE%29+inexact&selectBench_15=Quantum+Walk+%28no+ancilla%29&selectBench_16=Quantum+Walk+%28v-chain%29&selectBench_17=Variational+Quantum+Eigensolver+%28VQE%29&selectBench_18=Efficient+SU2+ansatz+with+Random+Parameters&selectBench_19=Real+Amplitudes+ansatz+with+Random+Parameters&selectBench_20=Two+Local+ansatz+with+Random+Parameters&selectBench_21=W-State&minQubits=&maxQubits=&selectBench_22=Ground+State&selectBench_23=HHL&selectBench_24=Pricing+Call+Option&selectBench_25=Pricing+Put+Option&selectBench_26=Routing&selectBench_27=Shor%27s&selectBench_28=Travelling+Salesman&indep_qiskit_compiler=true' \
        --output "$ROOT_DIR/bench.zip" ;
        unzip -uo "$ROOT_DIR/bench.zip" -d "$ROOT_DIR/orig" > /dev/null;
        rm -f "$ROOT_DIR/bench.zip" ;
    } ; then
        exit -1
    fi
fi

# run each circuit in orig through all specified settings
process_circuit() {
    FILE="$1"
    SRC=$(basename "$FILE")
    echo "[Info]: Process $FILE"

    LOG_FILE="$LOG_DIR/${SRC%.*}.log.yaml"
    touch "$LOG_FILE"
    echo "file: $FILE" >> "$LOG_FILE"
    echo "start: $(date)" >> "$LOG_FILE"
    echo "steps:" >> "$LOG_FILE"

    ERR_FILE="$LOG_DIR/${SRC%.*}.err.ansi"
    exec 3> "$ERR_FILE"

    count_gates() {
        FILE="$1"
        echo "      gates:" >> "$LOG_FILE"
        for gate in "ccx" "cp" "cry" "cswap" "cu" "cu1" "cu3" "cx" "cz" "ecr" "h" "p" "rccx" "rx" "rxx" "ry" "ryy" "rz" "rzz" "s" "sdg" "swap" "sx" "t" "tdg" "u" "u1" "u2" "u3" "x" "y" "z"; do
            n=$(grep -E "^$gate( |\\()" $FILE | wc -l | tr -d " ")
            echo "        $gate: $n" >> "$LOG_FILE"
        done
    }

    handle_return() {
        RVALUE="$1"
        ERR_FILE="$2"
        if [[ "$RVALUE" != 0 ]]; then
            if [ "$RVALUE" == 124 ]; then
                echo "      time: NA" >> "$LOG_FILE"
                echo "      error: NA" >> "$LOG_FILE"
                echo "[Warn]: Timeout while processing \"$SRC\""
            else
                echo "      error: $ERR_FILE" >> "$LOG_FILE"
                echo "[Error]: Error while processing \"$SRC\""
            fi
        else
            echo "      error: NA" >> "$LOG_FILE"
        fi
    }

    run_job() {
        JOB="$1"
        SRC="$2"
        LOG_FILE="$3"
        ERR_FILE="$4"
        JOB_NAME=$(yq ".jobs.$JOB.name" bench.yaml)
        echo "[Info]: Run job $JOB_NAME"
        echo "  - $JOB:" >> "$LOG_FILE"
        echo "      file: $SRC" >> "$LOG_FILE"
        INPUT=$(yq ".jobs.$JOB.input" bench.yaml)
        OUTPUT="$JOB"
        COMMAND=$(yq ".jobs.$JOB.steps[].run" bench.yaml | sed ':a; N; $!ba; s/\n/ | /g')
        timeout "$TIMEOUT" bash -c "TIMEFORMAT='      time: %U'; time ( cat $ROOT_DIR/$INPUT/$SRC | $COMMAND > $ROOT_DIR/$OUTPUT/$SRC 2>&3 ; ) >&3 2>> $LOG_FILE"
        handle_return "$?" "$ERR_FILE"
        count_gates "$ROOT_DIR/$OUTPUT/$SRC"
    }

    # no opt
    echo "  - orig:" >> "$LOG_FILE"
    echo "      file: $SRC" >> "$LOG_FILE"
    echo "      time: 0" >> "$LOG_FILE"
    echo "      error: NA" >> "$LOG_FILE"
    count_gates "$ROOT_DIR/orig/$SRC"

    # run jobs specified in bench.yaml
    export -f run_job
    export -f handle_return
    export -f count_gates
    yq '.jobs[] | key' bench.yaml | xargs -I {} bash -c "run_job {} $SRC $LOG_FILE $ERR_FILE"

    echo "end: $(date)" >> "$LOG_FILE"
    exec 3>&-
}

if "$EVAL_BENCH"; then
    echo "[Info]: Make directories for processed circuits"
    yq '.jobs[] | key' bench.yaml | xargs -I {} mkdir -p "$ROOT_DIR/{}"
    export -f process_circuit
    echo "[Info]: Start processing circuits"
    find "$ROOT_DIR/orig" -type f -name "*.qasm" | xargs -n 1 -I {} -P "$N_PROCS" bash -c "process_circuit {}"
    echo "[Info]: Combine logs into one log file"
    find "$LOG_DIR" -type f -name "*.log.yaml" -exec bash -c "head -1 {} | sed 's/^/  - /' >> $LOG_FILE; tail -n +2 {} | sed 's/^/    /' >> $LOG_FILE" \;
    echo "[Info]: Generate results table"
    yq '[["file", "method", "time", "ccx", "cp", "cry", "cswap", "cu", "cu1", "cu3", "cx", "cz", "ecr", "h", "p", "rccx", "rx", "rxx", "ry", "ryy", "rz", "rzz", "s", "sdg", "swap", "sx", "t", "tdg", "u", "u1", "u2", "u3", "x", "y", "z"]] + [.jobs.[].steps.[].[] | with(select(.error != "NA"); .time = "NA") | [.file, key, .time, .gates.ccx, .gates.cp, .gates.cry, .gates.cswap, .gates.cu, .gates.cu1, .gates.cu3, .gates.cx, .gates.cz, .gates.ecr, .gates.h, .gates.p, .gates.rccx, .gates.rx, .gates.rxx, .gates.ry, .gates.ryy, .gates.rz, .gates.rzz, .gates.s, .gates.sdg, .gates.swap, .gates.sx, .gates.t, .gates.tdg, .gates.u, .gates.u1, .gates.u2, .gates.u3, .gates.x, .gates.y, .gates.z]] | @csv' \
    "$LOG_FILE" > "$ROOT_DIR/results.csv"
fi

# visualise results in a plot
echo "[Info]: Visualise results"
mkdir -p "$ROOT_DIR/plots"
if ! { Rscript lib/bench.R "$ROOT_DIR/results.csv" 2>> "$ERR_FILE" ; } ; then
    echo "[Error] Error occured while generating plot, probably R is not installed, see requirements"
    exit -1
fi
