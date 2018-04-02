#!/usr/bin/env bash
#
# Run all tests.

set -e # Die on first error.

base_dir="$(dirname "$0")"
siris="$base_dir/../bin/siris"

# Find the directory containing the test programs.
tests_dir="$1"
if ! [ "$tests_dir" ]; then
    tests_dir="$base_dir/../tests"
fi
tests_dir="$(echo "$tests_dir" | sed 's/\/*$//')"

# Remove all whitespace and NUL bytes when comparing results, because
# Mars and the interpreter puts different amounts -- and to handle
# Windows/OSX/Unix line ending differences.
fix_whitespace() {
    cat "$1" | tr -d '\000' | tr -d ' \t\n\r\f' 1>&1
}

check_equal() {
    if [ -f $tests_dir/$OUTPUT ]; then

        EXPECTED=$(fix_whitespace "$tests_dir/$OUTPUT")
        ACTUAL=$(fix_whitespace "$TESTOUT")
        if [ "$EXPECTED" = "$ACTUAL" ]; then
            rm -f $TESTOUT
        else
            echo "Output for $PROG does not match expected output."
            echo "Compare $TESTOUT and $tests_dir/$OUTPUT."
            return 1
        fi
    fi
}

make -C "$base_dir/.."

file_len=0
for FO in $tests_dir/*si; do
    L=$(basename "$FO")
    if ((${#L} > $file_len)); then
        file_len=${#L}
    fi
done
file_len=$(($file_len+4))


## checks if 'tac' is installed on system
if ! [ -x "$(command -v tac)" ]; then
    echo "Program 'tac' is needed to run this script"
    echo "It's used for reversing a file inorder to "
    echo "run the programs in reverse"
    exit 1
fi


echo
echo "=========== Running Siris test programs ==========="
echo
for FO in $tests_dir/*.si; do
    FO=$(basename "$FO")
    PROG=$(echo $FO|sed 's/.si$//')
    INPUT=$(echo $FO|sed 's/si$/in/')
    OUTPUT=$(echo $FO|sed 's/si$/out/')
    ERROUT=$(echo $FO|sed 's/si$/err/')
    TESTOUT=$tests_dir/$OUTPUT-testresult

    if [ -f $tests_dir/$INPUT ]; then
        # Is positive test.
        echo -n "Testing forward  "
        printf "%*s" $file_len " $FO:  "
        # Interpret.
        cat $tests_dir/$INPUT | $siris -f $tests_dir/$FO  > $TESTOUT #2>&1
        if check_equal; then
            echo -e "\033[92mSuccess.\033[0m"
        else
            echo -e "\033[91mInterpretation error.\033[0m"
        fi
        ## SHOUDL CHECK IF PROGRAM EXISTS
        tac $tests_dir/$INPUT > $tests_dir/$OUTPUT-reversed
        tac $tests_dir/$OUTPUT > $tests_dir/$INPUT-reversed
        echo -n "Testing backwards"
        printf "%*s" $file_len " $FO:  "
        cat $tests_dir/$INPUT-reversed | $siris -r $tests_dir/$FO  > $TESTOUT #2>&1
        OUTPUT=$OUTPUT-reversed
        if check_equal; then
            echo -e "\033[92mSuccess.\033[0m"
        else
            echo -e "\033[91mInterpretation error.\033[0m"
        fi
        rm -f $tests_dir/$OUTPUT
        rm -f $tests_dir/$INPUT-reversed
    fi
done
