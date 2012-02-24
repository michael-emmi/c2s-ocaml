#!/bin/bash

PARSE_TESTS=" \
    $(find src/test/cp -name "*.cp") \
    $(find src/test/bp -name "*.bp") \
    $(find src/test/bpl -name "*.bpl") \
	"

VERIF_TESTS=" \
    $(find src/test/cp -name "*.cp") \
	"

LOGDIR=test-log

# Necessary tools
C2S=
BOOGIE=
GETAFIX=
MOPED=

TIMEOUT=1

## Run a given command $1, redirecting output to $2, error to $3, and saving
## the command in an executable script.
function run-test () {
	cmd=$1
	out=$2
	err=$3
	rerun=$4

	echo "#!/bin/bash" > $rerun
	echo "$cmd" >> $rerun
	chmod u+x $rerun
	$cmd 1> $out 2> $err
}

## Ensure everything is in place to actually run these tests.
function check-env () {
	FAILED=0

	echo 
	echo "Checking environment"
	echo "--------------------"

	printf " * locating \`c2s\` .......... "
	C2S=$(find bin -name c2s)
	if [[ -f ${C2S} ]]
	then
		echo "√"
	else
		echo "X"
		FAILED=$[ ${FAILED} + 1 ]
	fi

	printf " * locating \`Boogie\` .......... "
	BOOGIE=$(which Boogie)
	if [[ -f ${BOOGIE} ]]
	then
		echo "√"
	else
		echo "X"
		FAILED=$[ ${FAILED} + 1 ]
	fi

	printf " * creating \`${LOGDIR}\` .......... "
	if [[ -z $(mkdir -p "${LOGDIR}") ]]
	then
		echo "√"
	else
		echo "X"
		FAILED=$[ ${FAILED} + 1 ]
	fi
}

function test-parsers () {
	FAILED=0
	TOTAL=0
	FLAGS=

	echo
	echo "Parser(s) testing"
	echo "-----------------"

	if [[ -z ${C2S} ]]
	then
		echo "Skipping, since cannot find c2s."
		return
	fi

	for src in ${PARSE_TESTS}
	do
		out=${LOGDIR}/$(basename ${src}).parse.stdout
		err=${LOGDIR}/$(basename ${src}).parse.stderr
		run=${LOGDIR}/$(basename ${src}).parse.run.sh

		printf " * \`${src}\` .......... "
		run-test "${C2S} ${FLAGS} $src" $out $err $run
		(( TOTAL ++ ))

		if [[ -z $(cat $out) ]] 
		then
			echo "X"
			(( FAILED ++ ))
			continue
		else
			echo "√"
			continue
		fi
	done

	echo
	echo "${FAILED}/${TOTAL} tests failed."
}

function test-end-to-end () {
	FAILED=0
	TOTAL=0
	C2S_FLAGS="-task-scheduler bfs -task-interleaving 0 -phase-bound 3"
	C2S_FLAGS="${C2S_FLAGS} -back-end boogie"
	BOOGIE_FLAGS=

	if [[ ${TIMEOUT} ]]
	then
		BOOGIE_FLAGS="${BOOGIE_FLAGS} /timeLimit:${TIMEOUT}"
	fi

	echo
	echo "End-to-end verification testing"
	echo "-------------------------------"

	if [[ -z ${C2S} ]]
	then
		echo "Skipping, since cannot find c2s."
		return
	elif [[ -z ${BOOGIE} ]]
	then
		echo "Skipping, since cannot find Boogie."
		return
	fi

	for src in ${VERIF_TESTS}
	do
		feout=${LOGDIR}/$(basename ${src}).fe.stdout
		feerr=${LOGDIR}/$(basename ${src}).fe.stderr
		beout=${LOGDIR}/$(basename ${src}).be.stdout
		beerr=${LOGDIR}/$(basename ${src}).be.stderr
		run=${LOGDIR}/$(basename ${src}).e2e.run.sh
		bpl=${LOGDIR}/$(basename ${src}).e2e.bpl

		expected=$(cat ${src} | grep "@expect error")

		# the front-end #
		printf " * \`${src}\` .......... "

		if [[ $(cat ${src} | grep "@notest") ]]
		then
			echo "O -- found \`@notest\`, skipping"
			continue
		fi

		run-test "${C2S} $src ${C2S_FLAGS}" $feout $feerr $run
		(( TOTAL ++ ))

		if [[ -z $(cat $feout)
					|| -n $(cat $feerr | grep -e "Internal error") ]]
		then
			echo "X -- failed translating"
			(( FAILED ++ ))
			continue
		fi

		# the back-end #
		cp $feout $bpl
		run-test "${BOOGIE} ${BOOGIE_FLAGS} $bpl" $beout $beerr $run

		if [[ -n $(cat $beout | grep -e "0 errors")
					&& -z $expected ]]
		then
			echo "√"
			continue

		elif [[ -n $(cat $beout | grep -e "[1-9][0-9]* errors")
					&& -n $expected ]]
		then
			echo "√"
			continue

		elif [[ -n $(cat $beout | grep -e "0 errors")
					&& -n $expected ]]
		then
			echo "X -- expected error(s), but verified!"
			(( FAILED ++ ))
			continue

		elif [[ -n $(cat $beout | grep -e "[1-9][0-9]* errors")
					&& -z $expected ]]
		then
			echo "X -- expected verified, but found error(s)"
			(( FAILED ++ ))
			continue

		else
			echo "X -- failed reading translation"
			(( FAILED ++ ))
			continue
		fi
	done

	echo
	echo "${FAILED}/${TOTAL} tests failed."
}


echo "Regression Testing Log"
echo "======================"
check-env
test-parsers
test-end-to-end