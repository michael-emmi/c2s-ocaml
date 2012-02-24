#!/bin/bash

default_boogie="BoogieClient.ruby"

PARSE_TESTS=" \
    $(find src/test/cp -name "*.cp") \
    $(find src/test/bp -name "*.bp") \
    $(find src/test/bpl -name "*.bpl") \
	"

VERIF_TESTS=" \
    $(find src/test/cp -name "*.cp") \
	"

logdir="test-log"

# Necessary tools
c2s=
boogie=
time="$(which time) -p"

timeout=1

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
	$rerun 1> $out 2> $err
}

function check-result () {
	output=$1
	
	if [[ $(cat $output | grep "\<0\> errors") ]]
	then
		echo "safe"
	elif [[ $(cat $output | grep "errors") ]]
	then
		echo "error"
	else
		echo "?"
	fi
}

## Ensure everything is in place to actually run these tests.
function check-env () {
	FAILED=0

	echo 
	echo "Checking environment"
	echo "--------------------"

	printf " * locating \`c2s\` .......... "
	c2s=$(find bin -name c2s)
	if [[ -f $c2s ]]
	then
		echo "√ (using \`$c2s\`)"
	else
		echo "X"
		FAILED=$[ ${FAILED} + 1 ]
	fi

	printf " * locating \`Boogie\` .......... "
	boogie=$(which $default_boogie)
	if [[ -f $boogie ]]
	then
		echo "√ (using \`$boogie\`)"
	else
		echo "X"
		FAILED=$[ ${FAILED} + 1 ]
	fi

	printf " * creating \`$logdir\` .......... "
	if [[ -z $(mkdir -p "$logdir") ]]
	then
		echo "√"
	else
		echo "X"
		FAILED=$[ ${FAILED} + 1 ]
	fi
}

function compare-the-two () {
	
	function boogie-cmd () {
		# echo "$boogie /stratifiedInline:1 /extractLoops /recursionBound:$1 /timeout:$2"
		echo "$boogie /inline:assume /loopUnroll:$1 /timeout:$2"
	}
	
	# srcfiles="\
	# 	$(find . -name "case-posts-*.cp") \
	# 	$(find . -name "fact.fifo.cp") \
	# 	$(find . -name "fifo-filler.cp")"
	srcfiles=$(find . -name "fifo-flipper-*.cp")
	default_unroll=5
	# boogie_flags="/stratifiedInline:1 /extractLoops"
	time_expr="s/[^0-9]*\([0-9]*.[0-9][0-9][0-9]\).*/\1/"
	timeout=100
	
	if [[ -z $c2s ]]
	then
		echo "Skipping, since cannot find \`c2s\`."
		return
	elif [[ -z $boogie ]]
	then
		echo "Skipping, since canont find \`Boogie\`."
	fi
	
	for src in $srcfiles
	do
		# try to read parameters from the source file
		unroll=$(cat $src | grep "@unroll" \
			| sed "s/.*@unroll \([0-9]*\).*/\1/")
		unroll=${unroll:-$default_unroll}
		bfsdepth=$(cat $src | grep "@bfs-depth" \
			| sed "s/.*@bfs-depth \([0-9]*\).*/\1/")
		bfsdepth=${bfsdepth:-3}
		
		for k in $(seq 1 ${bfsdepth})
		do
			bfs[$k]=0
		done
		ub_fail=0
		dfs_fail=0
		bb_fail=0
		bq_fail=0


		echo
		echo "case = \`$src\`,"
		echo "unroll = $unroll, timeout = ${timeout}s"
		echo "N / Bag-DFS / Bag / Bag(N) / FiFo(N) / BFS(1) / .. / BFS($bfsdepth)"
		echo "------------------------------------------------------------"

		for n in $(seq 1 ${unroll})
		do
			fifobound=$[2*$n]
			printf "$n\t"

			# run DFS
			k=0
			if [[ ${dfs_fail} -ge 2 ]]
			then
				printf "S   "
			else
				bpl=$logdir/$(basename $src).dfs.$k.$n.bpl
				out=$logdir/$(basename $src).dfs.$k.$n.stdout
				err=$logdir/$(basename $src).dfs.$k.$n.stderr
				run=$logdir/$(basename $src).dfs.$k.$n.run.sh
				run-test \
					"$c2s $src \
						-task-order dfs -back-end boogie \
					| tee $bpl \
					| $(boogie-cmd $n $timeout)" \
					$out $err $run
			
				if [[ $(cat $out | grep "Timeout") ]]
				then
					printf "T/O "
					dfs_fail=$[${dfs_fail}+1]
			
				elif [[ $(check-result $out) != "safe" ]]
				then
					printf "X   " 
					dfs_fail=$[${dfs_fail}+1]
				else
					printf "$(cat $out | grep "Time:" | sed "$time_expr")"
				fi
			fi
			printf "\t"			

			# run (unbounded?) Bag
			k=-1
			if [[ ${ub_fail} -ge 2 ]]
			then
				printf "S   "
			else
				bpl=$logdir/$(basename $src).ub.$k.$n.bpl
				out=$logdir/$(basename $src).ub.$k.$n.stdout
				err=$logdir/$(basename $src).ub.$k.$n.stderr
				run=$logdir/$(basename $src).ub.$k.$n.run.sh
				run-test \
					"$c2s $src \
						-task-order bounded-bag -fifo-size $k -back-end boogie \
					| tee $bpl \
					| $(boogie-cmd $n $timeout)" \
					$out $err $run
			
				if [[ $(cat $out | grep "Timeout") ]]
				then
					printf "T/O "
					ub_fail=$[${ub_fail}+1]
			
				elif [[ $(check-result $out) != "safe" ]]
				then
					printf "X   " 
					ub_fail=$[${ub_fail}+1]
				else
					printf "$(cat $out | grep "Time:" | sed "$time_expr")"
				fi
			fi
			printf "\t"
			
			# run Bounded-Bag(N)
			for k in $fifobound
			do
				if [[ ${bb_fail} -ge 2 ]]
				then
					printf "S   "
				else
					bpl=$logdir/$(basename $src).bb.$k.$n.bpl
					out=$logdir/$(basename $src).bb.$k.$n.stdout
					err=$logdir/$(basename $src).bb.$k.$n.stderr
					run=$logdir/$(basename $src).bb.$k.$n.run.sh
					run-test \
						"$c2s $src \
							-task-order bounded-bag -fifo-size $k -back-end boogie \
						| tee $bpl \
						| $(boogie-cmd $n $timeout)" \
						$out $err $run
			
					if [[ $(cat $out | grep "Timeout") ]]
					then
						printf "T/O "
						bb_fail=$[${bb_fail}+1]
			
					elif [[ $(check-result $out) != "safe" ]]
					then
						printf "X   " 
						bb_fail=$[${bb_fail}+1]
					else
						printf "$(cat $out | grep "Time:" | sed "$time_expr")"
					fi
				fi
				printf "\t"
			done
						
			# run Bounded-FiFo(N)
			for k in ${fifobound}
			do
				if [[ ${bq_fail} -ge 2 ]]
				then
					printf "S   "
				else
					bpl=$logdir/$(basename $src).bq.$k.$n.bpl
					out=$logdir/$(basename $src).bq.$k.$n.stdout
					err=$logdir/$(basename $src).bq.$k.$n.stderr
					run=$logdir/$(basename $src).bq.$k.$n.run.sh
					run-test \
						"$c2s $src \
							-task-order bounded-fifo -fifo-size $k -back-end boogie \
						| tee $bpl \
						| $(boogie-cmd $n $timeout)" \
						$out $err $run

					if [[ $(cat $out | grep "Timeout") ]]
					then
						printf "T/O "
						bq_fail=$[${bq_fail}+1]
					
					elif [[ $(check-result $out) != "safe" ]]
					then
						printf "X   " 
						bq_fail=$[${bq_fail}+1]
					else
						printf "$(cat $out | grep "Time:" | sed "$time_expr")"
					fi
				fi
				printf "\t"
			done
			
			# run FiFo-Seq(1..K)
			for k in $(seq 1 ${bfsdepth})
			do
				if [[ ${bfs_fail[$k]} -ge 2 ]]
				then
					printf "S   "
				else
					bpl=$logdir/$(basename $src).bfs.$k.$n.bpl
					out=$logdir/$(basename $src).bfs.$k.$n.stdout
					err=$logdir/$(basename $src).bfs.$k.$n.stderr
					run=$logdir/$(basename $src).bfs.$k.$n.run.sh
					run-test \
						"$c2s $src \
							-task-order fifo -bfs-depth $[$k-1] -back-end boogie \
						| tee $bpl \
						| $(boogie-cmd $n $timeout)" \
						$out $err $run
						
					if [[ $(cat $out | grep "Timeout") ]]
					then
						printf "T/O "
						bfs_fail[$k]=$[${bfs_fail[$k]}+1]
			
					elif [[ $(check-result $out) != "safe" ]]
					then
						printf "X   "
						bfs_fail[$k]=$[${bfs_fail[$k]}+1]
					else
						printf "$(cat $out | grep "Time:" | sed "$time_expr")"
					fi
				fi
				printf "\t"
			done


			printf "\n"
			
		done
	done
}

function compare-the-two-rec () {
	
	function boogie-cmd () {
		# echo "$boogie /stratifiedInline:1 /extractLoops /recursionBound:$1 /timeout:$2"
		echo "$boogie /inline:assume /loopUnroll:$1 /timeout:$2"
	}
	
	# srcfiles="\
	# 	$(find . -name "case-posts-*.cp") \
	# 	$(find . -name "fact.fifo.cp") \
	# 	$(find . -name "fifo-filler.cp")"
	srcfiles=$(find . -name "ping-pong.cp")
	default_unroll=5
	# boogie_flags="/stratifiedInline:1 /extractLoops"
	time_expr="s/[^0-9]*\([0-9]*.[0-9][0-9][0-9]\).*/\1/"
	timeout=100
	
	if [[ -z $c2s ]]
	then
		echo "Skipping, since cannot find \`c2s\`."
		return
	elif [[ -z $boogie ]]
	then
		echo "Skipping, since canont find \`Boogie\`."
	fi
	
	for src in $srcfiles
	do
		# try to read parameters from the source file
		unroll=$(cat $src | grep "@unroll" | sed "s/.*@unroll \(.*\)/\1/")
		unroll=${unroll:-$default_unroll}
		phasebound=$(cat $src | grep "@phase-bound" | sed "s/.*@phase-bound \(.*\)/\1/")
		phasebound=${phasebound:-3}
		fifobound=$(cat $src | grep "@fifo-bound" | sed "s/.*@fifo-bound \(.*\)/\1/")
		fifobound=${fifobound:-3}
		
		bfs_fail=0
		bq_fail=0

		echo
		echo "case = \`$src\`,"
		echo "unroll = $unroll, timeout = ${timeout}s"
		echo "N / FiFo($fifobound) / BFS($phasebound)"
		echo "------------------------------------------------------------"

		for n in $(seq ${unroll})
		do
			printf "$n\t"
						
			# run Bounded-FiFo(N)
			for k in $(seq ${fifobound})
			do
				if [[ ${bq_fail} -ge 2 ]]
				then
					printf "S   "
				else
					bpl=$logdir/$(basename $src).bq.$k.$n.bpl
					out=$logdir/$(basename $src).bq.$k.$n.stdout
					err=$logdir/$(basename $src).bq.$k.$n.stderr
					run=$logdir/$(basename $src).bq.$k.$n.run.sh
					run-test \
						"$c2s $src \
							-task-order bounded-fifo -fifo-size $k -back-end boogie \
						| tee $bpl \
						| $(boogie-cmd $n $timeout)" \
						$out $err $run

					if [[ $(cat $out | grep "Timeout") ]]
					then
						printf "T/O "
						bq_fail=$[${bq_fail}+1]
					
					elif [[ $(check-result $out) != "safe" ]]
					then
						printf "X   " 
						bq_fail=$[${bq_fail}+1]
					else
						printf "$(cat $out | grep "Time:" | sed "$time_expr")"
					fi
				fi
				printf "\t"
			done
			
			printf "/\t"
			
			phasebound="$n $n"
			
			# run FiFo-Seq(1..K)
			for k in $(seq ${phasebound})
			do
				if [[ ${bfs_fail} -ge 2 ]]
				then
					printf "S   "
				else
					bpl=$logdir/$(basename $src).bfs.$k.$n.bpl
					out=$logdir/$(basename $src).bfs.$k.$n.stdout
					err=$logdir/$(basename $src).bfs.$k.$n.stderr
					run=$logdir/$(basename $src).bfs.$k.$n.run.sh
					run-test \
						"$c2s $src \
							-task-order fifo -bfs-depth $[$k-1] -recursion-depth $k -back-end boogie \
						| tee $bpl \
						| $(boogie-cmd $n $timeout)" \
						$out $err $run
						
					if [[ $(cat $out | grep "Timeout") ]]
					then
						printf "T/O "
						bfs_fail=$[${bfs_fail}+1]
			
					elif [[ $(check-result $out) != "safe" ]]
					then
						printf "X   "
						bfs_fail=$[${bfs_fail}+1]
					else
						printf "$(cat $out | grep "Time:" | sed "$time_expr")"
					fi
				fi
				printf "\t"
			done


			printf "\n"
			
		done
	done
}


echo "FiFo-Seq vs. Bounded-FiFo Evaluation"
echo "===================================="
check-env
compare-the-two