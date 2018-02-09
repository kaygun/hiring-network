#!/bin/bash

SRC=src
DATA=data
FIGURES=figures

CORE=${HOME}/local/lib/mlisp.core
SCRIPT=${SRC}/math-network.lisp
    
FORMAT=svg
GRAPHVIZ="circo -T $FORMAT"

BASENAME=$2
THRESHOLD=$3

if [ $1 = "universities" ]; then
    TYPE=universities
    ATTRIBUTE=1
else
    TYPE=countries
    ATTRIBUTE=2
fi

RAW_EDGES=${DATA}/all-edges.csv

if [ "$4" == "" ]; then
    echo "Using all..."
    RAW_VERTICES=${DATA}/all-vertices.csv
else
    echo "Using selected..."
    egrep $4 ${DATA}/all-vertices.csv > ${DATA}/${TYPE}-selected-vertices.csv
    RAW_VERTICES=${DATA}/${TYPE}-selected-vertices.csv
fi

EDGES=${DATA}/${TYPE}-edges-${BASENAME}.csv
VERTICES=${DATA}/${TYPE}-vertices-${BASENAME}.csv

case $2 in
    before-1930)
        TITLE="Flow of Mathematics Ph.D.'s prior to 1930"

        awk -F"\t" '{ if($3<1930) { print $0; } }' \
            ${RAW_EDGES} > ${EDGES}
        
	awk -F"\t" '{ if($5<1930) { print $0; } }' \
            ${RAW_VERTICES} > ${VERTICES}
        ;;
    1930-1950)
        TITLE="Flow of Mathematics Ph.D.'s between 1930 and 1950"
        
	awk -F"\t" '{ if($3>1929 && $3<1950) { print $0; } }' \
            ${RAW_EDGES} > ${EDGES}

	awk -F"\t" '{ if($5>1929 && $5<1950) { print $0; } }' \
            ${RAW_VERTICES} > ${VERTICES}
        ;;
    1950-1975)
        TITLE="Flow of Mathematics Ph.D.'s between 1950 and 1975" 

	awk -F"\t" '{ if($3>1949 && $3<1975) { print $0; } }' \
            ${RAW_EDGES} > ${EDGES}

	awk -F"\t" '{ if($5>1949 && $5<1975) { print $0; } }' \
            ${RAW_VERTICES} > ${VERTICES}
        ;;
    1975-2000)
        TITLE="Flow of Mathematics Ph.D.'s between 1975 and 2000"

        awk -F"\t" '{ if($3>1974 && $3<2000) { print $0; } }' \
            ${RAW_EDGES} > ${EDGES}

	awk -F"\t" '{ if($5>1974 && $5<2000) { print $0; } }' \
            ${RAW_VERTICES} > ${VERTICES}
        ;;
    1950-2000)
        TITLE="Flow of Mathematics Ph.D.'s between 1950 and 2000"

        awk -F"\t" '{ if($3>1949 && $3<2001) { print $0; } }' \
            ${RAW_EDGES} > ${EDGES}

	awk -F"\t" '{ if($5>1949 && $5<2001) { print $0; } }' \
            ${RAW_VERTICES} > ${VERTICES}
        ;;
    after-2000)
        TITLE="Flow of Mathematics Ph.D.'s after 2000" 

	awk -F"\t" '{ if($3>1999) { print $0; } }' \
            ${RAW_EDGES} > ${EDGES}

	awk -F"\t" '{ if($5>1999) { print $0; } }' \
            ${RAW_VERTICES} > ${VERTICES}
        ;;
esac

sbcl --core ${CORE} \
     --script ${SCRIPT} \
     ${VERTICES} \
     ${EDGES} \
     ${ATTRIBUTE} \
     ${THRESHOLD} \
     "${TITLE}" \
    | grep -v NIL \
    | grep -v \"\" \
    > ${FIGURES}/${TYPE}-${BASENAME}.dot

${GRAPHVIZ} \
 -o ${FIGURES}/${TYPE}-${BASENAME}.$FORMAT \
    ${FIGURES}/${TYPE}-${BASENAME}.dot

if [ "$5" == "us-removed" ]; then
   grep -v states ${FIGURES}/${TYPE}-${BASENAME}.dot \
       | sed s/with/US\ removed,\ with/ \
       > ${FIGURES}/${TYPE}-${BASENAME}-us-removed.dot

   ${GRAPHVIZ} \
    -o ${FIGURES}/${TYPE}-${BASENAME}-us-removed.$FORMAT \
       ${FIGURES}/${TYPE}-${BASENAME}-us-removed.dot
fi
