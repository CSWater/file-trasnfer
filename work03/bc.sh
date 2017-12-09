#!/bin/bash
DEST_BC="./testcases/bc/"
DEST_LL="./testcases/ll/"
SRC="./testcases/"
bc=".bc"
ll=".ll"
rm $DEST_BC/*
rm $DEST_LL/*

#generate bc
FILE_LIST=`ls ./testcases/`
for file in $FILE_LIST
do
  if [ -f "$SRC$file" ]
  then 
    file_name=$(basename $file .c)${bc}
    echo "generate $file_name, OK!";
    clang -emit-llvm -g3 -c ${SRC}${file} -o ${DEST_BC}${file_name}
#    llvm-dis ${DEST_BC}${file_name} -o ${DEST_LL}$(basename $file_name .bc)${ll}
  fi
done

#generate SSA
FILE_LIST=`ls ./testcases/bc/`
for file in $FILE_LIST
do
  if [ -f "$DEST_BC$file" ]
  then
    file_name=$(basename $file .bc).opt
    opt -mem2reg ${DEST_BC}${file} -o ${DEST_LL}${file_name}
    llvm-dis ${DEST_LL}${file_name} -o ${DEST_LL}${file_name}${ll}
  fi
done

rm $DEST_LL/*.opt
