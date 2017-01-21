#!/usr/bin/env bash
CWD=`pwd`
echo $CWD
scala -classpath "$CWD/../target/scala-2.11/stockanalyser-2.11_1.0.jar" com.vsftam.stockanalyser.DataPersister
