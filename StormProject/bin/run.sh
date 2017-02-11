#!/usr/bin/env bash
CWD=`pwd`
cd $CWD/..
sbt "run-main com.vsftam.stockanalyser.DataPersister"
