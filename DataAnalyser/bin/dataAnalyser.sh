#!/bin/bash
if [ "$#" -ne 3 ]; then
  echo "Usage: $0 start_date[yyyymmdd] end_date[yyyymmdd] ticker"
  exit 1;
fi
cd /Users/vincenttam/Dev/ScalaProjects/DataAnalyser
echo "Running with start_date $1, end_date $2 and ticker $3"
sbt "run-main dataanalyser.DataAnalyser --start $1 --end $2 --ticker $3"
echo "Done"
