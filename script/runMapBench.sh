export CP=~/src/scala/build/quick/classes/library/:../target/scala_2.8.0-quick/classes:../target/scala_2.8.0-quick/resources
export J_OPTS="-Xms1G -Xmx1G -XX:+UseCompressedOops -XX:+DoEscapeAnalysis -XX:+UseParallelGC -XX:+UseParallelOldGC"
# Include a better OpenHashMap here
export MAPS="scala.collection.mutable.HashMap scala.collection.mutable.OpenHashMap util.ChainedHashMap"
export SIZES="20 100 1500 36864 120000"
for search_type in r s
do
  for m in $MAPS
  do
    java $J_OPTS -cp $CP jsr166.scala.MapMicroBenchmark $m $search_type
  done
done

for key_type in int float string object
do
  for size in $SIZES
  do
    for m in $MAPS
    do
      java $J_OPTS -cp $CP jsr166.scala.MapCheck $m $key_type 100 $size true
    done
  done
done

for size in $SIZES
do
  for m in $MAPS
  do
    java $J_OPTS -cp $CP jsr166.scala.DenseMapMicroBenchmark $m $size
  done
done
  
for m in $MAPS
do
  java $J_OPTS -cp $CP jsr166.scala.MapWordLoops $m
done