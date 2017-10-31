CURRENT_DIR=$(pwd)
ROOT_DIR=${PWD%/*}
MAIN_DIR=${ROOT_DIR%/*}
INPUT=$CURRENT_DIR/input/itcont.txt
OUTPUT=$CURRENT_DIR/output/
POM_PATH=$MAIN_DIR/pom.xml
JAR_PATH=$MAIN_DIR/target/codeChallenge-1.0-jar-with-dependencies.jar
mvn -f $POM_PATH clean
mvn -f $POM_PATH compile
mvn -f $POM_PATH package
java -jar $JAR_PATH $INPUT $OUTPUT
