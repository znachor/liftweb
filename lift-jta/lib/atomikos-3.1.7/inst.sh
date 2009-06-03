#!/bin/bash

mvn install:install-file -Dfile=pom.xml -DgroupId=com.atomikos -DartifactId=atomikos-parent -Dversion=3.1.7  -Dpackaging=pom
mvn install:install-file -Dfile=atomikos-util/pom.xml -DgroupId=com.atomikos -DartifactId=atomikos-util -Dversion=3.1.7  -Dpackaging=pom
mvn install:install-file -Dfile=transactions-api/pom.xml -DgroupId=com.atomikos -DartifactId=transactions-api -Dversion=3.1.7  -Dpackaging=pom
mvn install:install-file -Dfile=transactions-jta/pom.xml -DgroupId=com.atomikos -DartifactId=transactions-jta -Dversion=3.1.7  -Dpackaging=pom
mvn install:install-file -Dfile=transactions/pom.xml -DgroupId=com.atomikos -DartifactId=transactions -Dversion=3.1.7  -Dpackaging=pom
mvn install:install-file -Dfile=atomikos-util.jar -DgroupId=com.atomikos -DartifactId=atomikos-util -Dversion=3.1.7  -Dpackaging=jar
mvn install:install-file -Dfile=transactions-api.jar -DgroupId=com.atomikos -DartifactId=transactions-api -Dversion=3.1.7  -Dpackaging=jar
mvn install:install-file -Dfile=transactions-jta.jar -DgroupId=com.atomikos -DartifactId=transactions-jta -Dversion=3.1.7  -Dpackaging=jar
mvn install:install-file -Dfile=transactions.jar -DgroupId=com.atomikos -DartifactId=transactions -Dversion=3.1.7  -Dpackaging=jar
