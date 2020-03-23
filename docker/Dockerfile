
FROM openjdk:11.0.6-jre-slim-buster

COPY accounting-clj-1.0-standalone.jar /usr/src/accounting.jar

WORKDIR /usr/src

CMD ["java", "-jar", "accounting.jar"]
