FROM maven:3.8.3-openjdk-16 as build

ENV SCALA_VERSION 2.12.15
ENV SBT_VERSION 1.5.5

ENV PATH /usr/sbt/bin:$PATH

# Install Scala
## Piping curl directly in tar
RUN \
  curl https://downloads.typesafe.com/scala/$SCALA_VERSION/scala-$SCALA_VERSION.tgz | tar xfz - -C /root/ && \
  echo >> /root/.bashrc && \
  echo "export PATH=~/scala-$SCALA_VERSION/bin:$PATH" >> /root/.bashrc

# Install SBT
RUN curl -L -o /usr/sbt.tar.gz https://github.com/sbt/sbt/releases/download/v1.5.5/sbt-1.5.5.tgz
RUN cd usr && tar xfz sbt.tar.gz
RUN sbt sbtVersion -Dsbt.rootdir=true

RUN mkdir -p /usr/application


COPY ./core /usr/application/core/
RUN cd /usr/application/core && \
    sbt publishM2 -Dsbt.rootdir=true

COPY ./plugin /usr/application/plugin/
RUN cd /usr/application/plugin && \
    mvn clean install




FROM maven:3.8.3-openjdk-16

ENV _JAVA_OPTIONS "-Xmx10G -Xss248m"
ENV MAVEN_OPTS "-Xmx10G -Xss248m"

COPY --from=build /root/.m2/repository/reachable-method-analysis-core/ /root/.m2/repository/reachable-method-analysis-core/
COPY --from=build /root/.m2/repository/org/tud/ /root/.m2/repository/org/tud/

RUN mkdir -p /usr/benchmark
RUN mkdir -p /data
COPY ./test /usr/benchmark/projects

RUN chmod +x /usr/benchmark/projects/run.sh

WORKDIR /usr/benchmark/projects/

ENTRYPOINT /usr/benchmark/projects/run.sh
