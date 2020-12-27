FROM mcr.microsoft.com/java/maven:11u9-zulu-debian10

RUN apt update && apt install -y git
RUN mkdir /src /build
ADD pom.xml /pom.xml
ADD src /src
# RUN cd /src; git clone https://github.com/Multibit-Legacy/multibit.git
RUN mvn package -Dmaven.test.skip=true
RUN cp -prv target/multibit-exe.jar /build/

ENTRYPOINT echo $(ls -lh target/multibit-exe.jar)
