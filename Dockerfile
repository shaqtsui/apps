# Prerequire: reply on xfcjscn/clojure-repo(build via Dockerfile-Clojure-Repo) to avoid download deps
# build command: docker build -t xfcjscn/apps .
# run command: docker run -p 7888:7888 -p 8080:8080 --name apps xfcjscn/apps
FROM xfcjscn/clojure-repo
COPY . /usr/src/app

# COPY, CMD... execute directory, default is /tmp
WORKDIR /usr/src/app
# default command when run container, default is bash
CMD ["/bin/sh", "-c", "java -cp $(lein classpath) clojure.main -m apps.nrepl-cider"]

