# correlate-build-failure

Use this tool to debug CI flakyness.

## Usage

Install the toolchain:

```
sudo dnf install -y ghc cabal-install openssl-devel
```

Get into `tools/correlate-build-failure` directory and then
run the tool by providing the gerrit change to collect build result from:

```
GERRIT_CHANGE=XXX cabal run
```

## Demo

```
$ GERRIT_CHANGE=33249 cabal run
...
https://microshift.softwarefactory-project.io/zuul/t/sf/build/26e78b7b854544d7aa8371e6261312e6 FAILURE {"controller":"2d4ede4ace3a8251de44b5611c85a0e20aa0f753e1164c4afc534ecf"} ["Run ./tools/deploy.sh"]
https://microshift.softwarefactory-project.io/zuul/t/sf/build/a68f1978f91a46a0a0e321b5b9e32704 FAILURE {"controller":"cf3fadb0410d89499fb09ee786da89c4d670d3b57bfbdae3849b96e4"} ["Run ./tools/deploy.sh"]
https://microshift.softwarefactory-project.io/zuul/t/sf/build/f52c08b9656d42099f4e218e72ff49f6 SUCCESS {"controller":"bc62b5ad08c88ca720bbcffb86aeab4ef8c4ed0ef18524e34d267e13"} []
https://microshift.softwarefactory-project.io/zuul/t/sf/build/5f92c4a86cf24748a0a0fff88530ff07 SUCCESS {"controller":"62681191f012177ab7b9845286e2dcd6772acd911995bb41a5e82cb3"} []
- 0e5701940c450a9870373511701178895bc1fafd7c34c463d6bdbb7d success:33 failed:19 [("Grab minikube logs",6),("Run ./tools/deploy.sh",7),("Start minikube",6)]
- 10ebd778fb90707a689fd7a245587c7cf5ca9083e000d9b0b39f07b2 success:19 failed:11 [("Grab minikube logs",4),("Run ./tools/deploy.sh",3),("Start minikube",4)]
- 2d4ede4ace3a8251de44b5611c85a0e20aa0f753e1164c4afc534ecf success:20 failed:10 [("Grab minikube logs",2),("Run ./tools/deploy.sh",6),("Start minikube",2)]
- 57cf2df478d079261d1b34c67c67cc24ad83a8ed8df59c50f8e63e9e success:29 failed:11 [("Grab minikube logs",4),("Run ./tools/deploy.sh",3),("Start minikube",4)]
- 62681191f012177ab7b9845286e2dcd6772acd911995bb41a5e82cb3 success:13 failed:11 [("Grab minikube logs",4),("Run ./tools/deploy.sh",3),("Start minikube",4)]
- 69485118b59ce4e2b7b4cfec4af600aaa745c724d2e5f7756114afe3 success:11 failed:4 [("Grab minikube logs",1),("Run ./tools/deploy.sh",2),("Start minikube",1)]
- 80eab0464eee6bf5ae6562b30be08e17f1c823dd5846a694aaeed666 success:13 failed:12 [("Grab minikube logs",4),("Run ./tools/deploy.sh",4),("Start minikube",4)]
- b539b040588a956d2b6bc38f450e93367907b303435e803f262244a1 success:14 failed:10 [("Grab minikube logs",1),("Run ./tools/deploy.sh",8),("Start minikube",1)]
- bc62b5ad08c88ca720bbcffb86aeab4ef8c4ed0ef18524e34d267e13 success:21 failed:5 [("Grab minikube logs",1),("Run ./tools/deploy.sh",3),("Start minikube",1)]
- cf3fadb0410d89499fb09ee786da89c4d670d3b57bfbdae3849b96e4 success:20 failed:16 [("Grab minikube logs",3),("Run ./tools/deploy.sh",10),("Start minikube",3)]
- d31ae5478fe0a3b5e1b5f3816d67bfa768a346a3e02a4dcaa3736520 success:19 failed:11 [("Grab minikube logs",5),("Run ./tools/deploy.sh",1),("Start minikube",5)]
```
