Use kindforall
==============

This guide explains how to use kind from a developer host:

# Create a kubeconfig

```ShellSession
$ ssh kindforall create-kubeconfig $USER > ~/.kube/config
$ kubectl cluster-info
Kubernetes master is running at https://<kind-ip>:6443
```

# Deploy hello service

Start the echoserver:

```ShellSession
$ kubectl create deployment hello-kind --image=k8s.gcr.io/echoserver:1.4
deployment.apps/hello-kind created
$ kubectl expose deployment hello-kind --port=8080
service/hello-kind exposed
```

Setup ingress:

```ShellSession
$ kubectl apply -f - <<EOF
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: tristan-deployment
spec:
  rules:
  - host: "tristan.local"
    http:
      paths:
      - pathType: Prefix
        path: "/"
        backend:
          service:
            name: hello-kind
            port:
              number: 8080
EOF
ingress.networking.k8s.io/tristan-deployment configured
```

Configure local host:

```ShellSession
$ echo "<kind-ip> tristan.local" | sudo tee -a /etc/hosts
```

Validate setup:

```ShellSession
$ curl tristan.local
HTTP/1.1 200 OK
...
```
