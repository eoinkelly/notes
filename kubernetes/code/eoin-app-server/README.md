# App server example

* load balanced nginx with outside access

```bash
# create initial deploy.yml
kubectl create deploy eoin-app-server --image=nginx --replicas=3 --dry-run=client -o yaml > deploy.yml


kubectl port-forward pod/eoin-app-server-cfd5fc55c-22tgp 8090:80

# create initial service.yml
kubectl expose deployment eoin-app-server --port 80 --target-port 80 --name eoin-app --dry-run=client -o yaml > service.yml


kubectl create ingress demo-localhost --class=nginx --rule=demo.localdev.me/*=demo:80
```

Q: how to get logs from all 3 pods?