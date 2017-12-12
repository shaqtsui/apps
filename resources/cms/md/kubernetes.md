# Use case
Manage containers. 

# Architecture:

Cluster
	Master
	Nodes
		Kubelet - communication between Master & Node
		Container rt - e.g. docker
		*Pod - atomic unit, a group of container & shared: volumes, networking, info about how to run container
		
	Service - logical set of Pods & access policy(expose Pods to outside), hide Pods die & replicate

	Deployment - seems: Pods* + Service



# Software:

Minikube
	lightweight Kubernetes impl
	install: curl -Lo minikube https://storage.googleapis.com/minikube/releases/latest/minikube-linux-amd64 && chmod +x minikube && sudo mv minikube /usr/local/bin/

Kubectl
	Kubernetes CLI, use Kubernetes API to interact with cluster
	install: curl -LO https://storage.googleapis.com/kubernetes-release/release/$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)/bin/linux/amd64/kubectl
	config:
		chmod +x ./kubectl
		mv ./kubectl ~/bin
	

# Commands:
minikube start ;; start cluster

kubectl version
kubectl cluster-info
kubectl get nodes
kubectl run my-first-deployment --image=docker.io/xfcjscn/apps --port=8080 ;; create deployment
kubectl get deployments

kubectl proxy ;; open proxy route request to cluster

kubectl get pods
kubectl describe pods
kubectl logs $POD_NAME


export POD_NAME=$(kubectl get pods -o go-template --template '{{range .items}}{{.metadata.name}}{{"\n"}}{{end}}')        ;; get pod name

kubectl exec $POD_NAME env ;; execute command in container, only one container in pod so ignore container name
kubectl exec -ti $POD_NAME bash

kubectl get services

kubectl expose  deployment/my-first-deployment --type="NodePort" --port 8080   ;; create service
kubectl describe services/my-first-deployment

export NODE_PORT=$(kubectl get services/kubernetes-bootcamp -o go-template='{{(index .spec.ports 0).nodePort}}')

;; check info (label)
kubectl describe deployment
;; qeury by label
kubectl get pods -l run=kubernetes-bootcamp
kubectl get services -l run=kubernetes-bootcamp

;; add new label
kubectl label pod $POD_NAME app=v1

kubectl get pods -l app=v1


kubectl delete service -l run=kubernetes-bootcamp

kubectl scale deployments/kubernetes-bootcamp --replicas=4 ;; scale service via repilate pods

kubectl set image deployments/kubernetes-bootcamp kubernetes-bootcamp=jocatalin/kubernetes-bootcamp:v2 ;; update

kubectl rollout status deployments/kubernetes-bootcamp ;; check update status

kubectl rollout undo deployments/kubernetes-bootcamp ;; rollback update


