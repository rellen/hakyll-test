---
title: Kubernetes Deployment Strategies
date: 2024-11-05
tags: kubernetes, devops, deployment, containers, cloud
---

# Kubernetes Deployment Strategies

Kubernetes offers several deployment strategies to update applications with minimal downtime and risk.

## Rolling Updates

The default strategy that gradually replaces old pods with new ones:

```yaml
spec:
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxUnavailable: 1
      maxSurge: 1
```

## Blue-Green Deployments

Maintain two identical environments and switch traffic between them:

```yaml
apiVersion: v1
kind: Service
metadata:
  name: my-app
spec:
  selector:
    app: my-app
    version: blue  # Switch to 'green' for deployment
```

## Canary Deployments

Gradually shift traffic to new version to minimize risk of bugs affecting all users.