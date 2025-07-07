---
title: Monitoring and Observability
date: 2024-08-30
tags: monitoring, observability, metrics, logging, tracing
---

# Monitoring and Observability

Effective monitoring and observability are crucial for maintaining reliable systems. The three pillars are metrics, logs, and traces.

## Three Pillars of Observability

### Metrics
Numerical measurements over time:

```python
import time
from prometheus_client import Counter, Histogram, start_http_server

# Counter for requests
REQUEST_COUNT = Counter('requests_total', 'Total requests', ['method', 'endpoint'])

# Histogram for response times
REQUEST_DURATION = Histogram('request_duration_seconds', 'Request duration')

@REQUEST_DURATION.time()
def process_request():
    REQUEST_COUNT.labels(method='GET', endpoint='/api/users').inc()
    # Process request
    time.sleep(0.1)
```

### Logging
Structured event records:

```python
import logging
import json

# Structured logging
logger = logging.getLogger(__name__)

def process_order(order_id, user_id):
    logger.info("Processing order", extra={
        "order_id": order_id,
        "user_id": user_id,
        "action": "order_start"
    })
    
    try:
        # Process order logic
        result = process_payment(order_id)
        logger.info("Order completed successfully", extra={
            "order_id": order_id,
            "action": "order_complete",
            "amount": result.amount
        })
    except PaymentError as e:
        logger.error("Payment failed", extra={
            "order_id": order_id,
            "action": "payment_failed",
            "error": str(e)
        })
```

### Tracing
Request flow across services:

```python
from opentelemetry import trace
from opentelemetry.exporter.jaeger.thrift import JaegerExporter
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.trace.export import BatchSpanProcessor

# Setup tracing
trace.set_tracer_provider(TracerProvider())
tracer = trace.get_tracer(__name__)

jaeger_exporter = JaegerExporter(
    agent_host_name="localhost",
    agent_port=6831,
)

span_processor = BatchSpanProcessor(jaeger_exporter)
trace.get_tracer_provider().add_span_processor(span_processor)

# Use tracing
def get_user_orders(user_id):
    with tracer.start_as_current_span("get_user_orders") as span:
        span.set_attribute("user_id", user_id)
        orders = database.get_orders(user_id)
        span.set_attribute("order_count", len(orders))
        return orders
```

## Key Metrics to Monitor

### Application Metrics
- Request rate, response time, error rate (RED method)
- Business metrics (orders/minute, revenue/hour)
- Custom application metrics

### Infrastructure Metrics  
- CPU, memory, disk usage
- Network I/O, disk I/O
- Database connection pools
- Queue lengths and processing times

## Alerting Best Practices

- Alert on symptoms, not causes
- Use multiple severity levels
- Include runbook links in alerts
- Avoid alert fatigue with proper thresholds