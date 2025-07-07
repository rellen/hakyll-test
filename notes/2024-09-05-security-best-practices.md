---
title: Web Application Security Best Practices
date: 2024-09-05
tags: security, web-development, owasp, authentication, best-practices
---

# Web Application Security Best Practices

Security should be built into applications from the ground up. Here are essential practices to protect against common vulnerabilities.

## Authentication & Authorization

### Secure Password Handling

```python
import bcrypt
import secrets

# Hash passwords with salt
def hash_password(password):
    salt = bcrypt.gensalt()
    return bcrypt.hashpw(password.encode('utf-8'), salt)

# Verify passwords
def verify_password(password, hashed):
    return bcrypt.checkpw(password.encode('utf-8'), hashed)

# Generate secure tokens
def generate_token():
    return secrets.token_urlsafe(32)
```

### JWT Best Practices

```javascript
// Short-lived access tokens with refresh tokens
const accessToken = jwt.sign(
  { userId: user.id, role: user.role },
  process.env.JWT_SECRET,
  { expiresIn: '15m' }
);

const refreshToken = jwt.sign(
  { userId: user.id },
  process.env.REFRESH_SECRET,
  { expiresIn: '7d' }
);
```

## Input Validation

```python
from marshmallow import Schema, fields, validate

class UserSchema(Schema):
    email = fields.Email(required=True)
    name = fields.String(required=True, validate=validate.Length(min=1, max=100))
    age = fields.Integer(validate=validate.Range(min=0, max=150))

# Validate input
schema = UserSchema()
try:
    result = schema.load(request.json)
except ValidationError as err:
    return {'errors': err.messages}, 400
```

## SQL Injection Prevention

```python
# Use parameterized queries
cursor.execute(
    "SELECT * FROM users WHERE email = %s AND active = %s",
    (email, True)
)

# Use ORM with proper escaping
user = User.query.filter_by(email=email).first()
```

## XSS Prevention

```javascript
// Sanitize user input
import DOMPurify from 'dompurify';

const cleanHTML = DOMPurify.sanitize(userInput);

// Use CSP headers
app.use((req, res, next) => {
  res.setHeader(
    'Content-Security-Policy',
    "default-src 'self'; script-src 'self' 'unsafe-inline'"
  );
  next();
});
```

## HTTPS & Security Headers

```javascript
// Force HTTPS
app.use((req, res, next) => {
  if (req.header('x-forwarded-proto') !== 'https') {
    res.redirect(`https://${req.header('host')}${req.url}`);
  } else {
    next();
  }
});

// Security headers
app.use(helmet({
  hsts: { maxAge: 31536000 },
  noSniff: true,
  xssFilter: true,
  frameguard: { action: 'deny' }
}));
```