---
title: Web Accessibility Guidelines (WCAG)
date: 2024-09-30
tags: accessibility, wcag, inclusive-design, frontend, standards
---

# Web Accessibility Guidelines (WCAG)

Web accessibility ensures that websites and applications are usable by people with disabilities. Following WCAG guidelines creates inclusive experiences.

## Four Principles (POUR)

### Perceivable
- Provide text alternatives for images
- Offer captions for videos
- Ensure sufficient color contrast
- Make content adaptable to different presentations

### Operable
- Make all functionality keyboard accessible
- Give users enough time to read content
- Don't use content that causes seizures
- Help users navigate and find content

### Understandable
- Make text readable and understandable
- Make content appear and operate predictably
- Help users avoid and correct mistakes

### Robust
- Maximize compatibility with assistive technologies
- Use valid, semantic HTML
- Ensure content works across different browsers and devices

## Implementation Tips

```html
<!-- Semantic HTML -->
<nav aria-label="Main navigation">
  <ul>
    <li><a href="/home">Home</a></li>
    <li><a href="/about">About</a></li>
  </ul>
</nav>

<!-- Proper form labels -->
<label for="email">Email Address</label>
<input type="email" id="email" required aria-describedby="email-help">
<div id="email-help">We'll never share your email</div>

<!-- Alt text for images -->
<img src="chart.png" alt="Sales increased 25% from Q1 to Q2">
```

## Testing Tools

- **Screen readers**: NVDA, JAWS, VoiceOver
- **Automated testing**: axe-core, Lighthouse
- **Manual testing**: Keyboard navigation, color contrast analyzers