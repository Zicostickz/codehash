# Codehash

A decentralized marketplace for smart contract templates on the Stacks blockchain, enabling developers to create, share, and monetize reusable smart contract components.

## Overview

Codehash is a decentralized platform that enables developers to:
- Publish and discover reusable smart contract templates
- Monetize their expertise through template sales and licensing
- Build trust through community reviews and reputation
- Participate in platform governance

## Architecture

The platform consists of several key smart contracts that work together:

### Template Registry
The core contract that manages template metadata, ownership, and version history. Features include:
- Template registration and metadata management
- Version control and compatibility tracking
- Template ownership and transfer capabilities

### Marketplace
Handles the economic aspects of the platform, including:
- Template purchases and licensing
- Royalty distribution
- Escrow payments for secure transactions
- Multiple licensing models (one-time, subscription, usage-based)

### Template License
Manages the licensing framework for templates:
- Multiple license types (MIT, GPL, Apache, Commercial, Custom)
- License validation and verification
- Usage tracking for different license models
- License transfer capabilities

### Reputation System
Tracks the reputation and reliability of templates and developers:
- User and template ratings
- Review management
- Usage statistics
- Purchase verification

### Governance
Enables community-driven platform evolution through:
- Proposal creation and voting
- Parameter updates
- Platform policy changes
- Weighted voting based on platform participation

## Smart Contract Interface

### Template Registry

```clarity
;; Register a new template
(define-public (register-template
    (title (string-ascii 50))
    (description (string-ascii 500))
    (tags (list 5 (string-ascii 20)))
    (documentation-url (optional (string-ascii 100)))
    (repository-url (optional (string-ascii 100)))
    (clarity-versions (list 5 (string-ascii 10)))
    (platforms (list 3 (string-ascii 20)))
) => (response uint bool))

;; Publish a new template version
(define-public (publish-template-version
    (template-id uint)
    (version (string-ascii 10))
    (content-hash (buff 32))
    (release-notes (string-ascii 200))
) => (response bool uint))
```

### Marketplace

```clarity
;; Purchase a template
(define-public (purchase-template 
    (template-id uint)
) => (response bool uint))

;; Create escrow payment
(define-public (create-escrow-payment 
    (template-id uint)
) => (response uint uint))
```

### License Management

```clarity
;; Create a new license
(define-public (create-license
    (license-type uint)
    (name (string-utf8 50))
    (description (string-utf8 500))
    (attribution-required bool)
    (modification-allowed bool)
    (commercial-use-allowed bool)
    (fee uint)
) => (response uint uint))
```

### Reputation System

```clarity
;; Submit a review
(define-public (submit-review 
    (template-id uint)
    (rating uint)
    (review-text (string-utf8 500))
    (template-owner principal)
) => (response bool uint))
```

### Governance

```clarity
;; Create governance proposal
(define-public (create-proposal 
    (title (string-ascii 100))
    (description (string-utf8 1000))
    (function-name (string-ascii 128))
    (function-args (list 20 (string-utf8 100)))
    (voting-period-days uint)
) => (response uint uint))
```

## Getting Started

To interact with the Codehash marketplace:

1. Connect your Stacks wallet
2. Browse available templates
3. Purchase or license templates
4. Submit reviews for purchased templates
5. Participate in governance

For template creators:

1. Register as a developer
2. Create and publish templates
3. Choose licensing models
4. Build reputation through quality templates
5. Earn from template sales

## Contributing

The platform is open for community contributions through:
- Template submissions
- Governance proposals
- Bug reports and fixes
- Feature suggestions

## Security

The platform implements several security measures:
- Escrow payments for safe transactions
- License verification
- Review verification
- Governance controls
- Access control checks

## License

The Codehash platform contracts are released under the MIT License.