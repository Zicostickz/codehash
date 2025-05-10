;; ============================================================================
;; template-registry contract for Codehash
;; ============================================================================
;; This contract serves as the central registry for all smart contract templates
;; in the Codehash ecosystem. It manages template metadata, ownership, and version
;; history, allowing developers to register, update, and share their templates.
;; The registry acts as the backbone of the Codehash marketplace, providing a
;; trustless and decentralized way to discover and verify smart contract templates.
;; ============================================================================
;; ============================================================================
;; Error Constants
;; ============================================================================
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-TEMPLATE-NOT-FOUND (err u101))
(define-constant ERR-TEMPLATE-ALREADY-EXISTS (err u102))
(define-constant ERR-INVALID-VERSION (err u103))
(define-constant ERR-VERSION-ALREADY-EXISTS (err u104))
(define-constant ERR-EMPTY-FIELD (err u105))
(define-constant ERR-TAG-LIMIT-EXCEEDED (err u106))
(define-constant ERR-INVALID-COMPATIBILITY (err u107))
;; ============================================================================
;; Constants
;; ============================================================================
(define-constant MAX-TAGS u5)
(define-constant MAX-VERSION-LENGTH u10)
(define-constant MIN-VERSION-LENGTH u3)
(define-constant MAX-TITLE-LENGTH u50)
(define-constant MAX-DESC-LENGTH u500)
;; ============================================================================
;; Data Maps and Variables
;; ============================================================================
;; Tracks the total number of templates registered
(define-data-var template-count uint u0)
;; Core template metadata - stores the basic information about each template
(define-map templates
  { template-id: uint }
  {
    title: (string-ascii MAX-TITLE-LENGTH),
    description: (string-ascii MAX-DESC-LENGTH),
    tags: (list 5 (string-ascii 20)),
    owner: principal,
    created-at: uint,
    last-updated: uint,
    documentation-url: (optional (string-ascii 100)),
    repository-url: (optional (string-ascii 100)),
    is-active: bool,
  }
)
;; Template compatibility information - lists the Clarity versions and platforms the template is compatible with
(define-map template-compatibility
  { template-id: uint }
  {
    clarity-versions: (list 5 (string-ascii 10)),
    platforms: (list 3 (string-ascii 20)),
  }
)
;; Version history for each template - stores the hash and metadata for each version
(define-map template-versions
  {
    template-id: uint,
    version: (string-ascii MAX-VERSION-LENGTH),
  }
  {
    content-hash: (buff 32),
    release-notes: (string-ascii 200),
    published-at: uint,
    is-deprecated: bool,
  }
)
;; Mapping of template IDs to their version list - allows easy tracking of all versions of a template
(define-map template-version-list
  { template-id: uint }
  { versions: (list 20 (string-ascii MAX-VERSION-LENGTH)) }
)
;; ============================================================================
;; Private Functions
;; ============================================================================
;; Check if a template exists
(define-private (template-exists? (template-id uint))
  (is-some (map-get? templates { template-id: template-id }))
)

;; Check if sender is the template owner
(define-private (is-owner? (template-id uint))
  (let ((template-data (map-get? templates { template-id: template-id })))
    (and
      (is-some template-data)
      (is-eq tx-sender (get owner (unwrap-panic template-data)))
    )
  )
)

;; Check if a version already exists for a template
(define-private (version-exists?
    (template-id uint)
    (version (string-ascii MAX-VERSION-LENGTH))
  )
  (is-some (map-get? template-versions {
    template-id: template-id,
    version: version,
  }))
)

;; Validate version string (semantic versioning format check)
(define-private (is-valid-version? (version (string-ascii MAX-VERSION-LENGTH)))
  (let ((version-length (len version)))
    (and
      (>= version-length MIN-VERSION-LENGTH)
      (<= version-length MAX-VERSION-LENGTH)
      ;; Basic check - more complex semantic version validation could be implemented
      (is-some (index-of version "."))
    )
  )
)

;; Validate basic template metadata
(define-private (validate-template-metadata
    (title (string-ascii MAX-TITLE-LENGTH))
    (description (string-ascii MAX-DESC-LENGTH))
    (tags (list 5 (string-ascii 20)))
  )
  (begin
    (asserts! (> (len title) u0) ERR-EMPTY-FIELD)
    (asserts! (> (len description) u0) ERR-EMPTY-FIELD)
    (asserts! (<= (len tags) MAX-TAGS) ERR-TAG-LIMIT-EXCEEDED)
    (ok true)
  )
)

;; Add a new version to the template-version-list map
(define-private (add-version-to-list
    (template-id uint)
    (version (string-ascii MAX-VERSION-LENGTH))
  )
  (let (
      (current-list (default-to { versions: (list) }
        (map-get? template-version-list { template-id: template-id })
      ))
      (new-list (unwrap-panic (as-max-len? (append (get versions current-list) version) u20)))
    )
    (map-set template-version-list { template-id: template-id } { versions: new-list })
  )
)

;; ============================================================================
;; Read-Only Functions
;; ============================================================================
;; Get the total number of templates registered
(define-read-only (get-template-count)
  (var-get template-count)
)

;; Get template metadata by ID
(define-read-only (get-template (template-id uint))
  (map-get? templates { template-id: template-id })
)

;; Get template compatibility information
(define-read-only (get-template-compatibility (template-id uint))
  (map-get? template-compatibility { template-id: template-id })
)

;; Get a specific version of a template
(define-read-only (get-template-version
    (template-id uint)
    (version (string-ascii MAX-VERSION-LENGTH))
  )
  (map-get? template-versions {
    template-id: template-id,
    version: version,
  })
)

;; Get all versions of a template
(define-read-only (get-template-versions (template-id uint))
  (map-get? template-version-list { template-id: template-id })
)

;; Check if the principal is the owner of the template
(define-read-only (is-template-owner
    (template-id uint)
    (owner principal)
  )
  (let ((template-data (map-get? templates { template-id: template-id })))
    (match template-data
      template-info (is-eq owner (get owner template-info))
      false
    )
  )
)

;; ============================================================================
;; Public Functions
;; ============================================================================
;; Register a new template
(define-public (register-template
    (title (string-ascii MAX-TITLE-LENGTH))
    (description (string-ascii MAX-DESC-LENGTH))
    (tags (list 5 (string-ascii 20)))
    (documentation-url (optional (string-ascii 100)))
    (repository-url (optional (string-ascii 100)))
    (clarity-versions (list 5 (string-ascii 10)))
    (platforms (list 3 (string-ascii 20)))
  )
  (let (
      (new-template-id (+ (var-get template-count) u1))
      (block-height block-height)
    )
    ;; Validate the template metadata
    (try! (validate-template-metadata title description tags))
    ;; Validate compatibility information
    (asserts! (> (len clarity-versions) u0) ERR-INVALID-COMPATIBILITY)
    (asserts! (> (len platforms) u0) ERR-INVALID-COMPATIBILITY)
    ;; Store template metadata
    (map-set templates { template-id: new-template-id } {
      title: title,
      description: description,
      tags: tags,
      owner: tx-sender,
      created-at: block-height,
      last-updated: block-height,
      documentation-url: documentation-url,
      repository-url: repository-url,
      is-active: true,
    })
    ;; Store template compatibility information
    (map-set template-compatibility { template-id: new-template-id } {
      clarity-versions: clarity-versions,
      platforms: platforms,
    })
    ;; Initialize an empty version list
    (map-set template-version-list { template-id: new-template-id } { versions: (list) })
    ;; Increment template count
    (var-set template-count new-template-id)
    (ok new-template-id)
  )
)

;; Update template metadata
(define-public (update-template
    (template-id uint)
    (title (string-ascii MAX-TITLE-LENGTH))
    (description (string-ascii MAX-DESC-LENGTH))
    (tags (list 5 (string-ascii 20)))
    (documentation-url (optional (string-ascii 100)))
    (repository-url (optional (string-ascii 100)))
  )
  (let ((template-data (map-get? templates { template-id: template-id })))
    ;; Check if template exists and user is authorized
    (asserts! (is-some template-data) ERR-TEMPLATE-NOT-FOUND)
    (asserts! (is-owner? template-id) ERR-NOT-AUTHORIZED)
    ;; Validate the template metadata
    (try! (validate-template-metadata title description tags))
    ;; Update the template metadata
    (map-set templates { template-id: template-id }
      (merge (unwrap-panic template-data) {
        title: title,
        description: description,
        tags: tags,
        documentation-url: documentation-url,
        repository-url: repository-url,
        last-updated: block-height,
      })
    )
    (ok true)
  )
)

;; Update template compatibility information
(define-public (update-template-compatibility
    (template-id uint)
    (clarity-versions (list 5 (string-ascii 10)))
    (platforms (list 3 (string-ascii 20)))
  )
  (begin
    ;; Check if template exists and user is authorized
    (asserts! (template-exists? template-id) ERR-TEMPLATE-NOT-FOUND)
    (asserts! (is-owner? template-id) ERR-NOT-AUTHORIZED)
    ;; Validate compatibility information
    (asserts! (> (len clarity-versions) u0) ERR-INVALID-COMPATIBILITY)
    (asserts! (> (len platforms) u0) ERR-INVALID-COMPATIBILITY)
    ;; Update the compatibility information
    (map-set template-compatibility { template-id: template-id } {
      clarity-versions: clarity-versions,
      platforms: platforms,
    })
    ;; Update last-updated timestamp in template metadata
    (map-set templates { template-id: template-id }
      (merge (unwrap-panic (map-get? templates { template-id: template-id })) { last-updated: block-height })
    )
    (ok true)
  )
)

;; Publish a new template version
(define-public (publish-template-version
    (template-id uint)
    (version (string-ascii MAX-VERSION-LENGTH))
    (content-hash (buff 32))
    (release-notes (string-ascii 200))
  )
  (begin
    ;; Check if template exists and user is authorized
    (asserts! (template-exists? template-id) ERR-TEMPLATE-NOT-FOUND)
    (asserts! (is-owner? template-id) ERR-NOT-AUTHORIZED)
    ;; Validate version
    (asserts! (is-valid-version? version) ERR-INVALID-VERSION)
    (asserts! (not (version-exists? template-id version))
      ERR-VERSION-ALREADY-EXISTS
    )
    ;; Store the new version
    (map-set template-versions {
      template-id: template-id,
      version: version,
    } {
      content-hash: content-hash,
      release-notes: release-notes,
      published-at: block-height,
      is-deprecated: false,
    })
    ;; Add version to the version list
    (add-version-to-list template-id version)
    ;; Update last-updated timestamp in template metadata
    (map-set templates { template-id: template-id }
      (merge (unwrap-panic (map-get? templates { template-id: template-id })) { last-updated: block-height })
    )
    (ok true)
  )
)

;; Deprecate a template version
(define-public (deprecate-template-version
    (template-id uint)
    (version (string-ascii MAX-VERSION-LENGTH))
  )
  (let ((version-data (map-get? template-versions {
      template-id: template-id,
      version: version,
    })))
    ;; Check if template and version exist and user is authorized
    (asserts! (is-some version-data) ERR-TEMPLATE-NOT-FOUND)
    (asserts! (is-owner? template-id) ERR-NOT-AUTHORIZED)
    ;; Update the version to mark it as deprecated
    (map-set template-versions {
      template-id: template-id,
      version: version,
    }
      (merge (unwrap-panic version-data) { is-deprecated: true })
    )
    (ok true)
  )
)

;; Transfer template ownership
(define-public (transfer-template-ownership
    (template-id uint)
    (new-owner principal)
  )
  (let ((template-data (map-get? templates { template-id: template-id })))
    ;; Check if template exists and user is authorized
    (asserts! (is-some template-data) ERR-TEMPLATE-NOT-FOUND)
    (asserts! (is-owner? template-id) ERR-NOT-AUTHORIZED)
    ;; Update the template owner
    (map-set templates { template-id: template-id }
      (merge (unwrap-panic template-data) {
        owner: new-owner,
        last-updated: block-height,
      })
    )
    (ok true)
  )
)

;; Deactivate a template (mark as inactive without deleting)
(define-public (deactivate-template (template-id uint))
  (let ((template-data (map-get? templates { template-id: template-id })))
    ;; Check if template exists and user is authorized
    (asserts! (is-some template-data) ERR-TEMPLATE-NOT-FOUND)
    (asserts! (is-owner? template-id) ERR-NOT-AUTHORIZED)
    ;; Update the template to mark it as inactive
    (map-set templates { template-id: template-id }
      (merge (unwrap-panic template-data) {
        is-active: false,
        last-updated: block-height,
      })
    )
    (ok true)
  )
)

;; Reactivate a template
(define-public (reactivate-template (template-id uint))
  (let ((template-data (map-get? templates { template-id: template-id })))
    ;; Check if template exists and user is authorized
    (asserts! (is-some template-data) ERR-TEMPLATE-NOT-FOUND)
    (asserts! (is-owner? template-id) ERR-NOT-AUTHORIZED)
    ;; Update the template to mark it as active
    (map-set templates { template-id: template-id }
      (merge (unwrap-panic template-data) {
        is-active: true,
        last-updated: block-height,
      })
    )
    (ok true)
  )
)
