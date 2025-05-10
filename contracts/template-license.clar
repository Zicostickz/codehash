;; template-license
;;
;; This contract provides a framework for defining and enforcing usage rights for code templates
;; through flexible licensing options. It enables creators to specify how their templates can be used,
;; with support for various licensing models (open-source and commercial).
;; License tokens are created as machine-readable assets that can be transferred to buyers and
;; verified on-chain by any application.
;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-LICENSE-NOT-FOUND (err u101))
(define-constant ERR-INVALID-LICENSE-TYPE (err u102))
(define-constant ERR-INVALID-LICENSE-PARAMS (err u103))
(define-constant ERR-LICENSE-ALREADY-EXISTS (err u104))
(define-constant ERR-LICENSE-EXPIRED (err u105))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u106))
(define-constant ERR-TRANSFER-FAILED (err u107))
;; License types
(define-constant LICENSE-TYPE-MIT u1)
(define-constant LICENSE-TYPE-GPL u2)
(define-constant LICENSE-TYPE-APACHE u3)
(define-constant LICENSE-TYPE-COMMERCIAL u4)
(define-constant LICENSE-TYPE-CUSTOM u5)
;; Data storage
;; Map to store license definitions
;; license-id -> license details
(define-map licenses
  { license-id: uint }
  {
    creator: principal,
    license-type: uint,
    name: (string-utf8 50),
    description: (string-utf8 500),
    attribution-required: bool,
    modification-allowed: bool,
    commercial-use-allowed: bool,
    fee: uint,
    created-at: uint,
  }
)
;; Map to track license ownership
;; (license-id, owner) -> license details with usage rights
(define-map license-ownership
  {
    license-id: uint,
    owner: principal,
  }
  {
    acquired-at: uint,
    expiration: (optional uint), ;; None means perpetual
    usage-count: (optional uint), ;; None means unlimited
  }
)
;; Counter for license IDs
(define-data-var next-license-id uint u1)
;; Private functions
;; Check if sender is authorized to modify a license
(define-private (is-license-creator (license-id uint))
  (match (map-get? licenses { license-id: license-id })
    license (is-eq tx-sender (get creator license))
    false
  )
)

;; Verify if a license type is valid
(define-private (is-valid-license-type (license-type uint))
  (or
    (is-eq license-type LICENSE-TYPE-MIT)
    (is-eq license-type LICENSE-TYPE-GPL)
    (is-eq license-type LICENSE-TYPE-APACHE)
    (is-eq license-type LICENSE-TYPE-COMMERCIAL)
    (is-eq license-type LICENSE-TYPE-CUSTOM)
  )
)

;; Check if a principal owns a valid license
(define-private (has-valid-license
    (license-id uint)
    (user principal)
  )
  (match (map-get? license-ownership {
    license-id: license-id,
    owner: user,
  })
    ownership (and
      ;; Check if license has not expired (if it has an expiration)
      (match (get expiration ownership)
        expiry-block (< block-height expiry-block)
        true
      )
      ;; Check if usage count is still valid (if it has a limit)
      (match (get usage-count ownership)
        count (> count u0)
        true
      )
    )
    false
  )
)

;; Decrement usage count if needed
(define-private (use-license
    (license-id uint)
    (user principal)
  )
  (match (map-get? license-ownership {
    license-id: license-id,
    owner: user,
  })
    ownership (match (get usage-count ownership)
      count
      (if (> count u0)
        (map-set license-ownership {
          license-id: license-id,
          owner: user,
        }
          (merge ownership { usage-count: (some (- count u1)) })
        )
        false
      )
      true ;; No usage count means unlimited
    )
    false
  )
)

;; Public functions
;; Create a new license definition
(define-public (create-license
    (license-type uint)
    (name (string-utf8 50))
    (description (string-utf8 500))
    (attribution-required bool)
    (modification-allowed bool)
    (commercial-use-allowed bool)
    (fee uint)
  )
  (let ((license-id (var-get next-license-id)))
    ;; Check if license type is valid
    (asserts! (is-valid-license-type license-type) ERR-INVALID-LICENSE-TYPE)
    ;; Store the license information
    (map-set licenses { license-id: license-id } {
      creator: tx-sender,
      license-type: license-type,
      name: name,
      description: description,
      attribution-required: attribution-required,
      modification-allowed: modification-allowed,
      commercial-use-allowed: commercial-use-allowed,
      fee: fee,
      created-at: block-height,
    })
    ;; Automatically assign ownership to creator
    (map-set license-ownership {
      license-id: license-id,
      owner: tx-sender,
    } {
      acquired-at: block-height,
      expiration: none, ;; Creator has perpetual license
      usage-count: none, ;; Creator has unlimited usage
    })
    ;; Increment the license ID counter
    (var-set next-license-id (+ license-id u1))
    ;; Return the new license ID
    (ok license-id)
  )
)

;; Update an existing license's details
(define-public (update-license
    (license-id uint)
    (name (string-utf8 50))
    (description (string-utf8 500))
    (attribution-required bool)
    (modification-allowed bool)
    (commercial-use-allowed bool)
    (fee uint)
  )
  (let ((license-opt (map-get? licenses { license-id: license-id })))
    ;; Check if license exists
    (asserts! (is-some license-opt) ERR-LICENSE-NOT-FOUND)
    ;; Check if sender is authorized
    (asserts! (is-license-creator license-id) ERR-NOT-AUTHORIZED)
    (let ((license (unwrap-panic license-opt)))
      ;; Update license information
      (map-set licenses { license-id: license-id }
        (merge license {
          name: name,
          description: description,
          attribution-required: attribution-required,
          modification-allowed: modification-allowed,
          commercial-use-allowed: commercial-use-allowed,
          fee: fee,
        })
      )
      (ok true)
    )
  )
)

;; Purchase a license
(define-public (purchase-license
    (license-id uint)
    (expiration-blocks (optional uint))
    (usage-limit (optional uint))
  )
  (let ((license-opt (map-get? licenses { license-id: license-id })))
    ;; Check if license exists
    (asserts! (is-some license-opt) ERR-LICENSE-NOT-FOUND)
    (let (
        (license (unwrap-panic license-opt))
        (fee (get fee license))
        (creator (get creator license))
      )
      ;; Check if payment is sufficient
      (asserts! (>= (stx-get-balance tx-sender) fee) ERR-INSUFFICIENT-PAYMENT)
      ;; Transfer fee to creator
      (if (> fee u0)
        (try! (stx-transfer? fee tx-sender creator))
        true
      )
      ;; Calculate expiration if provided
      (let ((expiry (match expiration-blocks
          blocks (some (+ block-height blocks))
          none
        )))
        ;; Grant license to buyer
        (map-set license-ownership {
          license-id: license-id,
          owner: tx-sender,
        } {
          acquired-at: block-height,
          expiration: expiry,
          usage-count: usage-limit,
        })
        (ok true)
      )
    )
  )
)

;; Verify if a user has valid license and log usage
(define-public (verify-and-use-license
    (license-id uint)
    (user principal)
  )
  (begin
    (asserts! (has-valid-license license-id user) ERR-LICENSE-NOT-FOUND)
    ;; Update usage count if needed
    (asserts! (use-license license-id user) ERR-LICENSE-EXPIRED)
    (ok true)
  )
)

;; Transfer a license to another user
(define-public (transfer-license
    (license-id uint)
    (recipient principal)
  )
  (let ((ownership-opt (map-get? license-ownership {
      license-id: license-id,
      owner: tx-sender,
    })))
    ;; Check if sender owns the license
    (asserts! (is-some ownership-opt) ERR-LICENSE-NOT-FOUND)
    (let ((ownership (unwrap-panic ownership-opt)))
      ;; Transfer license to recipient
      (map-set license-ownership {
        license-id: license-id,
        owner: recipient,
      }
        ownership
      )
      ;; Remove license from sender
      (map-delete license-ownership {
        license-id: license-id,
        owner: tx-sender,
      })
      (ok true)
    )
  )
)

;; Read-only functions
;; Get license details
(define-read-only (get-license (license-id uint))
  (map-get? licenses { license-id: license-id })
)

;; Check if a user owns a license
(define-read-only (check-license-ownership
    (license-id uint)
    (user principal)
  )
  (is-some (map-get? license-ownership {
    license-id: license-id,
    owner: user,
  }))
)

;; Check if a user's license is valid (not expired, has usage left)
(define-read-only (is-license-valid
    (license-id uint)
    (user principal)
  )
  (has-valid-license license-id user)
)

;; Get license ownership details
(define-read-only (get-license-ownership
    (license-id uint)
    (user principal)
  )
  (map-get? license-ownership {
    license-id: license-id,
    owner: user,
  })
)

;; Get total license count
(define-read-only (get-license-count)
  (- (var-get next-license-id) u1)
)
