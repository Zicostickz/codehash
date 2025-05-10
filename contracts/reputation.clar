;; Reputation Contract for CodeHash
;; This contract manages the reputation system for the CodeHash marketplace,
;; tracking developer and template ratings, reviews, and usage metrics to build
;; trust in the marketplace and help buyers make informed decisions.
;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-TEMPLATE-NOT-FOUND (err u101))
(define-constant ERR-USER-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-REVIEWED (err u103))
(define-constant ERR-NOT-PURCHASED (err u104))
(define-constant ERR-INVALID-RATING (err u105))
(define-constant ERR-REVIEW-NOT-FOUND (err u106))
(define-constant ERR-MARKETPLACE-ONLY (err u107))
(define-constant ERR-SELF-REVIEW (err u108))
;; Constants
(define-constant MARKETPLACE-CONTRACT .marketplace)
(define-constant MAX-RATING u5)
(define-constant MIN-RATING u1)
;; Data storage
;; Developer reputation tracking
(define-map developer-reputations
  { developer: principal }
  {
    total-rating: uint, ;; Sum of all ratings received
    rating-count: uint, ;; Number of ratings received
    total-templates: uint, ;; Number of templates created
    total-sales: uint, ;; Number of template sales
  }
)
;; Template reputation tracking
(define-map template-reputations
  { template-id: uint }
  {
    total-rating: uint, ;; Sum of all ratings received
    rating-count: uint, ;; Number of ratings received
    total-purchases: uint, ;; Number of times purchased
    total-deployments: uint, ;; Number of times deployed
  }
)
;; Reviews storage
(define-map template-reviews
  {
    template-id: uint,
    reviewer: principal,
  }
  {
    rating: uint, ;; Rating 1-5
    review-text: (string-utf8 500), ;; Optional review comment
    timestamp: uint, ;; Block height when reviewed
  }
)
;; Track template purchases to verify reviewer has purchased the template
(define-map template-purchases
  {
    template-id: uint,
    user: principal,
  }
  { purchased: bool }
)
;; Private functions
;; Calculate average rating from total rating and count
(define-private (calculate-average-rating
    (total-rating uint)
    (rating-count uint)
  )
  (if (is-eq rating-count u0)
    u0
    (/ (* total-rating u100) rating-count) ;; Return as integer with 2 decimal places (e.g. 425 = 4.25)
  )
)

;; Update developer reputation when a new review is submitted
(define-private (update-developer-reputation
    (developer principal)
    (rating uint)
  )
  (let ((current-reputation (default-to {
      total-rating: u0,
      rating-count: u0,
      total-templates: u0,
      total-sales: u0,
    }
      (map-get? developer-reputations { developer: developer })
    )))
    (map-set developer-reputations { developer: developer } {
      total-rating: (+ (get total-rating current-reputation) rating),
      rating-count: (+ (get rating-count current-reputation) u1),
      total-templates: (get total-templates current-reputation),
      total-sales: (get total-sales current-reputation),
    })
  )
)

;; Update template reputation when a new review is submitted
(define-private (update-template-reputation
    (template-id uint)
    (rating uint)
  )
  (let ((current-reputation (default-to {
      total-rating: u0,
      rating-count: u0,
      total-purchases: u0,
      total-deployments: u0,
    }
      (map-get? template-reputations { template-id: template-id })
    )))
    (map-set template-reputations { template-id: template-id } {
      total-rating: (+ (get total-rating current-reputation) rating),
      rating-count: (+ (get rating-count current-reputation) u1),
      total-purchases: (get total-purchases current-reputation),
      total-deployments: (get total-deployments current-reputation),
    })
  )
)

;; Check if sender has purchased the template - required to submit a review
(define-private (has-purchased-template
    (template-id uint)
    (user principal)
  )
  (default-to false
    (get purchased
      (map-get? template-purchases {
        template-id: template-id,
        user: user,
      })
    ))
)

;; Public functions
;; Register a template purchase - can only be called by the marketplace contract
(define-public (register-purchase
    (template-id uint)
    (buyer principal)
    (seller principal)
  )
  (begin
    ;; Only marketplace contract can call this function
    (asserts! (is-eq tx-sender MARKETPLACE-CONTRACT) ERR-MARKETPLACE-ONLY)
    ;; Register purchase to enable buyer to review
    (map-set template-purchases {
      template-id: template-id,
      user: buyer,
    } { purchased: true }
    )
    ;; Update template purchase count
    (let (
        (template-rep (default-to {
          total-rating: u0,
          rating-count: u0,
          total-purchases: u0,
          total-deployments: u0,
        }
          (map-get? template-reputations { template-id: template-id })
        ))
        (seller-rep (default-to {
          total-rating: u0,
          rating-count: u0,
          total-templates: u0,
          total-sales: u0,
        }
          (map-get? developer-reputations { developer: seller })
        ))
      )
      (map-set template-reputations { template-id: template-id } {
        total-rating: (get total-rating template-rep),
        rating-count: (get rating-count template-rep),
        total-purchases: (+ (get total-purchases template-rep) u1),
        total-deployments: (get total-deployments template-rep),
      })
      (map-set developer-reputations { developer: seller } {
        total-rating: (get total-rating seller-rep),
        rating-count: (get rating-count seller-rep),
        total-templates: (get total-templates seller-rep),
        total-sales: (+ (get total-sales seller-rep) u1),
      })
      (ok true)
    )
  )
)

;; Register a template deployment - can only be called by the marketplace contract
(define-public (register-deployment (template-id uint))
  (begin
    ;; Only marketplace contract can call this function
    (asserts! (is-eq tx-sender MARKETPLACE-CONTRACT) ERR-MARKETPLACE-ONLY)
    ;; Update template deployment count
    (let ((template-rep (default-to {
        total-rating: u0,
        rating-count: u0,
        total-purchases: u0,
        total-deployments: u0,
      }
        (map-get? template-reputations { template-id: template-id })
      )))
      (map-set template-reputations { template-id: template-id } {
        total-rating: (get total-rating template-rep),
        rating-count: (get rating-count template-rep),
        total-purchases: (get total-purchases template-rep),
        total-deployments: (+ (get total-deployments template-rep) u1),
      })
      (ok true)
    )
  )
)

;; Register a new template - can only be called by the marketplace contract
(define-public (register-template
    (template-id uint)
    (creator principal)
  )
  (begin
    ;; Only marketplace contract can call this function
    (asserts! (is-eq tx-sender MARKETPLACE-CONTRACT) ERR-MARKETPLACE-ONLY)
    ;; Initialize template reputation
    (map-set template-reputations { template-id: template-id } {
      total-rating: u0,
      rating-count: u0,
      total-purchases: u0,
      total-deployments: u0,
    })
    ;; Update developer template count
    (let ((developer-rep (default-to {
        total-rating: u0,
        rating-count: u0,
        total-templates: u0,
        total-sales: u0,
      }
        (map-get? developer-reputations { developer: creator })
      )))
      (map-set developer-reputations { developer: creator } {
        total-rating: (get total-rating developer-rep),
        rating-count: (get rating-count developer-rep),
        total-templates: (+ (get total-templates developer-rep) u1),
        total-sales: (get total-sales developer-rep),
      })
      (ok true)
    )
  )
)

;; Submit a review for a template
(define-public (submit-review
    (template-id uint)
    (rating uint)
    (review-text (string-utf8 500))
    (template-owner principal)
  )
  (begin
    ;; Check if rating is within valid range
    (asserts! (and (>= rating MIN-RATING) (<= rating MAX-RATING))
      ERR-INVALID-RATING
    )
    ;; Check if user has purchased the template
    (asserts! (has-purchased-template template-id tx-sender) ERR-NOT-PURCHASED)
    ;; Prevent self-reviews
    (asserts! (not (is-eq tx-sender template-owner)) ERR-SELF-REVIEW)
    ;; Check if user has already reviewed this template
    (asserts!
      (is-none (map-get? template-reviews {
        template-id: template-id,
        reviewer: tx-sender,
      }))
      ERR-ALREADY-REVIEWED
    )
    ;; Store the review
    (map-set template-reviews {
      template-id: template-id,
      reviewer: tx-sender,
    } {
      rating: rating,
      review-text: review-text,
      timestamp: block-height,
    })
    ;; Update template reputation
    (update-template-reputation template-id rating)
    ;; Update developer reputation
    (update-developer-reputation template-owner rating)
    (ok true)
  )
)

;; Update an existing review
(define-public (update-review
    (template-id uint)
    (rating uint)
    (review-text (string-utf8 500))
  )
  (let ((existing-review (map-get? template-reviews {
      template-id: template-id,
      reviewer: tx-sender,
    })))
    ;; Check if the review exists
    (asserts! (is-some existing-review) ERR-REVIEW-NOT-FOUND)
    ;; Check if rating is within valid range
    (asserts! (and (>= rating MIN-RATING) (<= rating MAX-RATING))
      ERR-INVALID-RATING
    )
    ;; Get the previous rating
    (let (
        (previous-rating (get rating (unwrap! existing-review ERR-REVIEW-NOT-FOUND)))
        (template-rep (unwrap! (map-get? template-reputations { template-id: template-id })
          ERR-TEMPLATE-NOT-FOUND
        ))
      )
      (let ()
        ;; Update the review
        (map-set template-reviews {
          template-id: template-id,
          reviewer: tx-sender,
        } {
          rating: rating,
          review-text: review-text,
          timestamp: block-height,
        })
        ;; Update template reputation
        (map-set template-reputations { template-id: template-id } {
          total-rating: (+ (- (get total-rating template-rep) previous-rating) rating),
          rating-count: (get rating-count template-rep),
          total-purchases: (get total-purchases template-rep),
          total-deployments: (get total-deployments template-rep),
        })
        (ok true)
      )
    )
  )
)

;; Read-only functions
;; Get template reputation with calculated average rating
(define-read-only (get-template-reputation (template-id uint))
  (let ((reputation (map-get? template-reputations { template-id: template-id })))
    (if (is-some reputation)
      (let ((unwrapped-rep (unwrap-panic reputation)))
        (ok {
          average-rating: (calculate-average-rating (get total-rating unwrapped-rep)
            (get rating-count unwrapped-rep)
          ),
          rating-count: (get rating-count unwrapped-rep),
          total-purchases: (get total-purchases unwrapped-rep),
          total-deployments: (get total-deployments unwrapped-rep),
        })
      )
      ERR-TEMPLATE-NOT-FOUND
    )
  )
)

;; Get developer reputation with calculated average rating
(define-read-only (get-developer-reputation (developer principal))
  (let ((reputation (map-get? developer-reputations { developer: developer })))
    (if (is-some reputation)
      (let ((unwrapped-rep (unwrap-panic reputation)))
        (ok {
          average-rating: (calculate-average-rating (get total-rating unwrapped-rep)
            (get rating-count unwrapped-rep)
          ),
          rating-count: (get rating-count unwrapped-rep),
          total-templates: (get total-templates unwrapped-rep),
          total-sales: (get total-sales unwrapped-rep),
        })
      )
      ERR-USER-NOT-FOUND
    )
  )
)

;; Get a specific review
(define-read-only (get-review
    (template-id uint)
    (reviewer principal)
  )
  (let ((review (map-get? template-reviews {
      template-id: template-id,
      reviewer: reviewer,
    })))
    (if (is-some review)
      (ok (unwrap-panic review))
      ERR-REVIEW-NOT-FOUND
    )
  )
)

;; Check if a user has purchased a template
(define-read-only (has-user-purchased
    (template-id uint)
    (user principal)
  )
  (ok (has-purchased-template template-id user))
)

;; Check if a user has already reviewed a template
(define-read-only (has-user-reviewed
    (template-id uint)
    (user principal)
  )
  (ok (is-some (map-get? template-reviews {
    template-id: template-id,
    reviewer: user,
  })))
)
