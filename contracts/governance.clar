;; governance.clar
;; Contract for community-driven governance of the Codehash marketplace
;; This contract allows template creators and marketplace participants to propose and vote on
;; changes to platform parameters, features, and policies through a weighted voting system.
;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u1001))
(define-constant ERR-PROPOSAL-EXPIRED (err u1002))
(define-constant ERR-ALREADY-VOTED (err u1003))
(define-constant ERR-PROPOSAL-ACTIVE (err u1004))
(define-constant ERR-INSUFFICIENT-VOTING-POWER (err u1005))
(define-constant ERR-INVALID-VOTING-PERIOD (err u1006))
(define-constant ERR-INVALID-QUORUM (err u1007))
(define-constant ERR-INVALID-PARAMETER-VALUE (err u1008))
(define-constant ERR-PROPOSAL-EXECUTED (err u1009))
(define-constant ERR-EXECUTION-FAILED (err u1010))
;; Proposal status values
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-PASSED u2)
(define-constant STATUS-REJECTED u3)
(define-constant STATUS-EXECUTED u4)
;; Governance parameters
(define-data-var min-voting-period uint u7) ;; Minimum voting period in days
(define-data-var quorum-percentage uint u20) ;; Minimum percentage of total votes required for valid proposal
(define-data-var execution-delay uint u2) ;; Delay before executing passed proposals (in days)
(define-data-var proposal-fee uint u100000000) ;; Fee in uSTX to submit a proposal (100 STX default)
;; Admin address for initial setup and emergency functions
(define-data-var contract-admin principal tx-sender)
;; Proposal counter to assign unique IDs
(define-data-var proposal-count uint u0)
;; Proposal data structure
(define-map proposals
  uint ;; proposal ID
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-utf8 1000),
    function-name: (string-ascii 128),
    function-args: (list 20 (string-utf8 100)),
    start-block-height: uint,
    end-block-height: uint,
    yes-votes: uint,
    no-votes: uint,
    status: uint,
    executed: bool,
  }
)
;; Track votes per address per proposal
(define-map votes
  {
    proposal-id: uint,
    voter: principal,
  }
  {
    vote: bool,
    weight: uint,
  }
)
;; Track voting power for each user
(define-map voting-power
  principal
  uint
)
;; Track delegated voting power
(define-map delegations
  principal ;; delegator
  principal ;; delegate
)
;; Private functions
;; Calculate end block height based on current block and voting period in days
(define-private (calculate-end-block (period-days uint))
  (let ((blocks-per-day u144))
    ;; ~10 minute blocks, 6 per hour, 144 per day
    (+ block-height (* period-days blocks-per-day))
  )
)

;; Check if a proposal is active
(define-private (is-proposal-active (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals proposal-id) false)))
    (and
      (is-eq (get status proposal) STATUS-ACTIVE)
      (<= block-height (get end-block-height proposal))
    )
  )
)

;; Get the effective voting power for a principal
(define-private (get-effective-voting-power (voter principal))
  (default-to u0 (map-get? voting-power voter))
)

;; Get the delegate for a principal or return the principal if no delegation
(define-private (get-effective-voter (voter principal))
  (default-to voter (map-get? delegations voter))
)

;; Update a proposal's status based on voting results
(define-private (update-proposal-status (proposal-id uint))
  (let (
      (proposal (unwrap! (map-get? proposals proposal-id) false))
      (yes-votes (get yes-votes proposal))
      (no-votes (get no-votes proposal))
      (total-votes (+ yes-votes no-votes))
      (quorum-needed (/ (* (var-get quorum-percentage) total-votes) u100))
    )
    (if (and (> total-votes u0) (>= total-votes quorum-needed))
      (if (> yes-votes no-votes)
        (map-set proposals proposal-id (merge proposal { status: STATUS-PASSED }))
        (map-set proposals proposal-id
          (merge proposal { status: STATUS-REJECTED })
        )
      )
      false
    )
  )
)

;; Check if caller is contract admin
(define-private (is-admin)
  (is-eq tx-sender (var-get contract-admin))
)

;; Read-only functions
;; Get proposal details by ID
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

;; Get total number of proposals
(define-read-only (get-proposal-count)
  (var-get proposal-count)
)

;; Get governance parameters
(define-read-only (get-governance-params)
  {
    min-voting-period: (var-get min-voting-period),
    quorum-percentage: (var-get quorum-percentage),
    execution-delay: (var-get execution-delay),
    proposal-fee: (var-get proposal-fee),
  }
)

;; Check if a user has voted on a specific proposal
(define-read-only (has-voted
    (proposal-id uint)
    (voter principal)
  )
  (is-some (map-get? votes {
    proposal-id: proposal-id,
    voter: voter,
  }))
)

;; Get a user's voting power
(define-read-only (get-voting-power (user principal))
  (get-effective-voting-power user)
)

;; Get a user's vote on a specific proposal
(define-read-only (get-vote
    (proposal-id uint)
    (voter principal)
  )
  (map-get? votes {
    proposal-id: proposal-id,
    voter: voter,
  })
)

;; Public functions
;; Create a new governance proposal
(define-public (create-proposal
    (title (string-ascii 100))
    (description (string-utf8 1000))
    (function-name (string-ascii 128))
    (function-args (list 20 (string-utf8 100)))
    (voting-period-days uint)
  )
  (let (
      (proposal-id (+ (var-get proposal-count) u1))
      (user-voting-power (get-effective-voting-power tx-sender))
    )
    ;; Check voter has minimum required voting power
    (asserts! (> user-voting-power u0) ERR-INSUFFICIENT-VOTING-POWER)
    ;; Check voting period is valid
    (asserts! (>= voting-period-days (var-get min-voting-period))
      ERR-INVALID-VOTING-PERIOD
    )
    ;; Collect proposal fee
    (try! (stx-transfer? (var-get proposal-fee) tx-sender (as-contract tx-sender)))
    ;; Create the proposal
    (map-set proposals proposal-id {
      proposer: tx-sender,
      title: title,
      description: description,
      function-name: function-name,
      function-args: function-args,
      start-block-height: block-height,
      end-block-height: (calculate-end-block voting-period-days),
      yes-votes: u0,
      no-votes: u0,
      status: STATUS-ACTIVE,
      executed: false,
    })
    ;; Increment proposal counter
    (var-set proposal-count proposal-id)
    (ok proposal-id)
  )
)

;; Vote on a proposal
(define-public (vote
    (proposal-id uint)
    (vote-for bool)
  )
  (let (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
      (effective-voter (get-effective-voter tx-sender))
      (voter-power (get-effective-voting-power effective-voter))
    )
    ;; Check proposal is active
    (asserts! (is-eq (get status proposal) STATUS-ACTIVE) ERR-PROPOSAL-ACTIVE)
    ;; Check proposal hasn't expired
    (asserts! (<= block-height (get end-block-height proposal))
      ERR-PROPOSAL-EXPIRED
    )
    ;; Check user hasn't already voted
    (asserts!
      (is-none (map-get? votes {
        proposal-id: proposal-id,
        voter: effective-voter,
      }))
      ERR-ALREADY-VOTED
    )
    ;; Check user has voting power
    (asserts! (> voter-power u0) ERR-INSUFFICIENT-VOTING-POWER)
    ;; Record the vote
    (map-set votes {
      proposal-id: proposal-id,
      voter: effective-voter,
    } {
      vote: vote-for,
      weight: voter-power,
    })
    ;; Update vote counts
    (if vote-for
      (map-set proposals proposal-id
        (merge proposal { yes-votes: (+ (get yes-votes proposal) voter-power) })
      )
      (map-set proposals proposal-id
        (merge proposal { no-votes: (+ (get no-votes proposal) voter-power) })
      )
    )
    (ok true)
  )
)

;; Delegate voting power to another address
(define-public (delegate-voting-power (delegate principal))
  (begin
    (asserts! (not (is-eq tx-sender delegate)) (err u1011))
    (map-set delegations tx-sender delegate)
    (ok true)
  )
)

;; Remove delegation
(define-public (remove-delegation)
  (begin
    (map-delete delegations tx-sender)
    (ok true)
  )
)

;; Finalize a proposal after voting period ends
(define-public (finalize-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND)))
    ;; Check voting period has ended
    (asserts! (> block-height (get end-block-height proposal))
      ERR-PROPOSAL-ACTIVE
    )
    ;; Check proposal is still active (not yet finalized)
    (asserts! (is-eq (get status proposal) STATUS-ACTIVE) ERR-PROPOSAL-NOT-FOUND)
    ;; Update proposal status
    (update-proposal-status proposal-id)
    (ok true)
  )
)

;; Execute a passed proposal
(define-public (execute-proposal (proposal-id uint))
  (let (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
      (execution-block (+ (get end-block-height proposal) (* (var-get execution-delay) u144)))
    )
    ;; Check proposal passed
    (asserts! (is-eq (get status proposal) STATUS-PASSED) ERR-PROPOSAL-NOT-FOUND)
    ;; Check execution delay has passed
    (asserts! (>= block-height execution-block) ERR-PROPOSAL-ACTIVE)
    ;; Check proposal hasn't been executed
    (asserts! (not (get executed proposal)) ERR-PROPOSAL-EXECUTED)
    ;; Mark as executed
    (map-set proposals proposal-id
      (merge proposal {
        executed: true,
        status: STATUS-EXECUTED,
      })
    )
    ;; TODO: Implement dynamic contract call based on function-name and function-args
    ;; This would require a pattern matching approach for common governance actions
    ;; For now, we'll return success
    (ok true)
  )
)

;; Set voting power for a user (only callable by admin or other authorized contracts)
(define-public (set-voting-power
    (user principal)
    (power uint)
  )
  (begin
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    (map-set voting-power user power)
    (ok true)
  )
)

;; Update governance parameters (only via passed proposal)
(define-public (update-governance-params
    (new-min-voting-period uint)
    (new-quorum-percentage uint)
    (new-execution-delay uint)
    (new-proposal-fee uint)
  )
  (begin
    ;; This should only be callable by the contract itself via execute-proposal
    (asserts! (is-eq tx-sender (as-contract tx-sender)) ERR-NOT-AUTHORIZED)
    ;; Basic validation
    (asserts! (> new-min-voting-period u0) ERR-INVALID-PARAMETER-VALUE)
    (asserts! (<= new-quorum-percentage u100) ERR-INVALID-QUORUM)
    ;; Update parameters
    (var-set min-voting-period new-min-voting-period)
    (var-set quorum-percentage new-quorum-percentage)
    (var-set execution-delay new-execution-delay)
    (var-set proposal-fee new-proposal-fee)
    (ok true)
  )
)

;; Transfer admin rights (only callable by current admin)
(define-public (transfer-admin (new-admin principal))
  (begin
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    (var-set contract-admin new-admin)
    (ok true)
  )
)
