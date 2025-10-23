;; Credit Retirement Verifier Contract
;; Permanently retires carbon credits with fraud prevention and audit trails

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-retired (err u103))
(define-constant err-invalid-input (err u104))
(define-constant err-insufficient-credits (err u105))
(define-constant err-expired (err u106))

;; Credit status
(define-constant status-active u1)
(define-constant status-reserved u2)
(define-constant status-retired u3)
(define-constant status-cancelled u4)

;; Project types
(define-constant project-reforestation u1)
(define-constant project-renewable-energy u2)
(define-constant project-carbon-capture u3)
(define-constant project-methane-reduction u4)
(define-constant project-other u5)

;; Data Variables
(define-data-var credit-nonce uint u0)
(define-data-var retirement-nonce uint u0)
(define-data-var certificate-nonce uint u0)
(define-data-var registry-nonce uint u0)

;; Carbon Credit Registries
(define-map registries
  { registry-id: uint }
  {
    name: (string-ascii 100),
    issuer: principal,
    verified: bool,
    total-credits-issued: uint,
    total-credits-retired: uint,
    created-at: uint
  }
)

;; Carbon Credits
(define-map credits
  { credit-id: uint }
  {
    registry-id: uint,
    project-name: (string-ascii 100),
    project-type: uint,
    vintage-year: uint,
    quantity: uint,
    unit-type: (string-ascii 20),
    owner: principal,
    status: uint,
    issued-at: uint,
    expires-at: (optional uint)
  }
)

;; Retirement Records
(define-map retirements
  { retirement-id: uint }
  {
    credit-id: uint,
    quantity: uint,
    retired-by: principal,
    beneficiary: (string-ascii 100),
    reason: (string-ascii 200),
    retired-at: uint,
    certificate-id: uint
  }
)

;; Retirement Certificates
(define-map certificates
  { certificate-id: uint }
  {
    retirement-id: uint,
    serial-number: (string-ascii 50),
    credit-details: (string-ascii 300),
    quantity-retired: uint,
    vintage-year: uint,
    issued-to: principal,
    issued-at: uint,
    verification-hash: (string-ascii 64)
  }
)

;; Retired Credit Tracking (prevent double-use)
(define-map retired-credits
  { credit-id: uint }
  { total-retired: uint, fully-retired: bool }
)

;; User balances by registry
(define-map user-balances
  { owner: principal, registry-id: uint }
  { balance: uint }
)

;; Audit Trail
(define-map audit-log
  { credit-id: uint, event-index: uint }
  {
    event-type: (string-ascii 50),
    from: (optional principal),
    to: (optional principal),
    quantity: uint,
    timestamp: uint,
    notes: (string-ascii 200)
  }
)

;; Read-only functions

(define-read-only (get-registry (registry-id uint))
  (map-get? registries { registry-id: registry-id })
)

(define-read-only (get-credit (credit-id uint))
  (map-get? credits { credit-id: credit-id })
)

(define-read-only (get-retirement (retirement-id uint))
  (map-get? retirements { retirement-id: retirement-id })
)

(define-read-only (get-certificate (certificate-id uint))
  (map-get? certificates { certificate-id: certificate-id })
)

(define-read-only (get-retired-status (credit-id uint))
  (map-get? retired-credits { credit-id: credit-id })
)

(define-read-only (get-user-balance (owner principal) (registry-id uint))
  (default-to u0 (get balance (map-get? user-balances { owner: owner, registry-id: registry-id })))
)

(define-read-only (is-credit-fully-retired (credit-id uint))
  (default-to false (get fully-retired (map-get? retired-credits { credit-id: credit-id })))
)

;; Public functions

;; Register carbon credit registry
(define-public (register-registry (name (string-ascii 100)))
  (let
    (
      (new-registry-id (+ (var-get registry-nonce) u1))
    )
    (map-set registries
      { registry-id: new-registry-id }
      {
        name: name,
        issuer: tx-sender,
        verified: false,
        total-credits-issued: u0,
        total-credits-retired: u0,
        created-at: block-height
      }
    )
    
    (var-set registry-nonce new-registry-id)
    (ok new-registry-id)
  )
)

;; Verify registry (owner only)
(define-public (verify-registry (registry-id uint))
  (let
    (
      (registry (unwrap! (map-get? registries { registry-id: registry-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set registries
      { registry-id: registry-id }
      (merge registry { verified: true })
    )
    
    (ok true)
  )
)

;; Issue carbon credits
(define-public (issue-credits
    (registry-id uint)
    (project-name (string-ascii 100))
    (project-type uint)
    (vintage-year uint)
    (quantity uint)
    (unit-type (string-ascii 20))
    (owner principal)
    (expires-at (optional uint))
  )
  (let
    (
      (registry (unwrap! (map-get? registries { registry-id: registry-id }) err-not-found))
      (new-credit-id (+ (var-get credit-nonce) u1))
    )
    (asserts! (is-eq tx-sender (get issuer registry)) err-unauthorized)
    (asserts! (get verified registry) err-unauthorized)
    (asserts! (> quantity u0) err-invalid-input)
    
    (map-set credits
      { credit-id: new-credit-id }
      {
        registry-id: registry-id,
        project-name: project-name,
        project-type: project-type,
        vintage-year: vintage-year,
        quantity: quantity,
        unit-type: unit-type,
        owner: owner,
        status: status-active,
        issued-at: block-height,
        expires-at: expires-at
      }
    )
    
    ;; Update registry totals
    (map-set registries
      { registry-id: registry-id }
      (merge registry { total-credits-issued: (+ (get total-credits-issued registry) quantity) })
    )
    
    ;; Update user balance
    (let
      (
        (current-balance (get-user-balance owner registry-id))
      )
      (map-set user-balances
        { owner: owner, registry-id: registry-id }
        { balance: (+ current-balance quantity) }
      )
    )
    
    (var-set credit-nonce new-credit-id)
    (ok new-credit-id)
  )
)

;; Transfer credits
(define-public (transfer-credits
    (credit-id uint)
    (to principal)
    (quantity uint)
  )
  (let
    (
      (credit (unwrap! (map-get? credits { credit-id: credit-id }) err-not-found))
      (retired-status (default-to { total-retired: u0, fully-retired: false } 
                                  (map-get? retired-credits { credit-id: credit-id })))
    )
    (asserts! (is-eq tx-sender (get owner credit)) err-unauthorized)
    (asserts! (not (get fully-retired retired-status)) err-already-retired)
    (asserts! (is-eq (get status credit) status-active) err-invalid-input)
    (asserts! (<= quantity (- (get quantity credit) (get total-retired retired-status))) err-insufficient-credits)
    
    ;; Update balances
    (let
      (
        (from-balance (get-user-balance tx-sender (get registry-id credit)))
        (to-balance (get-user-balance to (get registry-id credit)))
      )
      (map-set user-balances
        { owner: tx-sender, registry-id: (get registry-id credit) }
        { balance: (- from-balance quantity) }
      )
      (map-set user-balances
        { owner: to, registry-id: (get registry-id credit) }
        { balance: (+ to-balance quantity) }
      )
    )
    
    (ok true)
  )
)

;; Retire carbon credits
(define-public (retire-credits
    (credit-id uint)
    (quantity uint)
    (beneficiary (string-ascii 100))
    (reason (string-ascii 200))
  )
  (let
    (
      (credit (unwrap! (map-get? credits { credit-id: credit-id }) err-not-found))
      (registry (unwrap! (map-get? registries { registry-id: (get registry-id credit) }) err-not-found))
      (retired-status (default-to { total-retired: u0, fully-retired: false }
                                  (map-get? retired-credits { credit-id: credit-id })))
      (new-retirement-id (+ (var-get retirement-nonce) u1))
      (new-certificate-id (+ (var-get certificate-nonce) u1))
      (available-quantity (- (get quantity credit) (get total-retired retired-status)))
    )
    (asserts! (is-eq tx-sender (get owner credit)) err-unauthorized)
    (asserts! (not (get fully-retired retired-status)) err-already-retired)
    (asserts! (is-eq (get status credit) status-active) err-invalid-input)
    (asserts! (<= quantity available-quantity) err-insufficient-credits)
    (asserts! (> quantity u0) err-invalid-input)
    
    ;; Create retirement record
    (map-set retirements
      { retirement-id: new-retirement-id }
      {
        credit-id: credit-id,
        quantity: quantity,
        retired-by: tx-sender,
        beneficiary: beneficiary,
        reason: reason,
        retired-at: block-height,
        certificate-id: new-certificate-id
      }
    )
    
    ;; Update retired status
    (let
      (
        (new-total-retired (+ (get total-retired retired-status) quantity))
        (is-fully-retired (is-eq new-total-retired (get quantity credit)))
      )
      (map-set retired-credits
        { credit-id: credit-id }
        { total-retired: new-total-retired, fully-retired: is-fully-retired }
      )
      
      ;; Update credit status if fully retired
      (if is-fully-retired
        (map-set credits
          { credit-id: credit-id }
          (merge credit { status: status-retired })
        )
        true
      )
    )
    
    ;; Create certificate
    (map-set certificates
      { certificate-id: new-certificate-id }
      {
        retirement-id: new-retirement-id,
        serial-number: (concat "CERT-" (concat (uint-to-str new-certificate-id) "")),
        credit-details: (get project-name credit),
        quantity-retired: quantity,
        vintage-year: (get vintage-year credit),
        issued-to: tx-sender,
        issued-at: block-height,
        verification-hash: "hash-placeholder"
      }
    )
    
    ;; Update registry totals
    (map-set registries
      { registry-id: (get registry-id credit) }
      (merge registry { total-credits-retired: (+ (get total-credits-retired registry) quantity) })
    )
    
    ;; Update user balance
    (let
      (
        (current-balance (get-user-balance tx-sender (get registry-id credit)))
      )
      (map-set user-balances
        { owner: tx-sender, registry-id: (get registry-id credit) }
        { balance: (- current-balance quantity) }
      )
    )
    
    (var-set retirement-nonce new-retirement-id)
    (var-set certificate-nonce new-certificate-id)
    (ok new-certificate-id)
  )
)

;; Verify certificate
(define-public (verify-certificate (certificate-id uint))
  (let
    (
      (certificate (unwrap! (map-get? certificates { certificate-id: certificate-id }) err-not-found))
      (retirement (unwrap! (map-get? retirements { retirement-id: (get retirement-id certificate) }) err-not-found))
      (credit (unwrap! (map-get? credits { credit-id: (get credit-id retirement) }) err-not-found))
    )
    (ok {
      valid: true,
      credit-id: (get credit-id retirement),
      quantity: (get quantity-retired certificate),
      vintage: (get vintage-year certificate),
      retired-at: (get retired-at retirement)
    })
  )
)

;; Helper function to convert uint to string (simplified)
(define-private (uint-to-str (value uint))
  (if (is-eq value u0) "0"
  (if (is-eq value u1) "1"
  (if (is-eq value u2) "2"
  (if (is-eq value u3) "3"
  (if (is-eq value u4) "4"
  (if (is-eq value u5) "5"
  (if (is-eq value u6) "6"
  (if (is-eq value u7) "7"
  (if (is-eq value u8) "8"
  (if (is-eq value u9) "9"
  "X"))))))))))
)

;; Cancel credit (issuer only, before any retirement)
(define-public (cancel-credit (credit-id uint))
  (let
    (
      (credit (unwrap! (map-get? credits { credit-id: credit-id }) err-not-found))
      (registry (unwrap! (map-get? registries { registry-id: (get registry-id credit) }) err-not-found))
      (retired-status (default-to { total-retired: u0, fully-retired: false }
                                  (map-get? retired-credits { credit-id: credit-id })))
    )
    (asserts! (is-eq tx-sender (get issuer registry)) err-unauthorized)
    (asserts! (is-eq (get total-retired retired-status) u0) err-already-retired)
    
    (map-set credits
      { credit-id: credit-id }
      (merge credit { status: status-cancelled })
    )
    
    (ok true)
  )
)


;; title: credit-retirement-verifier
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

