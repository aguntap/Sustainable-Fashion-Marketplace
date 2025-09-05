;; Sustainable Fashion Marketplace Smart Contract
;; A decentralized marketplace for verified eco-friendly clothing and accessories

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-price (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-already-verified (err u105))
(define-constant err-not-verified (err u106))
(define-constant err-item-not-available (err u107))
(define-constant err-invalid-sustainability-score (err u108))

;; Data Variables
(define-data-var next-item-id uint u1)
(define-data-var marketplace-fee-percentage uint u250) ;; 2.5%
(define-data-var total-verified-items uint u0)
(define-data-var total-transactions uint u0)

;; Data Maps
(define-map fashion-items
  { item-id: uint }
  {
    owner: principal,
    name: (string-ascii 64),
    description: (string-ascii 256),
    category: (string-ascii 32),
    price: uint,
    is-verified: bool,
    sustainability-score: uint, ;; 1-100 scale
    eco-certifications: (list 5 (string-ascii 32)),
    materials: (list 10 (string-ascii 32)),
    carbon-footprint: uint, ;; in grams CO2
    is-available: bool,
    created-at: uint,
    verified-at: (optional uint)
  }
)

(define-map verifiers
  { verifier: principal }
  {
    is-authorized: bool,
    verification-count: uint,
    reputation-score: uint
  }
)

(define-map user-profiles
  { user: principal }
  {
    total-items-sold: uint,
    total-items-bought: uint,
    sustainability-rating: uint,
    is-eco-certified-seller: bool
  }
)

(define-map transactions
  { transaction-id: uint }
  {
    item-id: uint,
    seller: principal,
    buyer: principal,
    price: uint,
    timestamp: uint,
    sustainability-impact: uint
  }
)

(define-map item-reviews
  { item-id: uint, reviewer: principal }
  {
    rating: uint, ;; 1-5 stars
    sustainability-rating: uint, ;; 1-5 stars
    review-text: (string-ascii 256),
    timestamp: uint
  }
)

;; Private Functions
(define-private (calculate-marketplace-fee (price uint))
  (/ (* price (var-get marketplace-fee-percentage)) u10000)
)

(define-private (update-user-profile (user principal) (items-sold uint) (items-bought uint))
  (let ((current-profile (default-to 
    { total-items-sold: u0, total-items-bought: u0, sustainability-rating: u50, is-eco-certified-seller: false }
    (map-get? user-profiles { user: user }))))
    (map-set user-profiles { user: user }
      (merge current-profile {
        total-items-sold: (+ (get total-items-sold current-profile) items-sold),
        total-items-bought: (+ (get total-items-bought current-profile) items-bought)
      }))
  )
)

(define-private (calculate-sustainability-impact (carbon-footprint uint) (sustainability-score uint))
  (let ((base-impact (/ (* sustainability-score u100) u50)))
    (if (< carbon-footprint u1000)
      (+ base-impact u20)
      (if (< carbon-footprint u5000)
        base-impact
        (- base-impact u20))))
)

;; Public Functions

;; Initialize contract
(define-public (initialize)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set verifiers { verifier: contract-owner } 
      { is-authorized: true, verification-count: u0, reputation-score: u100 })
    (ok true)
  )
)

;; Add authorized verifier
(define-public (add-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set verifiers { verifier: verifier }
      { is-authorized: true, verification-count: u0, reputation-score: u80 })
    (ok true)
  )
)

;; List new fashion item
(define-public (list-item 
  (name (string-ascii 64))
  (description (string-ascii 256))
  (category (string-ascii 32))
  (price uint)
  (materials (list 10 (string-ascii 32)))
  (carbon-footprint uint)
  (eco-certifications (list 5 (string-ascii 32))))
  (let ((item-id (var-get next-item-id)))
    (asserts! (> price u0) err-invalid-price)
    (map-set fashion-items { item-id: item-id }
      {
        owner: tx-sender,
        name: name,
        description: description,
        category: category,
        price: price,
        is-verified: false,
        sustainability-score: u0,
        eco-certifications: eco-certifications,
        materials: materials,
        carbon-footprint: carbon-footprint,
        is-available: true,
        created-at: block-height,
        verified-at: none
      })
    (var-set next-item-id (+ item-id u1))
    (ok item-id)
  )
)

;; Verify item sustainability
(define-public (verify-item (item-id uint) (sustainability-score uint))
  (let ((item (unwrap! (map-get? fashion-items { item-id: item-id }) err-not-found))
        (verifier-info (unwrap! (map-get? verifiers { verifier: tx-sender }) err-unauthorized)))
    (asserts! (get is-authorized verifier-info) err-unauthorized)
    (asserts! (not (get is-verified item)) err-already-verified)
    (asserts! (and (>= sustainability-score u1) (<= sustainability-score u100)) err-invalid-sustainability-score)
    
    (map-set fashion-items { item-id: item-id }
      (merge item { 
        is-verified: true, 
        sustainability-score: sustainability-score,
        verified-at: (some block-height)
      }))
    
    (map-set verifiers { verifier: tx-sender }
      (merge verifier-info { 
        verification-count: (+ (get verification-count verifier-info) u1)
      }))
    
    (var-set total-verified-items (+ (var-get total-verified-items) u1))
    (ok true)
  )
)

;; Purchase item
(define-public (purchase-item (item-id uint))
  (let ((item (unwrap! (map-get? fashion-items { item-id: item-id }) err-not-found))
        (marketplace-fee (calculate-marketplace-fee (get price item)))
        (seller-amount (- (get price item) marketplace-fee))
        (transaction-id (var-get total-transactions)))
    
    (asserts! (get is-available item) err-item-not-available)
    (asserts! (get is-verified item) err-not-verified)
    (asserts! (not (is-eq tx-sender (get owner item))) err-unauthorized)
    
    ;; Transfer payment
    (try! (stx-transfer? (get price item) tx-sender (get owner item)))
    
    ;; Update item availability
    (map-set fashion-items { item-id: item-id }
      (merge item { is-available: false, owner: tx-sender }))
    
    ;; Record transaction
    (map-set transactions { transaction-id: transaction-id }
      {
        item-id: item-id,
        seller: (get owner item),
        buyer: tx-sender,
        price: (get price item),
        timestamp: block-height,
        sustainability-impact: (calculate-sustainability-impact 
          (get carbon-footprint item) 
          (get sustainability-score item))
      })
    
    ;; Update user profiles
    (update-user-profile (get owner item) u1 u0)
    (update-user-profile tx-sender u0 u1)
    
    (var-set total-transactions (+ transaction-id u1))
    (ok transaction-id)
  )
)

;; Add item review
(define-public (add-review 
  (item-id uint) 
  (rating uint) 
  (sustainability-rating uint) 
  (review-text (string-ascii 256)))
  (let ((item (unwrap! (map-get? fashion-items { item-id: item-id }) err-not-found)))
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-price)
    (asserts! (and (>= sustainability-rating u1) (<= sustainability-rating u5)) err-invalid-price)
    (asserts! (is-eq tx-sender (get owner item)) err-unauthorized)
    
    (map-set item-reviews { item-id: item-id, reviewer: tx-sender }
      {
        rating: rating,
        sustainability-rating: sustainability-rating,
        review-text: review-text,
        timestamp: block-height
      })
    (ok true)
  )
)

;; Update item price
(define-public (update-item-price (item-id uint) (new-price uint))
  (let ((item (unwrap! (map-get? fashion-items { item-id: item-id }) err-not-found)))
    (asserts! (is-eq tx-sender (get owner item)) err-unauthorized)
    (asserts! (get is-available item) err-item-not-available)
    (asserts! (> new-price u0) err-invalid-price)
    
    (map-set fashion-items { item-id: item-id }
      (merge item { price: new-price }))
    (ok true)
  )
)

;; Set marketplace fee (owner only)
(define-public (set-marketplace-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u1000) err-invalid-price) ;; Max 10%
    (var-set marketplace-fee-percentage new-fee)
    (ok true)
  )
)

;; Read-only Functions

;; Get item details
(define-read-only (get-item (item-id uint))
  (map-get? fashion-items { item-id: item-id })
)

;; Get user profile
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

;; Get verifier info
(define-read-only (get-verifier-info (verifier principal))
  (map-get? verifiers { verifier: verifier })
)

;; Get transaction details
(define-read-only (get-transaction (transaction-id uint))
  (map-get? transactions { transaction-id: transaction-id })
)

;; Get item review
(define-read-only (get-item-review (item-id uint) (reviewer principal))
  (map-get? item-reviews { item-id: item-id, reviewer: reviewer })
)

;; Get marketplace stats
(define-read-only (get-marketplace-stats)
  {
    total-items: (var-get next-item-id),
    total-verified-items: (var-get total-verified-items),
    total-transactions: (var-get total-transactions),
    marketplace-fee: (var-get marketplace-fee-percentage)
  }
)

;; Get sustainability impact
(define-read-only (get-sustainability-impact (item-id uint))
  (match (map-get? fashion-items { item-id: item-id })
    item (calculate-sustainability-impact 
      (get carbon-footprint item) 
      (get sustainability-score item))
    u0
  )
)