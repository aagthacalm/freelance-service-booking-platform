;; title: freelance_service_booking_platform
;; version: 1.0.0
;; summary: Smart contract for Freelance Service Booking Platform

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-INVALID-AMOUNT (err u400))
(define-constant ERR-BOOKING-EXISTS (err u409))
(define-constant ERR-INVALID-STATUS (err u422))
(define-constant ERR-INSUFFICIENT-BALANCE (err u402))
(define-constant ERR-SERVICE-NOT-ACTIVE (err u403))

(define-constant MIN-SERVICE-PRICE u1000)
(define-constant MAX-SERVICE-PRICE u1000000000)
(define-constant BOOKING-TIMEOUT u144)

(define-data-var platform-fee-percent uint u5)
(define-data-var total-bookings uint u0)
(define-data-var platform-balance uint u0)

(define-map freelancers
  { freelancer: principal }
  {
    name: (string-utf8 256),
    description: (string-utf8 1024),
    hourly-rate: uint,
    is-active: bool,
    total-completed: uint,
    rating-sum: uint,
    rating-count: uint,
    joined-block: uint
  }
)

(define-map services
  { service-id: uint }
  {
    freelancer: principal,
    title: (string-utf8 256),
    description: (string-utf8 1024),
    price: uint,
    duration-hours: uint,
    is-active: bool,
    created-block: uint
  }
)

(define-map bookings
  { booking-id: uint }
  {
    client: principal,
    freelancer: principal,
    service-id: uint,
    amount: uint,
    status: (string-ascii 20),
    created-block: uint,
    completed-block: (optional uint),
    rating: (optional uint)
  }
)

(define-map escrow
  { booking-id: uint }
  {
    amount: uint,
    held-block: uint
  }
)

(define-map client-bookings
  { client: principal }
  {
    booking-ids: (list 100 uint)
  }
)

(define-map freelancer-bookings
  { freelancer: principal }
  {
    booking-ids: (list 100 uint)
  }
)

(define-public (register-freelancer (name (string-utf8 256)) (description (string-utf8 1024)) (hourly-rate uint))
  (begin
    (asserts! (and (> hourly-rate u0) (<= hourly-rate MAX-SERVICE-PRICE)) ERR-INVALID-AMOUNT)
    (asserts! (is-none (map-get? freelancers { freelancer: tx-sender })) ERR-BOOKING-EXISTS)
    (map-set freelancers
      { freelancer: tx-sender }
      {
        name: name,
        description: description,
        hourly-rate: hourly-rate,
        is-active: true,
        total-completed: u0,
        rating-sum: u0,
        rating-count: u0,
        joined-block: burn-block-height
      }
    )
    (ok true)
  )
)

(define-public (create-service (title (string-utf8 256)) (description (string-utf8 1024)) (price uint) (duration-hours uint))
  (begin
    (asserts! (is-some (map-get? freelancers { freelancer: tx-sender })) ERR-UNAUTHORIZED)
    (asserts! (and (>= price MIN-SERVICE-PRICE) (<= price MAX-SERVICE-PRICE)) ERR-INVALID-AMOUNT)
    (asserts! (> duration-hours u0) ERR-INVALID-AMOUNT)
    (let ((service-id (+ (var-get total-bookings) u1)))
      (map-set services
        { service-id: service-id }
        {
          freelancer: tx-sender,
          title: title,
          description: description,
          price: price,
          duration-hours: duration-hours,
          is-active: true,
          created-block: burn-block-height
        }
      )
      (ok service-id)
    )
  )
)

(define-public (book-service (service-id uint) (amount uint))
  (begin
    (asserts! (is-some (map-get? services { service-id: service-id })) ERR-NOT-FOUND)
    (let ((service (unwrap! (map-get? services { service-id: service-id }) ERR-NOT-FOUND)))
      (asserts! (get is-active service) ERR-SERVICE-NOT-ACTIVE)
      (asserts! (> amount u0) ERR-INVALID-AMOUNT)
      (let ((booking-id (+ (var-get total-bookings) u1)))
        (map-set bookings
          { booking-id: booking-id }
          {
            client: tx-sender,
            freelancer: (get freelancer service),
            service-id: service-id,
            amount: amount,
            status: "pending",
            created-block: burn-block-height,
            completed-block: none,
            rating: none
          }
        )
        (map-set escrow
          { booking-id: booking-id }
          {
            amount: amount,
            held-block: burn-block-height
          }
        )
        (var-set total-bookings booking-id)
        (ok booking-id)
      )
    )
  )
)

(define-public (accept-booking (booking-id uint))
  (begin
    (asserts! (is-some (map-get? bookings { booking-id: booking-id })) ERR-NOT-FOUND)
    (let ((booking (unwrap! (map-get? bookings { booking-id: booking-id }) ERR-NOT-FOUND)))
      (asserts! (is-eq tx-sender (get freelancer booking)) ERR-UNAUTHORIZED)
      (asserts! (is-eq (get status booking) "pending") ERR-INVALID-STATUS)
      (map-set bookings
        { booking-id: booking-id }
        (merge booking { status: "accepted" })
      )
      (ok true)
    )
  )
)

(define-public (complete-booking (booking-id uint) (rating uint))
  (begin
    (asserts! (is-some (map-get? bookings { booking-id: booking-id })) ERR-NOT-FOUND)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-AMOUNT)
    (let ((booking (unwrap! (map-get? bookings { booking-id: booking-id }) ERR-NOT-FOUND))
          (escrow-data (unwrap! (map-get? escrow { booking-id: booking-id }) ERR-NOT-FOUND)))
      (asserts! (is-eq tx-sender (get client booking)) ERR-UNAUTHORIZED)
      (asserts! (is-eq (get status booking) "accepted") ERR-INVALID-STATUS)
      (let ((fee-amount (/ (* (get amount booking) (var-get platform-fee-percent)) u100))
            (freelancer-payout (- (get amount booking) fee-amount)))
        (map-set bookings
          { booking-id: booking-id }
          (merge booking { 
            status: "completed",
            completed-block: (some burn-block-height),
            rating: (some rating)
          })
        )
        (map-delete escrow { booking-id: booking-id })
        (var-set platform-balance (+ (var-get platform-balance) fee-amount))
        (ok freelancer-payout)
      )
    )
  )
)

(define-public (cancel-booking (booking-id uint))
  (begin
    (asserts! (is-some (map-get? bookings { booking-id: booking-id })) ERR-NOT-FOUND)
    (let ((booking (unwrap! (map-get? bookings { booking-id: booking-id }) ERR-NOT-FOUND)))
      (asserts! (or (is-eq tx-sender (get client booking)) (is-eq tx-sender (get freelancer booking))) ERR-UNAUTHORIZED)
      (asserts! (or (is-eq (get status booking) "pending") (is-eq (get status booking) "accepted")) ERR-INVALID-STATUS)
      (map-set bookings
        { booking-id: booking-id }
        (merge booking { status: "cancelled" })
      )
      (map-delete escrow { booking-id: booking-id })
      (ok true)
    )
  )
)

(define-read-only (get-freelancer (freelancer principal))
  (map-get? freelancers { freelancer: freelancer })
)

(define-read-only (get-service (service-id uint))
  (map-get? services { service-id: service-id })
)

(define-read-only (get-booking (booking-id uint))
  (map-get? bookings { booking-id: booking-id })
)

(define-read-only (get-escrow (booking-id uint))
  (map-get? escrow { booking-id: booking-id })
)

(define-read-only (get-platform-fee)
  (var-get platform-fee-percent)
)

(define-read-only (get-total-bookings)
  (var-get total-bookings)
)

(define-read-only (get-platform-balance)
  (var-get platform-balance)
)

(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (and (>= new-fee u0) (<= new-fee u100)) ERR-INVALID-AMOUNT)
    (var-set platform-fee-percent new-fee)
    (ok true)
  )
)

(define-public (update-freelancer-profile (name (string-utf8 256)) (description (string-utf8 1024)) (hourly-rate uint))
  (begin
    (asserts! (is-some (map-get? freelancers { freelancer: tx-sender })) ERR-NOT-FOUND)
    (asserts! (and (> hourly-rate u0) (<= hourly-rate MAX-SERVICE-PRICE)) ERR-INVALID-AMOUNT)
    (let ((freelancer-data (unwrap! (map-get? freelancers { freelancer: tx-sender }) ERR-NOT-FOUND)))
      (map-set freelancers
        { freelancer: tx-sender }
        (merge freelancer-data {
          name: name,
          description: description,
          hourly-rate: hourly-rate
        })
      )
      (ok true)
    )
  )
)

(define-public (deactivate-service (service-id uint))
  (begin
    (asserts! (is-some (map-get? services { service-id: service-id })) ERR-NOT-FOUND)
    (let ((service (unwrap! (map-get? services { service-id: service-id }) ERR-NOT-FOUND)))
      (asserts! (is-eq tx-sender (get freelancer service)) ERR-UNAUTHORIZED)
      (map-set services
        { service-id: service-id }
        (merge service { is-active: false })
      )
      (ok true)
    )
  )
)
