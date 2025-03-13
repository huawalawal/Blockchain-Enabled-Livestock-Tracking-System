;; Animal Registration Contract
;; Records details of individual animals from birth

;; Data variables
(define-data-var animal-counter uint u0)
(define-data-var species-counter uint u0)
(define-data-var breed-counter uint u0)

;; Data maps
(define-map animals
{ id: uint }
{
  species-id: uint,
  breed-id: uint,
  birth-date: uint,
  birth-location: (string-ascii 64),
  parent-female-id: (optional uint),
  parent-male-id: (optional uint),
  dna-hash: (optional (buff 32)),
  status: (string-ascii 16),
  registered-by: principal
}
)

(define-map species
{ id: uint }
{
  name: (string-ascii 32),
  active: bool
}
)

(define-map breeds
{ id: uint }
{
  species-id: uint,
  name: (string-ascii 32),
  active: bool
}
)

(define-map registrars
{ address: principal }
{ active: bool }
)

;; Initialize contract
(define-public (initialize)
(begin
  (map-set registrars { address: tx-sender } { active: true })
  (ok true)
)
)

;; Check if address is registrar
(define-read-only (is-registrar (address principal))
(default-to false (get active (map-get? registrars { address: address })))
)

;; Add a registrar
(define-public (add-registrar (address principal))
(begin
  ;; Only registrars can add registrars
  (asserts! (is-registrar tx-sender) (err u403))

  (map-set registrars
    { address: address }
    { active: true }
  )

  (ok true)
)
)

;; Register a species
(define-public (register-species (name (string-ascii 32)))
(let ((new-id (+ (var-get species-counter) u1)))
  ;; Only registrars can register species
  (asserts! (is-registrar tx-sender) (err u403))

  ;; Update counter
  (var-set species-counter new-id)

  ;; Store species data
  (map-set species
    { id: new-id }
    {
      name: name,
      active: true
    }
  )

  (ok new-id)
)
)

;; Register a breed
(define-public (register-breed (species-id uint) (name (string-ascii 32)))
(let ((new-id (+ (var-get breed-counter) u1)))
  ;; Only registrars can register breeds
  (asserts! (is-registrar tx-sender) (err u403))

  ;; Species must exist
  (asserts! (is-some (map-get? species { id: species-id })) (err u404))

  ;; Update counter
  (var-set breed-counter new-id)

  ;; Store breed data
  (map-set breeds
    { id: new-id }
    {
      species-id: species-id,
      name: name,
      active: true
    }
  )

  (ok new-id)
)
)

;; Register an animal
(define-public (register-animal
  (species-id uint)
  (breed-id uint)
  (birth-date uint)
  (birth-location (string-ascii 64))
  (parent-female-id (optional uint))
  (parent-male-id (optional uint))
  (dna-hash (optional (buff 32))))
(let ((new-id (+ (var-get animal-counter) u1)))
  ;; Only registrars can register animals
  (asserts! (is-registrar tx-sender) (err u403))

  ;; Species and breed must exist
  (asserts! (and
              (is-some (map-get? species { id: species-id }))
              (is-some (map-get? breeds { id: breed-id })))
            (err u404))

  ;; Breed must belong to species
  (asserts! (is-eq species-id (get species-id (unwrap-panic (map-get? breeds { id: breed-id })))) (err u400))

  ;; Birth date must be in the past
  (asserts! (<= birth-date block-height) (err u400))

  ;; Validate parent IDs if provided
  (if (is-some parent-female-id)
    (asserts! (is-some (map-get? animals { id: (unwrap-panic parent-female-id) })) (err u404))
    true
  )

  (if (is-some parent-male-id)
    (asserts! (is-some (map-get? animals { id: (unwrap-panic parent-male-id) })) (err u404))
    true
  )

  ;; Update counter
  (var-set animal-counter new-id)

  ;; Store animal data
  (map-set animals
    { id: new-id }
    {
      species-id: species-id,
      breed-id: breed-id,
      birth-date: birth-date,
      birth-location: birth-location,
      parent-female-id: parent-female-id,
      parent-male-id: parent-male-id,
      dna-hash: dna-hash,
      status: "active",
      registered-by: tx-sender
    }
  )

  (ok new-id)
)
)

;; Update animal status
(define-public (update-animal-status (animal-id uint) (status (string-ascii 16)))
(let ((animal (map-get? animals { id: animal-id })))
  ;; Only registrars can update status
  (asserts! (is-registrar tx-sender) (err u403))

  ;; Animal must exist
  (asserts! (is-some animal) (err u404))

  ;; Status must be valid
  (asserts! (or
              (is-eq status "active")
              (is-eq status "sold")
              (is-eq status "deceased")
              (is-eq status "slaughtered")
              (is-eq status "exported"))
            (err u400))

  ;; Store updated animal
  (map-set animals
    { id: animal-id }
    (merge (unwrap-panic animal) { status: status })
  )

  (ok true)
)
)

;; Get animal details
(define-read-only (get-animal (animal-id uint))
(map-get? animals { id: animal-id })
)

;; Get species details
(define-read-only (get-species (species-id uint))
(map-get? species { id: species-id })
)

;; Get breed details
(define-read-only (get-breed (breed-id uint))
(map-get? breeds { id: breed-id })
)

;; Check if animal exists and is active
(define-read-only (is-animal-active (animal-id uint))
(let ((animal (map-get? animals { id: animal-id })))
  (and
    (is-some animal)
    (is-eq (get status (unwrap-panic animal)) "active")
  )
)
)

