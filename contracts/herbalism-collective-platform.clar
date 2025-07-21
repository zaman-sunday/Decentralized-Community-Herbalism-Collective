;; ===============================================
;; CONTRACT 1: HERBALISM-KNOWLEDGE-REGISTRY
;; Core contract for plant knowledge and safety protocols
;; ===============================================

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_INPUT (err u103))
(define-constant ERR_SAFETY_VIOLATION (err u104))

;; Data Variables
(define-data-var next-plant-id uint u1)
(define-data-var next-preparation-id uint u1)
(define-data-var platform-fee-percentage uint u250) ;; 2.5%

;; Safety Levels
(define-constant SAFETY_LEVEL_SAFE u1)
(define-constant SAFETY_LEVEL_CAUTION u2)
(define-constant SAFETY_LEVEL_RESTRICTED u3)
(define-constant SAFETY_LEVEL_CEREMONIAL u4)

;; Data Maps
(define-map plant-registry
  { plant-id: uint }
  {
    scientific-name: (string-utf8 100),
    common-names: (list 5 (string-utf8 50)),
    safety-level: uint,
    indigenous-origin: (optional (string-utf8 100)),
    cultivation-zones: (list 10 uint),
    documented-by: principal,
    verification-status: bool,
    documentation-hash: (string-utf8 64),
    created-at: uint
  }
)

(define-map preparation-methods
  { preparation-id: uint }
  {
    plant-id: uint,
    method-name: (string-utf8 100),
    preparation-type: (string-utf8 50), ;; "tincture", "tea", "salve", "powder", etc.
    instructions: (string-utf8 500),
    safety-warnings: (list 5 (string-utf8 200)),
    dosage-guidelines: (string-utf8 200),
    contraindications: (list 10 (string-utf8 100)),
    documented-by: principal,
    verification-count: uint,
    created-at: uint
  }
)

(define-map cultivation-records
  { plant-id: uint, cultivator: principal }
  {
    location-zone: uint,
    planting-date: uint,
    harvest-dates: (list 20 uint),
    growth-notes: (string-utf8 500),
    yield-data: (list 20 uint), ;; grams per harvest
    soil-conditions: (string-utf8 200),
    organic-certified: bool,
    wildcrafting-ethics-followed: bool
  }
)

(define-map safety-protocols
  { protocol-id: uint }
  {
    protocol-name: (string-utf8 100),
    safety-level-required: uint,
    prerequisites: (list 5 (string-utf8 100)),
    emergency-procedures: (string-utf8 500),
    authorized-practitioners: (list 20 principal),
    created-by: principal,
    active: bool
  }
)

(define-map practitioner-credentials
  { practitioner: principal }
  {
    certification-level: uint,
    specializations: (list 10 (string-utf8 50)),
    years-experience: uint,
    indigenous-lineage: (optional (string-utf8 100)),
    verified-by: (list 3 principal),
    safety-clearance-level: uint,
    active-status: bool,
    last-updated: uint
  }
)

(define-map therapeutic-outcomes
  { outcome-id: uint }
  {
    plant-id: uint,
    preparation-id: uint,
    condition-treated: (string-utf8 100),
    effectiveness-rating: uint, ;; 1-10 scale
    side-effects: (list 5 (string-utf8 100)),
    dosage-used: (string-utf8 100),
    duration-of-treatment: uint,
    reported-by: principal,
    practitioner-verified: bool,
    anonymous: bool,
    created-at: uint
  }
)

;; Read-only functions
(define-read-only (get-plant-info (plant-id uint))
  (map-get? plant-registry { plant-id: plant-id })
)

(define-read-only (get-preparation-method (preparation-id uint))
  (map-get? preparation-methods { preparation-id: preparation-id })
)

(define-read-only (get-cultivation-record (plant-id uint) (cultivator principal))
  (map-get? cultivation-records { plant-id: plant-id, cultivator: cultivator })
)

(define-read-only (get-practitioner-credentials (practitioner principal))
  (map-get? practitioner-credentials { practitioner: practitioner })
)

(define-read-only (get-safety-protocol (protocol-id uint))
  (map-get? safety-protocols { protocol-id: protocol-id })
)

(define-read-only (is-authorized-practitioner (practitioner principal) (safety-level uint))
  (match (map-get? practitioner-credentials { practitioner: practitioner })
    creds (>= (get safety-clearance-level creds) safety-level)
    false
  )
)

(define-read-only (get-next-plant-id)
  (var-get next-plant-id)
)

(define-read-only (get-next-preparation-id)
  (var-get next-preparation-id)
)

;; Public functions
(define-public (register-plant
  (scientific-name (string-utf8 100))
  (common-names (list 5 (string-utf8 50)))
  (safety-level uint)
  (indigenous-origin (optional (string-utf8 100)))
  (cultivation-zones (list 10 uint))
  (documentation-hash (string-utf8 64)))
  (let ((plant-id (var-get next-plant-id)))
    (asserts! (and (>= safety-level u1) (<= safety-level u4)) ERR_INVALID_INPUT)
    (asserts! (> (len scientific-name) u0) ERR_INVALID_INPUT)
    (map-set plant-registry
      { plant-id: plant-id }
      {
        scientific-name: scientific-name,
        common-names: common-names,
        safety-level: safety-level,
        indigenous-origin: indigenous-origin,
        cultivation-zones: cultivation-zones,
        documented-by: tx-sender,
        verification-status: false,
        documentation-hash: documentation-hash,
        created-at: stacks-block-height
      }
    )
    (var-set next-plant-id (+ plant-id u1))
    (ok plant-id)
  )
)

(define-public (add-preparation-method
  (plant-id uint)
  (method-name (string-utf8 100))
  (preparation-type (string-utf8 50))
  (instructions (string-utf8 500))
  (safety-warnings (list 5 (string-utf8 200)))
  (dosage-guidelines (string-utf8 200))
  (contraindications (list 10 (string-utf8 100))))
  (let ((preparation-id (var-get next-preparation-id))
        (plant-data (unwrap! (map-get? plant-registry { plant-id: plant-id }) ERR_NOT_FOUND)))
    (asserts! (> (len method-name) u0) ERR_INVALID_INPUT)
    (asserts! (> (len instructions) u0) ERR_INVALID_INPUT)
    ;; Check safety authorization
    (asserts! (is-authorized-practitioner tx-sender (get safety-level plant-data)) ERR_UNAUTHORIZED)
    (map-set preparation-methods
      { preparation-id: preparation-id }
      {
        plant-id: plant-id,
        method-name: method-name,
        preparation-type: preparation-type,
        instructions: instructions,
        safety-warnings: safety-warnings,
        dosage-guidelines: dosage-guidelines,
        contraindications: contraindications,
        documented-by: tx-sender,
        verification-count: u0,
        created-at: stacks-block-height
      }
    )
    (var-set next-preparation-id (+ preparation-id u1))
    (ok preparation-id)
  )
)

(define-public (record-cultivation
  (plant-id uint)
  (location-zone uint)
  (planting-date uint)
  (soil-conditions (string-utf8 200))
  (organic-certified bool)
  (wildcrafting-ethics-followed bool)
  (initial-notes (string-utf8 500)))
  (begin
    (asserts! (is-some (map-get? plant-registry { plant-id: plant-id })) ERR_NOT_FOUND)
    (asserts! (> location-zone u0) ERR_INVALID_INPUT)
    (map-set cultivation-records
      { plant-id: plant-id, cultivator: tx-sender }
      {
        location-zone: location-zone,
        planting-date: planting-date,
        harvest-dates: (list),
        growth-notes: initial-notes,
        yield-data: (list),
        soil-conditions: soil-conditions,
        organic-certified: organic-certified,
        wildcrafting-ethics-followed: wildcrafting-ethics-followed
      }
    )
    (ok true)
  )
)

(define-public (register-practitioner
  (certification-level uint)
  (specializations (list 10 (string-utf8 50)))
  (years-experience uint)
  (indigenous-lineage (optional (string-utf8 100)))
  (safety-clearance-level uint))
  (begin
    (asserts! (and (>= certification-level u1) (<= certification-level u5)) ERR_INVALID_INPUT)
    (asserts! (<= safety-clearance-level u4) ERR_INVALID_INPUT)
    (map-set practitioner-credentials
      { practitioner: tx-sender }
      {
        certification-level: certification-level,
        specializations: specializations,
        years-experience: years-experience,
        indigenous-lineage: indigenous-lineage,
        verified-by: (list),
        safety-clearance-level: safety-clearance-level,
        active-status: true,
        last-updated: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (verify-plant-knowledge (plant-id uint))
  (let ((plant-data (unwrap! (map-get? plant-registry { plant-id: plant-id }) ERR_NOT_FOUND)))
    (asserts! (is-authorized-practitioner tx-sender u2) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq tx-sender (get documented-by plant-data))) ERR_UNAUTHORIZED)
    (map-set plant-registry
      { plant-id: plant-id }
      (merge plant-data { verification-status: true })
    )
    (ok true)
  )
)
