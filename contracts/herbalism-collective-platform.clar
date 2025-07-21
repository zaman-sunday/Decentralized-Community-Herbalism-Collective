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
  (documentation-hash (string-utf8 64))
)
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
  (contraindications (list 10 (string-utf8 100)))
)
  (let ((preparation-id (var-get next-preparation-id)))
    (asserts! (is-some (map-get? plant-registry { plant-id: plant-id })) ERR_NOT_FOUND)
    (asserts! (> (len method-name) u0) ERR_INVALID_INPUT)
    (asserts! (> (len instructions) u0) ERR_INVALID_INPUT)

    ;; Check safety authorization
    (match (map-get? plant-registry { plant-id: plant-id })
      plant-data (asserts! (is-authorized-practitioner tx-sender (get safety-level plant-data)) ERR_UNAUTHORIZED)
      ERR_NOT_FOUND
    )

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
  (initial-notes (string-utf8 500))
)
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
  (safety-clearance-level uint)
)
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

;; ===============================================
;; CONTRACT 2: HERBALISM-COMMUNITY-WORKSHOPS
;; Workshop coordination and community engagement
;; ===============================================

;; Constants
(define-constant MAX_PARTICIPANTS uint u50)
(define-constant MIN_ADVANCE_BOOKING uint u144) ;; ~1 day in blocks

;; Data Variables
(define-data-var next-workshop-id uint u1)
(define-data-var next-resource-id uint u1)

;; Workshop Types
(define-constant WORKSHOP_TYPE_CULTIVATION u1)
(define-constant WORKSHOP_TYPE_PREPARATION u2)
(define-constant WORKSHOP_TYPE_IDENTIFICATION u3)
(define-constant WORKSHOP_TYPE_HEALING u4)
(define-constant WORKSHOP_TYPE_CEREMONIAL u5)

;; Data Maps
(define-map workshops
  { workshop-id: uint }
  {
    title: (string-utf8 100),
    description: (string-utf8 500),
    workshop-type: uint,
    facilitator: principal,
    co-facilitators: (list 5 principal),
    max-participants: uint,
    current-participants: uint,
    location: (string-utf8 200),
    scheduled-date: uint,
    duration-blocks: uint,
    fee-amount: uint,
    materials-provided: (list 20 (string-utf8 100)),
    prerequisites: (list 5 (string-utf8 100)),
    safety-level-required: uint,
    indigenous-protocols: bool,
    status: (string-utf8 20), ;; "scheduled", "active", "completed", "cancelled"
    created-at: uint
  }
)

(define-map workshop-participants
  { workshop-id: uint, participant: principal }
  {
    registration-date: uint,
    payment-confirmed: bool,
    attendance-confirmed: bool,
    completion-verified: bool,
    feedback-submitted: bool,
    indigenous-background: (optional (string-utf8 100))
  }
)

(define-map community-resources
  { resource-id: uint }
  {
    resource-name: (string-utf8 100),
    resource-type: (string-utf8 50), ;; "tool", "book", "seeds", "equipment"
    description: (string-utf8 300),
    owner: principal,
    available: bool,
    location: (string-utf8 200),
    sharing-fee: uint,
    booking-duration-max: uint,
    safety-requirements: (list 5 (string-utf8 100)),
    created-at: uint
  }
)

(define-map resource-bookings
  { resource-id: uint, booking-id: uint }
  {
    borrower: principal,
    start-date: uint,
    end-date: uint,
    purpose: (string-utf8 200),
    approved: bool,
    completed: bool,
    condition-report: (optional (string-utf8 300))
  }
)

(define-map healing-circles
  { circle-id: uint }
  {
    circle-name: (string-utf8 100),
    facilitators: (list 3 principal),
    members: (list 30 principal),
    focus-area: (string-utf8 100),
    meeting-frequency: uint, ;; blocks between meetings
    privacy-level: uint, ;; 1=open, 2=invite-only, 3=closed
    indigenous-protocols: bool,
    location-type: (string-utf8 50), ;; "physical", "virtual", "hybrid"
    active: bool,
    created-at: uint
  }
)

(define-map knowledge-exchanges
  { exchange-id: uint }
  {
    offerer: principal,
    seeker: principal,
    knowledge-offered: (string-utf8 200),
    knowledge-sought: (string-utf8 200),
    exchange-type: (string-utf8 50), ;; "direct", "mentorship", "trade"
    status: (string-utf8 20), ;; "pending", "matched", "active", "completed"
    duration-estimate: uint,
    reciprocity-agreement: (string-utf8 300),
    created-at: uint
  }
)

;; Read-only functions
(define-read-only (get-workshop-info (workshop-id uint))
  (map-get? workshops { workshop-id: workshop-id })
)

(define-read-only (get-participant-info (workshop-id uint) (participant principal))
  (map-get? workshop-participants { workshop-id: workshop-id, participant: participant })
)

(define-read-only (is-workshop-full (workshop-id uint))
  (match (map-get? workshops { workshop-id: workshop-id })
    workshop (>= (get current-participants workshop) (get max-participants workshop))
    true
  )
)

(define-read-only (get-community-resource (resource-id uint))
  (map-get? community-resources { resource-id: resource-id })
)

(define-read-only (get-healing-circle (circle-id uint))
  (map-get? healing-circles { circle-id: circle-id })
)

;; Public functions
(define-public (create-workshop
  (title (string-utf8 100))
  (description (string-utf8 500))
  (workshop-type uint)
  (max-participants uint)
  (location (string-utf8 200))
  (scheduled-date uint)
  (duration-blocks uint)
  (fee-amount uint)
  (materials-provided (list 20 (string-utf8 100)))
  (safety-level-required uint)
  (indigenous-protocols bool)
)
  (let ((workshop-id (var-get next-workshop-id)))
    (asserts! (> (len title) u0) ERR_INVALID_INPUT)
    (asserts! (and (>= workshop-type u1) (<= workshop-type u5)) ERR_INVALID_INPUT)
    (asserts! (and (> max-participants u0) (<= max-participants MAX_PARTICIPANTS)) ERR_INVALID_INPUT)
    (asserts! (> scheduled-date (+ stacks-block-height MIN_ADVANCE_BOOKING)) ERR_INVALID_INPUT)
    (asserts! (is-authorized-practitioner tx-sender safety-level-required) ERR_UNAUTHORIZED)

    (map-set workshops
      { workshop-id: workshop-id }
      {
        title: title,
        description: description,
        workshop-type: workshop-type,
        facilitator: tx-sender,
        co-facilitators: (list),
        max-participants: max-participants,
        current-participants: u0,
        location: location,
        scheduled-date: scheduled-date,
        duration-blocks: duration-blocks,
        fee-amount: fee-amount,
        materials-provided: materials-provided,
        prerequisites: (list),
        safety-level-required: safety-level-required,
        indigenous-protocols: indigenous-protocols,
        status: "scheduled",
        created-at: stacks-block-height
      }
    )

    (var-set next-workshop-id (+ workshop-id u1))
    (ok workshop-id)
  )
)

(define-public (register-for-workshop
  (workshop-id uint)
  (indigenous-background (optional (string-utf8 100)))
)
  (let ((workshop (unwrap! (map-get? workshops { workshop-id: workshop-id }) ERR_NOT_FOUND)))
    (asserts! (not (is-workshop-full workshop-id)) ERR_INVALID_INPUT)
    (asserts! (is-eq (get status workshop) "scheduled") ERR_INVALID_INPUT)
    (asserts! (is-none (map-get? workshop-participants { workshop-id: workshop-id, participant: tx-sender })) ERR_ALREADY_EXISTS)

    ;; Check safety clearance
    (asserts! (is-authorized-practitioner tx-sender (get safety-level-required workshop)) ERR_UNAUTHORIZED)

    (map-set workshop-participants
      { workshop-id: workshop-id, participant: tx-sender }
      {
        registration-date: stacks-block-height,
        payment-confirmed: (is-eq (get fee-amount workshop) u0),
        attendance-confirmed: false,
        completion-verified: false,
        feedback-submitted: false,
        indigenous-background: indigenous-background
      }
    )

    ;; Update participant count
    (map-set workshops
      { workshop-id: workshop-id }
      (merge workshop { current-participants: (+ (get current-participants workshop) u1) })
    )

    (ok true)
  )
)

(define-public (add-community-resource
  (resource-name (string-utf8 100))
  (resource-type (string-utf8 50))
  (description (string-utf8 300))
  (location (string-utf8 200))
  (sharing-fee uint)
  (booking-duration-max uint)
  (safety-requirements (list 5 (string-utf8 100)))
)
  (let ((resource-id (var-get next-resource-id)))
    (asserts! (> (len resource-name) u0) ERR_INVALID_INPUT)
    (asserts! (> booking-duration-max u0) ERR_INVALID_INPUT)

    (map-set community-resources
      { resource-id: resource-id }
      {
        resource-name: resource-name,
        resource-type: resource-type,
        description: description,
        owner: tx-sender,
        available: true,
        location: location,
        sharing-fee: sharing-fee,
        booking-duration-max: booking-duration-max,
        safety-requirements: safety-requirements,
        created-at: stacks-block-height
      }
    )

    (var-set next-resource-id (+ resource-id u1))
    (ok resource-id)
  )
)

(define-public (confirm-workshop-attendance (workshop-id uint) (participant principal))
  (let (
    (workshop (unwrap! (map-get? workshops { workshop-id: workshop-id }) ERR_NOT_FOUND))
    (participant-info (unwrap! (map-get? workshop-participants { workshop-id: workshop-id, participant: participant }) ERR_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get facilitator workshop)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status workshop) "active") ERR_INVALID_INPUT)

    (map-set workshop-participants
      { workshop-id: workshop-id, participant: participant }
      (merge participant-info { attendance-confirmed: true })
    )

    (ok true)
  )
)

;; ===============================================
;; CONTRACT 3: HERBALISM-SUSTAINABILITY-TRACKER
;; Environmental impact and sustainability metrics
;; ===============================================

;; Constants
(define-constant SUSTAINABILITY_SCORE_MAX uint u100)
(define-constant WILDCRAFTING_SEASON_BLOCKS uint u8640) ;; ~60 days

;; Data Variables
(define-data-var next-sustainability-report-id uint u1)
(define-data-var carbon-offset-rate uint u100) ;; STX per kg CO2

;; Sustainability Metrics
(define-map garden-ecosystems
  { garden-id: uint }
  {
    owner: principal,
    location-coordinates: (string-utf8 50),
    size-square-meters: uint,
    biodiversity-score: uint,
    soil-health-rating: uint,
    water-conservation-methods: (list 10 (string-utf8 100)),
    companion-planting: bool,
    organic-practices: bool,
    native-species-count: uint,
    medicinal-species-count: uint,
    carbon-sequestration-estimate: uint, ;; kg CO2 per year
    established-date: uint,
    certification-level: uint
  }
)

(define-map wildcrafting-permits
  { permit-id: uint }
  {
    harvester: principal,
    species-list: (list 20 (string-utf8 100)),
    location-zone: uint,
    seasonal-restrictions: (list 4 uint), ;; blocks when harvesting allowed
    max-harvest-percentage: uint, ;; percentage of population
    sustainable-practices-agreement: bool,
    indigenous-permission-obtained: bool,
    monitoring-requirements: (list 5 (string-utf8 100)),
    valid-until: uint,
    renewal-count: uint,
    violations: uint
  }
)

(define-map harvest-reports
  { report-id: uint }
  {
    harvester: principal,
    species-harvested: (string-utf8 100),
    location-zone: uint,
    harvest-date: uint,
    quantity-harvested: uint, ;; grams
    population-estimated: uint,
    percentage-harvested: uint,
    regeneration-notes: (string-utf8 300),
    environmental-conditions: (string-utf8 200),
    sustainability-score: uint,
    permit-id: (optional uint)
  }
)

(define-map carbon-tracking
  { activity-id: uint }
  {
    actor: principal,
    activity-type: (string-utf8 50), ;; "cultivation", "processing", "transport", "workshop"
    carbon-footprint: uint, ;; kg CO2 equivalent
    offset-methods: (list 5 (string-utf8 100)),
    net-impact: int, ;; positive = sequestration, negative = emission
    measurement-date: uint,
    verification-status: bool
  }
)

(define-map biodiversity-monitoring
  { site-id: uint, monitoring-date: uint }
  {
    recorder: principal,
    species-observed: (list 50 (string-utf8 100)),
    endangered-species-present: (list 10 (string-utf8 100)),
    habitat-quality-score: uint,
    threats-identified: (list 10 (string-utf8 100)),
    conservation-actions-taken: (list 10 (string-utf8 100)),
    photo-documentation-hash: (optional (string-utf8 64))
  }
)

(define-map sustainability-certificates
  { certificate-id: uint }
  {
    recipient: principal,
    certificate-type: (string-utf8 50),
    sustainability-score: uint,
    practices-verified: (list 20 (string-utf8 100)),
    issued-by: principal,
    valid-from: uint,
    valid-until: uint,
    audit-trail: (list 10 (string-utf8 200))
  }
)

;; Read-only functions
(define-read-only (get-garden-ecosystem (garden-id uint))
  (map-get? garden-ecosystems { garden-id: garden-id })
)

(define-read-only (get-wildcrafting-permit (permit-id uint))
  (map-get? wildcrafting-permits { permit-id: permit-id })
)

(define-read-only (get-harvest-report (report-id uint))
  (map-get? harvest-reports { report-id: report-id })
)

(define-read-only (calculate-sustainability-score (garden-id uint))
  (match (map-get? garden-ecosystems { garden-id: garden-id })
    garden (let (
      (biodiversity-factor (* (get biodiversity-score garden) u2))
      (soil-factor (get soil-health-rating garden))
      (organic-bonus (if (get organic-practices garden) u20 u0))
      (native-bonus (* (get native-species-count garden) u2))
      (carbon-bonus (/ (get carbon-sequestration-estimate garden) u10))
    )
      (min (+ biodiversity-factor soil-factor organic-bonus native-bonus carbon-bonus) SUSTAINABILITY_SCORE_MAX)
    )
    u0
  )
)

(define-read-only (is-wildcrafting-season (species (string-utf8 100)) (zone uint))
  ;; Simplified check - in production would reference detailed seasonal calendars
  (let ((current-season (mod stacks-block-height WILDCRAFTING_SEASON_BLOCKS)))
    (and (> current-season u2160) (< current-season u6480)) ;; Rough approximation
  )
)

;; Public functions
(define-public (register-medicinal-garden
  (location-coordinates (string-utf8 50))
  (size-square-meters uint)
  (water-conservation-methods (list 10 (string-utf8 100)))
  (organic-practices bool)
  (native-species-count uint)
  (medicinal-species-count uint)
)
  (let ((garden-id (fold + (map len (list location-coordinates)) u0))) ;; Simple ID generation
    (asserts! (> size-square-meters u0) ERR_INVALID_INPUT)
    (asserts! (> medicinal-species-count u0) ERR_INVALID_INPUT)

    (map-set garden-ecosystems
      { garden-id: garden-id }
      {
        owner: tx-sender,
        location-coordinates: location-coordinates,
        size-square-meters: size-square-meters,
        biodiversity-score: u50, ;; Initial score, to be updated
        soil-health-rating: u50, ;; Initial rating
        water-conservation-methods: water-conservation-methods,
        companion-planting: false,
        organic-practices: organic-practices,
        native-species-count: native-species-count,
        medicinal-species-count: medicinal-species-count,
        carbon-sequestration-estimate: (/ size-square-meters u10), ;; Rough estimate
        established-date: stacks-block-height,
        certification-level: u1
      }
    )

    (ok garden-id)
  )
)

(define-public (apply-for-wildcrafting-permit
  (species-list (list 20 (string-utf8 100)))
  (location-zone uint)
  (max-harvest-percentage uint)
  (indigenous-permission-obtained bool)
  (monitoring-requirements (list 5 (string-utf8 100)))
)
  (let ((permit-id (+ (var-get next-sustainability-report-id) u1000)))
    (asserts! (> (len species-list) u0) ERR_INVALID_INPUT)
    (asserts! (and (> max-harvest-percentage u0) (<= max-harvest-percentage u30)) ERR_INVALID_INPUT)
    (asserts! (> location-zone u0) ERR_INVALID_INPUT)

    ;; Require safety clearance for wildcrafting
    (asserts! (is-authorized-practitioner tx-sender u2) ERR_UNAUTHORIZED)

    (map-set wildcrafting-permits
      { permit-id: permit-id }
      {
        harvester: tx-sender,
        species-list: species-list,
        location-zone: location-zone,
        seasonal-restrictions: (list u2160 u4320 u6480 u8640), ;; Default seasonal blocks
        max-harvest-percentage: max-harvest-percentage,
        sustainable-practices-agreement: true,
        indigenous-permission-obtained: indigenous-permission-obtained,
        monitoring-requirements: monitoring-requirements,
        valid-until: (+ stacks-block-height u52560), ;; ~1 year
        renewal-count: u0,
        violations: u0
      }
    )

    (ok permit-id)
  )
)
