;; Qflux-clinical-network

;; Core system configuration and administrative control
(define-constant vault-guardian tx-sender)

;; Comprehensive error code definitions for system responses
(define-constant ERR_PAYLOAD_EXCEEDED (err u304))
(define-constant ERR_AUTH_FAILED (err u305))
(define-constant ERR_PRACTITIONER_UNKNOWN (err u306))
(define-constant ERR_INVALID_KEY (err u303))
(define-constant ERR_TYPE_MISMATCH (err u307))
(define-constant ERR_FORBIDDEN_OPERATION (err u308))
(define-constant ERR_GUARDIAN_ONLY (err u300))
(define-constant ERR_RECORD_NOT_FOUND (err u301))
(define-constant ERR_RECORD_EXISTS (err u302))

;; Global system state tracking variables
(define-data-var total-records-created uint u0)

;; Enhanced data structures for medical record management

;; Main repository for all medical documentation entries
(define-map medical-record-vault
  { record-key: uint }
  {
    patient-code: (string-ascii 64),
    practitioner-principal: principal,
    data-size: uint,
    creation-block: uint,
    medical-notes: (string-ascii 128),
    classification-tags: (list 10 (string-ascii 32))
  }
)

;; Access control matrix for record permissions
(define-map access-permission-matrix
  { record-key: uint, accessor-principal: principal }
  { has-access: bool }
)

;; Internal validation and utility functions

;; Validates format and constraints of classification tag
(define-private (is-valid-tag (tag-value (string-ascii 32)))
  (and 
    (> (len tag-value) u0)
    (< (len tag-value) u33)
  )
)

;; Comprehensive validation for tag collection integrity
(define-private (validate-tag-collection (tag-list (list 10 (string-ascii 32))))
  (and
    (> (len tag-list) u0)
    (<= (len tag-list) u10)
    (is-eq (len (filter is-valid-tag tag-list)) (len tag-list))
  )
)

;; Verifies record existence in the vault system
(define-private (record-exists-in-vault? (record-key uint))
  (is-some (map-get? medical-record-vault { record-key: record-key }))
)

;; Confirms practitioner ownership and authority over record
(define-private (verify-practitioner-ownership? (record-key uint) (practitioner-principal principal))
  (match (map-get? medical-record-vault { record-key: record-key })
    record-data (is-eq (get practitioner-principal record-data) practitioner-principal)
    false
  )
)

;; Extracts data size information from vault record
(define-private (get-record-data-size (record-key uint))
  (default-to u0
    (get data-size
      (map-get? medical-record-vault { record-key: record-key })
    )
  )
)

;; Public interface functions for external interactions

;; Creates and stores new medical record in the quantum vault
(define-public (store-medical-record 
  (patient-code (string-ascii 64))
  (data-size uint)
  (medical-notes (string-ascii 128))
  (classification-tags (list 10 (string-ascii 32)))
)
  (let
    (
      (new-record-key (+ (var-get total-records-created) u1))
    )
    ;; Comprehensive input validation phase
    (asserts! (> (len patient-code) u0) ERR_INVALID_KEY)
    (asserts! (< (len patient-code) u65) ERR_INVALID_KEY)
    (asserts! (> data-size u0) ERR_PAYLOAD_EXCEEDED)
    (asserts! (< data-size u1000000000) ERR_PAYLOAD_EXCEEDED)
    (asserts! (> (len medical-notes) u0) ERR_INVALID_KEY)
    (asserts! (< (len medical-notes) u129) ERR_INVALID_KEY)
    (asserts! (validate-tag-collection classification-tags) ERR_TYPE_MISMATCH)

    ;; Store record in primary vault storage
    (map-insert medical-record-vault
      { record-key: new-record-key }
      {
        patient-code: patient-code,
        practitioner-principal: tx-sender,
        data-size: data-size,
        creation-block: block-height,
        medical-notes: medical-notes,
        classification-tags: classification-tags
      }
    )

    ;; Initialize access permissions for creating practitioner
    (map-insert access-permission-matrix
      { record-key: new-record-key, accessor-principal: tx-sender }
      { has-access: true }
    )

    ;; Update global record counter
    (var-set total-records-created new-record-key)
    (ok new-record-key)
  )
)

;; Transfers record ownership to different healthcare practitioner
(define-public (transfer-record-ownership (record-key uint) (new-practitioner-principal principal))
  (let
    (
      (record-data (unwrap! (map-get? medical-record-vault { record-key: record-key }) ERR_RECORD_NOT_FOUND))
    )
    ;; Verify record existence and current ownership
    (asserts! (record-exists-in-vault? record-key) ERR_RECORD_NOT_FOUND)
    (asserts! (is-eq (get practitioner-principal record-data) tx-sender) ERR_AUTH_FAILED)

    ;; Execute ownership transfer operation
    (map-set medical-record-vault
      { record-key: record-key }
      (merge record-data { practitioner-principal: new-practitioner-principal })
    )
    (ok true)
  )
)

;; Fetches classification tags associated with medical record
(define-public (fetch-record-tags (record-key uint))
  (let
    (
      (record-data (unwrap! (map-get? medical-record-vault { record-key: record-key }) ERR_RECORD_NOT_FOUND))
    )
    (ok (get classification-tags record-data))
  )
)

;; Retrieves practitioner information for specified record
(define-public (get-record-practitioner (record-key uint))
  (let
    (
      (record-data (unwrap! (map-get? medical-record-vault { record-key: record-key }) ERR_RECORD_NOT_FOUND))
    )
    (ok (get practitioner-principal record-data))
  )
)

;; Obtains creation timestamp for medical record
(define-public (get-record-creation-time (record-key uint))
  (let
    (
      (record-data (unwrap! (map-get? medical-record-vault { record-key: record-key }) ERR_RECORD_NOT_FOUND))
    )
    (ok (get creation-block record-data))
  )
)

;; Returns total count of records stored in vault system
(define-public (get-vault-record-count)
  (ok (var-get total-records-created))
)

;; Retrieves data size metrics for specific record
(define-public (get-record-size-info (record-key uint))
  (let
    (
      (record-data (unwrap! (map-get? medical-record-vault { record-key: record-key }) ERR_RECORD_NOT_FOUND))
    )
    (ok (get data-size record-data))
  )
)

;; Extracts medical notes and observations from record
(define-public (get-medical-observations (record-key uint))
  (let
    (
      (record-data (unwrap! (map-get? medical-record-vault { record-key: record-key }) ERR_RECORD_NOT_FOUND))
    )
    (ok (get medical-notes record-data))
  )
)

;; Validates access permissions for requesting principal
(define-public (check-access-rights (record-key uint) (accessor-principal principal))
  (let
    (
      (permission-data (unwrap! (map-get? access-permission-matrix { record-key: record-key, accessor-principal: accessor-principal }) ERR_FORBIDDEN_OPERATION))
    )
    (ok (get has-access permission-data))
  )
)

;; Modifies existing record metadata and clinical information
(define-public (modify-record-metadata 
  (record-key uint)
  (updated-patient-code (string-ascii 64))
  (updated-data-size uint)
  (updated-medical-notes (string-ascii 128))
  (updated-classification-tags (list 10 (string-ascii 32)))
)
  (let
    (
      (current-record-data (unwrap! (map-get? medical-record-vault { record-key: record-key }) ERR_RECORD_NOT_FOUND))
    )
    ;; Extensive validation and authorization checks
    (asserts! (record-exists-in-vault? record-key) ERR_RECORD_NOT_FOUND)
    (asserts! (is-eq (get practitioner-principal current-record-data) tx-sender) ERR_AUTH_FAILED)
    (asserts! (> (len updated-patient-code) u0) ERR_INVALID_KEY)
    (asserts! (< (len updated-patient-code) u65) ERR_INVALID_KEY)
    (asserts! (> updated-data-size u0) ERR_PAYLOAD_EXCEEDED)
    (asserts! (< updated-data-size u1000000000) ERR_PAYLOAD_EXCEEDED)
    (asserts! (> (len updated-medical-notes) u0) ERR_INVALID_KEY)
    (asserts! (< (len updated-medical-notes) u129) ERR_INVALID_KEY)
    (asserts! (validate-tag-collection updated-classification-tags) ERR_TYPE_MISMATCH)

    ;; Apply metadata updates to vault record
    (map-set medical-record-vault
      { record-key: record-key }
      (merge current-record-data { 
        patient-code: updated-patient-code, 
        data-size: updated-data-size, 
        medical-notes: updated-medical-notes, 
        classification-tags: updated-classification-tags 
      })
    )
    (ok true)
  )
)

