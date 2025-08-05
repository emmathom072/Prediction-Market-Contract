;; Prediction Market Contract
;; A decentralized betting system for future events with automatic payouts

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_MARKET_NOT_FOUND (err u101))
(define-constant ERR_MARKET_CLOSED (err u102))
(define-constant ERR_MARKET_RESOLVED (err u103))
(define-constant ERR_INVALID_BET_AMOUNT (err u104))
(define-constant ERR_INVALID_OUTCOME (err u105))
(define-constant ERR_INSUFFICIENT_BALANCE (err u106))
(define-constant ERR_MARKET_NOT_RESOLVED (err u107))
(define-constant ERR_ALREADY_CLAIMED (err u108))
(define-constant ERR_NO_WINNINGS (err u109))
(define-constant ERR_INVALID_RESOLUTION_TIME (err u110))

;; Data Variables
(define-data-var next-market-id uint u1)
(define-data-var platform-fee-rate uint u250) ;; 2.5% in basis points (250/10000)

;; Data Maps
(define-map markets uint {
    creator: principal,
    description: (string-ascii 256),
    resolution-time: uint,
    outcome-count: uint,
    total-pool: uint,
    resolved-outcome: (optional uint),
    is-resolved: bool,
    created-at: uint
})

(define-map outcome-pools {market-id: uint, outcome: uint} uint)

(define-map user-bets {user: principal, market-id: uint, outcome: uint} uint)

(define-map user-claims {user: principal, market-id: uint} bool)

;; Market authorizers (who can resolve markets)
(define-map market-resolvers principal bool)

;; Read-only functions

(define-read-only (get-market (market-id uint))
    (map-get? markets market-id)
)

(define-read-only (get-outcome-pool (market-id uint) (outcome uint))
    (default-to u0 (map-get? outcome-pools {market-id: market-id, outcome: outcome}))
)

(define-read-only (get-user-bet (user principal) (market-id uint) (outcome uint))
    (default-to u0 (map-get? user-bets {user: user, market-id: market-id, outcome: outcome}))
)

(define-read-only (has-claimed (user principal) (market-id uint))
    (default-to false (map-get? user-claims {user: user, market-id: market-id}))
)

(define-read-only (is-resolver (user principal))
    (default-to false (map-get? market-resolvers user))
)

(define-read-only (get-platform-fee-rate)
    (var-get platform-fee-rate)
)

(define-read-only (calculate-winnings (user principal) (market-id uint))
    (let (
        (market (unwrap! (get-market market-id) (err ERR_MARKET_NOT_FOUND)))
        (resolved-outcome (unwrap! (get resolved-outcome market) (err ERR_MARKET_NOT_RESOLVED)))
        (user-bet (get-user-bet user market-id resolved-outcome))
        (winning-pool (get-outcome-pool market-id resolved-outcome))
        (total-pool (get total-pool market))
        (platform-fee (/ (* total-pool (var-get platform-fee-rate)) u10000))
        (prize-pool (- total-pool platform-fee))
    )
    (if (and (> user-bet u0) (> winning-pool u0))
        (ok (/ (* user-bet prize-pool) winning-pool))
        (ok u0)
    ))
)

;; Private functions

(define-private (is-market-creator (market-id uint) (user principal))
    (match (get-market market-id)
        market (is-eq (get creator market) user)
        false
    )
)

;; Public functions

(define-public (create-market (description (string-ascii 256)) (resolution-time uint) (outcome-count uint))
    (let (
        (market-id (var-get next-market-id))
        (current-height stacks-block-height)
    )
    (asserts! (> resolution-time current-height) ERR_INVALID_RESOLUTION_TIME)
    (asserts! (and (> outcome-count u1) (<= outcome-count u10)) ERR_INVALID_OUTCOME)

    (map-set markets market-id {
        creator: tx-sender,
        description: description,
        resolution-time: resolution-time,
        outcome-count: outcome-count,
        total-pool: u0,
        resolved-outcome: none,
        is-resolved: false,
        created-at: current-height
    })

    (var-set next-market-id (+ market-id u1))
    (ok market-id)
    )
)

(define-public (place-bet (market-id uint) (outcome uint) (amount uint))
    (let (
        (market (unwrap! (get-market market-id) ERR_MARKET_NOT_FOUND))
        (current-height stacks-block-height)
        (current-bet (get-user-bet tx-sender market-id outcome))
        (current-pool (get-outcome-pool market-id outcome))
    )
    (asserts! (> amount u0) ERR_INVALID_BET_AMOUNT)
    (asserts! (< outcome (get outcome-count market)) ERR_INVALID_OUTCOME)
    (asserts! (< current-height (get resolution-time market)) ERR_MARKET_CLOSED)
    (asserts! (not (get is-resolved market)) ERR_MARKET_RESOLVED)

    ;; Transfer STX from user to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    ;; Update user bet
    (map-set user-bets
        {user: tx-sender, market-id: market-id, outcome: outcome}
        (+ current-bet amount)
    )

    ;; Update outcome pool
    (map-set outcome-pools
        {market-id: market-id, outcome: outcome}
        (+ current-pool amount)
    )

    ;; Update total pool
    (map-set markets market-id
        (merge market {total-pool: (+ (get total-pool market) amount)})
    )

    (ok true)
    )
)

(define-public (resolve-market (market-id uint) (winning-outcome uint))
    (let (
        (market (unwrap! (get-market market-id) ERR_MARKET_NOT_FOUND))
        (current-height stacks-block-height)
    )
    (asserts! (or
        (is-market-creator market-id tx-sender)
        (is-resolver tx-sender)
        (is-eq tx-sender CONTRACT_OWNER)
    ) ERR_NOT_AUTHORIZED)
    (asserts! (>= current-height (get resolution-time market)) ERR_MARKET_CLOSED)
    (asserts! (not (get is-resolved market)) ERR_MARKET_RESOLVED)
    (asserts! (< winning-outcome (get outcome-count market)) ERR_INVALID_OUTCOME)

    (map-set markets market-id
        (merge market {
            resolved-outcome: (some winning-outcome),
            is-resolved: true
        })
    )

    (ok true)
    )
)

(define-public (claim-winnings (market-id uint))
    (let (
        (market (unwrap! (get-market market-id) ERR_MARKET_NOT_FOUND))
        (winnings (unwrap! (calculate-winnings tx-sender market-id) ERR_MARKET_NOT_RESOLVED))
    )
    (asserts! (get is-resolved market) ERR_MARKET_NOT_RESOLVED)
    (asserts! (not (has-claimed tx-sender market-id)) ERR_ALREADY_CLAIMED)
    (asserts! (> winnings u0) ERR_NO_WINNINGS)

    ;; Mark as claimed
    (map-set user-claims {user: tx-sender, market-id: market-id} true)

    ;; Transfer winnings
    (as-contract (stx-transfer? winnings tx-sender tx-sender))
    )
)

(define-public (withdraw-platform-fees (market-id uint))
    (let (
        (market (unwrap! (get-market market-id) ERR_MARKET_NOT_FOUND))
        (total-pool (get total-pool market))
        (platform-fee (/ (* total-pool (var-get platform-fee-rate)) u10000))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (get is-resolved market) ERR_MARKET_NOT_RESOLVED)

    (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER))
    )
)

;; Admin functions

(define-public (add-resolver (resolver principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (map-set market-resolvers resolver true)
        (ok true)
    )
)

(define-public (remove-resolver (resolver principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (map-delete market-resolvers resolver)
        (ok true)
    )
)

(define-public (set-platform-fee-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (asserts! (<= new-rate u1000) ERR_INVALID_BET_AMOUNT) ;; Max 10%
        (var-set platform-fee-rate new-rate)
        (ok true)
    )
)

;; Initialize contract owner as resolver
(map-set market-resolvers CONTRACT_OWNER true)
