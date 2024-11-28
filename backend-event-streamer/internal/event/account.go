package event

type AccountCreatedEvent struct {
	Email     string `json:"email"`
	FullName  string `json:"fullName"`
	Username  string `json:"username"`
	AccountID string `json:"accountId"`
	CreatedAt string `json:"createdAt"`
}

type AccountApprovedEvent struct {
	AccountID     string `json:"accountId"`
	ApprovedAt    string `json:"approvedAt"`
	ApprovalNotes string `json:"approvalNotes"`
}

type AccountPendedEvent struct {
	AccountID string `json:"accountId"`
	Reason    string `json:"reason"`
	PendedAt  string `json:"pendedAt"`
}

type AccountSuspendedEvent struct {
	AccountID   string `json:"accountId"`
	Reason      string `json:"reason"`
	SuspendedAt string `json:"suspendedAt"`
}

type AccountActivatedEvent struct {
	AccountID   string `json:"accountId"`
	ActivatedAt string `json:"activatedAt"`
}

type AccountClosedEvent struct {
	AccountID string `json:"accountId"`
	Reason    string `json:"reason"`
	ClosedAt  string `json:"closedAt"`
}

type FundsDepositedEvent struct {
	AccountID   string  `json:"accountId"`
	Amount      float64 `json:"amount"`
	DepositedAt string  `json:"depositedAt"`
}

type FundsWithdrawnEvent struct {
	AccountID   string  `json:"accountId"`
	Amount      float64 `json:"amount"`
	WithdrawnAt string  `json:"withdrawnAt"`
}

type UserContactInfoUpsertedEvent struct {
	AccountID string `json:"accountId"`
	Email     string `json:"email"`
	UpdatedAt string `json:"updatedAt"`
}

type PhoneNumberUpsertedEvent struct {
	AccountID   string `json:"accountId"`
	PhoneNumber string `json:"phoneNumber"`
	PhoneType   string `json:"phoneType"`
	UpdatedAt   string `json:"updatedAt"`
}

type AddressUpsertedEvent struct {
	AccountID   string `json:"accountId"`
	Address     string `json:"address"`
	AddressType string `json:"addressType"`
	UpdatedAt   string `json:"updatedAt"`
}

type EmergencyContactUpsertedEvent struct {
	AccountID    string `json:"accountId"`
	ContactName  string `json:"contactName"`
	ContactPhone string `json:"contactPhone"`
	UpdatedAt    string `json:"updatedAt"`
}
