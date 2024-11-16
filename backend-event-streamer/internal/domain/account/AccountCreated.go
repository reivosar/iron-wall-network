package account

type AccountCreatedEvent struct {
	Email     string `json:"email"`
	FullName  string `json:"fullName"`
	Username  string `json:"username"`
	AccountID string `json:"accountId"`
	CreatedAt string `json:"createdAt"`
}

func HandleAccountCreated(eventData map[string]interface{}) error {
	return nil
}
