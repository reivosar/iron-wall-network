CREATE TABLE audit_logs (
    id SERIAL PRIMARY KEY,                     
    url TEXT NOT NULL,                         
    method TEXT NOT NULL,                      
    user_id INT,                               
    description TEXT,                          
    parameters TEXT,                           
    query TEXT,                                
    ip_address TEXT,                           
    response_status INT,                       
    response_message TEXT,                     
    request_started_at TIMESTAMP NOT NULL,     
    request_ended_at TIMESTAMP NOT NULL        
);

CREATE INDEX idx_audit_logs_url ON audit_logs(url);
CREATE INDEX idx_audit_logs_method ON audit_logs(method);
CREATE INDEX idx_audit_logs_request_started_at ON audit_logs(request_started_at);
CREATE INDEX idx_audit_logs_user_id ON audit_logs(user_id);

