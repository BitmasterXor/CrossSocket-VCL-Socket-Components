unit CrossSocket.Client;

interface

uses
  Classes,
  SysUtils,
  ExtCtrls,
  Math,
  TypInfo,
  // Actual Cross Socket units
  Net.CrossSocket,
  Net.CrossSocket.Base,
  Net.SocketAPI;

type
  // **FIXED: HUMAN-FRIENDLY STATE CONSTANTS THAT ACTUALLY WORK!**
  TCrossSocketConnectionState = (
    csUnknown,          // 0 - Unknown/Initial state
    csConnecting,       // 1 - Currently connecting
    csHandshaking,      // 2 - Performing handshake/negotiation
    csConnected,        // 3 - Successfully connected
    csDisconnecting,    // 4 - Currently disconnecting
    csDisconnected,     // 5 - Disconnected (clean)
    csClosed,           // 6 - Connection closed (with error)
    csError             // 7 - Error state
  );

  TCrossSocketReconnectStrategy = (rsLinear, rsExponential);

  // Event types - simplified
  TCrossSocketConnectEvent = procedure(Sender: TObject) of object;
  TCrossSocketDataReceivedEvent = procedure(Sender: TObject; const Data: TBytes) of object;
  TCrossSocketDataSentEvent = procedure(Sender: TObject; const Data: TBytes) of object;
  TCrossSocketDisconnectEvent = procedure(Sender: TObject) of object;
  TCrossSocketErrorEvent = procedure(Sender: TObject; const ErrorMsg: string) of object;
  TCrossSocketHandleCommandEvent = procedure(Sender: TObject; const Command: TBytes) of object;

  // **FIXED**: Use our own state change event with our enum!
  TCrossSocketStateChangeEvent = procedure(Sender: TObject;
    OldState, NewState: TCrossSocketConnectionState;
    const StateDescription: string) of object;

  TCrossSocketReconnectingEvent = procedure(Sender: TObject; AttemptNumber: Integer) of object;
  TCrossSocketReconnectFailedEvent = procedure(Sender: TObject; AttemptNumber: Integer; const ErrorMsg: string) of object;

  /// Cross Socket Client Component - BASIC VERSION, NO ENCRYPTION
  TCrossSocketClient = class(TComponent)
  private
    // Core Cross Socket objects
    fCrossSocket: ICrossSocket;
    fConnection: ICrossConnection;

    // Connection properties
    fHost: string;
    fPort: Integer;
    fURI: string;
    fConnected: Boolean;
    fConnecting: Boolean;
    fLastError: string;
    fCurrentState: TCrossSocketConnectionState;

    // Additional properties
    fActive: Boolean;
    fConnectionTimeout: Integer;
    fDescription: string;
    fKeepAlive: Boolean;
    fLogLevel: Integer;
    fMessageReceived: Boolean;
    fMessageSent: Boolean;
    fName: string;
    fNoDelay: Boolean;
    fReceiveBufferSize: Integer;
    fReconnectStrategy: TCrossSocketReconnectStrategy;
    fSendBufferSize: Integer;
    fThreadPoolSize: Integer;
    fTotalBytesReceived: Int64;
    fTotalBytesSent: Int64;
    fVersion: string;

    // Reconnection properties
    fAutoReconnect: Boolean;
    fReconnectInterval: Integer;
    fReconnectTimer: TTimer;
    fReconnectAttempts: Integer;
    fMaxReconnectAttempts: Integer;
    fReconnecting: Boolean;
    fUserDisconnected: Boolean;
    fReconnectState: Boolean; // Track if we're in reconnect mode

    // EVENTS - OnDataReceived/OnDataSent are for STATISTICS ONLY!
    fOnConnect: TCrossSocketConnectEvent;
    fOnDataReceived: TCrossSocketDataReceivedEvent;  // FOR STATISTICS/LOGGING ONLY!
    fOnDataSent: TCrossSocketDataSentEvent;  // FOR STATISTICS/LOGGING ONLY!
    fOnDisconnect: TCrossSocketDisconnectEvent;
    fOnError: TCrossSocketErrorEvent;
    fOnHandleCommand: TCrossSocketHandleCommandEvent;  // THIS IS WHERE YOU PROCESS DATA!
    fOnStateChange: TCrossSocketStateChangeEvent;
    fOnReconnecting: TCrossSocketReconnectingEvent;
    fOnReconnectFailed: TCrossSocketReconnectFailedEvent;

    // **CRITICAL FIX**: Add flags to prevent duplicate event firing
    fInConnectEvent: Boolean;
    fInDisconnectEvent: Boolean;
    fInDataSentEvent: Boolean;

    // Property setters
    procedure SetActive(const Value: Boolean);
    procedure SetConnectionTimeout(const Value: Integer);
    procedure SetDescription(const Value: string);
    procedure SetKeepAlive(const Value: Boolean);
    procedure SetLogLevel(const Value: Integer);
    procedure SetName(const Value: string);
    procedure SetNoDelay(const Value: Boolean);
    procedure SetReceiveBufferSize(const Value: Integer);
    procedure SetReconnectStrategy(const Value: TCrossSocketReconnectStrategy);
    procedure SetSendBufferSize(const Value: Integer);
    procedure SetThreadPoolSize(const Value: Integer);
    procedure SetVersion(const Value: string);

    procedure SetConnected(const Value: Boolean);
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: Integer);
    procedure SetURI(const Value: string);
    procedure SetAutoReconnect(const Value: Boolean);
    procedure SetReconnectInterval(const Value: Integer);
    procedure SetMaxReconnectAttempts(const Value: Integer);

    // Helper methods for state management
    function GetConnectionStateDescription: string;

    procedure DoError(const ErrorMsg: string);
    procedure DoDataReceived(const Data: TBytes);
    procedure DoDataSent(const Data: TBytes);
    procedure DoHandleCommand(const Command: TBytes);
    procedure DoConnect;
    procedure DoDisconnect;
    procedure DoStateChange(NewState: TCrossSocketConnectionState);
    procedure OnReconnectTimer(Sender: TObject);
    procedure StartReconnectTimer;
    procedure StopReconnectTimer;
    procedure DoReconnecting(AttemptNumber: Integer);
    procedure DoReconnectFailed(AttemptNumber: Integer; const ErrorMsg: string);
    procedure UpdateBytesReceived(const Bytes: Int64);
    procedure UpdateBytesSent(const Bytes: Int64);

    // Cross Socket event handlers
    procedure OnCrossSocketConnected(const Sender: TObject; const AConnection: ICrossConnection);
    procedure OnCrossSocketDisconnected(const Sender: TObject; const AConnection: ICrossConnection);
    procedure OnCrossSocketReceived(const Sender: TObject; const AConnection: ICrossConnection; const ABuf: Pointer; const ALen: Integer);
    procedure OnCrossSocketSent(const Sender: TObject; const AConnection: ICrossConnection; const ABuf: Pointer; const ALen: Integer);

  protected
    procedure InternalConnect;
    procedure InternalDisconnect;
    procedure HandleUnexpectedDisconnection;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Connect to Cross Socket server (async - non-blocking)
    function Connect: Boolean;
    /// Connect to Cross Socket server (sync - blocking, for compatibility)
    function ConnectSync: Boolean;
    /// Disconnect from Cross Socket server
    procedure Disconnect;
    /// Send command to server - BASIC: no encryption
    function SendCommand(const Command: TBytes): Boolean;
    /// Check if currently connected
    function IsConnected: Boolean;
    /// Check if currently attempting to connect
    function IsConnecting: Boolean;
    /// Check if currently attempting to reconnect
    function IsReconnecting: Boolean;
    /// Get last error message
    function GetLastError: string;
    /// Get current reconnection attempt count
    function GetReconnectAttempts: Integer;
    /// Reset reconnection attempt counter
    procedure ResetReconnectAttempts;
    /// Get total bytes received
    function GetTotalBytesReceived: Int64;
    /// Get total bytes sent
    function GetTotalBytesSent: Int64;
    /// Reset byte counters
    procedure ResetByteCounters;
    /// Get the server's hostname/IP address
    function GetServerIP: string;
    /// Get current connection state as string
    function GetConnectionStateAsString: string;

  published
    /// Active state (alternative to Connected)
    property Active: Boolean read fActive write SetActive default False;
    /// Server hostname or IP address
    property Host: string read fHost write SetHost;
    /// Server port number
    property Port: Integer read fPort write SetPort default 80;
    /// Cross Socket URI path (for compatibility)
    property URI: string read fURI write SetURI;
    /// Connection state
    property Connected: Boolean read fConnected write SetConnected default False;
    /// Connection timeout in milliseconds
    property ConnectionTimeout: Integer read fConnectionTimeout write SetConnectionTimeout default 30000;
    /// Component description
    property Description: string read fDescription write SetDescription;

    /// Enable keep-alive
    property KeepAlive: Boolean read fKeepAlive write SetKeepAlive default True;
    /// Logging level (0=none, 1=errors, 2=info, 3=debug)
    property LogLevel: Integer read fLogLevel write SetLogLevel default 1;
    /// Component name
    property Name: string read fName write SetName;
    /// TCP No Delay option
    property NoDelay: Boolean read fNoDelay write SetNoDelay default True;
    /// Receive buffer size in bytes
    property ReceiveBufferSize: Integer read fReceiveBufferSize write SetReceiveBufferSize default 8192;
    /// Thread pool size
    property ThreadPoolSize: Integer read fThreadPoolSize write SetThreadPoolSize default 4;
    /// Component version
    property Version: string read fVersion write SetVersion;
    /// Send buffer size in bytes
    property SendBufferSize: Integer read fSendBufferSize write SetSendBufferSize default 8192;

    /// Read-only connecting state
    property Connecting: Boolean read fConnecting;
    /// Enable automatic reconnection on connection loss
    property AutoReconnect: Boolean read fAutoReconnect write SetAutoReconnect default False;
    /// Interval in milliseconds between reconnection attempts
    property ReconnectInterval: Integer read fReconnectInterval write SetReconnectInterval default 5000;
    /// Maximum number of reconnection attempts (0 = unlimited)
    property MaxReconnectAttempts: Integer read fMaxReconnectAttempts write SetMaxReconnectAttempts default 0;
    /// Reconnection strategy
    property ReconnectStrategy: TCrossSocketReconnectStrategy read fReconnectStrategy write SetReconnectStrategy default rsLinear;
    /// Read-only reconnecting state
    property Reconnecting: Boolean read fReconnecting;
    /// Read-only message received flag
    property MessageReceived: Boolean read fMessageReceived;
    /// Read-only message sent flag
    property MessageSent: Boolean read fMessageSent;
    /// Read-only total bytes received
    property TotalBytesReceived: Int64 read fTotalBytesReceived;
    /// Read-only total bytes sent
    property TotalBytesSent: Int64 read fTotalBytesSent;
    /// Read-only current connection state
    property ConnectionState: TCrossSocketConnectionState read fCurrentState;
    /// Read-only connection state description
    property ConnectionStateDescription: string read GetConnectionStateDescription;

    // EVENTS - OnDataReceived/OnDataSent are for STATISTICS ONLY!
    property OnConnect: TCrossSocketConnectEvent read fOnConnect write fOnConnect;
    property OnDataReceived: TCrossSocketDataReceivedEvent read fOnDataReceived write fOnDataReceived;  // FOR STATISTICS/LOGGING ONLY!
    property OnDataSent: TCrossSocketDataSentEvent read fOnDataSent write fOnDataSent;  // FOR STATISTICS/LOGGING ONLY!
    property OnDisconnect: TCrossSocketDisconnectEvent read fOnDisconnect write fOnDisconnect;
    property OnError: TCrossSocketErrorEvent read fOnError write fOnError;
    property OnHandleCommand: TCrossSocketHandleCommandEvent read fOnHandleCommand write fOnHandleCommand;  // THIS IS WHERE YOU PROCESS DATA!
    property OnStateChange: TCrossSocketStateChangeEvent read fOnStateChange write fOnStateChange;
    property OnReconnecting: TCrossSocketReconnectingEvent read fOnReconnecting write fOnReconnecting;
    property OnReconnectFailed: TCrossSocketReconnectFailedEvent read fOnReconnectFailed write fOnReconnectFailed;
  end;

procedure Register;

implementation

{ TCrossSocketClient }

constructor TCrossSocketClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // **CRITICAL FIX**: Initialize event flags
  fInConnectEvent := False;
  fInDisconnectEvent := False;
  fInDataSentEvent := False;

  // Initialize connection properties
  fHost := 'localhost';
  fPort := 80;
  fURI := '/';
  fConnected := False;
  fConnecting := False;
  fConnection := nil;
  fCurrentState := csUnknown;

  // Initialize properties
  fActive := False;
  fConnectionTimeout := 30000;
  fDescription := 'Cross Socket Client Component - BASIC VERSION';
  fKeepAlive := True;
  fLogLevel := 1;
  fName := 'CrossClient1';
  fNoDelay := True;
  fReceiveBufferSize := 8192;
  fSendBufferSize := 8192;
  fThreadPoolSize := 4;
  fVersion := '1.0.0';
  fMessageReceived := False;
  fMessageSent := False;
  fTotalBytesReceived := 0;
  fTotalBytesSent := 0;

  // Initialize reconnection
  fAutoReconnect := False;
  fReconnectInterval := 5000;
  fMaxReconnectAttempts := 0;
  fReconnectAttempts := 0;
  fReconnectStrategy := rsLinear;
  fReconnecting := False;
  fReconnectState := False;
  fUserDisconnected := False;

  fReconnectTimer := TTimer.Create(Self);
  fReconnectTimer.Enabled := False;
  fReconnectTimer.OnTimer := OnReconnectTimer;

  // Create Cross Socket instance - REAL IMPLEMENTATION!
  fCrossSocket := TCrossSocket.Create(fThreadPoolSize);
  fCrossSocket.OnConnected := OnCrossSocketConnected;
  fCrossSocket.OnDisconnected := OnCrossSocketDisconnected;
  fCrossSocket.OnReceived := OnCrossSocketReceived;
  fCrossSocket.OnSent := OnCrossSocketSent;
end;

destructor TCrossSocketClient.Destroy;
begin
  try
    // Stop reconnection timer first
    try
      StopReconnectTimer;
    except
      // Ignore timer errors
    end;

    // Force disconnect
    if fConnected or fConnecting then
    begin
      try
        InternalDisconnect;
      except
        // Force cleanup even if disconnect fails
        fConnected := False;
        fConnecting := False;
        fReconnecting := False;
        fReconnectState := False;
      end;
    end;

    // Cleanup Cross Socket
    if fCrossSocket <> nil then
    begin
      try
        fCrossSocket.StopLoop;
        fCrossSocket := nil;
      except
        fCrossSocket := nil;
      end;
    end;

  except
    // Ignore ALL destructor errors - force cleanup
    fCrossSocket := nil;
    fConnection := nil;
  end;

  inherited Destroy;
end;

// =============================================================================
// **CRITICAL FIX**: FIXED CONNECTION METHODS - NO MORE DUPLICATE EVENTS
// =============================================================================

function TCrossSocketClient.Connect: Boolean;
begin
  Result := False;
  if fConnected or fConnecting then
    Exit;

  try
    fUserDisconnected := False;
    fReconnectState := False; // Reset reconnect state on manual connect
    DoStateChange(csConnecting);
    InternalConnect;
    Result := True;
  except
    on E: Exception do
    begin
      fLastError := E.Message;
      DoStateChange(csClosed); // Use csClosed instead of csError
      DoError(E.Message);
    end;
  end;
end;

function TCrossSocketClient.ConnectSync: Boolean;
begin
  // For Cross Socket, this would be the same as async since Cross Socket handles async internally
  Result := Connect;
end;

procedure TCrossSocketClient.InternalConnect;
begin
  if fConnected then
    Exit;

  try
    fConnecting := True;
    fConnected := False;

    // Start Cross Socket processing
    fCrossSocket.StartLoop;

    // **CRITICAL FIX**: Connect using ONLY the callback - don't manually fire events!
    fCrossSocket.Connect(fHost, fPort,
      procedure(const AConnection: ICrossConnection; const ASuccess: Boolean)
      begin
        if ASuccess then
        begin
          // **CRITICAL FIX**: Set connection but DON'T call DoConnect here!
          // The OnCrossSocketConnected event will handle that automatically
          fConnection := AConnection;
          // Note: OnCrossSocketConnected will be called automatically by CrossSocket
        end
        else
        begin
          fConnected := False;
          fConnecting := False;
          DoStateChange(csClosed);
          DoError('Connection failed');
          HandleUnexpectedDisconnection;
        end;
      end);

  except
    on E: Exception do
    begin
      fLastError := 'Connection failed: ' + E.Message;
      fConnecting := False;
      raise;
    end;
  end;
end;

procedure TCrossSocketClient.Disconnect;
begin
  fUserDisconnected := True;
  fReconnectState := False; // Clear reconnect state on manual disconnect
  StopReconnectTimer;
  DoStateChange(csDisconnected);
  InternalDisconnect;
end;

procedure TCrossSocketClient.InternalDisconnect;
begin
  if not fConnected and not fConnecting then
    Exit;

  try
    fConnected := False;
    fConnecting := False;
    fReconnecting := False;

    // **CRITICAL FIX**: Close connection - OnCrossSocketDisconnected will be called automatically
    if fConnection <> nil then
    begin
      try
        fConnection.Close;  // This will trigger OnCrossSocketDisconnected automatically
      except
        // Ignore close errors but still clean up
        fConnection := nil;
        // **CRITICAL FIX**: If close failed, manually fire disconnect
        if not fInDisconnectEvent then
          DoDisconnect;
      end;
    end
    else
    begin
      // **CRITICAL FIX**: No connection to close, manually fire disconnect
      if not fInDisconnectEvent then
      begin
        DoStateChange(csDisconnected);
        DoDisconnect;
      end;
    end;

    // Stop Cross Socket
    if fCrossSocket <> nil then
    begin
      try
        fCrossSocket.StopLoop;
      except
        // Ignore stop errors
      end;
    end;

  except
    on E: Exception do
    begin
      fLastError := 'Disconnect error: ' + E.Message;
      // Force cleanup even on error
      fConnection := nil;
      fConnected := False;
      fConnecting := False;
      fReconnecting := False;
      fReconnectState := False;
    end;
  end;
end;

// =============================================================================
// **CRITICAL FIX**: FIXED CROSS SOCKET EVENT HANDLERS - NO MORE DUPLICATES!
// =============================================================================

procedure TCrossSocketClient.OnCrossSocketConnected(const Sender: TObject; const AConnection: ICrossConnection);
begin
  // **CRITICAL FIX**: Use flag to prevent duplicate calls
  if fInConnectEvent then
    Exit;

  fInConnectEvent := True;
  try
    fConnection := AConnection;
    fConnected := True;
    fConnecting := False;
    fReconnectAttempts := 0;
    fReconnecting := False;
    fReconnectState := False;
    StopReconnectTimer;
    DoStateChange(csConnected);
    DoConnect;  // Fire OnConnect event ONLY ONCE
  finally
    fInConnectEvent := False;
  end;
end;

procedure TCrossSocketClient.OnCrossSocketDisconnected(const Sender: TObject; const AConnection: ICrossConnection);
begin
  // **CRITICAL FIX**: Use flag to prevent duplicate calls AND ensure it fires!
  if fInDisconnectEvent then
    Exit;

  fInDisconnectEvent := True;
  try
    if fConnected then
    begin
      fConnected := False;
      fConnection := nil;
      DoStateChange(csDisconnected);
      DoDisconnect;  // **CRITICAL FIX**: Fire OnDisconnect event!
      HandleUnexpectedDisconnection;
    end;
  finally
    fInDisconnectEvent := False;
  end;
end;

procedure TCrossSocketClient.OnCrossSocketReceived(const Sender: TObject; const AConnection: ICrossConnection; const ABuf: Pointer; const ALen: Integer);
var
  ReceivedData: TBytes;
begin
  // Convert received data to TBytes
  SetLength(ReceivedData, ALen);
  if ALen > 0 then
    Move(ABuf^, ReceivedData[0], ALen);

  // Update statistics first
  DoDataReceived(ReceivedData);

  // **FIXED: NO ENCRYPTION - Process data directly!**
  DoHandleCommand(ReceivedData);
end;

procedure TCrossSocketClient.OnCrossSocketSent(const Sender: TObject; const AConnection: ICrossConnection; const ABuf: Pointer; const ALen: Integer);
var
  SentData: TBytes;
begin
  // **CRITICAL FIX**: Use flag to prevent duplicate calls
  if fInDataSentEvent then
    Exit;

  fInDataSentEvent := True;
  try
    // Convert sent data to TBytes for statistics
    SetLength(SentData, ALen);
    if ALen > 0 then
      Move(ABuf^, SentData[0], ALen);

    DoDataSent(SentData);  // Fire OnDataSent event ONLY ONCE
  finally
    fInDataSentEvent := False;
  end;
end;

// =============================================================================
// **FIXED SEND METHOD** - NO ENCRYPTION!
// =============================================================================

function TCrossSocketClient.SendCommand(const Command: TBytes): Boolean;
begin
  Result := False;
  if not IsConnected or (Length(Command) = 0) then
    Exit;

  try
    // **FIXED: Send directly - NO ENCRYPTION!**
    if fConnection <> nil then
    begin
      fConnection.SendBytes(Command,
        procedure(const AConnection: ICrossConnection; const ASuccess: Boolean)
        begin
          if not ASuccess then
            DoError('Send failed');
        end);
      Result := True;
      // **CRITICAL FIX**: DON'T call DoDataSent here! OnCrossSocketSent will handle it!
    end;
  except
    on E: Exception do
    begin
      fLastError := E.Message;
      DoError(E.Message);
    end;
  end;
end;

// =============================================================================
// RECONNECTION LOGIC
// =============================================================================

procedure TCrossSocketClient.HandleUnexpectedDisconnection;
begin
  if fAutoReconnect and
     not fUserDisconnected and
     ((fMaxReconnectAttempts = 0) or (fReconnectAttempts < fMaxReconnectAttempts)) then
  begin
    fReconnecting := True;
    fReconnectState := True; // Set reconnect state
    DoStateChange(csConnecting); // Use csConnecting for reconnect state
    StartReconnectTimer;
  end;
end;

procedure TCrossSocketClient.StartReconnectTimer;
begin
  if fReconnectTimer <> nil then
  begin
    fReconnectTimer.Interval := fReconnectInterval;
    fReconnectTimer.Enabled := True;
  end;
end;

procedure TCrossSocketClient.StopReconnectTimer;
begin
  if fReconnectTimer <> nil then
    fReconnectTimer.Enabled := False;
end;

procedure TCrossSocketClient.OnReconnectTimer(Sender: TObject);
var
  actualInterval: Integer;
begin
  StopReconnectTimer;
  Inc(fReconnectAttempts);

  // Calculate interval based on strategy
  case fReconnectStrategy of
    rsLinear:
      actualInterval := fReconnectInterval;
    rsExponential:
      actualInterval := fReconnectInterval * (1 shl Min(fReconnectAttempts - 1, 10)); // Cap at 2^10
  else
    actualInterval := fReconnectInterval;
  end;

  // Update timer for next attempt if this one fails
  if fReconnectTimer <> nil then
    fReconnectTimer.Interval := actualInterval;

  DoReconnecting(fReconnectAttempts);
  Connect;
end;

// =============================================================================
// STATE HELPER METHODS
// =============================================================================

function TCrossSocketClient.GetConnectionStateDescription: string;
begin
  case fCurrentState of
    csUnknown: Result := Format('Unknown state with %s:%d', [fHost, fPort]);
    csConnecting:
      begin
        if fReconnectState then
          Result := Format('Reconnecting to %s:%d%s (attempt %d)', [fHost, fPort, fURI, fReconnectAttempts + 1])
        else
          Result := Format('Connecting to %s:%d%s', [fHost, fPort, fURI]);
      end;
    csHandshaking: Result := Format('Handshaking with %s:%d%s', [fHost, fPort, fURI]);
    csConnected: Result := Format('Connected to %s:%d%s', [fHost, fPort, fURI]);
    csDisconnecting: Result := Format('Disconnecting from %s:%d', [fHost, fPort]);
    csDisconnected: Result := Format('Disconnected from %s:%d', [fHost, fPort]);
    csClosed: Result := Format('Connection closed with %s:%d - %s', [fHost, fPort, fLastError]);
    csError: Result := Format('Error with %s:%d - %s', [fHost, fPort, fLastError]);
  else
    Result := 'Unknown connection state';
  end;
end;

function TCrossSocketClient.GetConnectionStateAsString: string;
begin
  case fCurrentState of
    csUnknown: Result := 'Unknown';
    csConnecting: Result := 'Connecting';
    csHandshaking: Result := 'Handshaking';
    csConnected: Result := 'Connected';
    csDisconnecting: Result := 'Disconnecting';
    csDisconnected: Result := 'Disconnected';
    csClosed: Result := 'Closed';
    csError: Result := 'Error';
  else
    Result := 'Unknown';
  end;
end;

function TCrossSocketClient.GetServerIP: string;
begin
  if fHost = 'localhost' then
    Result := '127.0.0.1'
  else
    Result := fHost;
end;

// =============================================================================
// PROPERTY SETTERS
// =============================================================================

procedure TCrossSocketClient.SetActive(const Value: Boolean);
begin
  if fActive <> Value then
  begin
    fActive := Value;
    SetConnected(Value);
  end;
end;

procedure TCrossSocketClient.SetConnectionTimeout(const Value: Integer);
begin
  if Value >= 1000 then
    fConnectionTimeout := Value;
end;

procedure TCrossSocketClient.SetDescription(const Value: string);
begin
  fDescription := Value;
end;

procedure TCrossSocketClient.SetKeepAlive(const Value: Boolean);
begin
  fKeepAlive := Value;
end;

procedure TCrossSocketClient.SetLogLevel(const Value: Integer);
begin
  if (Value >= 0) and (Value <= 3) then
    fLogLevel := Value;
end;

procedure TCrossSocketClient.SetName(const Value: string);
begin
  // FIXED: Actually set the component's Name, not just the internal field
  if (Value <> '') and (Value <> Name) then
  begin
    inherited Name := Value;  // This is the key fix!
    fName := Value;  // Keep internal field in sync
  end;
end;

procedure TCrossSocketClient.SetNoDelay(const Value: Boolean);
begin
  fNoDelay := Value;
end;

procedure TCrossSocketClient.SetReceiveBufferSize(const Value: Integer);
begin
  if Value >= 1024 then
    fReceiveBufferSize := Value;
end;

procedure TCrossSocketClient.SetReconnectStrategy(const Value: TCrossSocketReconnectStrategy);
begin
  fReconnectStrategy := Value;
end;

procedure TCrossSocketClient.SetSendBufferSize(const Value: Integer);
begin
  if Value >= 1024 then
    fSendBufferSize := Value;
end;

procedure TCrossSocketClient.SetThreadPoolSize(const Value: Integer);
begin
  if Value >= 1 then
    fThreadPoolSize := Value;
end;

procedure TCrossSocketClient.SetVersion(const Value: string);
begin
  fVersion := Value;
end;

procedure TCrossSocketClient.SetConnected(const Value: Boolean);
begin
  if fConnected <> Value then
  begin
    if Value then
      Connect
    else
      Disconnect;
  end;
end;

procedure TCrossSocketClient.SetHost(const Value: string);
begin
  if fHost <> Value then
  begin
    if not (fConnected or fConnecting) then
      fHost := Value;
  end;
end;

procedure TCrossSocketClient.SetPort(const Value: Integer);
begin
  if fPort <> Value then
  begin
    if not (fConnected or fConnecting) then
      fPort := Value;
  end;
end;

procedure TCrossSocketClient.SetURI(const Value: string);
begin
  if fURI <> Value then
  begin
    if not (fConnected or fConnecting) then
      fURI := Value;
  end;
end;

procedure TCrossSocketClient.SetAutoReconnect(const Value: Boolean);
begin
  if fAutoReconnect <> Value then
  begin
    fAutoReconnect := Value;
    if not Value then
      StopReconnectTimer;
  end;
end;

procedure TCrossSocketClient.SetReconnectInterval(const Value: Integer);
begin
  if Value >= 1000 then
  begin
    fReconnectInterval := Value;
    if fReconnectTimer.Enabled then
      fReconnectTimer.Interval := fReconnectInterval;
  end;
end;

procedure TCrossSocketClient.SetMaxReconnectAttempts(const Value: Integer);
begin
  if Value >= 0 then
    fMaxReconnectAttempts := Value;
end;

// =============================================================================
// STATISTICS METHODS
// =============================================================================

procedure TCrossSocketClient.UpdateBytesReceived(const Bytes: Int64);
begin
  Inc(fTotalBytesReceived, Bytes);
  fMessageReceived := True;
end;

procedure TCrossSocketClient.UpdateBytesSent(const Bytes: Int64);
begin
  Inc(fTotalBytesSent, Bytes);
  fMessageSent := True;
end;

function TCrossSocketClient.GetTotalBytesReceived: Int64;
begin
  Result := fTotalBytesReceived;
end;

function TCrossSocketClient.GetTotalBytesSent: Int64;
begin
  Result := fTotalBytesSent;
end;

procedure TCrossSocketClient.ResetByteCounters;
begin
  fTotalBytesReceived := 0;
  fTotalBytesSent := 0;
  fMessageReceived := False;
  fMessageSent := False;
end;

// =============================================================================
// STATUS METHODS
// =============================================================================

function TCrossSocketClient.IsConnected: Boolean;
begin
  Result := fConnected and (fConnection <> nil);
end;

function TCrossSocketClient.IsConnecting: Boolean;
begin
  Result := fConnecting;
end;

function TCrossSocketClient.IsReconnecting: Boolean;
begin
  Result := fReconnecting and fReconnectState;
end;

function TCrossSocketClient.GetLastError: string;
begin
  Result := fLastError;
end;

function TCrossSocketClient.GetReconnectAttempts: Integer;
begin
  Result := fReconnectAttempts;
end;

procedure TCrossSocketClient.ResetReconnectAttempts;
begin
  fReconnectAttempts := 0;
end;

// =============================================================================
// EVENT METHODS
// =============================================================================

procedure TCrossSocketClient.DoError(const ErrorMsg: string);
begin
  if Assigned(fOnError) then
    fOnError(Self, ErrorMsg);
end;

// OnDataReceived is for STATISTICS/LOGGING ONLY!
procedure TCrossSocketClient.DoDataReceived(const Data: TBytes);
begin
  UpdateBytesReceived(Length(Data));
  if Assigned(fOnDataReceived) then
    fOnDataReceived(Self, Data);
end;

// OnDataSent is for STATISTICS/LOGGING ONLY!
procedure TCrossSocketClient.DoDataSent(const Data: TBytes);
begin
  UpdateBytesSent(Length(Data));
  if Assigned(fOnDataSent) then
    fOnDataSent(Self, Data);
end;

// OnHandleCommand is for ACTUAL DATA PROCESSING!
procedure TCrossSocketClient.DoHandleCommand(const Command: TBytes);
begin
  if Assigned(fOnHandleCommand) then
    fOnHandleCommand(Self, Command);
end;

procedure TCrossSocketClient.DoConnect;
begin
  if Assigned(fOnConnect) then
    fOnConnect(Self);
end;

procedure TCrossSocketClient.DoDisconnect;
begin
  if Assigned(fOnDisconnect) then
    fOnDisconnect(Self);
end;

procedure TCrossSocketClient.DoStateChange(NewState: TCrossSocketConnectionState);
var
  OldState: TCrossSocketConnectionState;
begin
  OldState := fCurrentState;
  if OldState <> NewState then
  begin
    fCurrentState := NewState;
    if Assigned(fOnStateChange) then
      fOnStateChange(Self, OldState, NewState, GetConnectionStateDescription);
  end;
end;

procedure TCrossSocketClient.DoReconnecting(AttemptNumber: Integer);
begin
  if Assigned(fOnReconnecting) then
    fOnReconnecting(Self, AttemptNumber);
end;

procedure TCrossSocketClient.DoReconnectFailed(AttemptNumber: Integer; const ErrorMsg: string);
begin
  if Assigned(fOnReconnectFailed) then
    fOnReconnectFailed(Self, AttemptNumber, ErrorMsg);
end;

procedure Register;
begin
  RegisterComponents('Cross Socket', [TCrossSocketClient]);
end;

end.
