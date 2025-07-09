unit CrossSocket.Server;

interface

uses
  Classes,
  SysUtils,
  System.Generics.Collections,
  SyncObjs,
  TypInfo,
  // Actual Cross Socket units
  Net.CrossSocket,
  Net.CrossSocket.Base,
  Net.SocketAPI;

type
  // **FIXED: HUMAN-FRIENDLY SERVER STATE CONSTANTS THAT ACTUALLY WORK!**
  TCrossSocketServerState = (
    ssIdle,           // 0 - Server is idle/stopped
    ssStarting,       // 1 - Server is starting up
    ssListening,      // 2 - Server is listening for connections
    ssStopping,       // 3 - Server is shutting down
    ssError           // 4 - Server error state
  );

  // **FIXED: RENAME TO AVOID CONFLICT WITH CLIENT!**
  TServerClientState = (
    scsUnknown,        // 0 - Unknown/Initial state
    scsConnecting,     // 1 - Currently connecting
    scsHandshaking,    // 2 - Performing handshake/negotiation
    scsConnected,      // 3 - Successfully connected
    scsDisconnecting,  // 4 - Currently disconnecting
    scsDisconnected,   // 5 - Disconnected (clean)
    scsClosed,         // 6 - Connection closed (with error)
    scsError           // 7 - Error state
  );

  // **FIXED: Simple TClientConnectionInfo - NO ENCRYPTION**
  TClientConnectionInfo = record
    ClientID: Integer;
    Connection: ICrossConnection;
    IP: string;
    ConnectedAt: TDateTime;
    State: TServerClientState;
  end;

  // RE-EXPORT the event types from CrossSocket.Base
  TCrossSocketClientConnectedEvent = procedure(Sender: TObject; ClientID: Integer) of object;
  TCrossSocketClientDisconnectedEvent = procedure(Sender: TObject; ClientID: Integer) of object;
  TCrossSocketErrorEvent = procedure(Sender: TObject; const ErrorMsg: string) of object;
  TCrossSocketServerDataReceivedEvent = procedure(Sender: TObject; ClientID: Integer; const Data: TBytes) of object;
  TCrossSocketServerDataSentEvent = procedure(Sender: TObject; ClientID: Integer; const Data: TBytes) of object;
  TCrossSocketServerHandleCommandEvent = procedure(Sender: TObject; ClientID: Integer; const Command: TBytes) of object;

  // **FIXED**: Use our own server state change event with our enum!
  TCrossSocketServerStateChangeEvent = procedure(Sender: TObject;
    OldState, NewState: TCrossSocketServerState;
    const StateDescription: string) of object;

  /// COMPLETE Cross Socket Server Component - BASIC VERSION, NO ENCRYPTION
  TCrossSocketServer = class(TComponent)
  private
    // Core Cross Socket objects
    fCrossSocket: ICrossSocket;

    // Core properties (SAME AS mORMot2 - DON'T BREAK)
    fPort: Integer;
    fActive: boolean;
    fLastError: string;
    fClientCount: Integer;
    fShuttingDown: boolean;

    // Cross Socket server configuration
    fServerThreadPoolCount: Integer;
    fKeepAliveTimeOut: Integer;
    fBindInterface: string;

    // **FIXED: Better client tracking with connection-to-ID mapping**
    fClientInfoMap: TDictionary<Integer, TClientConnectionInfo>;
    fConnectionToIDMap: TDictionary<ICrossConnection, Integer>;  // **NEW: Direct connection lookup**
    fClientLock: TCriticalSection;
    fServerState: TCrossSocketServerState;
    fNextClientID: Integer;

    // Additional properties
    fConnectionTimeout: Integer;
    fDescription: string;
    fKeepAlive: Boolean;
    fLogLevel: Integer;
    fMaxConnections: Integer;
    fName: string;
    fNoDelay: Boolean;
    fReceiveBufferSize: Integer;
    fReusePort: Boolean;
    fSendBufferSize: Integer;
    fTag: Integer;
    fThreadPoolSize: Integer;
    fVersion: string;

    // Statistics
    fTotalActiveConnections: Integer;
    fTotalBytesReceived: Int64;
    fTotalBytesSent: Int64;
    fTotalConnections: Int64;
    fTotalMessagesReceived: Int64;
    fTotalMessagesSent: Int64;

    // EVENTS - OnDataReceived/OnDataSent are for STATISTICS ONLY!
    fOnClientConnected: TCrossSocketClientConnectedEvent;
    fOnClientDisconnected: TCrossSocketClientDisconnectedEvent;
    fOnError: TCrossSocketErrorEvent;
    fOnDataReceived: TCrossSocketServerDataReceivedEvent;  // FOR STATISTICS/LOGGING ONLY!
    fOnDataSent: TCrossSocketServerDataSentEvent;  // FOR STATISTICS/LOGGING ONLY!
    fOnHandleCommand: TCrossSocketServerHandleCommandEvent;  // THIS IS WHERE YOU PROCESS DATA!
    fOnServerStateChange: TCrossSocketServerStateChangeEvent;

    // Helper methods
    function GetServerStateDescription: string;
    function GetClientStateDescription(ClientID: Integer): string;
    procedure SetServerState(NewState: TCrossSocketServerState);
    procedure SetClientState(ClientID: Integer; NewState: TServerClientState);

    // Core setters (DON'T BREAK)
    procedure SetActive(const Value: boolean);
    procedure SetPort(const Value: Integer);
    procedure SetServerThreadPoolCount(const Value: Integer);
    procedure SetKeepAliveTimeOut(const Value: Integer);
    procedure SetBindInterface(const Value: string);

    // Additional property setters
    procedure SetConnectionTimeout(const Value: Integer);
    procedure SetDescription(const Value: string);
    procedure SetKeepAlive(const Value: Boolean);
    procedure SetLogLevel(const Value: Integer);
    procedure SetMaxConnections(const Value: Integer);
    procedure SetName(const Value: string);
    procedure SetNoDelay(const Value: Boolean);
    procedure SetReceiveBufferSize(const Value: Integer);
    procedure SetReusePort(const Value: Boolean);
    procedure SetSendBufferSize(const Value: Integer);
    procedure SetTag(const Value: Integer);
    procedure SetThreadPoolSize(const Value: Integer);
    procedure SetVersion(const Value: string);

    // Event triggers
    procedure DoError(const ErrorMsg: string);
    procedure DoServerStateChange(OldState, NewState: TCrossSocketServerState);

    // Cross Socket event handlers - REAL CROSSSOCKET EVENTS!
    procedure OnCrossSocketListened(const Sender: TObject; const AListen: ICrossListen);
    procedure OnCrossSocketListenEnd(const Sender: TObject; const AListen: ICrossListen);
    procedure OnCrossSocketConnected(const Sender: TObject; const AConnection: ICrossConnection);
    procedure OnCrossSocketDisconnected(const Sender: TObject; const AConnection: ICrossConnection);
    procedure OnCrossSocketReceived(const Sender: TObject; const AConnection: ICrossConnection; const ABuf: Pointer; const ALen: Integer);
    procedure OnCrossSocketSent(const Sender: TObject; const AConnection: ICrossConnection; const ABuf: Pointer; const ALen: Integer);

    // **FIXED: Better connection management**
    function GetConnectionID(const AConnection: ICrossConnection): Integer;
    function GetConnectionByID(ClientID: Integer): ICrossConnection;
    procedure RegisterConnection(const AConnection: ICrossConnection);
    procedure UnregisterConnection(const AConnection: ICrossConnection);

  protected
    procedure InternalStart;
    procedure InternalStop;
    procedure InitializeDefaults;
    procedure UpdateStatistics;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Main methods (DON'T BREAK)
    function Start: boolean;
    procedure Stop;
    function SendCommandToClient(ClientID: Integer; const Command: TBytes): boolean;
    procedure BroadcastCommand(const Command: TBytes);
    function IsActive: boolean;
    function GetLastError: string;
    function GetClientCount: Integer;
    function GetServerStats: string;

    // Additional methods
    procedure ResetStatistics;
    function GetClientInfo(ClientID: Integer): string;
    function GetClientIP(ClientID: Integer): string;
    function GetClientEndpoint(ClientID: Integer): string;
    function GetServerStateAsString: string;
    function GetClientStateAsString(ClientID: Integer): string;
    function GetAllClientStates: string;

    // Connection limit methods
    function CanAcceptNewConnection: Boolean;
    function GetConnectionsRemaining: Integer;

  published
    // Core properties (DON'T BREAK)
    property Port: Integer read fPort write SetPort default 8080;
    property Active: boolean read fActive write SetActive default False;

    // Server configuration
    property ServerThreadPoolCount: Integer read fServerThreadPoolCount
      write SetServerThreadPoolCount default 32;
    property KeepAliveTimeOut: Integer read fKeepAliveTimeOut
      write SetKeepAliveTimeOut default 30000;
    property BindInterface: string read fBindInterface write SetBindInterface;

    // State properties
    property ServerState: TCrossSocketServerState read fServerState;
    property ServerStateDescription: string read GetServerStateDescription;

    // Additional properties
    property ConnectionTimeout: Integer read fConnectionTimeout write SetConnectionTimeout default 10;
    property Description: string read fDescription write SetDescription;
    property KeepAlive: Boolean read fKeepAlive write SetKeepAlive default True;
    property LogLevel: Integer read fLogLevel write SetLogLevel default 1000;
    property MaxConnections: Integer read fMaxConnections write SetMaxConnections default 1000;
    property Name: string read fName write SetName;
    property NoDelay: Boolean read fNoDelay write SetNoDelay default True;
    property ReceiveBufferSize: Integer read fReceiveBufferSize write SetReceiveBufferSize default 8192;
    property ReusePort: Boolean read fReusePort write SetReusePort default True;
    property SendBufferSize: Integer read fSendBufferSize write SetSendBufferSize default 8192;
    property Tag: Integer read fTag write SetTag default 0;
    property ThreadPoolSize: Integer read fThreadPoolSize write SetThreadPoolSize default 4;
    property Version: string read fVersion write SetVersion;

    // Statistics
    property TotalActiveConnections: Integer read fTotalActiveConnections;
    property TotalBytesReceived: Int64 read fTotalBytesReceived;
    property TotalBytesSent: Int64 read fTotalBytesSent;
    property TotalConnections: Int64 read fTotalConnections;
    property TotalMessagesReceived: Int64 read fTotalMessagesReceived;
    property TotalMessagesSent: Int64 read fTotalMessagesSent;

    // EVENTS - OnDataReceived/OnDataSent are for STATISTICS ONLY!
    property OnClientConnected: TCrossSocketClientConnectedEvent
      read fOnClientConnected write fOnClientConnected;
    property OnClientDisconnected: TCrossSocketClientDisconnectedEvent
      read fOnClientDisconnected write fOnClientDisconnected;
    property OnError: TCrossSocketErrorEvent read fOnError write fOnError;
    property OnDataReceived: TCrossSocketServerDataReceivedEvent read fOnDataReceived
      write fOnDataReceived;  // FOR STATISTICS/LOGGING ONLY!
    property OnDataSent: TCrossSocketServerDataSentEvent read fOnDataSent
      write fOnDataSent;  // FOR STATISTICS/LOGGING ONLY!
    property OnHandleCommand: TCrossSocketServerHandleCommandEvent read fOnHandleCommand
      write fOnHandleCommand;  // THIS IS WHERE YOU PROCESS DATA!
    property OnServerStateChange: TCrossSocketServerStateChangeEvent
      read fOnServerStateChange write fOnServerStateChange;
  end;

procedure Register;

implementation

// =============================================================================
// MAIN COMPONENT IMPLEMENTATION
// =============================================================================

{ TCrossSocketServer }

constructor TCrossSocketServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fClientInfoMap := TDictionary<Integer, TClientConnectionInfo>.Create;
  fConnectionToIDMap := TDictionary<ICrossConnection, Integer>.Create;  // **NEW: Direct lookup**
  fClientLock := TCriticalSection.Create;
  fServerState := ssIdle;
  fNextClientID := 1;

  InitializeDefaults;

  // Create Cross Socket instance - REAL IMPLEMENTATION!
  fCrossSocket := TCrossSocket.Create(fThreadPoolSize);
  fCrossSocket.OnListened := OnCrossSocketListened;
  fCrossSocket.OnListenEnd := OnCrossSocketListenEnd;
  fCrossSocket.OnConnected := OnCrossSocketConnected;
  fCrossSocket.OnDisconnected := OnCrossSocketDisconnected;
  fCrossSocket.OnReceived := OnCrossSocketReceived;
  fCrossSocket.OnSent := OnCrossSocketSent;
end;

procedure TCrossSocketServer.InitializeDefaults;
begin
  // Core defaults (DON'T BREAK)
  fPort := 8080;
  fActive := False;
  fClientCount := 0;
  fShuttingDown := False;
  fServerThreadPoolCount := 32;
  fKeepAliveTimeOut := 30000;
  fBindInterface := '0.0.0.0';

  // Additional defaults
  fConnectionTimeout := 10;
  fDescription := 'Cross Socket Server Component - BASIC VERSION';
  fKeepAlive := True;
  fLogLevel := 1000;
  fMaxConnections := 1000;
  fName := 'CrossServer1';
  fNoDelay := True;
  fReceiveBufferSize := 8192;
  fReusePort := True;
  fSendBufferSize := 8192;
  fTag := 0;
  fThreadPoolSize := 4;
  fVersion := '1.0.0';

  // Statistics
  fTotalActiveConnections := 0;
  fTotalBytesReceived := 0;
  fTotalBytesSent := 0;
  fTotalConnections := 0;
  fTotalMessagesReceived := 0;
  fTotalMessagesSent := 0;
end;

destructor TCrossSocketServer.Destroy;
begin
  try
    if fActive then
      InternalStop;

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

    FreeAndNil(fClientInfoMap);
    FreeAndNil(fConnectionToIDMap);  // **CLEANUP NEW MAP**
    FreeAndNil(fClientLock);
  except
    // Ignore cleanup errors
  end;
  inherited Destroy;
end;

// =============================================================================
// **FIXED STATE HELPER METHODS** - Now with proper enum support!
// =============================================================================

function TCrossSocketServer.GetServerStateDescription: string;
begin
  case fServerState of
    ssIdle: Result := Format('Server is idle on port %d', [fPort]);
    ssStarting: Result := Format('Server is starting on port %d', [fPort]);
    ssListening: Result := Format('Server is listening on port %d (%d clients)', [fPort, fClientCount]);
    ssStopping: Result := Format('Server is stopping on port %d', [fPort]);
    ssError: Result := Format('Server error on port %d - %s', [fPort, fLastError]);
  else
    Result := Format('Server unknown state on port %d', [fPort]);
  end;
end;

function TCrossSocketServer.GetClientStateDescription(ClientID: Integer): string;
var
  clientInfo: TClientConnectionInfo;
begin
  if fClientInfoMap.TryGetValue(ClientID, clientInfo) then
  begin
    case clientInfo.State of
      scsUnknown: Result := Format('Client #%d (%s) state unknown', [ClientID, clientInfo.IP]);
      scsConnecting: Result := Format('Client #%d (%s) is connecting', [ClientID, clientInfo.IP]);
      scsHandshaking: Result := Format('Client #%d (%s) is handshaking', [ClientID, clientInfo.IP]);
      scsConnected: Result := Format('Client #%d (%s) is connected', [ClientID, clientInfo.IP]);
      scsDisconnecting: Result := Format('Client #%d (%s) is disconnecting', [ClientID, clientInfo.IP]);
      scsDisconnected: Result := Format('Client #%d (%s) is disconnected', [ClientID, clientInfo.IP]);
      scsClosed: Result := Format('Client #%d (%s) connection closed', [ClientID, clientInfo.IP]);
      scsError: Result := Format('Client #%d (%s) has error', [ClientID, clientInfo.IP]);
    else
      Result := Format('Client #%d (%s) unknown state', [ClientID, clientInfo.IP]);
    end;
  end
  else
    Result := Format('Client #%d not found', [ClientID]);
end;

function TCrossSocketServer.GetServerStateAsString: string;
begin
  case fServerState of
    ssIdle: Result := 'Idle';
    ssStarting: Result := 'Starting';
    ssListening: Result := 'Listening';
    ssStopping: Result := 'Stopping';
    ssError: Result := 'Error';
  else
    Result := 'Unknown';
  end;
end;

function TCrossSocketServer.GetClientStateAsString(ClientID: Integer): string;
var
  clientInfo: TClientConnectionInfo;
begin
  if fClientInfoMap.TryGetValue(ClientID, clientInfo) then
  begin
    case clientInfo.State of
      scsUnknown: Result := 'Unknown';
      scsConnecting: Result := 'Connecting';
      scsHandshaking: Result := 'Handshaking';
      scsConnected: Result := 'Connected';
      scsDisconnecting: Result := 'Disconnecting';
      scsDisconnected: Result := 'Disconnected';
      scsClosed: Result := 'Closed';
      scsError: Result := 'Error';
    else
      Result := 'Unknown';
    end;
  end
  else
    Result := 'Not Found';
end;

function TCrossSocketServer.GetAllClientStates: string;
var
  clientID: Integer;
  clientInfo: TClientConnectionInfo;
  states: TStringList;
begin
  states := TStringList.Create;
  try
    fClientLock.Enter;
    try
      for clientID in fClientInfoMap.Keys do
      begin
        if fClientInfoMap.TryGetValue(clientID, clientInfo) then
          states.Add(Format('Client #%d (%s): %s', [clientID, clientInfo.IP, GetClientStateAsString(clientID)]));
      end;
    finally
      fClientLock.Leave;
    end;

    if states.Count > 0 then
      Result := states.Text
    else
      Result := 'No active clients';
  finally
    states.Free;
  end;
end;

procedure TCrossSocketServer.SetServerState(NewState: TCrossSocketServerState);
var
  OldState: TCrossSocketServerState;
begin
  OldState := fServerState;
  if OldState <> NewState then
  begin
    fServerState := NewState;
    DoServerStateChange(OldState, NewState);
  end;
end;

procedure TCrossSocketServer.SetClientState(ClientID: Integer; NewState: TServerClientState);
var
  clientInfo: TClientConnectionInfo;
begin
  fClientLock.Enter;
  try
    if fClientInfoMap.TryGetValue(ClientID, clientInfo) then
    begin
      if clientInfo.State <> NewState then
      begin
        clientInfo.State := NewState;
        fClientInfoMap.AddOrSetValue(ClientID, clientInfo);
      end;
    end;
  finally
    fClientLock.Leave;
  end;
end;

// =============================================================================
// CONNECTION LIMIT METHODS
// =============================================================================

function TCrossSocketServer.CanAcceptNewConnection: Boolean;
begin
  Result := (fMaxConnections <= 0) or (fClientCount < fMaxConnections);
end;

function TCrossSocketServer.GetConnectionsRemaining: Integer;
begin
  if fMaxConnections <= 0 then
    Result := -1  // Unlimited
  else
    Result := fMaxConnections - fClientCount;
end;

// =============================================================================
// CLIENT INFO METHODS
// =============================================================================

function TCrossSocketServer.GetClientIP(ClientID: Integer): string;
var
  clientInfo: TClientConnectionInfo;
  connection: ICrossConnection;
begin
  fClientLock.Enter;
  try
    if fClientInfoMap.TryGetValue(ClientID, clientInfo) then
      Result := clientInfo.IP
    else
    begin
      // Try to get from connection as fallback
      connection := GetConnectionByID(ClientID);
      if connection <> nil then
        Result := connection.PeerAddr
      else
        Result := 'Unknown';
    end;
  finally
    fClientLock.Leave;
  end;
end;

function TCrossSocketServer.GetClientEndpoint(ClientID: Integer): string;
var
  clientIP: string;
begin
  clientIP := GetClientIP(ClientID);
  Result := Format('Client #%d (%s)', [ClientID, clientIP]);
end;

function TCrossSocketServer.GetClientInfo(ClientID: Integer): string;
var
  clientInfo: TClientConnectionInfo;
begin
  fClientLock.Enter;
  try
    if fClientInfoMap.TryGetValue(ClientID, clientInfo) then
      Result := Format('Client #%d - IP: %s - State: %s - Connected: %s',
        [ClientID, clientInfo.IP, GetClientStateAsString(ClientID),
         DateTimeToStr(clientInfo.ConnectedAt)])
    else
      Result := Format('Client #%d not found', [ClientID]);
  finally
    fClientLock.Leave;
  end;
end;

procedure TCrossSocketServer.ResetStatistics;
begin
  fClientLock.Enter;
  try
    fTotalBytesReceived := 0;
    fTotalBytesSent := 0;
    fTotalConnections := 0;
    fTotalMessagesReceived := 0;
    fTotalMessagesSent := 0;
    fTotalActiveConnections := fClientCount;
  finally
    fClientLock.Leave;
  end;
end;

procedure TCrossSocketServer.UpdateStatistics;
begin
  // Statistics are updated in real-time during operations
  fTotalActiveConnections := fClientCount;
end;

// =============================================================================
// **FIXED CONNECTION MANAGEMENT** - Proper tracking with dual maps!
// =============================================================================

function TCrossSocketServer.GetConnectionID(const AConnection: ICrossConnection): Integer;
begin
  Result := 0;
  fClientLock.Enter;
  try
    // **FIXED: Use direct lookup map for O(1) performance**
    if not fConnectionToIDMap.TryGetValue(AConnection, Result) then
      Result := 0;
  finally
    fClientLock.Leave;
  end;
end;

function TCrossSocketServer.GetConnectionByID(ClientID: Integer): ICrossConnection;
var
  clientInfo: TClientConnectionInfo;
begin
  Result := nil;
  fClientLock.Enter;
  try
    if fClientInfoMap.TryGetValue(ClientID, clientInfo) then
      Result := clientInfo.Connection;
  finally
    fClientLock.Leave;
  end;
end;

procedure TCrossSocketServer.RegisterConnection(const AConnection: ICrossConnection);
var
  clientInfo: TClientConnectionInfo;
  clientID: Integer;
begin
  // CHECK MAX CONNECTIONS FIRST - REJECT IF OVER LIMIT!
  if (fMaxConnections > 0) and (fClientCount >= fMaxConnections) then
  begin
    // LOG THE REJECTION
    DoError(Format('Connection REJECTED from %s: Maximum connections (%d) reached',
      [AConnection.PeerAddr, fMaxConnections]));

    // CLOSE THE CONNECTION IMMEDIATELY
    try
      AConnection.Close;
    except
      // Ignore close errors
    end;
    Exit;
  end;

  fClientLock.Enter;
  try
    clientID := fNextClientID;
    Inc(fNextClientID);

    // Create client info
    clientInfo.ClientID := clientID;
    clientInfo.Connection := AConnection;
    clientInfo.IP := AConnection.PeerAddr;
    clientInfo.ConnectedAt := Now;
    clientInfo.State := scsConnected;

    // **FIXED: Store in BOTH maps for fast lookup**
    fClientInfoMap.AddOrSetValue(clientID, clientInfo);
    fConnectionToIDMap.AddOrSetValue(AConnection, clientID);

    Inc(fClientCount);
    Inc(fTotalActiveConnections);
    Inc(fTotalConnections);
  finally
    fClientLock.Leave;
  end;

  // Fire connection event
  if Assigned(fOnClientConnected) then
    fOnClientConnected(Self, clientID);
end;

procedure TCrossSocketServer.UnregisterConnection(const AConnection: ICrossConnection);
var
  clientID: Integer;
begin
  clientID := GetConnectionID(AConnection);
  if clientID > 0 then
  begin
    fClientLock.Enter;
    try
      // **FIXED: Remove from BOTH maps**
      if fClientInfoMap.ContainsKey(clientID) then
        fClientInfoMap.Remove(clientID);

      if fConnectionToIDMap.ContainsKey(AConnection) then
        fConnectionToIDMap.Remove(AConnection);

      Dec(fClientCount);

      // **SAFETY CHECK**: Never let count go negative
      if fClientCount < 0 then
        fClientCount := 0;

      // Update active connections count
      fTotalActiveConnections := fClientCount;
    finally
      fClientLock.Leave;
    end;

    // Fire disconnection event
    if Assigned(fOnClientDisconnected) then
      fOnClientDisconnected(Self, clientID);
  end;
end;

// =============================================================================
// CROSS SOCKET EVENT HANDLERS - REAL CROSSSOCKET EVENTS!
// =============================================================================

procedure TCrossSocketServer.OnCrossSocketListened(const Sender: TObject; const AListen: ICrossListen);
begin
  fActive := True;
  SetServerState(ssListening);
end;

procedure TCrossSocketServer.OnCrossSocketListenEnd(const Sender: TObject; const AListen: ICrossListen);
begin
  if not fShuttingDown then
  begin
    fActive := False;
    SetServerState(ssError);
    DoError('Listen ended unexpectedly');
  end;
end;

procedure TCrossSocketServer.OnCrossSocketConnected(const Sender: TObject; const AConnection: ICrossConnection);
begin
  RegisterConnection(AConnection);
end;

procedure TCrossSocketServer.OnCrossSocketDisconnected(const Sender: TObject; const AConnection: ICrossConnection);
begin
  UnregisterConnection(AConnection);
end;

procedure TCrossSocketServer.OnCrossSocketReceived(const Sender: TObject; const AConnection: ICrossConnection; const ABuf: Pointer; const ALen: Integer);
var
  clientID: Integer;
  rawData: TBytes;
begin
  clientID := GetConnectionID(AConnection);
  if clientID > 0 then
  begin
    // Convert received data to TBytes
    SetLength(rawData, ALen);
    if ALen > 0 then
      Move(ABuf^, rawData[0], ALen);

    // Update statistics
    Inc(fTotalBytesReceived, Length(rawData));
    Inc(fTotalMessagesReceived);

    // Fire OnDataReceived for STATISTICS/LOGGING ONLY!
    if Assigned(fOnDataReceived) then
      fOnDataReceived(Self, clientID, rawData);

    // Fire OnHandleCommand for actual data processing - NO ENCRYPTION!
    if Assigned(fOnHandleCommand) then
      fOnHandleCommand(Self, clientID, rawData);
  end;
end;

procedure TCrossSocketServer.OnCrossSocketSent(const Sender: TObject; const AConnection: ICrossConnection; const ABuf: Pointer; const ALen: Integer);
var
  sentData: TBytes;
begin
  // Convert sent data to TBytes for statistics
  SetLength(sentData, ALen);
  if ALen > 0 then
    Move(ABuf^, sentData[0], ALen);

  // Statistics are updated in SendCommandToClient/BroadcastCommand
end;

// =============================================================================
// CORE METHODS (DON'T BREAK)
// =============================================================================

function TCrossSocketServer.Start: boolean;
begin
  Result := False;
  try
    InternalStart;
    Result := True;
  except
    on E: Exception do
    begin
      fLastError := E.Message;
      DoError(E.Message);
    end;
  end;
end;

procedure TCrossSocketServer.InternalStart;
begin
  if fActive then
    Exit;

  try
    SetServerState(ssStarting);

    // Stop any existing server
    InternalStop;
    fLastError := '';

    // **FIXED: Clear BOTH tracking maps**
    fClientLock.Enter;
    try
      fClientInfoMap.Clear;
      fConnectionToIDMap.Clear;
      fClientCount := 0;
    finally
      fClientLock.Leave;
    end;

    // Start Cross Socket processing
    fCrossSocket.StartLoop;

    // Listen for connections using REAL CrossSocket!
    fCrossSocket.Listen(fBindInterface, fPort,
      procedure(const AListen: ICrossListen; const ASuccess: Boolean)
      begin
        if not ASuccess then
        begin
          fLastError := 'Failed to start listening on ' + fBindInterface + ':' + IntToStr(fPort);
          SetServerState(ssError);
          DoError(fLastError);
        end;
        // Success is handled in OnCrossSocketListened
      end);

  except
    on E: Exception do
    begin
      fLastError := E.Message;
      SetServerState(ssError);
      if fCrossSocket <> nil then
      begin
        try
          fCrossSocket.StopLoop;
        except
        end;
      end;
      fActive := False;
      raise;
    end;
  end;
end;

procedure TCrossSocketServer.Stop;
begin
  if fShuttingDown then
    Exit;
  fShuttingDown := True;
  try
    InternalStop;
  finally
    fShuttingDown := False;
  end;
end;

procedure TCrossSocketServer.InternalStop;
begin
  if not fActive then
    Exit;

  SetServerState(ssStopping);
  fActive := False;

  if fCrossSocket <> nil then
  begin
    try
      fCrossSocket.CloseAllListens;
      fCrossSocket.CloseAllConnections;
      fCrossSocket.StopLoop;
    except
      // Ignore shutdown errors
    end;
  end;

  // **FIXED: Clear BOTH tracking maps**
  fClientLock.Enter;
  try
    fClientInfoMap.Clear;
    fConnectionToIDMap.Clear;
    fClientCount := 0;
    fTotalActiveConnections := 0;
  finally
    fClientLock.Leave;
  end;

  SetServerState(ssIdle);
end;

function TCrossSocketServer.SendCommandToClient(ClientID: Integer; const Command: TBytes): boolean;
var
  connection: ICrossConnection;
begin
  Result := False;

  if (fCrossSocket = nil) or not fActive then
    Exit;

  // **FIXED: Direct connection lookup**
  connection := GetConnectionByID(ClientID);
  if connection = nil then
  begin
    DoError(Format('Client #%d not found - cannot send message', [ClientID]));
    Exit;
  end;

  try
    // **FIXED: Send directly - NO ENCRYPTION!**
    connection.SendBytes(Command,
      procedure(const AConnection: ICrossConnection; const ASuccess: Boolean)
      begin
        if not ASuccess then
          DoError('Send to client ' + IntToStr(ClientID) + ' failed');
      end);
    Result := True;

    if Result then
    begin
      // Update statistics
      Inc(fTotalBytesSent, Length(Command));
      Inc(fTotalMessagesSent);

      // Fire OnDataSent for STATISTICS/LOGGING ONLY!
      if Assigned(fOnDataSent) then
        fOnDataSent(Self, ClientID, Command);
    end;
  except
    on E: Exception do
    begin
      fLastError := E.Message;
      DoError('Send to client ' + IntToStr(ClientID) + ' failed: ' + E.Message);
    end;
  end;
end;

procedure TCrossSocketServer.BroadcastCommand(const Command: TBytes);
var
  ClientID: Integer;
  clientInfo: TClientConnectionInfo;
  sentCount: Integer;
begin
  if (fCrossSocket = nil) or not fActive then
    Exit;

  sentCount := 0;
  fClientLock.Enter;
  try
    for ClientID in fClientInfoMap.Keys do
    begin
      if fClientInfoMap.TryGetValue(ClientID, clientInfo) and (clientInfo.Connection <> nil) then
      begin
        try
          // **FIXED: Send directly - NO ENCRYPTION!**
          clientInfo.Connection.SendBytes(Command,
            procedure(const AConnection: ICrossConnection; const ASuccess: Boolean)
            begin
              if not ASuccess then
                DoError('Broadcast to client ' + IntToStr(ClientID) + ' failed');
            end);

          Inc(sentCount);

          // Update statistics
          Inc(fTotalBytesSent, Length(Command));
          Inc(fTotalMessagesSent);

          // Fire OnDataSent for STATISTICS/LOGGING ONLY!
          if Assigned(fOnDataSent) then
            fOnDataSent(Self, ClientID, Command);
        except
          on E: Exception do
          begin
            DoError('Broadcast to client ' + IntToStr(ClientID) + ' failed: ' + E.Message);
          end;
        end;
      end;
    end;
  finally
    fClientLock.Leave;
  end;
end;

function TCrossSocketServer.IsActive: boolean;
begin
  Result := fActive and (fCrossSocket <> nil);
end;

function TCrossSocketServer.GetLastError: string;
begin
  Result := fLastError;
end;

function TCrossSocketServer.GetClientCount: Integer;
begin
  Result := fClientCount;
end;

function TCrossSocketServer.GetServerStats: string;
var
  ConnectionInfo: string;
begin
  if fMaxConnections > 0 then
    ConnectionInfo := Format('%d/%d', [fClientCount, fMaxConnections])
  else
    ConnectionInfo := Format('%d/unlimited', [fClientCount]);

  Result := Format
    ('State: %s, Active: %s, Connections: %s, Total Connections: %d, Msgs Sent: %d, Msgs Received: %d, Bytes Sent: %d, Bytes Received: %d',
    [GetServerStateAsString, BoolToStr(fActive, True), ConnectionInfo,
    fTotalConnections, fTotalMessagesSent, fTotalMessagesReceived, fTotalBytesSent, fTotalBytesReceived]);
end;

// =============================================================================
// PROPERTY SETTERS (CORE - DON'T BREAK)
// =============================================================================

procedure TCrossSocketServer.SetActive(const Value: boolean);
begin
  if fActive <> Value then
  begin
    if Value then
      Start
    else
      Stop;
  end;
end;

procedure TCrossSocketServer.SetPort(const Value: Integer);
begin
  if fPort <> Value then
  begin
    if fActive then
      raise Exception.Create('Cannot change Port while server is active');
    fPort := Value;
  end;
end;

procedure TCrossSocketServer.SetServerThreadPoolCount(const Value: Integer);
begin
  if fServerThreadPoolCount <> Value then
  begin
    if fActive then
      raise Exception.Create('Cannot change thread pool size while server is active');
    fServerThreadPoolCount := Value;
  end;
end;

procedure TCrossSocketServer.SetKeepAliveTimeOut(const Value: Integer);
begin
  fKeepAliveTimeOut := Value;
end;

procedure TCrossSocketServer.SetBindInterface(const Value: string);
begin
  fBindInterface := Value;
end;

// =============================================================================
// ADDITIONAL PROPERTY SETTERS
// =============================================================================

procedure TCrossSocketServer.SetConnectionTimeout(const Value: Integer);
begin
  if fConnectionTimeout <> Value then
    fConnectionTimeout := Value;
end;

procedure TCrossSocketServer.SetDescription(const Value: string);
begin
  if fDescription <> Value then
    fDescription := Value;
end;

procedure TCrossSocketServer.SetKeepAlive(const Value: Boolean);
begin
  if fKeepAlive <> Value then
    fKeepAlive := Value;
end;

procedure TCrossSocketServer.SetLogLevel(const Value: Integer);
begin
  if fLogLevel <> Value then
    fLogLevel := Value;
end;

procedure TCrossSocketServer.SetMaxConnections(const Value: Integer);
begin
  if Value <= 0 then
    raise Exception.Create('MaxConnections must be greater than 0');

  if fMaxConnections <> Value then
  begin
    if fActive and (Value < fClientCount) then
      raise Exception.Create(Format('Cannot set MaxConnections to %d - currently have %d active connections',
        [Value, fClientCount]));
    fMaxConnections := Value;
  end;
end;

procedure TCrossSocketServer.SetName(const Value: string);
begin
  // FIXED: Actually set the component's Name, not just the internal field
  if (Value <> '') and (Value <> Name) then
  begin
    inherited Name := Value;  // This is the key fix!
    fName := Value;  // Keep internal field in sync
  end;
end;

procedure TCrossSocketServer.SetNoDelay(const Value: Boolean);
begin
  if fNoDelay <> Value then
    fNoDelay := Value;
end;

procedure TCrossSocketServer.SetReceiveBufferSize(const Value: Integer);
begin
  if fReceiveBufferSize <> Value then
    fReceiveBufferSize := Value;
end;

procedure TCrossSocketServer.SetReusePort(const Value: Boolean);
begin
  if fReusePort <> Value then
    fReusePort := Value;
end;

procedure TCrossSocketServer.SetSendBufferSize(const Value: Integer);
begin
  if fSendBufferSize <> Value then
    fSendBufferSize := Value;
end;

procedure TCrossSocketServer.SetTag(const Value: Integer);
begin
  if fTag <> Value then
    fTag := Value;
end;

procedure TCrossSocketServer.SetThreadPoolSize(const Value: Integer);
begin
  if fThreadPoolSize <> Value then
    fThreadPoolSize := Value;
end;

procedure TCrossSocketServer.SetVersion(const Value: string);
begin
  if fVersion <> Value then
    fVersion := Value;
end;

// =============================================================================
// EVENT TRIGGERS
// =============================================================================

procedure TCrossSocketServer.DoError(const ErrorMsg: string);
begin
  if Assigned(fOnError) then
    fOnError(Self, ErrorMsg);
end;

procedure TCrossSocketServer.DoServerStateChange(OldState, NewState: TCrossSocketServerState);
begin
  if Assigned(fOnServerStateChange) then
    fOnServerStateChange(Self, OldState, NewState, GetServerStateDescription);
end;

procedure Register;
begin
  RegisterComponents('Cross Socket', [TCrossSocketServer]);
end;

end.
