unit CrossSocket.WebSocket.Server;

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  SyncObjs,
  // Actual Cross Socket WebSocket units ONLY
  Net.CrossWebSocketServer,
  Net.CrossWebSocketParser,
  Net.CrossSocket.Base;

type
  // RE-EXPORT INTERFACES FOR IDE COMPATIBILITY - THIS IS THE FIX!
  ICrossWebSocketConnection = Net.CrossWebSocketServer.ICrossWebSocketConnection;
  ICrossWebSocketServer = Net.CrossWebSocketServer.ICrossWebSocketServer;
  TWsMessageType = Net.CrossWebSocketParser.TWsMessageType;

  // MINIMAL server state - NO BULLSHIT
  TCrossWebSocketServerState = (
    ssIdle,
    ssListening,
    ssError
  );

  // Client ID type for easy identification
  TClientID = string;

  // MINIMAL events - NO STATISTICS BULLSHIT
  TCrossWebSocketClientConnectedEvent = procedure(Sender: TObject; const ClientID: TClientID; ClientConnection: ICrossWebSocketConnection) of object;
  TCrossWebSocketClientDisconnectedEvent = procedure(Sender: TObject; const ClientID: TClientID; ClientConnection: ICrossWebSocketConnection) of object;
  TCrossWebSocketErrorEvent = procedure(Sender: TObject; const ErrorMsg: string) of object;
  TCrossWebSocketServerHandleMessageEvent = procedure(Sender: TObject; const ClientID: TClientID; ClientConnection: ICrossWebSocketConnection; const Data: TBytes) of object;

  // Client connection record for tracking
  TClientConnectionInfo = record
    ClientID: TClientID;
    Connection: ICrossWebSocketConnection;
  end;

  /// HIGH-PERFORMANCE Cross Socket WebSocket Server - OPTIMIZED FOR 100K CONNECTIONS WITH CLIENT ID SUPPORT
  TCrossSocketWebSocketServer = class(TComponent)
  private
    // ONLY essential objects
    fWebSocketServer: ICrossWebSocketServer;

    // HIGH-PERFORMANCE connection tracking for broadcasting and client ID management
    fConnections: TList<TClientConnectionInfo>;
    fClientLookup: TDictionary<TClientID, ICrossWebSocketConnection>;
    fConnectionToClientID: TDictionary<ICrossWebSocketConnection, TClientID>; // REVERSE LOOKUP - CRITICAL FIX!
    fConnectionsLock: TCriticalSection; // MINIMAL locking - only for connection list

    // ONLY essential properties
    fPort: Integer;
    fActive: Boolean;
    fLastError: string;
    fIoThreads: Integer;
    fBindInterface: string;
    fServerState: TCrossWebSocketServerState;

    // ATOMIC counters ONLY - NO LOCKING
    fClientCount: Integer;
    fNextClientID: Integer; // For generating unique client IDs

    // MINIMAL events
    fOnClientConnected: TCrossWebSocketClientConnectedEvent;
    fOnClientDisconnected: TCrossWebSocketClientDisconnectedEvent;
    fOnError: TCrossWebSocketErrorEvent;
    fOnHandleMessage: TCrossWebSocketServerHandleMessageEvent;

    // Property setters
    procedure SetActive(const Value: Boolean);
    procedure SetPort(const Value: Integer);
    procedure SetIoThreads(const Value: Integer);
    procedure SetBindInterface(const Value: string);

    // MINIMAL helpers
    procedure DoError(const ErrorMsg: string);
    function GenerateClientID: TClientID;
    function AddConnection(const Connection: ICrossWebSocketConnection): TClientID; // RETURN CLIENT ID!
    procedure RemoveConnection(const Connection: ICrossWebSocketConnection);
    function FindClientID(const Connection: ICrossWebSocketConnection): TClientID;

  protected
    procedure InternalStart;
    procedure InternalStop;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // MINIMAL methods - OPTIMIZED FOR PERFORMANCE
    function Start: Boolean;
    procedure Stop;

    // SINGLE CLIENT SEND - ASYNC ONLY - TBYTES ONLY!
    function SendCommand(const ClientID: TClientID; const Data: TBytes): Boolean;

    // BROADCASTING - ASYNC ONLY - OPTIMIZED FOR 100K CONNECTIONS - TBYTES ONLY!
    function BroadcastCommand(const Data: TBytes): Boolean;

    // CLIENT MANAGEMENT
    function GetClientIDs: TArray<TClientID>;
    function IsClientConnected(const ClientID: TClientID): Boolean;
    function GetClientConnection(const ClientID: TClientID): ICrossWebSocketConnection;

    // UTILITY METHODS
    function IsActive: Boolean;
    function GetLastError: string;
    function GetClientCount: Integer;
    function GetConnectionCount: Integer; // Actual connection list count

  published
    // MINIMAL properties - OPTIMIZED FOR 100K CONNECTIONS
    property Port: Integer read fPort write SetPort default 8080;
    property Active: Boolean read fActive write SetActive default False;
    property IoThreads: Integer read fIoThreads write SetIoThreads default 64; // OPTIMIZED FOR 100K!
    property BindInterface: string read fBindInterface write SetBindInterface;

    // State (read-only)
    property ServerState: TCrossWebSocketServerState read fServerState;
    property ClientCount: Integer read fClientCount;
    property ConnectionCount: Integer read GetConnectionCount;

    // MINIMAL events
    property OnClientConnected: TCrossWebSocketClientConnectedEvent read fOnClientConnected write fOnClientConnected;
    property OnClientDisconnected: TCrossWebSocketClientDisconnectedEvent read fOnClientDisconnected write fOnClientDisconnected;
    property OnError: TCrossWebSocketErrorEvent read fOnError write fOnError;
    property OnHandleMessage: TCrossWebSocketServerHandleMessageEvent read fOnHandleMessage write fOnHandleMessage;
  end;

procedure Register;

implementation

uses
  Windows; // For InterlockedIncrement/Decrement

{ TCrossSocketWebSocketServer }

constructor TCrossSocketWebSocketServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // MINIMAL initialization - OPTIMIZED FOR 100K CONNECTIONS
  fPort := 8080;
  fActive := False;
  fClientCount := 0;
  fNextClientID := 0;
  fIoThreads := 64; // OPTIMIZED IO THREADS FOR 100K CONCURRENT CONNECTIONS
  fBindInterface := '0.0.0.0';
  fServerState := ssIdle;

  // HIGH-PERFORMANCE connection tracking with Client ID support + REVERSE LOOKUP
  fConnections := TList<TClientConnectionInfo>.Create;
  fClientLookup := TDictionary<TClientID, ICrossWebSocketConnection>.Create;
  fConnectionToClientID := TDictionary<ICrossWebSocketConnection, TClientID>.Create; // CRITICAL FIX!
  fConnectionsLock := TCriticalSection.Create;

  // Create Cross Socket WebSocket Server with MAXIMUM threads for 100K connections
  fWebSocketServer := TCrossWebSocketServer.Create(fIoThreads, False);

  // Set up MINIMAL event handlers - ZERO OVERHEAD!
  fWebSocketServer
    .OnOpen(
      procedure(const AConnection: ICrossWebSocketConnection)
      var
        ClientID: TClientID;
      begin
        // PROPER client ID assignment and tracking - CRITICAL FIX!
        ClientID := AddConnection(AConnection);

        // ATOMIC increment - NO LOCKING!
        InterlockedIncrement(fClientCount);

        // Fire event if assigned
        if Assigned(fOnClientConnected) then
          fOnClientConnected(Self, ClientID, AConnection);
      end)
    .OnMessage(
      procedure(const AConnection: ICrossWebSocketConnection; const ARequestType: TWsMessageType; const ARequestData: TBytes)
      var
        ClientID: TClientID;
      begin
        // FAST lookup using reverse dictionary - CRITICAL FIX!
        ClientID := FindClientID(AConnection);

        // DIRECT message processing - NO OVERHEAD!
        if Assigned(fOnHandleMessage) then
          fOnHandleMessage(Self, ClientID, AConnection, ARequestData);
      end)
    .OnClose(
      procedure(const AConnection: ICrossWebSocketConnection)
      var
        ClientID: TClientID;
      begin
        // Find client ID before removing - CRITICAL FIX!
        ClientID := FindClientID(AConnection);

        // Remove from connection tracking FIRST
        RemoveConnection(AConnection);

        // ATOMIC decrement - NO LOCKING!
        InterlockedDecrement(fClientCount);

        // Safety check
        if fClientCount < 0 then
          fClientCount := 0;

        // Fire event if assigned
        if Assigned(fOnClientDisconnected) then
          fOnClientDisconnected(Self, ClientID, AConnection);
      end);
end;

destructor TCrossSocketWebSocketServer.Destroy;
begin
  try
    if fActive then
      InternalStop;

    if fWebSocketServer <> nil then
    begin
      try
        fWebSocketServer.Stop;
        fWebSocketServer := nil;
      except
        fWebSocketServer := nil;
      end;
    end;

    // Cleanup connection tracking
    if fConnectionsLock <> nil then
    begin
      fConnectionsLock.Enter;
      try
        if fConnections <> nil then
        begin
          fConnections.Clear;
          FreeAndNil(fConnections);
        end;
        if fClientLookup <> nil then
        begin
          fClientLookup.Clear;
          FreeAndNil(fClientLookup);
        end;
        if fConnectionToClientID <> nil then
        begin
          fConnectionToClientID.Clear;
          FreeAndNil(fConnectionToClientID);
        end;
      finally
        fConnectionsLock.Leave;
      end;
      FreeAndNil(fConnectionsLock);
    end;
  except
    // Ignore cleanup errors
  end;
  inherited Destroy;
end;

// =============================================================================
// CLIENT ID MANAGEMENT - OPTIMIZED FOR 100K CONNECTIONS - CRITICAL FIXES!
// =============================================================================

function TCrossSocketWebSocketServer.GenerateClientID: TClientID;
begin
  Result := IntToStr(InterlockedIncrement(fNextClientID));
end;

function TCrossSocketWebSocketServer.FindClientID(const Connection: ICrossWebSocketConnection): TClientID;
begin
  Result := '';
  if (Connection = nil) or (fConnectionToClientID = nil) then
    Exit;

  fConnectionsLock.Enter;
  try
    // FAST reverse lookup - CRITICAL FIX!
    fConnectionToClientID.TryGetValue(Connection, Result);
  finally
    fConnectionsLock.Leave;
  end;
end;

// =============================================================================
// CONNECTION MANAGEMENT - OPTIMIZED FOR 100K CONNECTIONS - CRITICAL FIXES!
// =============================================================================

function TCrossSocketWebSocketServer.AddConnection(const Connection: ICrossWebSocketConnection): TClientID;
var
  ClientInfo: TClientConnectionInfo;
begin
  Result := '';
  if (Connection = nil) or (fConnections = nil) then
    Exit;

  fConnectionsLock.Enter;
  try
    // Check if connection already exists in reverse lookup
    if fConnectionToClientID.TryGetValue(Connection, Result) then
      Exit; // Already exists, return existing client ID

    // Generate new client ID
    Result := GenerateClientID;

    // Add to all tracking structures - CRITICAL FIX!
    ClientInfo.ClientID := Result;
    ClientInfo.Connection := Connection;

    fConnections.Add(ClientInfo);
    fClientLookup.Add(Result, Connection);
    fConnectionToClientID.Add(Connection, Result); // REVERSE LOOKUP - CRITICAL!
  finally
    fConnectionsLock.Leave;
  end;
end;

procedure TCrossSocketWebSocketServer.RemoveConnection(const Connection: ICrossWebSocketConnection);
var
  I: Integer;
  ClientID: TClientID;
begin
  if (Connection = nil) or (fConnections = nil) then
    Exit;

  fConnectionsLock.Enter;
  try
    // Get client ID from reverse lookup first - CRITICAL FIX!
    if fConnectionToClientID.TryGetValue(Connection, ClientID) then
    begin
      // Remove from reverse lookup
      fConnectionToClientID.Remove(Connection);

      // Remove from client lookup
      if fClientLookup.ContainsKey(ClientID) then
        fClientLookup.Remove(ClientID);

      // Remove from connections list
      for I := fConnections.Count - 1 downto 0 do
      begin
        if fConnections[I].Connection = Connection then
        begin
          fConnections.Delete(I);
          Break;
        end;
      end;
    end;
  finally
    fConnectionsLock.Leave;
  end;
end;

// =============================================================================
// CORE METHODS - MINIMAL IMPLEMENTATION - OPTIMIZED FOR 100K CONNECTIONS
// =============================================================================

function TCrossSocketWebSocketServer.Start: Boolean;
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

procedure TCrossSocketWebSocketServer.InternalStart;
begin
  if fActive then
    Exit;

  try
    fServerState := ssListening;
    fLastError := '';
    fClientCount := 0; // Reset counter
    fNextClientID := 0; // Reset client ID counter

    // Clear ALL connection tracking structures - CRITICAL FIX!
    fConnectionsLock.Enter;
    try
      fConnections.Clear;
      fClientLookup.Clear;
      fConnectionToClientID.Clear; // CRITICAL!
    finally
      fConnectionsLock.Leave;
    end;

    // Start WebSocket server - OPTIMIZED FOR 100K CONNECTIONS
    fWebSocketServer.Addr := fBindInterface;
    fWebSocketServer.Port := fPort;
    fWebSocketServer.Start;

    fActive := True;

  except
    on E: Exception do
    begin
      fLastError := E.Message;
      fServerState := ssError;
      fActive := False;
      raise;
    end;
  end;
end;

procedure TCrossSocketWebSocketServer.Stop;
begin
  InternalStop;
end;

procedure TCrossSocketWebSocketServer.InternalStop;
begin
  if not fActive then
    Exit;

  fActive := False;

  if fWebSocketServer <> nil then
  begin
    try
      fWebSocketServer.Stop;
    except
      // Ignore shutdown errors
    end;
  end;

  // Clear ALL connections - CRITICAL FIX!
  fConnectionsLock.Enter;
  try
    fConnections.Clear;
    fClientLookup.Clear;
    fConnectionToClientID.Clear; // CRITICAL!
  finally
    fConnectionsLock.Leave;
  end;

  fClientCount := 0;
  fNextClientID := 0;
  fServerState := ssIdle;
end;

// =============================================================================
// SINGLE CLIENT SEND - ASYNC ONLY - TBYTES ONLY!
// =============================================================================

function TCrossSocketWebSocketServer.SendCommand(const ClientID: TClientID; const Data: TBytes): Boolean;
begin
  Result := True;
  if (fWebSocketServer = nil) or not fActive or (ClientID = '') then
  begin
    Result := False;
    Exit;
  end;

  try
    // ASYNC - Fire and forget - non-blocking
    TThread.CreateAnonymousThread(
      procedure
      var
        Connection: ICrossWebSocketConnection;
      begin
        fConnectionsLock.Enter;
        try
          if fClientLookup.TryGetValue(ClientID, Connection) then
          begin
            if Connection <> nil then
            begin
              try
                // DIRECT send - NO OVERHEAD - OPTIMIZED FOR 100K CONNECTIONS
                Connection.WsSend(Data);
              except
                on E: Exception do
                begin
                  fLastError := E.Message;
                  DoError('Send to client ' + ClientID + ' failed: ' + E.Message);
                end;
              end;
            end;
          end;
        finally
          fConnectionsLock.Leave;
        end;
      end).Start;
  except
    Result := False;
  end;
end;

// =============================================================================
// BROADCASTING - ASYNC ONLY - OPTIMIZED FOR 100K CONNECTIONS - TBYTES ONLY!
// =============================================================================

function TCrossSocketWebSocketServer.BroadcastCommand(const Data: TBytes): Boolean;
begin
  Result := True;
  if (fWebSocketServer = nil) or not fActive or (fConnections = nil) then
  begin
    Result := False;
    Exit;
  end;

  try
    // ASYNC - Fire and forget - non-blocking
    TThread.CreateAnonymousThread(
      procedure
      var
        ConnectionsCopy: TArray<ICrossWebSocketConnection>;
        Connection: ICrossWebSocketConnection;
        I: Integer;
      begin
        // FAST: Copy connection list once under lock
        fConnectionsLock.Enter;
        try
          SetLength(ConnectionsCopy, fConnections.Count);
          for I := 0 to fConnections.Count - 1 do
            ConnectionsCopy[I] := fConnections[I].Connection;
        finally
          fConnectionsLock.Leave;
        end;

        // FAST: Broadcast without any locking
        for Connection in ConnectionsCopy do
        begin
          if Connection <> nil then
          begin
            try
              Connection.WsSend(Data);
            except
              // Ignore individual send failures - keeps broadcasting fast
            end;
          end;
        end;
      end).Start;
  except
    Result := False;
  end;
end;

// =============================================================================
// CLIENT MANAGEMENT METHODS
// =============================================================================

function TCrossSocketWebSocketServer.GetClientIDs: TArray<TClientID>;
var
  I: Integer;
begin
  if fConnections = nil then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  fConnectionsLock.Enter;
  try
    SetLength(Result, fConnections.Count);
    for I := 0 to fConnections.Count - 1 do
      Result[I] := fConnections[I].ClientID;
  finally
    fConnectionsLock.Leave;
  end;
end;

function TCrossSocketWebSocketServer.IsClientConnected(const ClientID: TClientID): Boolean;
begin
  if (fClientLookup = nil) or (ClientID = '') then
  begin
    Result := False;
    Exit;
  end;

  fConnectionsLock.Enter;
  try
    Result := fClientLookup.ContainsKey(ClientID);
  finally
    fConnectionsLock.Leave;
  end;
end;

function TCrossSocketWebSocketServer.GetClientConnection(const ClientID: TClientID): ICrossWebSocketConnection;
begin
  Result := nil;
  if (fClientLookup = nil) or (ClientID = '') then
    Exit;

  fConnectionsLock.Enter;
  try
    fClientLookup.TryGetValue(ClientID, Result);
  finally
    fConnectionsLock.Leave;
  end;
end;

// =============================================================================
// UTILITY METHODS
// =============================================================================

function TCrossSocketWebSocketServer.IsActive: Boolean;
begin
  Result := fActive and (fWebSocketServer <> nil);
end;

function TCrossSocketWebSocketServer.GetLastError: string;
begin
  Result := fLastError;
end;

function TCrossSocketWebSocketServer.GetClientCount: Integer;
begin
  Result := fClientCount;
end;

function TCrossSocketWebSocketServer.GetConnectionCount: Integer;
begin
  if fConnections = nil then
  begin
    Result := 0;
    Exit;
  end;

  fConnectionsLock.Enter;
  try
    Result := fConnections.Count;
  finally
    fConnectionsLock.Leave;
  end;
end;

// =============================================================================
// PROPERTY SETTERS - OPTIMIZED FOR 100K CONNECTIONS
// =============================================================================

procedure TCrossSocketWebSocketServer.SetActive(const Value: Boolean);
begin
  if fActive <> Value then
  begin
    if Value then
      Start
    else
      Stop;
  end;
end;

procedure TCrossSocketWebSocketServer.SetPort(const Value: Integer);
begin
  if fPort <> Value then
  begin
    if fActive then
      raise Exception.Create('Cannot change Port while server is active');
    fPort := Value;
  end;
end;

procedure TCrossSocketWebSocketServer.SetIoThreads(const Value: Integer);
begin
  if fIoThreads <> Value then
  begin
    if fActive then
      raise Exception.Create('Cannot change IoThreads while server is active');
    if Value > 0 then
      fIoThreads := Value;
  end;
end;

procedure TCrossSocketWebSocketServer.SetBindInterface(const Value: string);
begin
  fBindInterface := Value;
end;

procedure TCrossSocketWebSocketServer.DoError(const ErrorMsg: string);
begin
  if Assigned(fOnError) then
    fOnError(Self, ErrorMsg);
end;

procedure Register;
begin
  RegisterComponents('Cross Socket', [TCrossSocketWebSocketServer]);
end;

end.
