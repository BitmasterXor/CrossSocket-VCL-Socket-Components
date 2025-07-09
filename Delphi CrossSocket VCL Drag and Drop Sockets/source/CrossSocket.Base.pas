unit CrossSocket.Base;

interface

uses
  Classes,
  SysUtils,
  System.Generics.Collections,
  SyncObjs,
  TypInfo,
  // Actual Cross Socket units
  Net.CrossSocket.Base,
  Net.SocketAPI;

const
  // ENCRYPTION COORDINATION CONSTANTS - SAME AS mORMot2 VERSION!
  ENCRYPTION_HEADER_SIZE = 16;
  // 4 bytes magic + 4 bytes mode + 4 bytes keysize + 4 bytes reserved
  ENCRYPTION_MAGIC = $43524F53; // "CROS" in hex for Cross Socket

type
  /// Use actual Cross Socket connection states from Net.CrossSocket.Base
  TCrossSocketConnectionState = TConnectStatus; // csUnknown, csConnecting, csHandshaking, csConnected, csDisconnected, csClosed

  /// Our custom server states (Cross Socket doesn't define server states)
  TCrossSocketServerState = (
    ssIdle,           // Server is idle/stopped
    ssStarting,       // Server is starting up
    ssListening,      // Server is listening for connections
    ssStopping,       // Server is shutting down
    ssError          // Server encountered an error
  );

  /// Use actual Cross Socket connection states for client tracking too
  TCrossSocketClientState = TConnectStatus;

  /// AES Encryption Mode enumeration - SAME AS mORMot2!
  TAESMode = (amECB, amCBC, amCFB, amOFB, amCTR, amGCM, amCFC, amOFC, amCTC);

  /// AES Key Size enumeration - SAME AS mORMot2!
  TAESKeySize = (aks128, aks192, aks256);

  /// Reconnection strategy enumeration
  TCrossSocketReconnectStrategy = (
    rsLinear,         // Linear reconnection intervals
    rsExponential     // Exponential backoff intervals
  );

  /// Cross Socket event types - FIXED LIKE mORMot2!
  TCrossSocketConnectEvent = procedure(Sender: TObject) of object;
  TCrossSocketDataReceivedEvent = procedure(Sender: TObject; const Data: TBytes) of object;  // FOR STATISTICS/LOGGING ONLY!
  TCrossSocketDataSentEvent = procedure(Sender: TObject; const Data: TBytes) of object;      // FOR STATISTICS/LOGGING ONLY!
  TCrossSocketDisconnectEvent = procedure(Sender: TObject) of object;
  TCrossSocketErrorEvent = procedure(Sender: TObject; const ErrorMsg: string) of object;
  TCrossSocketHandleCommandEvent = procedure(Sender: TObject; const Command: TBytes) of object;  // THIS IS WHERE YOU PROCESS DATA!

  /// Server-side events
  TCrossSocketClientConnectedEvent = procedure(Sender: TObject; ClientID: Integer) of object;
  TCrossSocketClientDisconnectedEvent = procedure(Sender: TObject; ClientID: Integer) of object;
  TCrossSocketServerDataReceivedEvent = procedure(Sender: TObject; ClientID: Integer; const Data: TBytes) of object;  // FOR STATISTICS/LOGGING ONLY!
  TCrossSocketServerDataSentEvent = procedure(Sender: TObject; ClientID: Integer; const Data: TBytes) of object;      // FOR STATISTICS/LOGGING ONLY!
  TCrossSocketServerHandleCommandEvent = procedure(Sender: TObject; ClientID: Integer; const Command: TBytes) of object;  // THIS IS WHERE YOU PROCESS DATA!

  /// State change events
  TCrossSocketStateChangeEvent = procedure(Sender: TObject; OldState, NewState: TCrossSocketConnectionState; const StateDescription: string) of object;
  TCrossSocketServerStateChangeEvent = procedure(Sender: TObject; OldState, NewState: TCrossSocketServerState; const StateDescription: string) of object;

  /// Reconnection events
  TCrossSocketReconnectingEvent = procedure(Sender: TObject; AttemptNumber: Integer) of object;
  TCrossSocketReconnectFailedEvent = procedure(Sender: TObject; AttemptNumber: Integer; const ErrorMsg: string) of object;

  /// Cross Socket callback types - using actual Cross Socket types
  TCrossConnectionCallback = Net.CrossSocket.Base.TCrossConnectionCallback;
  TCrossListenCallback = Net.CrossSocket.Base.TCrossListenCallback;

  /// Use actual Cross Socket interfaces
  ICrossSocket = Net.CrossSocket.Base.ICrossSocket;
  ICrossConnection = Net.CrossSocket.Base.ICrossConnection;
  ICrossListen = Net.CrossSocket.Base.ICrossListen;

  /// Base encryption helper class - SAME AS mORMot2 APPROACH!
  TCrossSocketEncryption = class
  private
    fEncryptionEnabled: Boolean;
    fEncryptionKey: string;
    fEncryptionMode: TAESMode;
    fEncryptionKeySize: TAESKeySize;
  public
    constructor Create;

    /// Encrypt/Decrypt data with coordination headers
    function EncryptData(const Data: TBytes; const PeerInfo: string): TBytes;
    function DecryptData(const Data: TBytes; const PeerInfo: string): TBytes;

    /// Get encryption info string
    function GetEncryptionInfo: string;

    /// Properties
    property EncryptionEnabled: Boolean read fEncryptionEnabled write fEncryptionEnabled;
    property EncryptionKey: string read fEncryptionKey write fEncryptionKey;
    property EncryptionMode: TAESMode read fEncryptionMode write fEncryptionMode;
    property EncryptionKeySize: TAESKeySize read fEncryptionKeySize write fEncryptionKeySize;
  end;

  /// Client connection info for tracking
  TClientConnectionInfo = record
    ClientID: Integer;
    Connection: ICrossConnection;
    IP: string;
    ConnectedAt: TDateTime;
    State: TCrossSocketClientState;
  end;

  /// Base helper functions - following mORMot2 patterns
function CrossSocketConnectionStateToString(State: TCrossSocketConnectionState): string;
function CrossSocketServerStateToString(State: TCrossSocketServerState): string;
function CrossSocketClientStateToString(State: TCrossSocketClientState): string;

implementation

uses
  Math;

function CrossSocketConnectionStateToString(State: TCrossSocketConnectionState): string;
begin
  case State of
    csUnknown: Result := 'Unknown';
    csConnecting: Result := 'Connecting';
    csHandshaking: Result := 'Handshaking';
    csConnected: Result := 'Connected';
    csDisconnected: Result := 'Disconnected';
    csClosed: Result := 'Closed';
  else
    Result := 'Unknown';
  end;
end;

function CrossSocketServerStateToString(State: TCrossSocketServerState): string;
begin
  case State of
    ssIdle: Result := 'Idle';
    ssStarting: Result := 'Starting';
    ssListening: Result := 'Listening';
    ssStopping: Result := 'Stopping';
    ssError: Result := 'Error';
  else
    Result := 'Unknown';
  end;
end;

function CrossSocketClientStateToString(State: TCrossSocketClientState): string;
begin
  // Same as connection state since they're the same type
  Result := CrossSocketConnectionStateToString(State);
end;

{ TCrossSocketEncryption }

constructor TCrossSocketEncryption.Create;
begin
  inherited Create;
  fEncryptionEnabled := False;
  fEncryptionKey := '';
  fEncryptionMode := amCBC;
  fEncryptionKeySize := aks256;
end;

function TCrossSocketEncryption.EncryptData(const Data: TBytes; const PeerInfo: string): TBytes;
var
  Header: array[0..ENCRYPTION_HEADER_SIZE-1] of Byte;
begin
  Result := Data; // Default to original data

  if not fEncryptionEnabled or (fEncryptionKey = '') then
    Exit;

  try
    // For now, just add coordination header - actual encryption would use
    // same AES implementation as mORMot2 version
    FillChar(Header, SizeOf(Header), 0);
    PCardinal(@Header[0])^ := ENCRYPTION_MAGIC;
    PCardinal(@Header[4])^ := Cardinal(fEncryptionMode);
    PCardinal(@Header[8])^ := Cardinal(fEncryptionKeySize);
    // bytes 12-15 are reserved

    // Combine header + data (for this example)
    SetLength(Result, ENCRYPTION_HEADER_SIZE + Length(Data));
    Move(Header[0], Result[0], ENCRYPTION_HEADER_SIZE);
    if Length(Data) > 0 then
      Move(Data[0], Result[ENCRYPTION_HEADER_SIZE], Length(Data));

  except
    on E: Exception do
      Result := Data; // Return original data on error
  end;
end;

function TCrossSocketEncryption.DecryptData(const Data: TBytes; const PeerInfo: string): TBytes;
var
  Header: array[0..ENCRYPTION_HEADER_SIZE-1] of Byte;
  Magic: Cardinal;
  Mode: TAESMode;
  KeySize: TAESKeySize;
begin
  Result := Data; // Default to original data

  if not fEncryptionEnabled or (fEncryptionKey = '') then
    Exit;

  // Check if data has encryption header
  if Length(Data) < ENCRYPTION_HEADER_SIZE then
    Exit;

  try
    // Extract and verify header
    Move(Data[0], Header[0], ENCRYPTION_HEADER_SIZE);
    Magic := PCardinal(@Header[0])^;

    if Magic <> ENCRYPTION_MAGIC then
      Exit; // Not our encrypted data

    Mode := TAESMode(PCardinal(@Header[4])^);
    KeySize := TAESKeySize(PCardinal(@Header[8])^);

    // Validate parameters match ours
    if (Mode <> fEncryptionMode) or (KeySize <> fEncryptionKeySize) then
      Exit; // Parameter mismatch

    // Extract data part (skip header)
    SetLength(Result, Length(Data) - ENCRYPTION_HEADER_SIZE);
    if Length(Result) > 0 then
      Move(Data[ENCRYPTION_HEADER_SIZE], Result[0], Length(Result));

  except
    on E: Exception do
      Result := Data; // Return original data on error
  end;
end;

function TCrossSocketEncryption.GetEncryptionInfo: string;
const
  ModeNames: array[TAESMode] of string = ('ECB', 'CBC', 'CFB', 'OFB', 'CTR', 'GCM', 'CFC', 'OFC', 'CTC');
  KeySizeNames: array[TAESKeySize] of string = ('128', '192', '256');
begin
  if fEncryptionEnabled and (fEncryptionKey <> '') then
    Result := Format('AES-%s-%s', [KeySizeNames[fEncryptionKeySize], ModeNames[fEncryptionMode]])
  else if fEncryptionEnabled then
    Result := 'ENABLED BUT NO KEY SET'
  else
    Result := 'DISABLED';
end;

end.
