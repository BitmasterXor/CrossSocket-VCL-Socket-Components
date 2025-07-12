unit CrossSocket.WebSocket.Client;

interface

uses
  Classes,
  SysUtils,
  Math,
  TypInfo,
  // Actual Cross Socket WebSocket units
  Net.CrossWebSocketClient,
  Net.CrossWebSocketParser,
  Net.CrossSocket.Base,
  // Our WebSocket base types
  CrossSocket.WebSocket.Base;

type
  // RE-EXPORT INTERFACES FOR IDE COMPATIBILITY - THIS IS THE FIX!
  ICrossWebSocket = Net.CrossWebSocketClient.ICrossWebSocket;
  ICrossWebSocketMgr = Net.CrossWebSocketClient.ICrossWebSocketMgr;
  TWsMessageType = Net.CrossWebSocketParser.TWsMessageType;
  TCrossWebSocketStatus = CrossSocket.WebSocket.Base.TCrossWebSocketStatus;

  // MINIMAL events - NO CLIENT ID BULLSHIT
  TCrossWebSocketConnectEvent = procedure(Sender: TObject) of object;
  TCrossWebSocketDisconnectEvent = procedure(Sender: TObject) of object;
  TCrossWebSocketErrorEvent = procedure(Sender: TObject; const ErrorMsg: string) of object;
  TCrossWebSocketHandleMessageEvent = procedure(Sender: TObject; const Data: TBytes) of object;  // ONLY TBYTES!

  /// OPTIMIZED Cross Socket WebSocket Client - FOR 100K CONNECTIONS - ASYNC ONLY
  TCrossSocketWebSocketClient = class(TComponent)
  private
    // Core Cross Socket WebSocket objects
    fWebSocket: ICrossWebSocket;
    fWebSocketMgr: ICrossWebSocketMgr;

    // MINIMAL connection properties - NO OVERHEAD
    fUrl: string;
    fConnected: Boolean;
    fConnecting: Boolean;
    fLastError: string;
    fCurrentStatus: TCrossWebSocketStatus;

    // ESSENTIAL properties ONLY
    fActive: Boolean;
    fMaskingKey: Cardinal;

    // OPTIMIZED reconnection - NO VCL TIMERS!
    fAutoReconnect: Boolean;
    fReconnectInterval: Integer;
    fReconnectAttempts: Integer;
    fMaxReconnectAttempts: Integer;
    fReconnecting: Boolean;
    fUserDisconnected: Boolean;
    fLastReconnectTime: TDateTime;

    // MINIMAL events - NO STATISTICS OVERHEAD
    fOnConnect: TCrossWebSocketConnectEvent;
    fOnDisconnect: TCrossWebSocketDisconnectEvent;
    fOnError: TCrossWebSocketErrorEvent;
    fOnHandleMessage: TCrossWebSocketHandleMessageEvent;  // ONLY ESSENTIAL EVENT!

    // ATOMIC event flags - THREAD SAFE
    fInConnectEvent: Integer;   // Use Integer for atomic operations
    fInDisconnectEvent: Integer; // Use Integer for atomic operations

    // Property setters - MINIMAL ONLY
    procedure SetActive(const Value: Boolean);
    procedure SetConnected(const Value: Boolean);
    procedure SetUrl(const Value: string);
    procedure SetAutoReconnect(const Value: Boolean);
    procedure SetReconnectInterval(const Value: Integer);
    procedure SetMaxReconnectAttempts(const Value: Integer);
    procedure SetMaskingKey(const Value: Cardinal);

    // MINIMAL helper methods
    procedure DoError(const ErrorMsg: string);
    procedure DoHandleMessage(const Data: TBytes);
    procedure DoConnect;
    procedure DoDisconnect;
    procedure HandleUnexpectedDisconnection;
    procedure AttemptReconnect;

  protected
    procedure InternalConnect;
    procedure InternalDisconnect;
    procedure InitializeDefaults;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// OPTIMIZED METHODS - NO OVERHEAD
    function Connect: Boolean;
    procedure Disconnect;

    /// SINGLE SEND METHOD - ASYNC ONLY - TBYTES ONLY!
    function SendCommand(const Data: TBytes): Boolean;

    procedure Ping;

    /// ESSENTIAL STATUS METHODS ONLY
    function IsConnected: Boolean;
    function IsConnecting: Boolean;
    function IsReconnecting: Boolean;
    function GetLastError: string;
    function GetReconnectAttempts: Integer;
    procedure ResetReconnectAttempts;

  published
    /// MINIMAL PROPERTIES - NO OVERHEAD
    property Active: Boolean read fActive write SetActive default False;
    property Url: string read fUrl write SetUrl;
    property Connected: Boolean read fConnected write SetConnected default False;
    property MaskingKey: Cardinal read fMaskingKey write SetMaskingKey default 0;

    /// OPTIMIZED RECONNECTION - NO VCL TIMERS
    property AutoReconnect: Boolean read fAutoReconnect write SetAutoReconnect default False;
    property ReconnectInterval: Integer read fReconnectInterval write SetReconnectInterval default 5000;
    property MaxReconnectAttempts: Integer read fMaxReconnectAttempts write SetMaxReconnectAttempts default 0;

    /// READ-ONLY STATUS
    property Connecting: Boolean read fConnecting;
    property Reconnecting: Boolean read fReconnecting;
    property Status: TCrossWebSocketStatus read fCurrentStatus;

    /// MINIMAL EVENTS - NO STATISTICS OVERHEAD
    property OnConnect: TCrossWebSocketConnectEvent read fOnConnect write fOnConnect;
    property OnDisconnect: TCrossWebSocketDisconnectEvent read fOnDisconnect write fOnDisconnect;
    property OnError: TCrossWebSocketErrorEvent read fOnError write fOnError;
    property OnHandleMessage: TCrossWebSocketHandleMessageEvent read fOnHandleMessage write fOnHandleMessage;  // ONLY ESSENTIAL EVENT!
  end;

procedure Register;

implementation

uses
  Windows, // For InterlockedExchange
  DateUtils; // For MilliSecondsBetween

{ TCrossSocketWebSocketClient }

constructor TCrossSocketWebSocketClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // ATOMIC event flags initialization
  fInConnectEvent := 0;
  fInDisconnectEvent := 0;

  InitializeDefaults;

  // Create Cross Socket WebSocket Manager - OPTIMIZED FOR HIGH CONCURRENCY
  fWebSocketMgr := TCrossWebSocketMgr.Create(1); // MINIMAL THREADS - Scale at server level!
end;

procedure TCrossSocketWebSocketClient.InitializeDefaults;
begin
  // MINIMAL initialization - NO OVERHEAD
  fUrl := 'ws://localhost:8080';
  fConnected := False;
  fConnecting := False;
  fCurrentStatus := wsUnknown;
  fActive := False;
  fMaskingKey := 0; // Auto-generate

  // OPTIMIZED reconnection - NO VCL TIMERS
  fAutoReconnect := False;
  fReconnectInterval := 5000;
  fMaxReconnectAttempts := 0;
  fReconnectAttempts := 0;
  fReconnecting := False;
  fUserDisconnected := False;
  fLastReconnectTime := 0;
end;

destructor TCrossSocketWebSocketClient.Destroy;
begin
  try
    // Force disconnect - NO TIMER CLEANUP NEEDED
    if fConnected or fConnecting then
    begin
      try
        InternalDisconnect;
      except
        // Force cleanup even if disconnect fails
        fConnected := False;
        fConnecting := False;
        fReconnecting := False;
      end;
    end;

    // Cleanup WebSocket
    if fWebSocket <> nil then
    begin
      try
        fWebSocket.Close;
        fWebSocket := nil;
      except
        fWebSocket := nil;
      end;
    end;

    // Cleanup WebSocket Manager
    if fWebSocketMgr <> nil then
    begin
      try
        fWebSocketMgr.CancelAll;
        fWebSocketMgr := nil;
      except
        fWebSocketMgr := nil;
      end;
    end;

  except
    // Ignore ALL destructor errors - force cleanup
    fWebSocket := nil;
    fWebSocketMgr := nil;
  end;

  inherited Destroy;
end;

// =============================================================================
// OPTIMIZED CONNECTION METHODS - THREAD SAFE, NO OVERHEAD
// =============================================================================

function TCrossSocketWebSocketClient.Connect: Boolean;
begin
  Result := False;
  if fConnected or fConnecting then
    Exit;

  try
    fUserDisconnected := False;
    fCurrentStatus := wsConnecting;
    InternalConnect;
    Result := True;
  except
    on E: Exception do
    begin
      fLastError := E.Message;
      fCurrentStatus := wsDisconnected;
      DoError(E.Message);
    end;
  end;
end;

procedure TCrossSocketWebSocketClient.InternalConnect;
begin
  if fConnected then
    Exit;

  try
    fConnecting := True;
    fConnected := False;

    // Create WebSocket instance
    fWebSocket := fWebSocketMgr.CreateWebSocket(fUrl);

    // Set masking key if specified
    if fMaskingKey <> 0 then
      fWebSocket.MaskingKey := fMaskingKey;

    // OPTIMIZED event handlers - ATOMIC FLAGS, NO OVERHEAD
    fWebSocket
      .OnOpen(
        procedure
        begin
          // ATOMIC check to prevent duplicate calls - THREAD SAFE
          if InterlockedExchange(fInConnectEvent, 1) = 1 then
            Exit;

          try
            fConnected := True;
            fConnecting := False;
            fReconnectAttempts := 0;
            fReconnecting := False;
            fCurrentStatus := wsConnected;
            DoConnect;  // Fire OnConnect event ONLY ONCE
          finally
            InterlockedExchange(fInConnectEvent, 0);
          end;
        end)
      .OnMessage(
        procedure(const AMessageType: TWsMessageType; const AMessageData: TBytes)
        begin
          // DIRECT message processing - NO OVERHEAD - ONLY TBYTES!
          DoHandleMessage(AMessageData);
        end)
      .OnClose(
        procedure
        begin
          // ATOMIC check to prevent duplicate calls - THREAD SAFE
          if InterlockedExchange(fInDisconnectEvent, 1) = 1 then
            Exit;

          try
            if fConnected then
            begin
              fConnected := False;
              fConnecting := False;
              fCurrentStatus := wsDisconnected;
              DoDisconnect;  // Fire OnDisconnect event ONLY ONCE
              HandleUnexpectedDisconnection;
            end;
          finally
            InterlockedExchange(fInDisconnectEvent, 0);
          end;
        end);

    // Open connection - events will be fired automatically
    fWebSocket.Open;

  except
    on E: Exception do
    begin
      fLastError := 'Connection failed: ' + E.Message;
      fConnecting := False;
      raise;
    end;
  end;
end;

procedure TCrossSocketWebSocketClient.Disconnect;
begin
  fUserDisconnected := True;
  fCurrentStatus := wsDisconnected;
  InternalDisconnect;
end;

procedure TCrossSocketWebSocketClient.InternalDisconnect;
begin
  if not fConnected and not fConnecting then
    Exit;

  try
    fConnected := False;
    fConnecting := False;
    fReconnecting := False;

    // Close WebSocket - OnClose will be called automatically
    if fWebSocket <> nil then
    begin
      try
        fWebSocket.Close;  // This will trigger OnClose automatically
      except
        // Ignore close errors but still clean up
        fWebSocket := nil;
        // If close failed, manually fire disconnect
        if InterlockedExchange(fInDisconnectEvent, 1) = 0 then
        begin
          try
            DoDisconnect;
          finally
            InterlockedExchange(fInDisconnectEvent, 0);
          end;
        end;
      end;
    end
    else
    begin
      // No WebSocket to close, manually fire disconnect
      if InterlockedExchange(fInDisconnectEvent, 1) = 0 then
      begin
        try
          fCurrentStatus := wsDisconnected;
          DoDisconnect;
        finally
          InterlockedExchange(fInDisconnectEvent, 0);
        end;
      end;
    end;

  except
    on E: Exception do
    begin
      fLastError := 'Disconnect error: ' + E.Message;
      // Force cleanup even on error
      fWebSocket := nil;
      fConnected := False;
      fConnecting := False;
      fReconnecting := False;
    end;
  end;
end;

// =============================================================================
// SINGLE SEND METHOD - ASYNC ONLY - TBYTES ONLY!
// =============================================================================

function TCrossSocketWebSocketClient.SendCommand(const Data: TBytes): Boolean;
begin
  Result := True;
  if not IsConnected or (fWebSocket = nil) then
  begin
    Result := False;
    Exit;
  end;

  try
    // ASYNC - Fire and forget - non-blocking
    TThread.CreateAnonymousThread(
      procedure
      begin
        try
          // DIRECT send - NO OVERHEAD - OPTIMIZED FOR PERFORMANCE
          fWebSocket.Send(Data);
        except
          on E: Exception do
          begin
            fLastError := E.Message;
            DoError('Send failed: ' + E.Message);
          end;
        end;
      end).Start;
  except
    Result := False;
  end;
end;

procedure TCrossSocketWebSocketClient.Ping;
begin
  if IsConnected and (fWebSocket <> nil) then
    fWebSocket.Ping;
end;

// =============================================================================
// OPTIMIZED RECONNECTION - NO VCL TIMERS!
// =============================================================================

procedure TCrossSocketWebSocketClient.HandleUnexpectedDisconnection;
begin
  if fAutoReconnect and
     not fUserDisconnected and
     ((fMaxReconnectAttempts = 0) or (fReconnectAttempts < fMaxReconnectAttempts)) then
  begin
    fReconnecting := True;
    fCurrentStatus := wsConnecting;

    // OPTIMIZED: Schedule reconnection using Cross Socket's thread pool
    TThread.CreateAnonymousThread(
      procedure
      begin
        Sleep(fReconnectInterval); // Simple sleep instead of VCL timer
        AttemptReconnect;
      end).Start;
  end;
end;

procedure TCrossSocketWebSocketClient.AttemptReconnect;
begin
  // Check if we should still reconnect
  if not fAutoReconnect or fUserDisconnected or fConnected then
    Exit;

  // Check time-based throttling
  if (fLastReconnectTime > 0) and
     (MilliSecondsBetween(Now, fLastReconnectTime) < fReconnectInterval) then
    Exit;

  fLastReconnectTime := Now;
  Inc(fReconnectAttempts);

  try
    Connect;
  except
    on E: Exception do
    begin
      DoError('Reconnection failed: ' + E.Message);

      // Schedule next attempt if we haven't exceeded max attempts
      if (fMaxReconnectAttempts = 0) or (fReconnectAttempts < fMaxReconnectAttempts) then
      begin
        TThread.CreateAnonymousThread(
          procedure
          begin
            Sleep(fReconnectInterval);
            AttemptReconnect;
          end).Start;
      end
      else
      begin
        fReconnecting := False;
        fCurrentStatus := wsDisconnected;
      end;
    end;
  end;
end;

// =============================================================================
// OPTIMIZED PROPERTY SETTERS - NO OVERHEAD
// =============================================================================

procedure TCrossSocketWebSocketClient.SetActive(const Value: Boolean);
begin
  if fActive <> Value then
  begin
    fActive := Value;
    SetConnected(Value);
  end;
end;

procedure TCrossSocketWebSocketClient.SetConnected(const Value: Boolean);
begin
  if fConnected <> Value then
  begin
    if Value then
      Connect
    else
      Disconnect;
  end;
end;

procedure TCrossSocketWebSocketClient.SetUrl(const Value: string);
begin
  if fUrl <> Value then
  begin
    if not (fConnected or fConnecting) then
      fUrl := Value;
  end;
end;

procedure TCrossSocketWebSocketClient.SetAutoReconnect(const Value: Boolean);
begin
  fAutoReconnect := Value;
end;

procedure TCrossSocketWebSocketClient.SetReconnectInterval(const Value: Integer);
begin
  if Value >= 1000 then
    fReconnectInterval := Value;
end;

procedure TCrossSocketWebSocketClient.SetMaxReconnectAttempts(const Value: Integer);
begin
  if Value >= 0 then
    fMaxReconnectAttempts := Value;
end;

procedure TCrossSocketWebSocketClient.SetMaskingKey(const Value: Cardinal);
begin
  fMaskingKey := Value;
  if fWebSocket <> nil then
    fWebSocket.MaskingKey := Value;
end;

// =============================================================================
// ESSENTIAL STATUS METHODS ONLY
// =============================================================================

function TCrossSocketWebSocketClient.IsConnected: Boolean;
begin
  Result := fConnected and (fWebSocket <> nil) and (fWebSocket.Status = Net.CrossWebSocketClient.wsConnected);
end;

function TCrossSocketWebSocketClient.IsConnecting: Boolean;
begin
  Result := fConnecting or (fCurrentStatus = wsConnecting);
end;

function TCrossSocketWebSocketClient.IsReconnecting: Boolean;
begin
  Result := fReconnecting;
end;

function TCrossSocketWebSocketClient.GetLastError: string;
begin
  Result := fLastError;
end;

function TCrossSocketWebSocketClient.GetReconnectAttempts: Integer;
begin
  Result := fReconnectAttempts;
end;

procedure TCrossSocketWebSocketClient.ResetReconnectAttempts;
begin
  fReconnectAttempts := 0;
end;

// =============================================================================
// MINIMAL EVENT METHODS - NO OVERHEAD
// =============================================================================

procedure TCrossSocketWebSocketClient.DoError(const ErrorMsg: string);
begin
  if Assigned(fOnError) then
    fOnError(Self, ErrorMsg);
end;

// ONLY ESSENTIAL EVENT - NO OVERHEAD - TBYTES ONLY!
procedure TCrossSocketWebSocketClient.DoHandleMessage(const Data: TBytes);
begin
  if Assigned(fOnHandleMessage) then
    fOnHandleMessage(Self, Data);
end;

procedure TCrossSocketWebSocketClient.DoConnect;
begin
  if Assigned(fOnConnect) then
    fOnConnect(Self);
end;

procedure TCrossSocketWebSocketClient.DoDisconnect;
begin
  if Assigned(fOnDisconnect) then
    fOnDisconnect(Self);
end;

procedure Register;
begin
  RegisterComponents('Cross Socket', [TCrossSocketWebSocketClient]);
end;

end.
