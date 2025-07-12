unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CrossSocket.WebSocket.Server,
  CrossSocket.WebSocket.Client, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  CrossSocket.WebSocket.Base, Net.CrossWebSocketServer, Net.CrossWebSocketParser, DateUtils;

type
  TForm1 = class(TForm)
    CrossSocketWebSocketClient1: TCrossSocketWebSocketClient;
    CrossSocketWebSocketServer1: TCrossSocketWebSocketServer;
    Panel1: TPanel;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    edtServerPort: TEdit;
    btnStartServer: TButton;
    btnStopServer: TButton;
    lblServerStatus: TLabel;
    lblClientCount: TLabel;
    memoServerLog: TMemo;
    Label2: TLabel;
    edtClientURL: TEdit;
    btnConnect: TButton;
    btnDisconnect: TButton;
    lblClientStatus: TLabel;
    memoClientLog: TMemo;
    Panel3: TPanel;
    GroupBox3: TGroupBox;
    Label3: TLabel;
    edtMessage: TEdit;
    btnSendCommand: TButton;
    btnPing: TButton;
    Panel4: TPanel;
    GroupBox4: TGroupBox;
    edtBroadcast: TEdit;
    btnBroadcastCommand: TButton;
    Label4: TLabel;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    chkAutoReconnect: TCheckBox;
    btnMassClients: TButton;
    btnClearLogs: TButton;
    lblPerformance: TLabel;
    edtClientID: TEdit;
    Label5: TLabel;
    btnSendToClient: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartServerClick(Sender: TObject);
    procedure btnStopServerClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnSendCommandClick(Sender: TObject);
    procedure btnPingClick(Sender: TObject);
    procedure btnBroadcastCommandClick(Sender: TObject);
    procedure btnSendToClientClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure chkAutoReconnectClick(Sender: TObject);
    procedure edtMessageKeyPress(Sender: TObject; var Key: Char);
    procedure edtBroadcastKeyPress(Sender: TObject; var Key: Char);
    procedure btnMassClientsClick(Sender: TObject);
    procedure btnClearLogsClick(Sender: TObject);

    // WebSocket Server Events
    procedure ServerClientConnected(Sender: TObject; const ClientID: TClientID; ClientConnection: ICrossWebSocketConnection);
    procedure ServerClientDisconnected(Sender: TObject; const ClientID: TClientID; ClientConnection: ICrossWebSocketConnection);
    procedure ServerHandleMessage(Sender: TObject; const ClientID: TClientID; ClientConnection: ICrossWebSocketConnection; const Data: TBytes);
    procedure ServerError(Sender: TObject; const ErrorMsg: string);

    // WebSocket Client Events
    procedure ClientConnect(Sender: TObject);
    procedure ClientDisconnect(Sender: TObject);
    procedure ClientHandleMessage(Sender: TObject; const Data: TBytes);
    procedure ClientError(Sender: TObject; const ErrorMsg: string);

  private
    { Private declarations }
    FMassClients: array of TCrossSocketWebSocketClient;
    FStartTime: TDateTime;
    FMessageCount: Integer;
    procedure LogServer(const Msg: string);
    procedure LogClient(const Msg: string);
    procedure UpdateUI;
    function GetClientIP(ClientConnection: ICrossWebSocketConnection): string;
    procedure CreateMassClients(Count: Integer);
    procedure DestroyMassClients;

    // Helper functions for TBytes conversion
    function BytesOf(const AString: string): TBytes;
    function StringOf(const ABytes: TBytes): string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// =============================================================================
// HELPER FUNCTIONS FOR TBYTES CONVERSION
// =============================================================================

function TForm1.BytesOf(const AString: string): TBytes;
begin
  Result := TEncoding.UTF8.GetBytes(AString);
end;

function TForm1.StringOf(const ABytes: TBytes): string;
begin
  Result := TEncoding.UTF8.GetString(ABytes);
end;

// =============================================================================
// FORM EVENTS
// =============================================================================

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialize UI
  Caption := 'CrossSocket WebSocket Demo - ASYNC TBYTES ONLY - 100K Connections Ready!';
  FStartTime := Now;
  FMessageCount := 0;

  // Setup Server
  CrossSocketWebSocketServer1.Port := StrToIntDef(edtServerPort.Text, 8080);
  CrossSocketWebSocketServer1.OnClientConnected := ServerClientConnected;
  CrossSocketWebSocketServer1.OnClientDisconnected := ServerClientDisconnected;
  CrossSocketWebSocketServer1.OnHandleMessage := ServerHandleMessage;
  CrossSocketWebSocketServer1.OnError := ServerError;

  // Setup Client
  CrossSocketWebSocketClient1.Url := edtClientURL.Text;
  CrossSocketWebSocketClient1.AutoReconnect := chkAutoReconnect.Checked;
  CrossSocketWebSocketClient1.OnConnect := ClientConnect;
  CrossSocketWebSocketClient1.OnDisconnect := ClientDisconnect;
  CrossSocketWebSocketClient1.OnHandleMessage := ClientHandleMessage;
  CrossSocketWebSocketClient1.OnError := ClientError;

  // Initialize UI state
  UpdateUI;
  Timer1.Enabled := True;

  LogServer('ASYNC TBYTES WebSocket Demo Started - Ready for 100K connections!');
  LogClient('Ready to connect...');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;

  // Cleanup mass clients
  DestroyMassClients;

  if CrossSocketWebSocketClient1.IsConnected then
    CrossSocketWebSocketClient1.Disconnect;

  if CrossSocketWebSocketServer1.IsActive then
    CrossSocketWebSocketServer1.Stop;
end;

// =============================================================================
// SERVER BUTTON EVENTS
// =============================================================================

procedure TForm1.btnStartServerClick(Sender: TObject);
begin
  try
    CrossSocketWebSocketServer1.Port := StrToIntDef(edtServerPort.Text, 8080);
    if CrossSocketWebSocketServer1.Start then
      LogServer('ASYNC Server started on port ' + IntToStr(CrossSocketWebSocketServer1.Port) + ' - Ready for 100K connections!')
    else
      LogServer('Failed to start server: ' + CrossSocketWebSocketServer1.GetLastError);
  except
    on E: Exception do
      LogServer('Server start error: ' + E.Message);
  end;
  UpdateUI;
end;

procedure TForm1.btnStopServerClick(Sender: TObject);
begin
  try
    CrossSocketWebSocketServer1.Stop;
    LogServer('Server stopped');
  except
    on E: Exception do
      LogServer('Server stop error: ' + E.Message);
  end;
  UpdateUI;
end;

procedure TForm1.btnBroadcastCommandClick(Sender: TObject);
var
  message: string;
  data: TBytes;
  startTime: TDateTime;
  elapsed: Integer;
begin
  message := edtBroadcast.Text;
  if message <> '' then
  begin
    data := BytesOf(message);
    startTime := Now;

    if CrossSocketWebSocketServer1.BroadcastCommand(data) then
    begin
      elapsed := MilliSecondsBetween(Now, startTime);
      LogServer('ASYNC Broadcast queued: "' + message + '" to ' + IntToStr(CrossSocketWebSocketServer1.GetClientCount) +
        ' clients (' + IntToStr(Length(data)) + ' bytes) in ' + IntToStr(elapsed) + 'ms');
    end
    else
      LogServer('Failed to queue broadcast');
  end;
end;

procedure TForm1.btnSendToClientClick(Sender: TObject);
var
  clientID: TClientID;
  message: string;
  data: TBytes;
begin
  clientID := edtClientID.Text;
  message := edtMessage.Text;

  if (clientID <> '') and (message <> '') then
  begin
    data := BytesOf(message + ' [to client ' + clientID + ']');

    if CrossSocketWebSocketServer1.SendCommand(clientID, data) then
      LogServer('ASYNC Send queued to client ' + clientID + ': "' + message + '"')
    else
      LogServer('Failed to queue send to client ' + clientID);
  end
  else
    LogServer('Please enter both Client ID and message');
end;

// =============================================================================
// CLIENT BUTTON EVENTS
// =============================================================================

procedure TForm1.btnConnectClick(Sender: TObject);
begin
  try
    CrossSocketWebSocketClient1.Url := edtClientURL.Text;
    if CrossSocketWebSocketClient1.Connect then
      LogClient('Connecting to ' + CrossSocketWebSocketClient1.Url + '...')
    else
      LogClient('Failed to start connection: ' + CrossSocketWebSocketClient1.GetLastError);
  except
    on E: Exception do
      LogClient('Connection error: ' + E.Message);
  end;
  UpdateUI;
end;

procedure TForm1.btnDisconnectClick(Sender: TObject);
begin
  try
    CrossSocketWebSocketClient1.Disconnect;
    LogClient('Disconnecting...');
  except
    on E: Exception do
      LogClient('Disconnect error: ' + E.Message);
  end;
  UpdateUI;
end;

procedure TForm1.btnSendCommandClick(Sender: TObject);
var
  message: string;
  data: TBytes;
begin
  message := edtMessage.Text;
  if message <> '' then
  begin
    data := BytesOf(message);

    if CrossSocketWebSocketClient1.SendCommand(data) then
    begin
      LogClient('ASYNC Send queued: "' + message + '" (' + IntToStr(Length(data)) + ' bytes)');
      Inc(FMessageCount);
    end
    else
      LogClient('Failed to queue send');
  end;
end;

procedure TForm1.btnPingClick(Sender: TObject);
begin
  CrossSocketWebSocketClient1.Ping;
  LogClient('Sent ping');
end;

procedure TForm1.btnMassClientsClick(Sender: TObject);
var
  clientCount: Integer;
  response: string;
begin
  response := InputBox('Mass Client Test', 'Number of clients to create (WARNING: High numbers may stress your system):', '100');
  if response <> '' then
  begin
    clientCount := StrToIntDef(response, 0);
    if (clientCount > 0) and (clientCount <= 10000) then // Safety limit
    begin
      LogClient('Creating ' + IntToStr(clientCount) + ' mass clients...');
      CreateMassClients(clientCount);
    end
    else
      LogClient('Invalid client count (must be 1-10000)');
  end;
end;

procedure TForm1.btnClearLogsClick(Sender: TObject);
begin
  memoServerLog.Clear;
  memoClientLog.Clear;
  FMessageCount := 0;
  FStartTime := Now;
end;

// =============================================================================
// UI EVENT HANDLERS
// =============================================================================

procedure TForm1.chkAutoReconnectClick(Sender: TObject);
begin
  CrossSocketWebSocketClient1.AutoReconnect := chkAutoReconnect.Checked;
  LogClient('Auto-reconnect: ' + BoolToStr(chkAutoReconnect.Checked, True));
end;

procedure TForm1.edtMessageKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then // Enter key
  begin
    btnSendCommandClick(nil);
    Key := #0;
  end;
end;

procedure TForm1.edtBroadcastKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then // Enter key
  begin
    btnBroadcastCommandClick(nil);
    Key := #0;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  UpdateUI;
end;

// =============================================================================
// WEBSOCKET SERVER EVENTS
// =============================================================================

procedure TForm1.ServerClientConnected(Sender: TObject; const ClientID: TClientID; ClientConnection: ICrossWebSocketConnection);
begin
  LogServer('Client ID ' + ClientID + ' connected from: ' + GetClientIP(ClientConnection) +
    ' (Total: ' + IntToStr(CrossSocketWebSocketServer1.GetClientCount) + ')');
end;

procedure TForm1.ServerClientDisconnected(Sender: TObject; const ClientID: TClientID; ClientConnection: ICrossWebSocketConnection);
begin
  LogServer('Client ID ' + ClientID + ' disconnected from: ' + GetClientIP(ClientConnection) +
    ' (Total: ' + IntToStr(CrossSocketWebSocketServer1.GetClientCount) + ')');
end;

procedure TForm1.ServerHandleMessage(Sender: TObject; const ClientID: TClientID; ClientConnection: ICrossWebSocketConnection; const Data: TBytes);
var
  msgText: string;
  response: TBytes;
begin
  Inc(FMessageCount);

  msgText := StringOf(Data);
  LogServer('Received from Client ' + ClientID + ' (' + GetClientIP(ClientConnection) + '): "' + msgText + '" (' + IntToStr(Length(Data)) + ' bytes)');

  // Echo the message back with a prefix - ASYNC!
  response := BytesOf('Echo: ' + msgText);
  if CrossSocketWebSocketServer1.SendCommand(ClientID, response) then
    LogServer('ASYNC Echo queued back to Client ' + ClientID + ': "Echo: ' + msgText + '"')
  else
    LogServer('Failed to queue echo to Client ' + ClientID);
end;

procedure TForm1.ServerError(Sender: TObject; const ErrorMsg: string);
begin
  LogServer('ERROR: ' + ErrorMsg);
end;

// =============================================================================
// WEBSOCKET CLIENT EVENTS
// =============================================================================

procedure TForm1.ClientConnect(Sender: TObject);
begin
  LogClient('Connected to server!');
end;

procedure TForm1.ClientDisconnect(Sender: TObject);
begin
  LogClient('Disconnected from server');
end;

procedure TForm1.ClientHandleMessage(Sender: TObject; const Data: TBytes);
var
  msgText: string;
begin
  Inc(FMessageCount);

  msgText := StringOf(Data);
  LogClient('Received: "' + msgText + '" (' + IntToStr(Length(Data)) + ' bytes)');
end;

procedure TForm1.ClientError(Sender: TObject; const ErrorMsg: string);
begin
  LogClient('ERROR: ' + ErrorMsg);
end;

// =============================================================================
// MASS CLIENT TESTING
// =============================================================================

procedure TForm1.CreateMassClients(Count: Integer);
var
  i: Integer;
  startTime: TDateTime;
  elapsed: Integer;
begin
  DestroyMassClients; // Clean up any existing mass clients

  startTime := Now;
  SetLength(FMassClients, Count);

  for i := 0 to Count - 1 do
  begin
    FMassClients[i] := TCrossSocketWebSocketClient.Create(Self);
    FMassClients[i].Url := 'ws://127.0.0.1:' + edtServerPort.Text;
    FMassClients[i].AutoReconnect := False;

    // Connect each client
    FMassClients[i].Connect;

    // Process messages to keep UI responsive
    Application.ProcessMessages;

    // Small delay between connections
    Sleep(1);
  end;

  elapsed := MilliSecondsBetween(Now, startTime);
  LogClient('Created and connected ' + IntToStr(Count) + ' mass clients in ' + IntToStr(elapsed) + 'ms');
  if elapsed > 0 then
    LogClient('Rate: ' + FormatFloat('0.0', Count / (elapsed / 1000)) + ' connections/second');
end;

procedure TForm1.DestroyMassClients;
var
  i: Integer;
begin
  if Length(FMassClients) > 0 then
  begin
    LogClient('Destroying ' + IntToStr(Length(FMassClients)) + ' mass clients...');

    for i := 0 to High(FMassClients) do
    begin
      if FMassClients[i] <> nil then
      begin
        try
          FMassClients[i].Disconnect;
          FMassClients[i].Free;
        except
          // Ignore cleanup errors
        end;
        FMassClients[i] := nil;
      end;
    end;

    SetLength(FMassClients, 0);
    LogClient('Mass clients destroyed');
  end;
end;

// =============================================================================
// HELPER METHODS
// =============================================================================

procedure TForm1.LogServer(const Msg: string);
begin
  if memoServerLog.Lines.Count > 1000 then
    memoServerLog.Clear; // Prevent memory issues with large logs

  memoServerLog.Lines.Add(FormatDateTime('[hh:nn:ss] ', Now) + Msg);
  SendMessage(memoServerLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure TForm1.LogClient(const Msg: string);
begin
  if memoClientLog.Lines.Count > 1000 then
    memoClientLog.Clear; // Prevent memory issues with large logs

  memoClientLog.Lines.Add(FormatDateTime('[hh:nn:ss] ', Now) + Msg);
  SendMessage(memoClientLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure TForm1.UpdateUI;
var
  serverActive: Boolean;
  clientConnected: Boolean;
  uptime: Integer;
  rate: Double;
  clientIDs: TArray<TClientID>;
begin
  serverActive := CrossSocketWebSocketServer1.IsActive;
  clientConnected := CrossSocketWebSocketClient1.IsConnected;

  // Server UI
  btnStartServer.Enabled := not serverActive;
  btnStopServer.Enabled := serverActive;
  edtServerPort.Enabled := not serverActive;

  btnBroadcastCommand.Enabled := serverActive and (CrossSocketWebSocketServer1.GetClientCount > 0);
  btnSendToClient.Enabled := serverActive and (CrossSocketWebSocketServer1.GetClientCount > 0);

  if serverActive then
    lblServerStatus.Caption := 'Status: LISTENING on port ' + IntToStr(CrossSocketWebSocketServer1.Port)
  else
    lblServerStatus.Caption := 'Status: STOPPED';

  if serverActive then
    lblServerStatus.Font.Color := clGreen
  else
    lblServerStatus.Font.Color := clRed;

  lblClientCount.Caption := 'Clients: ' + IntToStr(CrossSocketWebSocketServer1.GetClientCount) +
    ' (+ ' + IntToStr(Length(FMassClients)) + ' mass)';

  // Update Client ID list for sending to specific clients
  if serverActive and (CrossSocketWebSocketServer1.GetClientCount > 0) then
  begin
    clientIDs := CrossSocketWebSocketServer1.GetClientIDs;
    if Length(clientIDs) > 0 then
    begin
      // Only update if edtClientID is empty or has invalid ID
      if (edtClientID.Text = '') or not CrossSocketWebSocketServer1.IsClientConnected(edtClientID.Text) then
        edtClientID.Text := clientIDs[0]; // Show first client ID as example
    end;
  end;

  // Client UI
  btnConnect.Enabled := not clientConnected and not CrossSocketWebSocketClient1.IsConnecting;
  btnDisconnect.Enabled := clientConnected or CrossSocketWebSocketClient1.IsConnecting;
  edtClientURL.Enabled := not clientConnected and not CrossSocketWebSocketClient1.IsConnecting;

  btnSendCommand.Enabled := clientConnected;
  btnPing.Enabled := clientConnected;

  if clientConnected then
    lblClientStatus.Caption := 'Status: CONNECTED to ' + CrossSocketWebSocketClient1.Url
  else if CrossSocketWebSocketClient1.IsConnecting then
    lblClientStatus.Caption := 'Status: CONNECTING...'
  else if CrossSocketWebSocketClient1.IsReconnecting then
    lblClientStatus.Caption := 'Status: RECONNECTING... (attempt ' + IntToStr(CrossSocketWebSocketClient1.GetReconnectAttempts + 1) + ')'
  else
    lblClientStatus.Caption := 'Status: DISCONNECTED';

  if clientConnected then
    lblClientStatus.Font.Color := clGreen
  else if CrossSocketWebSocketClient1.IsConnecting or CrossSocketWebSocketClient1.IsReconnecting then
    lblClientStatus.Font.Color := clBlue
  else
    lblClientStatus.Font.Color := clRed;

  // Performance stats
  uptime := SecondsBetween(Now, FStartTime);
  if uptime > 0 then
    rate := FMessageCount / uptime
  else
    rate := 0;

  lblPerformance.Caption := Format('Messages: %d | Rate: %.1f msg/s | Uptime: %ds',
    [FMessageCount, rate, uptime]);

  // Status bar
  if serverActive then
    StatusBar1.Panels[0].Text := 'Server: Active (' + IntToStr(CrossSocketWebSocketServer1.GetClientCount) + ' clients)'
  else
    StatusBar1.Panels[0].Text := 'Server: Inactive';

  if clientConnected then
    StatusBar1.Panels[1].Text := 'Client: Connected'
  else
    StatusBar1.Panels[1].Text := 'Client: Disconnected';

  StatusBar1.Panels[2].Text := Format('Performance: %.1f msg/s', [rate]);
end;

function TForm1.GetClientIP(ClientConnection: ICrossWebSocketConnection): string;
begin
  try
    // Get client IP from Cross Socket connection
    Result := ClientConnection.PeerAddr;
  except
    Result := 'Unknown';
  end;
end;

end.
