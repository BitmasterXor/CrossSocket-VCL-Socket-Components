unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, CrossSocket.Client,
  CrossSocket.Server, Vcl.Samples.Spin;

type
  TForm1 = class(TForm)
    CrossSocketServer1: TCrossSocketServer;
    CrossSocketClient1: TCrossSocketClient;
    PanelMain: TPanel;
    PanelServer: TPanel;
    PanelClient: TPanel;
    PanelLog: TPanel;
    GroupBoxServer: TGroupBox;
    GroupBoxClient: TGroupBox;
    GroupBoxLog: TGroupBox;
    ButtonStartServer: TButton;
    ButtonStopServer: TButton;
    ButtonSendToClient: TButton;
    ButtonBroadcast: TButton;
    ButtonConnect: TButton;
    ButtonDisconnect: TButton;
    ButtonSendToServer: TButton;
    ButtonClearLog: TButton;
    EditClientIP: TEdit;
    LabelServerPort: TLabel;
    LabelClientIP: TLabel;
    LabelClientPort: TLabel;
    LabelServerStatus: TLabel;
    LabelClientStatus: TLabel;
    MemoLog: TMemo;
    StatusBar1: TStatusBar;
    PanelServerControls: TPanel;
    PanelClientControls: TPanel;
    SpinEditServerPort: TSpinEdit;
    SpinEditClientPort: TSpinEdit;
    LabelConnectedClients: TLabel;
    procedure ButtonStartServerClick(Sender: TObject);
    procedure ButtonStopServerClick(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonDisconnectClick(Sender: TObject);
    procedure ButtonSendToClientClick(Sender: TObject);
    procedure ButtonSendToServerClick(Sender: TObject);
    procedure ButtonBroadcastClick(Sender: TObject);
    procedure ButtonClearLogClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CrossSocketServer1ServerStateChange(Sender: TObject;
      OldState, NewState: TCrossSocketServerState;
      const StateDescription: string);
    procedure CrossSocketServer1ClientConnected(Sender: TObject;
      ClientID: Integer);
    procedure CrossSocketServer1ClientDisconnected(Sender: TObject;
      ClientID: Integer);
    procedure CrossSocketServer1DataReceived(Sender: TObject; ClientID: Integer;
      const Data: TBytes);
    procedure CrossSocketServer1DataSent(Sender: TObject; ClientID: Integer;
      const Data: TBytes);
    procedure CrossSocketServer1HandleCommand(Sender: TObject;
      ClientID: Integer; const Command: TBytes);
    procedure CrossSocketServer1Error(Sender: TObject; const ErrorMsg: string);
    procedure CrossSocketClient1Connect(Sender: TObject);
    procedure CrossSocketClient1Disconnect(Sender: TObject);
    procedure CrossSocketClient1StateChange(Sender: TObject; OldState,
      NewState: TCrossSocketConnectionState; const StateDescription: string);
    procedure CrossSocketClient1DataReceived(Sender: TObject;
      const Data: TBytes);
    procedure CrossSocketClient1DataSent(Sender: TObject; const Data: TBytes);
    procedure CrossSocketClient1HandleCommand(Sender: TObject;
      const Command: TBytes);
    procedure CrossSocketClient1Error(Sender: TObject; const ErrorMsg: string);
    procedure CrossSocketClient1ReconnectFailed(Sender: TObject;
      AttemptNumber: Integer; const ErrorMsg: string);
    procedure CrossSocketClient1Reconnecting(Sender: TObject;
      AttemptNumber: Integer);
    procedure FormDestroy(Sender: TObject);
  private
    FConnectedClients: TStringList;
    procedure LogMessage(const Msg: string);
    procedure UpdateServerStatus(const Status: string);
    procedure UpdateClientStatus(const Status: string);
    procedure UpdateConnectedClientsCount;
    procedure UpdateButtonStates;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FConnectedClients := TStringList.Create;

  // Set default values
  SpinEditServerPort.Value := 3434;
  EditClientIP.Text := '127.0.0.1';
  SpinEditClientPort.Value := 3434;

  UpdateServerStatus('Stopped');
  UpdateClientStatus('Disconnected');
  UpdateConnectedClientsCount;
  UpdateButtonStates;

  LogMessage('Application started - Ready for connections');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 // Stop server and disconnect client before freeing resources
  if CrossSocketServer1.Active then
    CrossSocketServer1.Stop;

  if CrossSocketClient1.Connected then
    CrossSocketClient1.Disconnect;

  // Free the string list
  FConnectedClients.Free;
end;

procedure TForm1.LogMessage(const Msg: string);
begin
  MemoLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Msg);
  MemoLog.Perform(EM_SCROLLCARET, 0, 0);
end;

procedure TForm1.UpdateServerStatus(const Status: string);
begin
  LabelServerStatus.Caption := 'Status: ' + Status;
  StatusBar1.Panels[0].Text := 'Server: ' + Status;
end;

procedure TForm1.UpdateClientStatus(const Status: string);
begin
  LabelClientStatus.Caption := 'Status: ' + Status;
  StatusBar1.Panels[1].Text := 'Client: ' + Status;
end;

procedure TForm1.UpdateConnectedClientsCount;
begin
  LabelConnectedClients.Caption := 'Connected Clients: ' + IntToStr(FConnectedClients.Count);
end;

procedure TForm1.UpdateButtonStates;
var
  ServerRunning: Boolean;
  ClientConnected: Boolean;
begin
  ServerRunning := CrossSocketServer1.Active;
  ClientConnected := CrossSocketClient1.Connected;

  // Server buttons
  ButtonStartServer.Enabled := not ServerRunning;
  ButtonStopServer.Enabled := ServerRunning;
  ButtonSendToClient.Enabled := ServerRunning and (FConnectedClients.Count > 0);
  ButtonBroadcast.Enabled := ServerRunning and (FConnectedClients.Count > 0);
  SpinEditServerPort.Enabled := not ServerRunning;

  // Client buttons
  ButtonConnect.Enabled := not ClientConnected;
  ButtonDisconnect.Enabled := ClientConnected;
  ButtonSendToServer.Enabled := ClientConnected;
  EditClientIP.Enabled := not ClientConnected;
  SpinEditClientPort.Enabled := not ClientConnected;
end;

procedure TForm1.ButtonStartServerClick(Sender: TObject);
begin
  try
    CrossSocketServer1.Port := SpinEditServerPort.Value;
    CrossSocketServer1.Start;
    LogMessage('Starting server on port ' + IntToStr(SpinEditServerPort.Value));
  except
    on E: Exception do
    begin
      LogMessage('Error starting server: ' + E.Message);
      ShowMessage('Error starting server: ' + E.Message);
    end;
  end;
end;

procedure TForm1.ButtonStopServerClick(Sender: TObject);
begin
  try
    CrossSocketServer1.Stop;
    LogMessage('Server stopped');
  except
    on E: Exception do
    begin
      LogMessage('Error stopping server: ' + E.Message);
    end;
  end;
end;

procedure TForm1.ButtonConnectClick(Sender: TObject);
begin
  try
    CrossSocketClient1.Host := EditClientIP.Text;
    CrossSocketClient1.Port := SpinEditClientPort.Value;
    CrossSocketClient1.Connect;
    LogMessage('Connecting to ' + EditClientIP.Text + ':' + IntToStr(SpinEditClientPort.Value));
  except
    on E: Exception do
    begin
      LogMessage('Error connecting: ' + E.Message);
      ShowMessage('Error connecting: ' + E.Message);
    end;
  end;
end;

procedure TForm1.ButtonDisconnectClick(Sender: TObject);
begin
  try
    CrossSocketClient1.Disconnect;
    LogMessage('Disconnecting from server');
  except
    on E: Exception do
    begin
      LogMessage('Error disconnecting: ' + E.Message);
    end;
  end;
end;

procedure TForm1.ButtonSendToClientClick(Sender: TObject);
var
  Message: string;
  ClientID: Integer;
begin
  if FConnectedClients.Count > 0 then
  begin
    // Send to first connected client (you can modify this to select specific client)
    ClientID := StrToInt(FConnectedClients[0]);
    Message := 'Hello Client! Server message at ' + FormatDateTime('hh:nn:ss', Now);
    CrossSocketServer1.SendCommandToClient(ClientID, BytesOf(Message));
    LogMessage('Sent message to Client ID ' + IntToStr(ClientID));
  end;
end;

procedure TForm1.ButtonSendToServerClick(Sender: TObject);
var
  Message: string;
begin
  Message := 'Hello Server! Client message at ' + FormatDateTime('hh:nn:ss', Now);
  CrossSocketClient1.SendCommand(BytesOf(Message));
  LogMessage('Sent message to server');
end;

procedure TForm1.ButtonBroadcastClick(Sender: TObject);
var
  Message: string;
begin
  Message := 'Broadcast message to all clients at ' + FormatDateTime('hh:nn:ss', Now);
  CrossSocketServer1.BroadcastCommand(BytesOf(Message));
  LogMessage('Broadcast message sent to all clients');
end;

procedure TForm1.ButtonClearLogClick(Sender: TObject);
begin
  MemoLog.Clear;
  LogMessage('Log cleared');
end;

// Server Events
procedure TForm1.CrossSocketServer1ServerStateChange(Sender: TObject;
  OldState, NewState: TCrossSocketServerState; const StateDescription: string);
begin
  case NewState of
    ssListening:
    begin
      UpdateServerStatus('Listening on port ' + IntToStr(CrossSocketServer1.Port));
      LogMessage('Server is now listening for connections');
    end;
    ssStopping:
    begin
      UpdateServerStatus('Stopped');
      LogMessage('Server stopped');
      FConnectedClients.Clear;
      UpdateConnectedClientsCount;
    end;
  end;
  UpdateButtonStates;
end;

procedure TForm1.CrossSocketServer1ClientConnected(Sender: TObject;
  ClientID: Integer);
begin
  FConnectedClients.Add(IntToStr(ClientID));
  UpdateConnectedClientsCount;
  LogMessage('Client connected - ID: ' + IntToStr(ClientID) +
    ', IP: ' + CrossSocketServer1.GetClientIP(ClientID));
  UpdateButtonStates;
end;

procedure TForm1.CrossSocketServer1ClientDisconnected(Sender: TObject;
  ClientID: Integer);
begin
  FConnectedClients.Delete(FConnectedClients.IndexOf(IntToStr(ClientID)));
  UpdateConnectedClientsCount;
  LogMessage('Client disconnected - ID: ' + IntToStr(ClientID));
  UpdateButtonStates;
end;

procedure TForm1.CrossSocketServer1DataReceived(Sender: TObject;
  ClientID: Integer; const Data: TBytes);
begin
  LogMessage('Server received data from Client ' + IntToStr(ClientID) +
    ' - Size: ' + IntToStr(Length(Data)) + ' bytes');
end;

procedure TForm1.CrossSocketServer1DataSent(Sender: TObject; ClientID: Integer;
  const Data: TBytes);
begin
  LogMessage('Server sent data to Client ' + IntToStr(ClientID) +
    ' - Size: ' + IntToStr(Length(Data)) + ' bytes');
end;

procedure TForm1.CrossSocketServer1HandleCommand(Sender: TObject;
  ClientID: Integer; const Command: TBytes);
begin
  LogMessage('Server received command from Client ' + IntToStr(ClientID) +
    ': ' + StringOf(Command));
end;

procedure TForm1.CrossSocketServer1Error(Sender: TObject; const ErrorMsg: string);
begin
  LogMessage('Server Error: ' + ErrorMsg);
end;

// Client Events
procedure TForm1.CrossSocketClient1Connect(Sender: TObject);
begin
  UpdateClientStatus('Connected to ' + CrossSocketClient1.GetServerIP);
  LogMessage('Client connected to server at ' + CrossSocketClient1.GetServerIP);
  UpdateButtonStates;
end;

procedure TForm1.CrossSocketClient1Disconnect(Sender: TObject);
begin
  UpdateClientStatus('Disconnected');
  LogMessage('Client disconnected from server');
  UpdateButtonStates;
end;

procedure TForm1.CrossSocketClient1StateChange(Sender: TObject; OldState,
  NewState: TCrossSocketConnectionState; const StateDescription: string);
begin
  case NewState of
    csConnecting:
    begin
      UpdateClientStatus('Connecting...');
      LogMessage('Client connecting to server...');
    end;
    csConnected:
    begin
      UpdateClientStatus('Connected');
    end;
    csDisconnected:
    begin
      UpdateClientStatus('Disconnected');
    end;
  end;
  UpdateButtonStates;
end;

procedure TForm1.CrossSocketClient1DataReceived(Sender: TObject;
  const Data: TBytes);
begin
  LogMessage('Client received data - Size: ' + IntToStr(Length(Data)) + ' bytes');
end;

procedure TForm1.CrossSocketClient1DataSent(Sender: TObject; const Data: TBytes);
begin
  LogMessage('Client sent data - Size: ' + IntToStr(Length(Data)) + ' bytes');
end;

procedure TForm1.CrossSocketClient1HandleCommand(Sender: TObject;
  const Command: TBytes);
begin
  LogMessage('Client received command: ' + StringOf(Command));
end;

procedure TForm1.CrossSocketClient1Error(Sender: TObject; const ErrorMsg: string);
begin
  LogMessage('Client Error: ' + ErrorMsg);
end;

procedure TForm1.CrossSocketClient1ReconnectFailed(Sender: TObject;
  AttemptNumber: Integer; const ErrorMsg: string);
begin
  LogMessage('Client reconnection failed - Attempt #' + IntToStr(AttemptNumber) +
    ': ' + ErrorMsg);
end;

procedure TForm1.CrossSocketClient1Reconnecting(Sender: TObject;
  AttemptNumber: Integer);
begin
  LogMessage('Client reconnecting - Attempt #' + IntToStr(AttemptNumber));
end;

end.
