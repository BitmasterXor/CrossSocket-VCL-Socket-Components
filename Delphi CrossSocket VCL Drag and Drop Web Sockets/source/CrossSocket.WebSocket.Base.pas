unit CrossSocket.WebSocket.Base;

interface

uses
  Classes,
  SysUtils,
  System.Generics.Collections,
  SyncObjs,
  TypInfo,
  // Actual Cross Socket WebSocket units
  Net.CrossWebSocketClient,
  Net.CrossWebSocketServer,
  Net.CrossWebSocketParser,
  Net.CrossSocket.Base;

type
  // Client ID type for easy identification - MATCH THE SERVER!
  TClientID = string;

  /// **FIXED: HUMAN-FRIENDLY WEBSOCKET STATE CONSTANTS THAT ACTUALLY WORK!**
  TCrossWebSocketStatus = (
    wsUnknown,        // 0 - Unknown/Initial state
    wsConnecting,     // 1 - Currently connecting
    wsConnected,      // 2 - Successfully connected
    wsDisconnected,   // 3 - Disconnected (clean)
    wsShutdown        // 4 - Shutdown/Closed
  );

  /// WebSocket message types
  TCrossWebSocketMessageType = (
    wmtUnknown,       // 0 - Unknown message type
    wmtText,          // 1 - Text message (UTF-8)
    wmtBinary         // 2 - Binary message
  );

  /// Cross Socket WebSocket event types - FIXED TO MATCH SERVER!
  TCrossWebSocketConnectEvent = procedure(Sender: TObject) of object;
  TCrossWebSocketDataReceivedEvent = procedure(Sender: TObject; const MessageType: TCrossWebSocketMessageType; const Data: TBytes) of object;  // FOR STATISTICS/LOGGING ONLY!
  TCrossWebSocketDataSentEvent = procedure(Sender: TObject; const Data: TBytes) of object;      // FOR STATISTICS/LOGGING ONLY!
  TCrossWebSocketDisconnectEvent = procedure(Sender: TObject) of object;
  TCrossWebSocketErrorEvent = procedure(Sender: TObject; const ErrorMsg: string) of object;
  TCrossWebSocketHandleMessageEvent = procedure(Sender: TObject; const MessageType: TCrossWebSocketMessageType; const Data: TBytes) of object;  // THIS IS WHERE YOU PROCESS DATA!

  /// Server-side WebSocket events - FIXED TO MATCH SERVER IMPLEMENTATION!
  TCrossWebSocketClientConnectedEvent = procedure(Sender: TObject; const ClientID: TClientID; ClientConnection: ICrossWebSocketConnection) of object;
  TCrossWebSocketClientDisconnectedEvent = procedure(Sender: TObject; const ClientID: TClientID; ClientConnection: ICrossWebSocketConnection) of object;
  TCrossWebSocketServerDataReceivedEvent = procedure(Sender: TObject; ClientConnection: ICrossWebSocketConnection; const MessageType: TCrossWebSocketMessageType; const Data: TBytes) of object;  // FOR STATISTICS/LOGGING ONLY!
  TCrossWebSocketServerDataSentEvent = procedure(Sender: TObject; ClientConnection: ICrossWebSocketConnection; const Data: TBytes) of object;      // FOR STATISTICS/LOGGING ONLY!
  TCrossWebSocketServerHandleMessageEvent = procedure(Sender: TObject; const ClientID: TClientID; ClientConnection: ICrossWebSocketConnection; const MessageType: TCrossWebSocketMessageType; const Data: TBytes) of object;  // FIXED TO MATCH SERVER!

  /// State change events
  TCrossWebSocketStateChangeEvent = procedure(Sender: TObject; OldState, NewState: TCrossWebSocketStatus; const StateDescription: string) of object;

  /// Ping/Pong events
  TCrossWebSocketPingEvent = procedure(Sender: TObject) of object;
  TCrossWebSocketPongEvent = procedure(Sender: TObject) of object;
  TCrossWebSocketServerPingEvent = procedure(Sender: TObject; ClientConnection: ICrossWebSocketConnection) of object;
  TCrossWebSocketServerPongEvent = procedure(Sender: TObject; ClientConnection: ICrossWebSocketConnection) of object;

  /// Re-export Cross Socket WebSocket interfaces for IDE compatibility
  ICrossWebSocket = Net.CrossWebSocketClient.ICrossWebSocket;
  ICrossWebSocketMgr = Net.CrossWebSocketClient.ICrossWebSocketMgr;
  ICrossWebSocketServer = Net.CrossWebSocketServer.ICrossWebSocketServer;
  ICrossWebSocketConnection = Net.CrossWebSocketServer.ICrossWebSocketConnection;

  /// Re-export WebSocket types from Cross Socket
  TWsStatus = Net.CrossWebSocketClient.TWsStatus;
  TWsMessageType = Net.CrossWebSocketParser.TWsMessageType;

  /// Client connection info for tracking
  TWebSocketClientConnectionInfo = record
    ClientID: TClientID;  // ADDED CLIENT ID!
    Connection: ICrossWebSocketConnection;
    IP: string;
    ConnectedAt: TDateTime;
    LastActivity: TDateTime;
    TotalBytesSent: Int64;
    TotalBytesReceived: Int64;
    TotalMessagesSent: Int64;
    TotalMessagesReceived: Int64;
  end;

  /// Base helper functions - following mORMot2 patterns
function CrossWebSocketStatusToString(Status: TCrossWebSocketStatus): string;
function CrossWebSocketMessageTypeToString(MessageType: TCrossWebSocketMessageType): string;
function WsStatusToCrossStatus(WsStatus: TWsStatus): TCrossWebSocketStatus;
function WsMessageTypeToCrossMessageType(WsType: TWsMessageType): TCrossWebSocketMessageType;
function CrossMessageTypeToWsMessageType(CrossType: TCrossWebSocketMessageType): TWsMessageType;

implementation

function CrossWebSocketStatusToString(Status: TCrossWebSocketStatus): string;
begin
  case Status of
    wsUnknown: Result := 'Unknown';
    wsConnecting: Result := 'Connecting';
    wsConnected: Result := 'Connected';
    wsDisconnected: Result := 'Disconnected';
    wsShutdown: Result := 'Shutdown';
  else
    Result := 'Unknown';
  end;
end;

function CrossWebSocketMessageTypeToString(MessageType: TCrossWebSocketMessageType): string;
begin
  case MessageType of
    wmtUnknown: Result := 'Unknown';
    wmtText: Result := 'Text';
    wmtBinary: Result := 'Binary';
  else
    Result := 'Unknown';
  end;
end;

function WsStatusToCrossStatus(WsStatus: TWsStatus): TCrossWebSocketStatus;
begin
  case WsStatus of
    Net.CrossWebSocketClient.wsUnknown: Result := wsUnknown;
    Net.CrossWebSocketClient.wsConnecting: Result := wsConnecting;
    Net.CrossWebSocketClient.wsConnected: Result := wsConnected;
    Net.CrossWebSocketClient.wsDisconnected: Result := wsDisconnected;
    Net.CrossWebSocketClient.wsShutdown: Result := wsShutdown;
  else
    Result := wsUnknown;
  end;
end;

function WsMessageTypeToCrossMessageType(WsType: TWsMessageType): TCrossWebSocketMessageType;
begin
  case WsType of
    wtUnknown: Result := wmtUnknown;
    wtText: Result := wmtText;
    wtBinary: Result := wmtBinary;
  else
    Result := wmtUnknown;
  end;
end;

function CrossMessageTypeToWsMessageType(CrossType: TCrossWebSocketMessageType): TWsMessageType;
begin
  case CrossType of
    wmtUnknown: Result := wtUnknown;
    wmtText: Result := wtText;
    wmtBinary: Result := wtBinary;
  else
    Result := wtUnknown;
  end;
end;

end.
