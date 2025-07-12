unit CrossSocket.WebSocket.Register;

interface

procedure Register;

implementation

uses
  Classes,
  DesignIntf,
  DesignEditors,
  // Our WebSocket components
  CrossSocket.WebSocket.Client,
  CrossSocket.WebSocket.Server;

{$R CrossSocketWebSocketComponents.res}  // Resource file containing icons

/// Custom property editor for WebSocket URL property
type
  TCrossWebSocketUrlPropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

/// Custom property editor for BindInterface property (same as regular sockets)
type
  TCrossWebSocketBindInterfacePropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TCrossWebSocketUrlPropertyEditor }

function TCrossWebSocketUrlPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

procedure TCrossWebSocketUrlPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  // WebSocket URL examples
  Proc('ws://localhost:8080');
  Proc('ws://127.0.0.1:8080');
  Proc('ws://localhost:3000');
  Proc('wss://localhost:8443');  // Secure WebSocket
  Proc('ws://echo.websocket.org'); // Public test server
end;

function TCrossWebSocketUrlPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TCrossWebSocketUrlPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

{ TCrossWebSocketBindInterfacePropertyEditor }

function TCrossWebSocketBindInterfacePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

procedure TCrossWebSocketBindInterfacePropertyEditor.GetValues(Proc: TGetStrProc);
begin
  Proc('0.0.0.0');     // All interfaces
  Proc('127.0.0.1');   // Localhost only
  Proc('localhost');   // Localhost name
end;

function TCrossWebSocketBindInterfacePropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TCrossWebSocketBindInterfacePropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

procedure Register;
begin
  // Register WebSocket components on the component palette
  RegisterComponents('Cross Socket', [
    TCrossSocketWebSocketClient,
    TCrossSocketWebSocketServer
  ]);

  // Icons are loaded automatically from resource file

  // Register property editors for enhanced design-time experience
  RegisterPropertyEditor(TypeInfo(string), TCrossSocketWebSocketClient, 'Url', TCrossWebSocketUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TCrossSocketWebSocketServer, 'BindInterface', TCrossWebSocketBindInterfacePropertyEditor);
end;

end.
