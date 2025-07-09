unit CrossSocket.Register;

interface

procedure Register;

implementation

uses
  Classes,
  DesignIntf,
  DesignEditors,
  CrossSocket.Client,
  CrossSocket.Server;

/// Custom property editor for Host property
type
  TCrossSocketHostPropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

/// Custom property editor for BindInterface property
type
  TCrossSocketBindInterfacePropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TCrossSocketHostPropertyEditor }

function TCrossSocketHostPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

procedure TCrossSocketHostPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  Proc('localhost');
  Proc('127.0.0.1');
  Proc('0.0.0.0');
end;

function TCrossSocketHostPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TCrossSocketHostPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

{ TCrossSocketBindInterfacePropertyEditor }

function TCrossSocketBindInterfacePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

procedure TCrossSocketBindInterfacePropertyEditor.GetValues(Proc: TGetStrProc);
begin
  Proc('0.0.0.0');     // All interfaces
  Proc('127.0.0.1');   // Localhost only
  Proc('localhost');   // Localhost name
end;

function TCrossSocketBindInterfacePropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TCrossSocketBindInterfacePropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

procedure Register;
begin
  // Register components on the component palette
  RegisterComponents('Cross Socket', [
    TCrossSocketClient,
    TCrossSocketServer
  ]);

  // Register property editors for enhanced design-time experience
  RegisterPropertyEditor(TypeInfo(string), TCrossSocketClient, 'Host', TCrossSocketHostPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TCrossSocketServer, 'BindInterface', TCrossSocketBindInterfacePropertyEditor);
end;

end.