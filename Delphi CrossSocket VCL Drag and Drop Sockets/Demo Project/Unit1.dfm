object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Professional CrossSocket Client/Server'
  ClientHeight = 600
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 581
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object PanelServer: TPanel
      Left = 0
      Top = 0
      Width = 400
      Height = 280
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      TabOrder = 0
      object GroupBoxServer: TGroupBox
        Left = 8
        Top = 8
        Width = 384
        Height = 264
        Caption = ' Server Configuration '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object LabelServerPort: TLabel
          Left = 16
          Top = 24
          Width = 59
          Height = 13
          Caption = 'Server Port:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object LabelServerStatus: TLabel
          Left = 16
          Top = 56
          Width = 78
          Height = 13
          Caption = 'Status: Stopped'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object LabelConnectedClients: TLabel
          Left = 16
          Top = 80
          Width = 100
          Height = 13
          Caption = 'Connected Clients: 0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object SpinEditServerPort: TSpinEdit
          Left = 80
          Top = 21
          Width = 89
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxValue = 65535
          MinValue = 1024
          ParentFont = False
          TabOrder = 0
          Value = 3434
        end
        object PanelServerControls: TPanel
          Left = 16
          Top = 104
          Width = 352
          Height = 152
          BevelOuter = bvNone
          TabOrder = 1
          object ButtonStartServer: TButton
            Left = 8
            Top = 8
            Width = 100
            Height = 32
            Caption = 'Start Server'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnClick = ButtonStartServerClick
          end
          object ButtonStopServer: TButton
            Left = 120
            Top = 8
            Width = 100
            Height = 32
            Caption = 'Stop Server'
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            OnClick = ButtonStopServerClick
          end
          object ButtonSendToClient: TButton
            Left = 8
            Top = 48
            Width = 100
            Height = 32
            Caption = 'Send to Client'
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnClick = ButtonSendToClientClick
          end
          object ButtonBroadcast: TButton
            Left = 120
            Top = 48
            Width = 100
            Height = 32
            Caption = 'Broadcast'
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 3
            OnClick = ButtonBroadcastClick
          end
        end
      end
    end
    object PanelClient: TPanel
      Left = 400
      Top = 0
      Width = 400
      Height = 280
      Align = alCustom
      Anchors = [akTop, akRight]
      BevelOuter = bvNone
      TabOrder = 1
      object GroupBoxClient: TGroupBox
        Left = 8
        Top = 8
        Width = 384
        Height = 264
        Caption = ' Client Configuration '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object LabelClientIP: TLabel
          Left = 16
          Top = 24
          Width = 49
          Height = 13
          Caption = 'Server IP:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object LabelClientPort: TLabel
          Left = 16
          Top = 56
          Width = 59
          Height = 13
          Caption = 'Server Port:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object LabelClientStatus: TLabel
          Left = 16
          Top = 88
          Width = 102
          Height = 13
          Caption = 'Status: Disconnected'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object EditClientIP: TEdit
          Left = 80
          Top = 21
          Width = 121
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          Text = '127.0.0.1'
        end
        object SpinEditClientPort: TSpinEdit
          Left = 80
          Top = 53
          Width = 89
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxValue = 65535
          MinValue = 1024
          ParentFont = False
          TabOrder = 1
          Value = 3434
        end
        object PanelClientControls: TPanel
          Left = 16
          Top = 112
          Width = 352
          Height = 144
          BevelOuter = bvNone
          TabOrder = 2
          object ButtonConnect: TButton
            Left = 8
            Top = 8
            Width = 100
            Height = 32
            Caption = 'Connect'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnClick = ButtonConnectClick
          end
          object ButtonDisconnect: TButton
            Left = 120
            Top = 8
            Width = 100
            Height = 32
            Caption = 'Disconnect'
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            OnClick = ButtonDisconnectClick
          end
          object ButtonSendToServer: TButton
            Left = 8
            Top = 48
            Width = 212
            Height = 32
            Caption = 'Send to Server'
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnClick = ButtonSendToServerClick
          end
        end
      end
    end
    object PanelLog: TPanel
      Left = 0
      Top = 288
      Width = 800
      Height = 293
      Align = alCustom
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 2
      object GroupBoxLog: TGroupBox
        Left = 8
        Top = 0
        Width = 784
        Height = 285
        Caption = ' Connection Log '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object MemoLog: TMemo
          Left = 8
          Top = 48
          Width = 768
          Height = 221
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object ButtonClearLog: TButton
          Left = 8
          Top = 16
          Width = 100
          Height = 25
          Caption = 'Clear Log'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = ButtonClearLogClick
        end
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 581
    Width = 800
    Height = 19
    Panels = <
      item
        Text = 'Server: Stopped'
        Width = 200
      end
      item
        Text = 'Client: Disconnected'
        Width = 200
      end
      item
        Width = 50
      end>
  end
  object CrossSocketServer1: TCrossSocketServer
    Name = 'CrossServer1'
    BindInterface = '0.0.0.0'
    Description = 'Cross Socket Server Component - BASIC VERSION'
    Version = '1.0.0'
    OnClientConnected = CrossSocketServer1ClientConnected
    OnClientDisconnected = CrossSocketServer1ClientDisconnected
    OnError = CrossSocketServer1Error
    OnDataReceived = CrossSocketServer1DataReceived
    OnDataSent = CrossSocketServer1DataSent
    OnHandleCommand = CrossSocketServer1HandleCommand
    OnServerStateChange = CrossSocketServer1ServerStateChange
    Left = 288
    Top = 40
  end
  object CrossSocketClient1: TCrossSocketClient
    Name = 'CrossClient1'
    Host = 'localhost'
    URI = '/'
    Description = 'Cross Socket Client Component - BASIC VERSION'
    Version = '1.0.0'
    OnConnect = CrossSocketClient1Connect
    OnDataReceived = CrossSocketClient1DataReceived
    OnDataSent = CrossSocketClient1DataSent
    OnDisconnect = CrossSocketClient1Disconnect
    OnError = CrossSocketClient1Error
    OnHandleCommand = CrossSocketClient1HandleCommand
    OnStateChange = CrossSocketClient1StateChange
    OnReconnecting = CrossSocketClient1Reconnecting
    OnReconnectFailed = CrossSocketClient1ReconnectFailed
    Left = 688
    Top = 40
  end
end
