object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 
    'CrossSocket WebSocket Demo - ASYNC TBYTES ONLY - 100K Connection' +
    's Ready!'
  ClientHeight = 600
  ClientWidth = 900
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 450
    Height = 581
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 8
      Top = 8
      Width = 434
      Height = 200
      Caption = ' WebSocket Server - ASYNC COMMANDS '
      TabOrder = 0
      object Label1: TLabel
        Left = 16
        Top = 24
        Width = 20
        Height = 13
        Caption = 'Port'
      end
      object lblServerStatus: TLabel
        Left = 16
        Top = 80
        Width = 93
        Height = 13
        Caption = 'Status: STOPPED'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblClientCount: TLabel
        Left = 16
        Top = 96
        Width = 45
        Height = 13
        Caption = 'Clients: 0'
      end
      object edtServerPort: TEdit
        Left = 16
        Top = 40
        Width = 65
        Height = 21
        TabOrder = 0
        Text = '8080'
      end
      object btnStartServer: TButton
        Left = 88
        Top = 38
        Width = 75
        Height = 25
        Caption = 'Start Server'
        TabOrder = 1
        OnClick = btnStartServerClick
      end
      object btnStopServer: TButton
        Left = 169
        Top = 38
        Width = 75
        Height = 25
        Caption = 'Stop Server'
        Enabled = False
        TabOrder = 2
        OnClick = btnStopServerClick
      end
      object memoServerLog: TMemo
        Left = 16
        Top = 120
        Width = 402
        Height = 70
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 3
        WordWrap = False
      end
    end
    object GroupBox4: TGroupBox
      Left = 8
      Top = 214
      Width = 434
      Height = 120
      Caption = ' Server Broadcasting - ASYNC ONLY '
      TabOrder = 1
      object Label4: TLabel
        Left = 16
        Top = 24
        Width = 93
        Height = 13
        Caption = 'Broadcast Message'
      end
      object Label5: TLabel
        Left = 16
        Top = 72
        Width = 85
        Height = 13
        Caption = 'Send to Client ID:'
      end
      object edtBroadcast: TEdit
        Left = 16
        Top = 40
        Width = 250
        Height = 21
        TabOrder = 0
        Text = 'Hello to all clients!'
        OnKeyPress = edtBroadcastKeyPress
      end
      object btnBroadcastCommand: TButton
        Left = 272
        Top = 38
        Width = 146
        Height = 25
        Caption = 'Broadcast Command (Async)'
        Enabled = False
        TabOrder = 1
        OnClick = btnBroadcastCommandClick
      end
      object edtClientID: TEdit
        Left = 16
        Top = 88
        Width = 65
        Height = 21
        TabOrder = 2
        Text = '1'
      end
      object btnSendToClient: TButton
        Left = 88
        Top = 86
        Width = 115
        Height = 25
        Caption = 'Send to Client (Async)'
        Enabled = False
        TabOrder = 3
        OnClick = btnSendToClientClick
      end
    end
    object Panel4: TPanel
      Left = 8
      Top = 340
      Width = 434
      Height = 80
      BevelOuter = bvLowered
      TabOrder = 2
      object lblPerformance: TLabel
        Left = 8
        Top = 8
        Width = 171
        Height = 13
        Caption = 'Messages: 0 | Rate: 0.0 msg/s'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object btnClearLogs: TButton
        Left = 8
        Top = 32
        Width = 75
        Height = 25
        Caption = 'Clear Logs'
        TabOrder = 0
        OnClick = btnClearLogsClick
      end
      object btnMassClients: TButton
        Left = 96
        Top = 32
        Width = 100
        Height = 25
        Caption = 'Mass Client Test'
        TabOrder = 1
        OnClick = btnMassClientsClick
      end
    end
  end
  object Panel2: TPanel
    Left = 450
    Top = 0
    Width = 450
    Height = 581
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object GroupBox2: TGroupBox
      Left = 8
      Top = 8
      Width = 434
      Height = 200
      Caption = ' WebSocket Client - ASYNC COMMANDS '
      TabOrder = 0
      object Label2: TLabel
        Left = 16
        Top = 24
        Width = 54
        Height = 13
        Caption = 'Server URL'
      end
      object lblClientStatus: TLabel
        Left = 16
        Top = 80
        Width = 126
        Height = 13
        Caption = 'Status: DISCONNECTED'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object edtClientURL: TEdit
        Left = 16
        Top = 40
        Width = 180
        Height = 21
        TabOrder = 0
        Text = 'ws://127.0.0.1:8080'
      end
      object btnConnect: TButton
        Left = 202
        Top = 38
        Width = 75
        Height = 25
        Caption = 'Connect'
        TabOrder = 1
        OnClick = btnConnectClick
      end
      object btnDisconnect: TButton
        Left = 283
        Top = 38
        Width = 75
        Height = 25
        Caption = 'Disconnect'
        Enabled = False
        TabOrder = 2
        OnClick = btnDisconnectClick
      end
      object memoClientLog: TMemo
        Left = 16
        Top = 120
        Width = 402
        Height = 70
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 3
        WordWrap = False
      end
      object chkAutoReconnect: TCheckBox
        Left = 16
        Top = 96
        Width = 97
        Height = 17
        Caption = 'Auto Reconnect'
        TabOrder = 4
        OnClick = chkAutoReconnectClick
      end
    end
    object Panel3: TPanel
      Left = 8
      Top = 214
      Width = 434
      Height = 120
      BevelOuter = bvLowered
      TabOrder = 1
      object GroupBox3: TGroupBox
        Left = 8
        Top = 8
        Width = 418
        Height = 104
        Caption = ' Client Sending - ASYNC ONLY '
        TabOrder = 0
        object Label3: TLabel
          Left = 16
          Top = 24
          Width = 42
          Height = 13
          Caption = 'Message'
        end
        object edtMessage: TEdit
          Left = 16
          Top = 40
          Width = 250
          Height = 21
          TabOrder = 0
          Text = 'Hello Server!'
          OnKeyPress = edtMessageKeyPress
        end
        object btnSendCommand: TButton
          Left = 272
          Top = 38
          Width = 130
          Height = 25
          Caption = 'Send Command (Async)'
          Enabled = False
          TabOrder = 1
          OnClick = btnSendCommandClick
        end
        object btnPing: TButton
          Left = 16
          Top = 72
          Width = 75
          Height = 25
          Caption = 'Ping'
          Enabled = False
          TabOrder = 2
          OnClick = btnPingClick
        end
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 581
    Width = 900
    Height = 19
    Panels = <
      item
        Text = 'Server: Inactive'
        Width = 200
      end
      item
        Text = 'Client: Disconnected'
        Width = 200
      end
      item
        Text = 'Performance: 0.0 msg/s'
        Width = 50
      end>
  end
  object CrossSocketWebSocketClient1: TCrossSocketWebSocketClient
    Url = 'ws://localhost:8080'
    Left = 720
    Top = 456
  end
  object CrossSocketWebSocketServer1: TCrossSocketWebSocketServer
    BindInterface = '0.0.0.0'
    Left = 192
    Top = 456
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 584
    Top = 376
  end
end
