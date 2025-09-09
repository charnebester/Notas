object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 642
  ClientWidth = 1057
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object redAfvoer: TRichEdit
    Left = 24
    Top = 137
    Width = 537
    Height = 264
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object btnVertoonOef1: TButton
    Left = 640
    Top = 136
    Width = 75
    Height = 25
    Caption = 'VertoonOef1'
    TabOrder = 1
    OnClick = btnVertoonOef1Click
  end
  object btnClearRED: TButton
    Left = 648
    Top = 376
    Width = 75
    Height = 25
    Caption = 'Clear Red'
    TabOrder = 2
    OnClick = btnClearREDClick
  end
  object Button1: TButton
    Left = 712
    Top = 240
    Width = 75
    Height = 25
    Caption = 'VertoonOef2'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 656
    Top = 304
    Width = 75
    Height = 25
    Caption = 'VertoonOef3'
    TabOrder = 4
    OnClick = Button2Click
  end
end
