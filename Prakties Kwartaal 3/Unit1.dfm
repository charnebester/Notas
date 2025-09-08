object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 763
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
end
