unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    redAfvoer: TRichEdit;
    btnVertoonOef1: TButton;
    btnClearRED: TButton;
    procedure btnClearREDClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}



procedure TForm1.btnClearREDClick(Sender: TObject);
begin
redAfvoer.Lines.Clear;
end;

end.
