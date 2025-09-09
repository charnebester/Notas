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
    Button1: TButton;
    Button2: TButton;
    procedure btnClearREDClick(Sender: TObject);
    procedure btnVertoonOef1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

procedure TForm1.btnVertoonOef1Click(Sender: TObject);
var
tf : textfile;
sEen : string;
 iPos : integer;
sID, sNaam : string;

begin
 try
   AssignFile(tf , 'DataEen.txt') ;
   Reset(tf);

   while not Eof(tf) do
   begin
     Readln(tf , sEen);
    // redAfvoer.Lines.Add(sEen);
     iPos := Pos('|', sEen);
     sID := Copy(sEen,1,iPos-1);
     Delete(sEen,1,iPos);

     iPos := Pos('~', sEen);
     sNaam := copy(sEen,1,iPos-1);
     Delete(sEen,1,iPos) ;

     redAfvoer.Lines.Add(sNaam +#9 + sID);



   end;

 finally
  CloseFile(tf);
 end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
tf : TextFile;
sEen : string;
iPos, I : integer;
sID : string;
 sNaam, sVan, sGetal, sX : string;


begin
 try
 AssignFile(tf , 'DataTwee.txt')  ;
 Reset(tf);
 sGetal := '';
 while not Eof(tf) do
 begin
 sGetal := ''; //elke keer moet die sterre of X'e vir elke persoon nuut beoooooogoionooo

   Readln(tf , sEen);
  //redAfvoer.lines.Add(sEen);
   // more oef 3 add weer n btnVertoonOef3 is dit reg? oky maar ek dink ek moet iets met somme oef soos gem. Kyk na oef 3, gem is algewmene wisk jy kan dit self uitfigure maar oef 3 het so n bietjie wisk nag lekker slaap trots op jou ! dankie jy ook
  iPos := Pos('|', sEen);
  sID := copy(sEen,1,iPos-1)  ;
  Delete(sEen,1,iPos);

  iPos := Pos('~',sEen)     ;
  sNaam := Copy(sEen,1,iPos-1);
  delete(sEen,1,iPos);

  iPos := pos('#', sEen);
  sVan := copy(sEen,1,iPos-1);
  Delete(sEen,1,iPos-1);

  for I := 0 to Length(sVan) do
  begin
    sGetal := sGetal + 'X';

  end;

  redAfvoer.Lines.Add(UpperCase(sNaam[1]) +'.' + ' ' + sVan + ' ' + sGetal  ) ;

  Delete(sGetal,1,Length(sVan)) ;



 end;



 finally
  CloseFile(tf);
 end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
tf : textfile;
sEen, sID, sNaam, sVan, sLaaste, sEwe : string;
iPos, I : integer;
begin
 try
   AssignFile(tf , 'DataDrie.txt') ;
   Reset(tf);

   while not Eof(tf) do
   begin
     Readln(tf , sEen);
     sLaaste := '';
    //redAfvoer.Lines.Add(sEen);
     iPos := Pos('|', sEen);
     sID := copy(sEen,1,iPos-1);
     Delete(sEen,1,iPos);

      iPos := Pos('~',sEen)     ;
  sNaam := Copy(sEen,1,iPos-1);
  delete(sEen,1,iPos);

  iPos := pos('#', sEen);
  sVan := copy(sEen,1,iPos-1);
  Delete(sEen,1,iPos-1);

  for I := 1 to Length(sVan) do
    begin
     sLaaste := sVan[I] ;
    end;

    if StrToInt(sID)/2 = 0 then
    begin
    sEwe := 'Even ID';
    end
    else
begin
  sEwe := 'Odd ID';
end;

    redAfvoer.Lines.Add(sID[3] + sID[4] + ' ' + sLaaste + ' ' + sEwe )  ;


   end;
 finally
   CloseFile(tf);


   end;


  end;

end.
