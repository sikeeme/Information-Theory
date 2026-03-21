program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {LFSR};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TLFSR, LFSR);
  Application.Run;
end.
