unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Unit3; // Unit2 удален, если он не используется

type
  TForm1 = class(TForm)
    Button1: TButton; // Кнопка для Плейфейра
    Button2: TButton; // Кнопка для Виженера
    Label1: TLabel;
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  Form3.IsVigenere := False;
  Form3.KeySecondEd.Visible := True;
  Form3.KeyThirdEd.Visible := True;
  Form3.KeyFourthEd.Visible := True;
  Form3.Caption := 'Шифр Плейфейра';
  Form3.Label1.Caption := 'Шифр Плейфейра с 4мя матрицами (EN)';
  Form3.Show;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Form3.IsVigenere := True;
  Form3.KeySecondEd.Visible := False;
  Form3.KeyThirdEd.Visible := False;
  Form3.KeyFourthEd.Visible := False;
  Form3.KeyFirstEd.TextHint := '';
  Form3.Caption := 'Шифр Виженера';
  Form3.Label1.Caption := 'Шифр Виженера, прямой ключ (RU)';
  Form3.Show;
end;

end.
