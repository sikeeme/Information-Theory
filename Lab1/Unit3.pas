Unit Unit3;

Interface

Uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  System.IOUtils;

Type
  TPlayfairMatrix = Array [0 .. 4, 0 .. 4] Of Char;

Type
  TForm3 = Class(TForm)
    OrEd: TEdit;
    KeyFirstEd: TEdit;
    KeySecondEd: TEdit;
    KeyThirdEd: TEdit;
    KeyFourthEd: TEdit;
    ResEd: TEdit;
    EncryptBt: TButton;
    OpenBt: TButton;
    SaveBt: TButton;
    ClearBt: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Procedure EncryptBtClick(Sender: TObject);
    Procedure DecipherBtClick(Sender: TObject);
    Procedure OpenBtClick(Sender: TObject);
    Procedure SaveBtClick(Sender: TObject);
    Procedure ClearBtClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  Private
    { Private declarations }
  Public
    IsVigenere: Boolean;
    Encrypt: Boolean;
    Function Playfair(Original, Key1, Key2, Key3, Key4: String): String;
    Procedure Matrix(Key: String; Var Arr: TPlayfairMatrix);
    Function Vigenere(Original, Key: String): String;
  End;

Var
  Form3: TForm3;

Implementation

{$R *.dfm}

Procedure TForm3.EncryptBtClick(Sender: TObject);
Begin
  Encrypt := True;

  If IsVigenere Then
    ResEd.Text := Vigenere(OrEd.Text, KeyFirstEd.Text)
  Else
    ResEd.Text := Playfair(OrEd.Text, KeyFirstEd.Text, KeySecondEd.Text, KeyThirdEd.Text, KeyFourthEd.Text);
End;

procedure TForm3.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  OrEd.Clear;
  KeyFirstEd.Clear;
  KeySecondEd.Clear;
  KeyThirdEd.Clear;
  KeyFourthEd.Clear;
  ResEd.Clear;
end;

Procedure TForm3.DecipherBtClick(Sender: TObject);
Begin
  Encrypt := False;

  If IsVigenere Then
    ResEd.Text := Vigenere(OrEd.Text, KeyFirstEd.Text)
  Else
    ResEd.Text := Playfair(OrEd.Text, KeyFirstEd.Text, KeySecondEd.Text, KeyThirdEd.Text, KeyFourthEd.Text);
End;

Procedure TForm3.Matrix(Key: String; Var Arr: TPlayfairMatrix);
Var
  Temp, Alphabet: String;
  I, J, K: Integer;
Begin
  Temp := '';
  Key := AnsiUpperCase(Key);
  Key := StringReplace(Key, 'J', 'I', [RfReplaceAll]);

  For I := 1 To Length(Key) Do
    If (Key[I] >= 'A') And (Key[I] <= 'Z') Then
      If Pos(Key[I], Temp) = 0 Then
        Temp := Temp + Key[I];

  Alphabet := 'ABCDEFGHIKLMNOPQRSTUVWXYZ';

  For I := 1 To Length(Alphabet) Do
    If Pos(Alphabet[I], Temp) = 0 Then
      Temp := Temp + Alphabet[I];

  K := 1;

  For I := 0 To 4 Do
    For J := 0 To 4 Do
    Begin
      Arr[I, J] := Temp[K];
      Inc(K);
    End;
End;

Function TForm3.Playfair(Original, Key1, Key2, Key3, Key4: String): String;
Var
  First, Second, Third, Fourth: TPlayfairMatrix;
  OnlyLetters: String;
  I, J, K: Integer;
  R1, C1, R2, C2: Integer;
Begin
  Original := StringReplace(AnsiUpperCase(Original), 'J', 'I', [RfReplaceAll]);

  If Encrypt Then
  Begin
    Matrix(Key1, First);
    Matrix(Key2, Second);
    Matrix(Key3, Third);
    Matrix(Key4, Fourth);
  End
  Else
  Begin
    Matrix(Key1, Second);
    Matrix(Key2, First);
    Matrix(Key3, Fourth);
    Matrix(Key4, Third);
  End;

  OnlyLetters := '';

  For I := 1 To Length(Original) Do
    If (Original[I] >= 'A') And (Original[I] <= 'Z') Then
      OnlyLetters := OnlyLetters + Original[I];

  If (Length(OnlyLetters) Mod 2 <> 0) Then
    OnlyLetters := OnlyLetters + 'X';

  K := 1;
  While K < Length(OnlyLetters) Do
  Begin
    R1 := 0;
    C1 := 0;
    R2 := 0;
    C2 := 0;

    For I := 0 To 4 Do
      For J := 0 To 4 Do
        If First[I, J] = OnlyLetters[K] Then
        Begin
          R1 := I;
          C1 := J;
        End;

    For I := 0 To 4 Do
      For J := 0 To 4 Do
        If Fourth[I, J] = OnlyLetters[K + 1] Then
        Begin
          R2 := I;
          C2 := J;
        End;

    Result := Result + Second[R1, C2] + Third[R2, C1];
    K := K + 2;
  End;
End;

Function TForm3.Vigenere(Original, Key: String): String;
Const
  RussianAlphabet = 'ÀÁÂÃÄÅ¨ÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞß';
Var
  I, J, CharOr, CharKey, CharNew, count: Integer;
  temp: string;
Begin
  Original := AnsiUpperCase(Original);
  Key := AnsiUpperCase(Key);
  Result := '';
  count := 0;
  temp := '';

  for i := 1 to Length(Original) do
    if (Pos(Original[I], RussianAlphabet) > 0)then
      temp := temp + Original[I];

  for i := 1 to Length(Key) do
    if pos(key[i], RussianAlphabet) > 0 then
      Inc(count);

  if count = 0 then
    exit(temp);

  J := 1;
  For I := 1 To Length(temp) Do
  Begin
      While Pos(Key[J], RussianAlphabet) = 0 Do
        J := (J Mod Length(Key)) + 1;

      CharOr := Pos(temp[I], RussianAlphabet) - 1;
      CharKey := Pos(Key[J], RussianAlphabet) - 1;

      If Encrypt Then
        CharNew := (CharOr + CharKey) Mod 33 + 1
      Else
        CharNew := (CharOr + 33 - CharKey) Mod 33 + 1;

      Result := Result + RussianAlphabet[CharNew];
      J := (J Mod Length(Key)) + 1;
  End;
End;

Procedure TForm3.OpenBtClick(Sender: TObject);
Begin
  If OpenDialog1.Execute Then
    OrEd.Text := TFile.ReadAllText(OpenDialog1.FileName, TEncoding.UTF8);
End;

Procedure TForm3.SaveBtClick(Sender: TObject);
Begin
  If SaveDialog1.Execute Then
  Begin
    TFile.WriteAllText(SaveDialog1.FileName, ResEd.Text, TEncoding.UTF8);
    ShowMessage('Ñîõðàíåíî!');
  End;
End;

Procedure TForm3.ClearBtClick(Sender: TObject);
Begin
  OrEd.Clear;
  KeyFirstEd.Clear;
  KeySecondEd.Clear;
  KeyThirdEd.Clear;
  KeyFourthEd.Clear;
  ResEd.Clear;
End;

End.
