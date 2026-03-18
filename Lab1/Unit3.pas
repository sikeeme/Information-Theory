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
    KeyFirstEd: TEdit;
    KeySecondEd: TEdit;
    KeyThirdEd: TEdit;
    KeyFourthEd: TEdit;
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
    ResM: TMemo;
    OrM: TMemo;
    Procedure EncryptBtClick(Sender: TObject);
    Procedure DecipherBtClick(Sender: TObject);
    Procedure OpenBtClick(Sender: TObject);
    Procedure SaveBtClick(Sender: TObject);
    Procedure ClearBtClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
  Private
    { Private declarations }
  Public
    IsVigenere, Encrypt, Correct: Boolean;
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
    ResM.Text := Vigenere(OrM.Text, KeyFirstEd.Text)
  Else
    ResM.Text := Playfair(OrM.Text, KeyFirstEd.Text, KeySecondEd.Text, KeyThirdEd.Text, KeyFourthEd.Text);
End;

Procedure TForm3.FormClose(Sender: TObject; Var Action: TCloseAction);
Begin
  OrM.Clear;
  KeyFirstEd.Clear;
  KeySecondEd.Clear;
  KeyThirdEd.Clear;
  KeyFourthEd.Clear;
  ResM.Clear;
End;

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  OrM.Clear;
  ResM.Clear;
End;

Procedure TForm3.DecipherBtClick(Sender: TObject);
Begin
  Encrypt := False;

  If IsVigenere Then
    ResM.Text := Vigenere(OrM.Text, KeyFirstEd.Text)
  Else
    ResM.Text := Playfair(OrM.Text, KeyFirstEd.Text, KeySecondEd.Text, KeyThirdEd.Text, KeyFourthEd.Text);
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

  If Temp = '' Then
    Correct := False;

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
  I, J, Row, Col, K: Integer;
  R1, C1, R2, C2: Integer;
Begin
  Correct := True;
  Result := '';
  Original := StringReplace(AnsiUpperCase(Original), 'J', 'I', [RfReplaceAll]);

  If Correct Then
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
    If (Original[I] In ['A' .. 'Z']) Then
      OnlyLetters := OnlyLetters + Original[I];

  If (OnlyLetters = '') Or (Not Correct) Then
  Begin
    ShowMessage('ќтсутсвуют допустимые символы A-Za-z');
    Exit;
  End;

  If Length(OnlyLetters) Mod 2 <> 0 Then
    OnlyLetters := OnlyLetters + 'X';

  I := 1;
  While I < Length(OnlyLetters) Do
  Begin
    R1 := 0;
    C1 := 0;
    R2 := 0;
    C2 := 0;

    For Row := 0 To 4 Do
      For Col := 0 To 4 Do
        If First[Row, Col] = OnlyLetters[I] Then
        Begin
          R1 := Row;
          C1 := Col;
        End;

    For Row := 0 To 4 Do
      For Col := 0 To 4 Do
        If Fourth[Row, Col] = OnlyLetters[I + 1] Then
        Begin
          R2 := Row;
          C2 := Col;
        End;

    Result := Result + Second[R1, C2] + Third[R2, C1];

    I := I + 2;
  End;
End;

Function TForm3.Vigenere(Original, Key: String): String;
Const
  RussianAlphabet = 'јЅ¬√ƒ≈®∆«»… ЋћЌќѕ–—“”‘’÷„ЎўЏџ№Ёёя';
Var
  I, J, CharOr, CharKey, CharNew: Integer;
  Temp, TempK: String;
Begin
  Original := AnsiUpperCase(Original);
  Key := AnsiUpperCase(Key);
  Result := '';
  Temp := '';
  TempK := '';

  For I := 1 To Length(Original) Do
    If (Pos(Original[I], RussianAlphabet) > 0) Then
      Temp := Temp + Original[I];

  For I := 1 To Length(Key) Do
    If (Pos(Key[I], RussianAlphabet) > 0) Then
      TempK := TempK + Key[I];

  If (Temp = '') Or (TempK = '') Then
  Begin
    ShowMessage('ќтсутсвуют допустимые символы ј-яa-€');
    Exit;
  End;

  J := 1;
  For I := 1 To Length(Temp) Do
  Begin
    CharOr := Pos(Temp[I], RussianAlphabet) - 1;
    CharKey := Pos(TempK[J], RussianAlphabet) - 1;

    If Encrypt Then
      CharNew := (CharOr + CharKey) Mod 33
    Else
      CharNew := (CharOr + 33 - CharKey) Mod 33;

    Result := Result + RussianAlphabet[CharNew + 1];
    J := (J Mod Length(TempK)) + 1;
  End;
End;

Procedure TForm3.OpenBtClick(Sender: TObject);
Begin
  If OpenDialog1.Execute Then
    OrM.Text := TFile.ReadAllText(OpenDialog1.FileName, TEncoding.UTF8);
End;

Procedure TForm3.SaveBtClick(Sender: TObject);
Begin
  If SaveDialog1.Execute Then
  Begin
    TFile.WriteAllText(SaveDialog1.FileName, ResM.Text, TEncoding.UTF8);
    ShowMessage('—охранено!');
  End;
End;

Procedure TForm3.ClearBtClick(Sender: TObject);
Begin
  OrM.Clear;
  KeyFirstEd.Clear;
  KeySecondEd.Clear;
  KeyThirdEd.Clear;
  KeyFourthEd.Clear;
  ResM.Clear;
End;

End.
