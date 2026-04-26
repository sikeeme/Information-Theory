Unit Unit1;

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
  Vcl.Menus,
  System.IOUtils;

Type
  TForm1 = Class(TForm)
    PEd: TEdit;
    Label1: TLabel;
    QEd: TEdit;
    Label2: TLabel;
    BEd: TEdit;
    Label3: TLabel;
    OrigM: TMemo;
    ResultM: TMemo;
    EncryptBt: TButton;
    DecryptBt: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Label4: TLabel;
    Label5: TLabel;

    Procedure Now;
    Procedure MainFormOnCreate(Sender: TObject);
    Procedure FirstEditOnChange(Sender: TObject);
    Procedure SecondEditOnChange(Sender: TObject);
    Procedure CheckButtonState();
    Procedure ThirdEditOnChange(Sender: TObject);
    Procedure EncryptBtClick(Sender: TObject);
    Procedure DecryptBtClick(Sender: TObject);
    Function DecryptBlock(C_val: Cardinal; P, Q, B: Int64): Byte;
    Procedure FormCreate(Sender: TObject);
  Private
    { Private declarations }
    FilePath: String;
    SaveFilePath: String;
  Public
    { Public declarations }
  End;

Var
  Form1: TForm1;

Implementation

{$R *.dfm}

Type
  ResultArray = Array Of Integer;

Function CheckModFour(Number: Integer): Boolean;
Begin
  Result := (Number Mod 4 = 3);
End;

Function ModPow(Base, Exp, ModN: Int64): Int64;
Begin
  Result := 1;
  Base := Base Mod ModN;
  If Base < 0 Then
    Base := Base + ModN;
  While Exp > 0 Do
  Begin
    If (Exp Mod 2) = 1 Then
      Result := (Result * Base) Mod ModN;
    Base := (Base * Base) Mod ModN;
    Exp := Exp Div 2;
  End;
End;

Function NormalizeMod(X, M: Int64): Int64;
Begin
  Result := X Mod M;
  If Result < 0 Then
    Result := Result + M;
End;

Function ModInverse(A, N: Int64): Int64;
Var
  T, Newt, R, Newr, Q, Temp: Int64;
Begin
  T := 0;
  Newt := 1;
  R := N;
  Newr := A;
  While Newr <> 0 Do
  Begin
    Q := R Div Newr;
    Temp := T;
    T := Newt;
    Newt := Temp - Q * Newt;
    Temp := R;
    R := Newr;
    Newr := Temp - Q * Newr;
  End;
  If R > 1 Then
    Exit(0);
  If T < 0 Then
    T := T + N;
  Result := T;
End;

Function TForm1.DecryptBlock(C_val: Cardinal; P, Q, B: Int64): Byte;
Var
  N, Inv2: Int64;
  D, RootP, RootQ: Int64;
  Yp, Yq: Int64;
  Roots: Array [0 .. 3] Of Int64;
  I: Integer;
  M_candidate: Int64;
Begin
  N := P * Q;
  Inv2 := ModInverse(2, N);

  D := NormalizeMod((B * B) Mod N + (4 * C_val) Mod N, N);

  RootP := ModPow(D, (P + 1) Div 4, P);
  RootQ := ModPow(D, (Q + 1) Div 4, Q);

  Yp := ModInverse(Q, P);
  Yq := ModInverse(P, Q);

  Roots[0] := NormalizeMod(Yp * Q * RootP + Yq * P * RootQ, N);
  Roots[1] := N - Roots[0];
  Roots[2] := NormalizeMod(Yp * Q * RootP - Yq * P * RootQ, N);
  Roots[3] := N - Roots[2];

  Result := 0;
  For I := 0 To 3 Do
  Begin

    M_candidate := NormalizeMod((Roots[I] - B) * Inv2, N);
    If (M_candidate >= 0) And (M_candidate <= 255) Then
    Begin
      Result := Byte(M_candidate);
      Exit;
    End;
  End;
End;

Function IsPrime(Number: Integer): Boolean;
Var
  I: Integer;
Begin
  Result := True;
  If Number < 2 Then
    Result := False;
  I := 2;
  While I <= Sqrt(Number) Do
  Begin
    If Number Mod I = 0 Then
    Begin
      Result := False;
      Break;
    End;
    Inc(I);
  End;
End;

Procedure TForm1.Now;
Begin
  Now;
End;

Procedure TForm1.CheckButtonState();
Begin
  If (PEd.Text = '') Or (QEd.Text = '') Or (BEd.Text = '') Then
  Begin
    Now;
  End
  Else
    If Not(IsPrime(StrToInt(PEd.Text)) And IsPrime(StrToInt(QEd.Text))) Then
    Begin
      Now;
    End
    Else
      If Not(CheckModFour(StrToInt(PEd.Text)) And CheckModFour(StrToInt(QEd.Text))) Then
      Begin
        Now;
      End
      Else
        If (StrToInt(PEd.Text) * StrToInt(QEd.Text) < StrToInt(BEd.Text)) Then
        Begin
          Now;
        End
        Else
        Begin
          EncryptBt.Enabled := True;
          DecryptBt.Enabled := True;
        End
End;

Procedure TForm1.MainFormOnCreate(Sender: TObject);
Begin
  Now;
End;

Function CheckIfAllNumbers(Text: String): String;
Var
  I: Integer;
Begin
  I := 1;
  While I <= Length(Text) Do
  Begin
    If Not(Text[I] In ['0' .. '9']) Then
      Delete(Text, I, 1)
    Else
      Inc(I);
  End;
  Result := Text;
End;

Procedure TForm1.FirstEditOnChange(Sender: TObject);
Var
  Text: String;
  CaretPos: Integer;
Begin
  CaretPos := PEd.SelStart;
  Text := PEd.Text;

  PEd.Text := CheckIfAllNumbers(Text);
  If CaretPos <= Length(Text) Then
    PEd.SelStart := CaretPos
  Else
    PEd.SelStart := Length(Text);

  CheckButtonState();
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  OrigM.Clear;
  ResultM.Clear;
End;

Procedure TForm1.SecondEditOnChange(Sender: TObject);
Var
  Text: String;
  CaretPos: Integer;
Begin
  CaretPos := QEd.SelStart;
  Text := QEd.Text;

  QEd.Text := CheckIfAllNumbers(Text);
  If CaretPos <= Length(Text) Then
    QEd.SelStart := CaretPos
  Else
    QEd.SelStart := Length(Text);
  CheckButtonState();
End;

Procedure TForm1.ThirdEditOnChange(Sender: TObject);
Var
  Text: String;
  CaretPos: Integer;
Begin
  CaretPos := BEd.SelStart;
  Text := BEd.Text;

  BEd.Text := CheckIfAllNumbers(Text);
  If CaretPos <= Length(Text) Then
    BEd.SelStart := CaretPos
  Else
    BEd.SelStart := Length(Text);
  CheckButtonState();
End;

Procedure SaveToFile(Res: ResultArray);
Var
  F: File;
  ToSave: ResultArray;
Begin
  If Form1.SaveDialog.Execute Then
  Begin
    ToSave := Res;
    AssignFile(F, Form1.SaveDialog.FileName);
    Rewrite(F, 1);
    BlockWrite(F, ToSave[0], Length(ToSave) * SizeOf(Integer));
    CloseFile(F);
  End;
End;

Function Encrypt(FileName, P, Q, B: String): ResultArray;
Var
  Bytes: TBytes;
  I: Integer;
  N, NumB: Int64;
Begin
  Bytes := TFile.ReadAllBytes(FileName);
  SetLength(Result, Length(Bytes));

  N := StrToInt64(P) * StrToInt64(Q);
  NumB := StrToInt64(B);

  For I := 0 To High(Bytes) Do
  Begin
    Result[I] := Integer(NormalizeMod(Int64(Bytes[I]) * (Bytes[I] + NumB), N));
  End;

  SaveToFile(Result);
End;

Procedure TForm1.DecryptBtClick(Sender: TObject);
Var
  F: File;
  EncryptedData: ResultArray;
  DecryptedBytes: TBytes;
  I: Integer;
  P, Q, B: Int64;
Begin
  If Not OpenDialog.Execute Then
    Exit;

  OrigM.Clear;
  ResultM.Clear;

  P := StrToInt64(PEd.Text);
  Q := StrToInt64(QEd.Text);
  B := StrToInt64(BEd.Text);

  AssignFile(F, OpenDialog.FileName);
  Reset(F, 1);
  SetLength(EncryptedData, FileSize(F) Div SizeOf(Integer));
  BlockRead(F, EncryptedData[0], FileSize(F));
  CloseFile(F);

  OrigM.Lines.BeginUpdate;
  Try
    If Length(EncryptedData) > 100 Then
    Begin
      For I := 0 To 49 Do
        OrigM.Text := OrigM.Text + ' ' + IntToStr(EncryptedData[I]);

      OrigM.Text := OrigM.Text + ' ...';

      For I := Length(EncryptedData) - 50 To High(EncryptedData) Do
        OrigM.Text := OrigM.Text + ' ' + IntToStr(EncryptedData[I]);
    End
    Else
      For I := 0 To High(EncryptedData) Do
        OrigM.Text := OrigM.Text + ' ' + IntToStr(EncryptedData[I]);
  Finally
    OrigM.Lines.EndUpdate;
  End;

  SetLength(DecryptedBytes, Length(EncryptedData));
  For I := 0 To High(EncryptedData) Do
    DecryptedBytes[I] := DecryptBlock(EncryptedData[I], P, Q, B);

  ResultM.Lines.BeginUpdate;
  Try
    If Length(DecryptedBytes) > 100 Then
    Begin
      For I := 0 To 49 Do
        ResultM.Text := ResultM.Text + ' ' + IntToStr(DecryptedBytes[I]);

      ResultM.Text := ResultM.Text + ' ...';

      For I := Length(DecryptedBytes) - 50 To High(DecryptedBytes) Do
        ResultM.Text := ResultM.Text + ' ' + IntToStr(DecryptedBytes[I]);
    End
    Else
      For I := 0 To High(DecryptedBytes) Do
        ResultM.Text := ResultM.Text + ' ' + IntToStr(DecryptedBytes[I]);
  Finally
    ResultM.Lines.EndUpdate;
  End;

  If SaveDialog.Execute Then
    TFile.WriteAllBytes(SaveDialog.FileName, DecryptedBytes);

End;

Procedure TForm1.EncryptBtClick(Sender: TObject);
Var
  EncryptedArray: ResultArray;
  OriginalBytes: TBytes;
  I: Integer;
Begin
  ResultM.Clear;
  OrigM.Clear;

  Try
    If Not OpenDialog.Execute Then
      Exit;

    FilePath := OpenDialog.FileName;

    OriginalBytes := TFile.ReadAllBytes(FilePath);
    OrigM.Lines.BeginUpdate;
    If Length(OriginalBytes) > 100 Then
    Begin
      For I := 0 To 49 Do
        OrigM.Text := OrigM.Text + ' ' + IntToStr(OriginalBytes[I]);
      OrigM.Text := OrigM.Text + ' ...';
      For I := Length(OriginalBytes) - 50 To High(OriginalBytes) Do
        OrigM.Text := OrigM.Text + ' ' + IntToStr(OriginalBytes[I]);
    End
    Else
      For I := 0 To High(OriginalBytes) Do
        OrigM.Text := OrigM.Text + ' ' + IntToStr(OriginalBytes[I]);
    OrigM.Lines.EndUpdate;

    EncryptedArray := Encrypt(FilePath, PEd.Text, QEd.Text, BEd.Text);

    ResultM.Lines.BeginUpdate;
    If Length(EncryptedArray) > 100 Then
    Begin
      For I := 0 To 49 Do
        ResultM.Text := ResultM.Text + ' ' + IntToStr(EncryptedArray[I]);
      ResultM.Text := ResultM.Text + ' ...';
      For I := Length(EncryptedArray) - 50 To High(EncryptedArray) Do
        ResultM.Text := ResultM.Text + ' ' + IntToStr(EncryptedArray[I]);
    End
    Else
      For I := 0 To High(EncryptedArray) Do
        ResultM.Text := ResultM.Text + ' ' + IntToStr(EncryptedArray[I]);
    ResultM.Lines.EndUpdate;

  Except
    On E: Exception Do
      ShowMessage('Ошибка: ' + E.Message);
  End;
End;

End.
