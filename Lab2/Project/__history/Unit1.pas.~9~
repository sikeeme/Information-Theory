Unit Unit1;

Interface

Uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

Type
  TLFSR = Class(TForm)
    CipherM: TMemo;
    FileM: TMemo;
    AlgoritmBtn: TButton;
    ClearBtn: TButton;
    KeyEd: TEdit;
    LoadBtn: TButton;
    ResKeyM: TMemo;
    OpenDialog: TOpenDialog;
    SaveBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Procedure FormCreate(Sender: TObject);
    Procedure ClearBtnClick(Sender: TObject);
    Procedure LoadBtnClick(Sender: TObject);
    Procedure AlgoritmBtnClick(Sender: TObject);
    Procedure KeyEdKeyPress(Sender: TObject; Var Key: Char);
    Procedure SaveBtnClick(Sender: TObject);
  Private
    FFileBinary: String;
    FFileSize: Int64;
    FCipher: String;
    FFileExt: String;
    FGeneratedKey: String;

    Procedure FileToBinary(Const FileName: String);
    Function GenerateLFSRKey(Const Seed: String; BitCount: Int64): String;
    Procedure XorWithKey(Const KeyStr: String);
    Procedure ClearAll;
  Public
  End;

Var
  LFSR: TLFSR;

Implementation

{$R *.dfm}

Uses
  System.IOUtils,
  System.Math,
  System.StrUtils;

Function IntToBin(Value: Byte; Digits: Integer): String;
Var
  I: Integer;
Begin
  SetLength(Result, Digits);
  For I := Digits - 1 Downto 0 Do
    Result[Digits - I] := Chr(Ord('0') + ((Value Shr I) And 1));
End;

Function BinToInt(Const BinStr: String): UInt32;
Var
  I: Integer;
Begin
  Result := 0;
  For I := 1 To Length(BinStr) Do
  Begin
    Result := Result Shl 1;
    If BinStr[I] = '1' Then
      Result := Result Or 1;
  End;
End;

Function FormatBinWithSpaces(Const BinStr: String): String;
Var
  I: Integer;
  Sb: TStringBuilder;
Begin
  Sb := TStringBuilder.Create(Length(BinStr) + Length(BinStr) Div 8 + 2);
  Try
    For I := 1 To Length(BinStr) Do
    Begin
      Sb.Append(BinStr[I]);
      If (I Mod 8 = 0) And (I < Length(BinStr)) Then
        Sb.Append(' ');
    End;
    Result := Sb.ToString;
  Finally
    Sb.Free;
  End;
End;

Procedure TLFSR.ClearAll;
Begin
  FileM.Clear;
  CipherM.Clear;
  ResKeyM.Clear;
  KeyEd.Clear;
  FFileBinary := '';
  FCipher := '';
  FGeneratedKey := '';
  FFileSize := 0;
  FFileExt := '';
End;

Procedure TLFSR.FormCreate(Sender: TObject);
Begin
  ClearAll;
  FileM.ReadOnly := True;
  CipherM.ReadOnly := True;
  ResKeyM.ReadOnly := True;
  KeyEd.MaxLength := 30;
End;

Procedure TLFSR.ClearBtnClick(Sender: TObject);
Begin
  ClearAll;
End;

Procedure TLFSR.KeyEdKeyPress(Sender: TObject; Var Key: Char);
Begin
  If Not(Key In ['0', '1', #8, #3, #22, #24, #26, #1]) Then
    Key := #0;
End;

Procedure TLFSR.FileToBinary(Const FileName: String);
Var
  Ms: TMemoryStream;
  I: Integer;
  B: Byte;
  S: String;
Begin
  If Not FileExists(FileName) Then
  Begin
    MessageDlg('Файл не найден', MtError, [MbOK], 0);
    Exit;
  End;

  Try
    Ms := TMemoryStream.Create;
    Try
      Ms.LoadFromFile(FileName);
      FFileSize := Ms.Size;
      FFileExt := ExtractFileExt(FileName);

      S := '';
      Ms.Position := 0;
      For I := 0 To Ms.Size - 1 Do
      Begin
        Ms.Read(B, 1);
        S := S + IntToBin(B, 8);
      End;
      FFileBinary := S;

      If FFileSize * 8 > 288 Then
        FileM.Lines.Text := FormatBinWithSpaces(Copy(S, 1, 144)) + SLineBreak + '...' + SLineBreak +
            FormatBinWithSpaces(Copy(S, Length(S) - 143, 144))
      Else
        FileM.Lines.Text := FormatBinWithSpaces(S);

    Finally
      Ms.Free;
    End;
  Except
    On E: Exception Do
      MessageDlg('Ошибка чтения файла: ' + E.Message, MtError, [MbOK], 0);
  End;
End;

Procedure TLFSR.LoadBtnClick(Sender: TObject);
Begin
  If OpenDialog.Execute Then
  Begin
    ClearAll;
    FileToBinary(OpenDialog.FileName);
  End;
End;

Function BinaryToBytes(Const BinStr: String): TBytes;
Var
  Len, I, J: Integer;
Begin
  Len := (Length(BinStr) + 7) Div 8;
  SetLength(Result, Len);

  For I := 0 To Len - 1 Do
  Begin
    Result[I] := 0;
    For J := 0 To 7 Do
    Begin
      If I * 8 + J + 1 > Length(BinStr) Then
        Break;
      If BinStr[I * 8 + J + 1] = '1' Then
        Result[I] := Result[I] Or (1 Shl (7 - J));
    End;
  End;
End;
Procedure TLFSR.SaveBtnClick(Sender: TObject);
Var
  SaveDlg: TSaveDialog;
Begin
  SaveDlg := TSaveDialog.Create(Nil);
  Try
    SaveDlg.Title := 'Сохранить зашифрованный файл';

    SaveDlg.Filter := 'Все файлы (*.*)|*.*|Исходный тип (*' + FFileExt + ')|*' + FFileExt;
    SaveDlg.FilterIndex := 1;
    SaveDlg.DefaultExt := '';

    SaveDlg.FileName := 'encrypted' + FFileExt;

    If SaveDlg.Execute Then
    Begin
      TFile.WriteAllBytes(SaveDlg.FileName, BinaryToBytes(FCipher));
      MessageDlg('Сохранено: ' + SaveDlg.FileName, MtInformation, [MbOK], 0);
    End;
  Finally
    SaveDlg.Free;
  End;
End;

Procedure TLFSR.XorWithKey(Const KeyStr: String);
Var
  I: Integer;
  Sb: TStringBuilder;
Begin
  Sb := TStringBuilder.Create(Length(FFileBinary));
  Try
    For I := 1 To Length(FFileBinary) Do
      Sb.Append(Chr(Ord('0') + (Ord(FFileBinary[I]) Xor Ord(KeyStr[I]))));
    FCipher := Sb.ToString;
  Finally
    Sb.Free;
  End;

  If Length(FCipher) > 288 Then
    CipherM.Lines.Text := FormatBinWithSpaces(Copy(FCipher, 1, 144)) + SLineBreak + '...' + SLineBreak +
        FormatBinWithSpaces(Copy(FCipher, Length(FCipher) - 143, 144))
  Else
    CipherM.Lines.Text := FormatBinWithSpaces(FCipher);
End;

Procedure TLFSR.AlgoritmBtnClick(Sender: TObject);
Var
  Seed: String;
  ReqBits: Int64;
  Key: String;
Begin
  Seed := Trim(KeyEd.Text);
  If FFileBinary = '' Then
  Begin
    MessageDlg('Сначала загрузите файл', MtWarning, [MbOK], 0);
    Exit;
  End;

  If Length(Seed) <> 30 Then
  Begin
    MessageDlg('Ключ должен содержать ровно 30 бит (0 и 1)', MtWarning, [MbOK], 0);
    Exit;
  End;

  ReqBits := FFileSize * 8;
  Key := GenerateLFSRKey(Seed, ReqBits);
  FGeneratedKey := Key;

  If ReqBits > 288 Then
    ResKeyM.Lines.Text := FormatBinWithSpaces(Copy(Key, 1, 144)) + SLineBreak + '...' + SLineBreak +
        FormatBinWithSpaces(Copy(Key, ReqBits - 143, 144))
  Else
    ResKeyM.Lines.Text := FormatBinWithSpaces(Key);

  XorWithKey(Key);

  SaveBtn.Enabled := True;
End;

Function TLFSR.GenerateLFSRKey(Const Seed: String; BitCount: Int64): String;
Var
  Lfsr: UInt32;
  I: Integer;
  NewBit: Integer;
Begin
  SetLength(Result, BitCount);
  Lfsr := BinToInt(Seed);

  For I := 0 To BitCount - 1 Do
  Begin
    Result[I + 1] := Chr(Ord('0') + ((Lfsr Shr 29) And 1));

    NewBit := ((Lfsr Shr 29) Xor (Lfsr Shr 15) Xor (Lfsr Shr 14) Xor Lfsr) And 1;
    Lfsr := ((Lfsr Shl 1) Or NewBit) And $3FFFFFFF;
  End;
End;

End.
