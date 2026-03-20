unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    CipherM: TMemo;
    FileM: TMemo;
    AlgoritmBtn: TButton;
    ClearBtn: TButton;
    KeyEd: TEdit;
    LoadBtn: TButton;
    ResKeyM: TMemo;
    OpenDialog: TOpenDialog;
    SaveBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure AlgoritmBtnClick(Sender: TObject);
    procedure KeyEdKeyPress(Sender: TObject; var Key: Char);
    procedure SaveBtnClick(Sender: TObject);
  private
    { Private declarations }
    FFileBinaryData: string;
    FFileSize: Int64;
    FKey: string;
    FSeed: string;
    FCipher: string;
    FFileExt: string;
    procedure FileToBinaryString(const FileName: string);
    function GenerateLFSRKey(const InitialKey: string; RequiredBits: Int64): string;
    procedure XorWithKey(const KeyStr: string);
    procedure ClearFields;
    function FormatWithSpaces(const BinStr: string): string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.IOUtils, System.Math, System.StrUtils;

function IntToBin(Value: Byte; Digits: Integer): string; forward;
function BinToInt(const BinStr: string): Integer; forward;

function TForm1.FormatWithSpaces(const BinStr: string): string;
var
  i: Integer;
  StringBuilder: TStringBuilder;
begin
  StringBuilder := TStringBuilder.Create;
  try
    for i := 1 to Length(BinStr) do
    begin
      StringBuilder.Append(BinStr[i]);
      if (i mod 8 = 0) and (i < Length(BinStr)) then
        StringBuilder.Append(' ');
    end;
    Result := StringBuilder.ToString;
  finally
    StringBuilder.Free;
  end;
end;

procedure TForm1.ClearFields;
begin
  FileM.Lines.Text := '';
  CipherM.Lines.Text := '';
  ResKeyM.Lines.Text := '';
  KeyEd.Text := '';
  FCipher := '';
  FKey := '';
  FSeed := '';
  FFileBinaryData := '';
  FFileSize := 0;
  FFileExt := '';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ClearFields;
  // Запрещаем ручной ввод в Memo
  FileM.ReadOnly := True;
  CipherM.ReadOnly := True;
  ResKeyM.ReadOnly := True;
  // Ограничиваем длину ввода в KeyEd до 30 символов
  KeyEd.MaxLength := 30;
  AlgoritmBtn.Caption := 'Сгенерировать ключ и зашифровать';
end;

procedure TForm1.ClearBtnClick(Sender: TObject);
begin
  ClearFields;
end;

procedure TForm1.KeyEdKeyPress(Sender: TObject; var Key: Char);
begin
  // Фильтруем ввод, оставляя только 0 и 1 и Backspace
  if not (Key in ['0', '1', #8]) then
  begin
    Key := #0;
    MessageBeep(0);
  end;
end;

procedure TForm1.FileToBinaryString(const FileName: string);
var
  FileStream: TFileStream;
  MemoryStream: TMemoryStream;
  I: Integer;
  B: Byte;
  BinaryStr: string;
  TotalBytes: Int64;
  Temp: TStringBuilder;
begin
  if not FileExists(FileName) then
  begin
    ShowMessage('Файл не найден!');
    Exit;
  end;

  try
    TotalBytes := TFile.GetSize(FileName);
    FFileSize := TotalBytes;
    FFileExt := ExtractFileExt(FileName);

    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      MemoryStream := TMemoryStream.Create;
      try
        MemoryStream.CopyFrom(FileStream, FileStream.Size);
        MemoryStream.Position := 0;

        BinaryStr := '';
        for I := 0 to MemoryStream.Size - 1 do
        begin
          MemoryStream.Read(B, 1);
          BinaryStr := BinaryStr + IntToBin(B, 8);
        end;

        FFileBinaryData := BinaryStr;

        if TotalBytes * 8 > 160 then
        begin
          Temp := TStringBuilder.Create;
          try
            Temp.AppendLine(FormatWithSpaces(Copy(BinaryStr, 1, 80)));
            Temp.AppendLine('...');
            Temp.Append(FormatWithSpaces(Copy(BinaryStr, TotalBytes * 8 - 79, 80)));
            FileM.Lines.Text := Temp.ToString;
          finally
            Temp.Free;
          end;
        end
        else
          FileM.Lines.Text := FormatWithSpaces(BinaryStr);
      finally
        MemoryStream.Free;
      end;
    finally
      FileStream.Free;
    end;

    FileM.SelStart := 0;
  except
    on E: Exception do
      ShowMessage('Ошибка при чтении файла: ' + E.Message);
  end;
end;

procedure TForm1.LoadBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    ClearFields;
    FileToBinaryString(OpenDialog.FileName);
  end;
end;

function BinaryStringToBytes(const BinStr: string): TBytes;
var
  i: Integer;
  b: Byte;
  len: Integer;
begin
  len := (Length(BinStr) + 7) div 8;
  SetLength(Result, len);

  for i := 0 to len - 1 do
  begin
    b := 0;
    if (i*8 + 1) <= Length(BinStr) then b := b or (StrToIntDef(Copy(BinStr, i*8 + 1, 1), 0) shl 7);
    if (i*8 + 2) <= Length(BinStr) then b := b or (StrToIntDef(Copy(BinStr, i*8 + 2, 1), 0) shl 6);
    if (i*8 + 3) <= Length(BinStr) then b := b or (StrToIntDef(Copy(BinStr, i*8 + 3, 1), 0) shl 5);
    if (i*8 + 4) <= Length(BinStr) then b := b or (StrToIntDef(Copy(BinStr, i*8 + 4, 1), 0) shl 4);
    if (i*8 + 5) <= Length(BinStr) then b := b or (StrToIntDef(Copy(BinStr, i*8 + 5, 1), 0) shl 3);
    if (i*8 + 6) <= Length(BinStr) then b := b or (StrToIntDef(Copy(BinStr, i*8 + 6, 1), 0) shl 2);
    if (i*8 + 7) <= Length(BinStr) then b := b or (StrToIntDef(Copy(BinStr, i*8 + 7, 1), 0) shl 1);
    if (i*8 + 8) <= Length(BinStr) then b := b or  StrToIntDef(Copy(BinStr, i*8 + 8, 1), 0);

    Result[i] := b;
  end;
end;

procedure TForm1.SaveBtnClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  Bytes: TBytes;
  FileStream: TFileStream;
begin
  if FCipher = '' then
  begin
    ShowMessage('Нет зашифрованных данных. Сначала выполните шифрование.');
    Exit;
  end;

  if FFileExt = '' then
  begin
    ShowMessage('Не определено расширение исходного файла.');
    Exit;
  end;

  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Title := 'Сохранить зашифрованный файл';
    SaveDialog.Filter := 'Исходный тип (*' + FFileExt + ')|*' + FFileExt + '|Все файлы|*.*';
    SaveDialog.DefaultExt := Copy(FFileExt, 2, MaxInt); // без точки
    SaveDialog.FileName := 'encrypted' + FFileExt;

    if SaveDialog.Execute then
    begin
      Bytes := BinaryStringToBytes(FCipher);

      try
        FileStream := TFileStream.Create(SaveDialog.FileName, fmCreate);
        try
          if Length(Bytes) > 0 then
            FileStream.Write(Bytes[0], Length(Bytes));
        finally
          FileStream.Free;
        end;

        ShowMessage('Файл успешно сохранён:' + sLineBreak + SaveDialog.FileName);
      except
        on E: Exception do
          ShowMessage('Ошибка сохранения:' + sLineBreak + E.Message);
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TForm1.XorWithKey(const KeyStr: string);
var
  i: Integer;
  XorResult: string;
  FileBit, KeyBit, ResultBit: Char;
  TotalBits: Int64;
  Temp: TStringBuilder;
begin
  if Length(KeyStr) <> Length(FFileBinaryData) then
  begin
    ShowMessage('Ошибка: длина ключа (' + IntToStr(Length(KeyStr)) +
                ' бит) не совпадает с длиной файла (' +
                IntToStr(Length(FFileBinaryData)) + ' бит)');
    Exit;
  end;

  TotalBits := Length(FFileBinaryData);
  XorResult := '';

  for i := 1 to TotalBits do
  begin
    FileBit := FFileBinaryData[i];
    KeyBit := KeyStr[i];
    if FileBit = KeyBit then
      ResultBit := '0'
    else
      ResultBit := '1';
    XorResult := XorResult + ResultBit;
  end;

  FCipher := XorResult;

  if TotalBits > 160 then
  begin
    Temp := TStringBuilder.Create;
    try
      Temp.AppendLine(FormatWithSpaces(Copy(FCipher, 1, 80)));
      Temp.AppendLine('...');
      Temp.Append(FormatWithSpaces(Copy(FCipher, TotalBits - 79, 80)));
      CipherM.Lines.Text := Temp.ToString;
    finally
      Temp.Free;
    end;
  end
  else
    CipherM.Lines.Text := FormatWithSpaces(FCipher);
end;

procedure TForm1.AlgoritmBtnClick(Sender: TObject);
var
  RequiredBits: Int64;
  GeneratedKey: string;
  Temp: TStringBuilder;
begin
  FSeed := Trim(KeyEd.Text);

  if FFileBinaryData = '' then
  begin
    ShowMessage('Сначала загрузите файл');
    Exit;
  end;

  if FSeed = '' then
  begin
    ShowMessage('Введите начальный ключ (30 бит)');
    Exit;
  end;

  if Length(FSeed) <> 30 then
  begin
    ShowMessage('Ключ должен быть длиной ровно 30 бит (0 и 1)');
    Exit;
  end;

  if FFileSize = 0 then
  begin
    ShowMessage('Ошибка: размер файла не определён');
    Exit;
  end;

  RequiredBits := FFileSize * 8;
  GeneratedKey := GenerateLFSRKey(FSeed, RequiredBits);
  FKey := GeneratedKey;

  if RequiredBits > 160 then
  begin
    Temp := TStringBuilder.Create;
    try
      Temp.AppendLine(FormatWithSpaces(Copy(FKey, 1, 80)));
      Temp.AppendLine('...');
      Temp.Append(FormatWithSpaces(Copy(FKey, RequiredBits - 79, 80)));
      ResKeyM.Lines.Text := Temp.ToString;
    finally
      Temp.Free;
    end;
  end
  else
    ResKeyM.Lines.Text := FormatWithSpaces(FKey);

  XorWithKey(FKey);
end;

function TForm1.GenerateLFSRKey(const InitialKey: string; RequiredBits: Int64): string;
var
  LFSR: UInt32;
  i: Integer;
  Bit: Integer;
  NewBit: Integer;
  ResultBuilder: TStringBuilder;
begin
  ResultBuilder := TStringBuilder.Create;
  try
    LFSR := BinToInt(InitialKey);

    for i := 0 to RequiredBits - 1 do
    begin
      Bit := (LFSR shr 29) and 1;
      ResultBuilder.Append(IntToStr(Bit));

      NewBit := ((LFSR shr 29) xor
                 (LFSR shr 15) xor
                 (LFSR shr 14) xor
                 (LFSR shr 0)) and 1;

      LFSR := ((LFSR shl 1) or NewBit) and $3FFFFFFF; // 30 бит
    end;

    Result := ResultBuilder.ToString;
  finally
    ResultBuilder.Free;
  end;
end;

function IntToBin(Value: Byte; Digits: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := Digits - 1 downto 0 do
  begin
    if (Value and (1 shl i)) <> 0 then
      Result := Result + '1'
    else
      Result := Result + '0';
  end;
end;

function BinToInt(const BinStr: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(BinStr) do
  begin
    Result := Result shl 1;
    if BinStr[i] = '1' then
      Result := Result or 1;
  end;
end;

end.
