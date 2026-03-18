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
    procedure FormCreate(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure AlgoritmBtnClick(Sender: TObject);
    procedure KeyEdKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    FFileBinaryData: string;
    FFileSize: Int64; // Добавляем поле для хранения размера файла
    procedure FileToBinaryString(const FileName: string);
    function GenerateLFSRKey(const InitialKey: string; RequiredBits: Int64): string;
    procedure XorWithKey(const KeyStr: string);
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
function FormatBinaryWithEllipsis(const BinaryStr: string; TotalBits: Int64): string; forward;
function BinToInt(const BinStr: string): Integer; forward;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FileM.Clear;
  CipherM.Clear;
  ResKeyM.Clear;
  KeyEd.Clear;
  AlgoritmBtn.Caption := 'Сгенерировать ключ и зашифровать';
  FFileBinaryData := '';
  FFileSize := 0;

  // Запрещаем ручной ввод в Memo
  FileM.ReadOnly := True;
  CipherM.ReadOnly := True;
  ResKeyM.ReadOnly := True;

  // Ограничиваем длину ввода в KeyEd до 30 символов
  KeyEd.MaxLength := 30;
end;

procedure TForm1.ClearBtnClick(Sender: TObject);
begin
  FileM.Clear;
  CipherM.Clear;
  ResKeyM.Clear;
  KeyEd.Clear;
  FFileBinaryData := '';
  FFileSize := 0;
end;

procedure TForm1.KeyEdKeyPress(Sender: TObject; var Key: Char);
begin
  // Разрешаем только цифры 0 и 1, а также управляющие клавиши (Backspace, Delete и т.д.)
  if not (Key in ['0', '1', #8, #13, #3, #22, #24]) then
  begin
    Key := #0; // Отменяем ввод
    MessageBeep(0); // Звуковой сигнал
  end;

  // Дополнительно можно ограничить вставку через Ctrl+V
  if (Key = #22) then // Ctrl+V
  begin
    Key := #0;
    MessageBeep(0);
    ShowMessage('Вставка запрещена. Вводите только 0 и 1 с клавиатуры.');
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
begin
  FileM.Clear;
  FFileBinaryData := '';
  FFileSize := 0;

  if not FileExists(FileName) then
  begin
    ShowMessage('Файл не найден!');
    Exit;
  end;

  try
    // Получаем размер файла
    TotalBytes := TFile.GetSize(FileName);
    FFileSize := TotalBytes; // Сохраняем размер файла

    // Создаем файловый поток для чтения файла
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      MemoryStream := TMemoryStream.Create;
      try
        // Копируем содержимое файла в MemoryStream
        MemoryStream.CopyFrom(FileStream, FileStream.Size);
        MemoryStream.Position := 0;

        // Преобразуем каждый байт в двоичный вид
        BinaryStr := '';
        for I := 0 to MemoryStream.Size - 1 do
        begin
          MemoryStream.Read(B, 1);
          BinaryStr := BinaryStr + IntToBin(B, 8);
        end;

        // Сохраняем бинарные данные для дальнейшего использования
        FFileBinaryData := BinaryStr;

        // Добавляем информацию о файле
        FileM.Lines.Add('Имя файла: ' + ExtractFileName(FileName));
        FileM.Lines.Add('Размер: ' + IntToStr(TotalBytes) + ' байт');
        FileM.Lines.Add('Бинарное представление файла:');
        FileM.Lines.Add(FormatBinaryWithEllipsis(BinaryStr, TotalBytes * 8));

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
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(Self);
  try
    OpenDialog.Title := 'Выберите файл для загрузки';
    OpenDialog.Filter := 'Все файлы|*.*';
    OpenDialog.Options := [ofFileMustExist, ofHideReadOnly];

    if OpenDialog.Execute then
    begin
      FileToBinaryString(OpenDialog.FileName);
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TForm1.XorWithKey(const KeyStr: string);
var
  i: Integer;
  XorResult: string;
  FileBit, KeyBit, ResultBit: Char;
  TotalBits: Int64;
begin
  // Проверяем, что длина ключа совпадает с длиной файла
  if Length(KeyStr) <> Length(FFileBinaryData) then
  begin
    ShowMessage('Ошибка: длина ключа (' + IntToStr(Length(KeyStr)) +
                ' бит) не совпадает с длиной файла (' +
                IntToStr(Length(FFileBinaryData)) + ' бит)');
    Exit;
  end;

  TotalBits := Length(FFileBinaryData);
  XorResult := '';

  // Выполняем XOR для каждого бита
  for i := 1 to TotalBits do
  begin
    FileBit := FFileBinaryData[i];
    KeyBit := KeyStr[i];

    // XOR: 1 если биты разные, 0 если одинаковые
    if FileBit = KeyBit then
      ResultBit := '0'
    else
      ResultBit := '1';

    XorResult := XorResult + ResultBit;
  end;

  // Выводим результат в CipherM
  CipherM.Clear;
  CipherM.Lines.Add('Зашифрованные данные (XOR с ключом):');
  CipherM.Lines.Add('Размер: ' + IntToStr(TotalBits) + ' бит');
  CipherM.Lines.Add('');
  CipherM.Lines.Add(FormatBinaryWithEllipsis(XorResult, TotalBits));
end;

procedure TForm1.AlgoritmBtnClick(Sender: TObject);
var
  InitialKey: string;
  RequiredBits: Int64;
  GeneratedKey: string;
  i: Integer;
begin
  // Проверяем, что файл загружен
  if FFileBinaryData = '' then
  begin
    ShowMessage('Сначала загрузите файл');
    Exit;
  end;

  // Проверяем, что введен ключ
  if KeyEd.Text = '' then
  begin
    ShowMessage('Введите начальный ключ (30 бит)');
    Exit;
  end;

  // Проверяем, что ключ состоит только из 0 и 1
  for i := 1 to Length(KeyEd.Text) do
  begin
    if not (KeyEd.Text[i] in ['0', '1']) then
    begin
      ShowMessage('Ключ должен состоять только из 0 и 1');
      Exit;
    end;
  end;

  // Проверяем длину ключа (должен быть 30 бит)
  if Length(KeyEd.Text) <> 30 then
  begin
    ShowMessage('Ключ должен быть длиной 30 бит');
    Exit;
  end;

  InitialKey := KeyEd.Text;

  // Используем сохраненный размер файла
  if FFileSize = 0 then
  begin
    ShowMessage('Ошибка: не удалось определить размер файла');
    Exit;
  end;

  RequiredBits := FFileSize * 8;

  // Генерируем ключ с помощью LFSR
  GeneratedKey := GenerateLFSRKey(InitialKey, RequiredBits);

  // Выводим результат в ResKeyM
  ResKeyM.Clear;
  ResKeyM.Lines.Add('Сгенерированный ключ (LFSR, x^30+x^16+x^15+x+1):');
  ResKeyM.Lines.Add('Размер ключа: ' + IntToStr(RequiredBits) + ' бит');
  ResKeyM.Lines.Add('');
  ResKeyM.Lines.Add(FormatBinaryWithEllipsis(GeneratedKey, RequiredBits));

  // Выполняем XOR с каждым элементом ключа и файла
  XorWithKey(GeneratedKey);
end;

// Функция генерации ключа с помощью LFSR для многочлена x^30+x^16+x^15+x+1
function TForm1.GenerateLFSRKey(const InitialKey: string; RequiredBits: Int64): string;
var
  LFSR: UInt32; // 32-битное число для хранения состояния (используем 30 бит)
  i: Integer;
  Bit: Integer;
  NewBit: Integer;
  ResultBuilder: TStringBuilder;
begin
  ResultBuilder := TStringBuilder.Create;
  try
    // Инициализируем LFSR начальным ключом
    LFSR := BinToInt(InitialKey);

    // Генерируем требуемое количество бит
    for i := 0 to RequiredBits - 1 do
    begin
      // Сохраняем текущий бит (старший бит 30-битного регистра)
      // В 30-битном регистре старший бит - это бит 29 (считая с 0)
      Bit := (LFSR shr 29) and 1;
      ResultBuilder.Append(IntToStr(Bit));

      // Вычисляем новый бит для многочлена x^30 + x^16 + x^15 + x + 1
      // Биты: 30, 16, 15, 1, 0 (в 1-индексации)
      // В 0-индексации: 29, 15, 14, 0
      NewBit := ((LFSR shr 29) xor   // x^30
                 (LFSR shr 15) xor   // x^16
                 (LFSR shr 14) xor   // x^15
                 (LFSR shr 0)) and 1; // x^1 и x^0 (последние два члена)

      // Сдвигаем регистр влево и добавляем новый бит в младший разряд
      LFSR := ((LFSR shl 1) or NewBit) and $3FFFFFFF; // Маска для 30 бит (0x3FFFFFFF)
    end;

    Result := ResultBuilder.ToString;
  finally
    ResultBuilder.Free;
  end;
end;

// Реализация функции IntToBin
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

// Функция для преобразования бинарной строки в число
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

// Функция для форматирования бинарной строки с троеточием
function FormatBinaryWithEllipsis(const BinaryStr: string; TotalBits: Int64): string;
const
  DISPLAY_BITS = 80; // Изменено с 88 на 80
var
  FirstPart, LastPart: string;
  StringBuilder: TStringBuilder;
  i: Integer;
begin
  // Если строка короткая, показываем целиком
  if Length(BinaryStr) <= DISPLAY_BITS * 2 then
  begin
    StringBuilder := TStringBuilder.Create;
    try
      for i := 1 to Length(BinaryStr) do
      begin
        StringBuilder.Append(BinaryStr[i]);
        if (i mod 8 = 0) and (i < Length(BinaryStr)) then
          StringBuilder.Append(' ');
      end;
      Result := StringBuilder.ToString;
    finally
      StringBuilder.Free;
    end;
    Exit;
  end;

  // Получаем первые DISPLAY_BITS символов
  FirstPart := Copy(BinaryStr, 1, DISPLAY_BITS);

  // Получаем последние DISPLAY_BITS символов
  LastPart := Copy(BinaryStr, Length(BinaryStr) - DISPLAY_BITS + 1, DISPLAY_BITS);

  StringBuilder := TStringBuilder.Create;
  try
    // Добавляем первую часть с пробелами
    for i := 1 to Length(FirstPart) do
    begin
      StringBuilder.Append(FirstPart[i]);
      if (i mod 8 = 0) and (i < Length(FirstPart)) then
        StringBuilder.Append(' ');
    end;

    StringBuilder.AppendLine;

    // Добавляем информацию о пропущенных битах
    StringBuilder.Append('... (');
    StringBuilder.Append(IntToStr(TotalBits - DISPLAY_BITS * 2));
    StringBuilder.Append(' бит пропущено) ...');

    StringBuilder.AppendLine;

    // Добавляем последнюю часть с пробелами
    for i := 1 to Length(LastPart) do
    begin
      StringBuilder.Append(LastPart[i]);
      if (i mod 8 = 0) and (i < Length(LastPart)) then
        StringBuilder.Append(' ');
    end;

    Result := StringBuilder.ToString;
  finally
    StringBuilder.Free;
  end;
end;

end.
