unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Math, System.IOUtils;

type
  TLFSR = class(TForm)
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
    procedure FormCreate(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure AlgoritmBtnClick(Sender: TObject);
    procedure KeyEdKeyPress(Sender: TObject; var Key: Char);
    procedure SaveBtnClick(Sender: TObject);
  private
    FSourceData: TBytes;
    FResultData: TBytes;
    FGeneratedKey: TBytes;
    FFileExt: string;

    procedure ClearAll;
    function BytesToBinString(const Data: TBytes): string;
  public
  end;

var
  LFSR: TLFSR;

implementation

{$R *.dfm}

function TLFSR.BytesToBinString(const Data: TBytes): string;
var
  I, Bit: Integer;
  Sb: TStringBuilder;

  procedure AppendByte(Idx: Integer);
  var
    B: Integer;
  begin
    for B := 7 downto 0 do
    begin
      if (Data[Idx] and (1 shl B)) <> 0 then
        Sb.Append('1')
      else
        Sb.Append('0');
    end;
  end;

begin
  if Length(Data) = 0 then Exit('');

  Sb := TStringBuilder.Create;
  try
    if Length(Data) <= 36 then
    begin
      for I := 0 to High(Data) do
      begin
        AppendByte(I);
        if I < High(Data) then Sb.Append(' ');
      end;
    end
    else
    begin
      for I := 0 to 17 do
      begin
        AppendByte(I);
        Sb.Append(' ');
      end;

      Sb.Append(' ... ');
      Sb.Append(SLineBreak);

      for I := Length(Data) - 18 to High(Data) do
      begin
        AppendByte(I);
        if I < High(Data) then Sb.Append(' ');
      end;
    end;
    Result := Sb.ToString;
  finally
    Sb.Free;
  end;
end;

procedure TLFSR.ClearAll;
begin
  FileM.Clear;
  CipherM.Clear;
  ResKeyM.Clear;
  KeyEd.Clear;
  SetLength(FSourceData, 0);
  SetLength(FResultData, 0);
  SetLength(FGeneratedKey, 0);
  FFileExt := '';
  SaveBtn.Enabled := False;
end;

procedure TLFSR.FormCreate(Sender: TObject);
begin
  ClearAll;
  KeyEd.MaxLength := 30;
end;

procedure TLFSR.ClearBtnClick(Sender: TObject);
begin
  ClearAll;
end;

procedure TLFSR.KeyEdKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['0', '1', #8, #3, #22, #24]) then
    Key := #0;
end;

procedure TLFSR.LoadBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    ClearAll;
    FSourceData := TFile.ReadAllBytes(OpenDialog.FileName);
    FFileExt := ExtractFileExt(OpenDialog.FileName);
    FileM.Text := BytesToBinString(FSourceData);
  end;
end;

procedure TLFSR.AlgoritmBtnClick(Sender: TObject);
var
  SeedStr: string;
  LfsrReg: UInt32;
  I, J: Integer;
  NewBit, OutBit: Byte;
  CurrentByteKey: Byte;
begin
  SeedStr := Trim(KeyEd.Text);

  if Length(FSourceData) = 0 then
  begin
    MessageDlg('Сначала загрузите файл!', mtWarning, [mbOK], 0);
    Exit;
  end;

  if Length(SeedStr) <> 30 then
  begin
    MessageDlg('Ключ должен содержать ровно 30 бит (0 или 1)', mtWarning, [mbOK], 0);
    Exit;
  end;

  LfsrReg := 0;
  for I := 1 to Length(SeedStr) do
    LfsrReg := (LfsrReg shl 1) or (UInt32(Ord(SeedStr[I]) - 48) and 1);

  SetLength(FResultData, Length(FSourceData));
  SetLength(FGeneratedKey, Length(FSourceData));

  for I := 0 to High(FSourceData) do
  begin
    CurrentByteKey := 0;

    for J := 7 downto 0 do
    begin
      OutBit := (LfsrReg shr 29) and 1;
      CurrentByteKey := CurrentByteKey or (OutBit shl J);

      NewBit := ((LfsrReg shr 29) xor (LfsrReg shr 15) xor (LfsrReg shr 14) xor LfsrReg) and 1;

      LfsrReg := ((LfsrReg shl 1) or NewBit) and $3FFFFFFF;
    end;

    FGeneratedKey[I] := CurrentByteKey;
    FResultData[I] := FSourceData[I] xor CurrentByteKey;
  end;

  ResKeyM.Text := BytesToBinString(FGeneratedKey);
  CipherM.Text := BytesToBinString(FResultData);

  SaveBtn.Enabled := True;
  MessageDlg('Шифрование завершено!', mtInformation, [mbOK], 0);
end;

procedure TLFSR.SaveBtnClick(Sender: TObject);
var
  SaveDlg: TSaveDialog;
begin
  SaveDlg := TSaveDialog.Create(nil);
  try
    SaveDlg.Title := 'Сохранить результат';
    SaveDlg.FileName := 'output' + FFileExt;
    if SaveDlg.Execute then
    begin
      TFile.WriteAllBytes(SaveDlg.FileName, FResultData);
      MessageDlg('Файл сохранен: ' + ExtractFileName(SaveDlg.FileName), mtInformation, [mbOK], 0);
    end;
  finally
    SaveDlg.Free;
  end;
end;

end.
