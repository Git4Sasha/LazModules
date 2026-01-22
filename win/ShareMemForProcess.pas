unit ShareMemForProcess;

interface

uses
  Windows;

type
  TSMName=string[20];

  TShareMemProcess=class
  private
    FName:TSMName; // Имя для общей памяти
    FHandler:THandle; // Идентификатор
    FSize:Cardinal;
    FPointer:Pointer;
  public
    Error:Integer;
    ErrorMsg:string;

    constructor Create(name:string; size:Cardinal);
    destructor Destroy; override;

    property SMName:TSMName read FName; // имя общей памяти
    property SMSize:Cardinal read FSize; // размер общей памяти
    property Addr:Pointer read FPointer; // Адрес начала общей памяти
  end;

implementation

{ TShareMemProcess }

constructor TShareMemProcess.Create(name:string; size:Cardinal);
begin
  FName:=name+#0;
  FSize:=size;
  FHandler:=CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, FSize, @FName[1]);
  if FHandler=0 then
    begin
      Error:=GetLastError;
      ErrorMsg:='Функция CreateFileMapping вернула ошибку';
      Exit;
    end;

  FPointer:=MapViewOfFile(FHandler, FILE_MAP_WRITE, 0, 0, FSize);
  if FPointer=nil then
    begin
      Error:=GetLastError;
      ErrorMsg:='Функция MapViewOfFile вернула ошибку';
      Exit;
    end;

  Error:=0;
  ErrorMsg:='OK';
end;

destructor TShareMemProcess.Destroy;
begin
  if FPointer<>nil then
    UnmapViewOfFile(FPointer);
  if FHandler<>0 then
    CloseHandle(FHandler);
  inherited;
end;

end.
