unit SerialPort;

// Модуль предназначен для чтения и записи данных через COM-порт
// обмен данными выполняется фиксированными по размеру буферами

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, SimpleThread, MemoryWork;

type
  TOnDataFromPort=procedure(buf:PByte; len:Integer) of object;

  { TSerialPortWork }

  TSerialPortWork=class
  private
    FPortOpened:Boolean; // Признак того, что порт открыт
    FCOMHandleRW:THandle; // Идентификатор открытого порта для чтения/записи данных
    FReadThread:TSimpleThread; // Объект для потока чтения данных
    FOutBuf:TMemWork;  // Буфер для выходных данных
    FOnDataFromPort:TOnDataFromPort; // Переменная обслуживает событие прихода данных
    FOnErrorReadPort:TNotifyEvent;                  // Переменная обслуживает событие ошибки чтения из порта (такой ошибки, при которой поток чтения закрывается)
    FErrStrings:TStrings;                           // Строки, которые отображают текущую ошибку

    procedure ReadFunc(Sender: TObject); // Функция, которая будет работать в потоке и ожидать данные от порта
  public
    constructor Create;
    destructor Destroy; override;

    function OpenPort(name:string; speed, sb:Integer):Boolean; // Открытие порта
    procedure ClosePort; // Закрытие порта
    function WritePort:Cardinal;                 // Запись данных в порт (выполняется запись в порт буфера OutBuf)
    function WritePort(buf:Pointer; len:Integer):Integer; //
    function GetPortsList: TStrings; // Функция определяет наличие COM-портов в системе

    property PortOpened:Boolean read FPortOpened; // Свойство-признак того, что порт открыт
    property ErrStrings:TStrings read FErrStrings;
    property OnDataFromPort:TOnDataFromPort read FOnDataFromPort write FOnDataFromPort; // Событие которое запускается всякий раз когда приходят данные с порта
    property OnErrorReadPort:TNotifyEvent read FOnErrorReadPort write FOnErrorReadPort; // Событие которое запускается если дальнейшее чтение из порта невозможно
    property OutBuf:TMemWork read FOutBuf;
  end;


implementation

{ TSerialPortWork }

procedure TSerialPortWork.ReadFunc(Sender: TObject);
var
  tob:TSimpleThread;
  l,boffset:DWord;
  CurrentState:TComStat;
  ErrCode:Cardinal;
  ReadOL : TOverLapped;     // структура для асинхронного чтения
  Signaled, Mask : DWORD;
  buf:array [0..2000] of Byte;
begin
  tob:=Sender as TSimpleThread;

  // создание события для асинхронного чтения
  FillChar(ReadOL, SizeOf(ReadOL), 0);
  ReadOL.hEvent:= CreateEvent(nil, True, True, nil);

  boffset:=0; // Смещение в буфере, оно то нулевое, то равно середине буфера (двойная буферизация)
  l:=0;

  repeat
    WaitCommEvent(FCOMHandleRW, @Mask, @ReadOL);
    Signaled:=WaitForSingleObject(ReadOL.hEvent, 100);

    if (Signaled=WAIT_OBJECT_0) then begin
      if GetOverlappedResult(FCOMHandleRW, ReadOL, l, False) then begin
        // после GetOverlappedResult в переменной mask, которая
        // передавалась в WaitCommEvent, появятся флаги произошедших
        // событий, либо 0 в случае ошибки.
        if Mask and EV_RXCHAR<>0 then begin
          ClearCommError(FCOMHandleRW, @ErrCode, @CurrentState); //Заполняем структуру CurrentState
          if CurrentState.cbInQue>0 then begin  // Число полученных, но еще не прочитанных байт
            if ReadFile(FCOMHandleRW, buf[boffset], CurrentState.cbInQue, l, @ReadOL) then begin
              if Assigned(FOnDataFromPort) then FOnDataFromPort(@buf[boffset], l);
              if boffset=0 then boffset:=1000;
            end;
          end;
        end;
      end;
    end;

    if Signaled=WAIT_TIMEOUT then begin
      if GetLastError=ERROR_ACCESS_DENIED then begin
        FErrStrings.Append('Ошибка при чтении с порта');
        FPortOpened:=False;
        CloseHandle(FCOMHandleRW); // Закрываем идентификатор файла-ком порта
        FCOMHandleRW:=0;
        if Assigned(FOnErrorReadPort) then FOnErrorReadPort(Self);
        Break;
      end;
    end;
  until tob.CheckTerminated;

  CloseHandle(ReadOL.hEvent);
end;

constructor TSerialPortWork.Create;
begin
  FPortOpened:=False; // Признак того, что порт открыт
  FErrStrings:=TStringList.Create;  // Объект для работы со строками
  FOutBuf:=TMemWork.Create(2048);  // Буфер для выходных данных
end;

destructor TSerialPortWork.Destroy;
begin
  ClosePort; // Закрытие порта
  FreeAndNil(FOutBuf); // Буфер для выходных данных
  FErrStrings.Free;
  inherited Destroy;
end;

function TSerialPortWork.OpenPort(name: string; speed, sb: Integer): Boolean;
var
	s:_DCB;
  touts:TCOMMTIMEOUTS;
begin
  Result:=True;
  FErrStrings.Clear;
  if FPortOpened then begin
    FErrStrings.Append('Com allready open');
    Exit(False);
  end;

  FCOMHandleRW:= CreateFile(
                             PChar(name),                                   // передаем имя открываемого порта
                             GENERIC_READ or GENERIC_WRITE,                 // ресурс для чтения и записи
                             0,                                             // не разделяемый ресурс
                             nil,                                           // Нет атрибутов защиты
                             OPEN_EXISTING,                                 // вернуть ошибку, если ресурс не существует}
                             FILE_ATTRIBUTE_NORMAL or FILE_FLAG_OVERLAPPED, // асинхронный режим доступа
                             0                                              // Должно быть 0 для COM портов
                            ) ;




	if FCOMHandleRW=INVALID_HANDLE_VALUE then begin
    FErrStrings.Append('COM Port not open for read');
    Exit(False);
  end;

	if not GetCommState(FCOMHandleRW, s) then begin // Определяем состояние порта
		FErrStrings.Append('Problem with GetCommState');
    Exit(False);
  end;

  // Если определение состояния порта прошло успешно, то меняем часть параметров в порту
  s.BaudRate:=speed;
  s.Parity:=NOPARITY;
  s.ByteSize:=8;
  case sb of
    1:  s.StopBits:=ONESTOPBIT;
    2:  s.StopBits:=TWOSTOPBITS;
  else
    s.StopBits:=ONESTOPBIT;
  end;

	if not SetCommState(FCOMHandleRW, s) then begin // После изменения необходимых нам параметров, задаём новое состояние порта
		FErrStrings.Append('Problem with SetCommState');
    Exit(False);
  end;

  //// Установка предельного времени на чтение
  //touts.ReadIntervalTimeout:=10; // Предел ожидания операции чтения из порта
  //touts.ReadTotalTimeoutConstant:=10;
  //touts.ReadTotalTimeoutMultiplier:=10;
  //touts.WriteTotalTimeoutConstant:=10;
  //touts.WriteTotalTimeoutMultiplier:=10;
  //if not SetCommTimeouts(FCOMHandleRW, touts) then
  //  FErrStrings.Append('Problem with SetCommTimeouts');

  if not PurgeComm(FCOMHandleRW, PURGE_TXABORT or PURGE_RXABORT or PURGE_TXCLEAR or PURGE_RXCLEAR) then begin // Очищаем очереди на COM порту
    FErrStrings.Append('Problem with PurgeComm');
    Exit(False);
  end;
  // Установка того, что будет формироваться событие если был принят байт данных
  if not SetCommMask(FCOMHandleRW, EV_RXCHAR) then begin // SetCommMask - эта функция задаёт маску ожидаемых событий
    FErrStrings.Append('Problem with SetCommMask');
    Exit(False);
  end;

  FReadThread:=TSimpleThread.Create(True); // Объект для потока чтения данных
  FReadThread.OnExec:=@ReadFunc; // Назначаем функцию, которая будет работать в потоке
  FReadThread.FreeOnTerminate:=True;
  FReadThread.Start; // Запуск читающего потока

  FPortOpened:=True;
end;

procedure TSerialPortWork.ClosePort; // Закрытие порта
begin
  if FPortOpened then begin
      FPortOpened:=False;
      FReadThread.Terminate; // Остановка работы потока чтения данных
      FReadThread.WaitFor;
      PurgeComm(FCOMHandleRW, PURGE_TXABORT or PURGE_RXABORT or PURGE_TXCLEAR or PURGE_RXCLEAR);
      CloseHandle(FCOMHandleRW); // Закрываем идентификатор файла-ком порта
      FCOMHandleRW:=0;
    end;
end;

function TSerialPortWork.WritePort: Cardinal;
begin
  Result:=0;
  if not FPortOpened then Exit;
  Result:=WritePort(FOutBuf.Memory[0], FOutBuf.Pos);
end;

function TSerialPortWork.WritePort(buf: Pointer; len: Integer): Integer;
var
  Signaled, RealWrite, BytesTrans : Cardinal;
  WriteOL : TOverLapped;  // структура для асинхронной записи
begin
  Result:=0;
  if not FPortOpened then Exit;

  // создание события для асинхронной записи
  Result:=0;
  FillChar(WriteOL, SizeOf(WriteOL), 0);
  WriteOL.hEvent:=CreateEvent(nil, True, True, nil);

  WriteFile(FCOMHandleRW, buf^, len, RealWrite, @WriteOL); // начало асинхронной записи
  Signaled:=WaitForSingleObject(WriteOL.hEvent, 100);  // ожидания завершения асинхронной операции (с ограничением 100мс)

  if Signaled = WAIT_OBJECT_0 then begin
    GetOverlappedResult(FCOMHandleRW, WriteOL, BytesTrans, False);
    Result:=BytesTrans;
    CloseHandle(WriteOL.hEvent);
    Exit;
  end;

  if Signaled = WAIT_TIMEOUT then begin
    if GetLastError=ERROR_ACCESS_DENIED then begin
      FErrStrings.Append('Ошибка при записи в порт');
    end;
  end;
end;

function TSerialPortWork.GetPortsList: TStrings; // Процедура формирует список доступных портов
var
  ch:THandle;
  i:Integer;
  name:string;
begin
  Result:=TStringList.Create;
  for i:=1 to 100 do // Цикл по 100 портам ( надеюсь больше на компе быть не может )
    begin // Пробует открыть порт, если открывается, значит такой порт есть
      name:=Format('\\.\COM%d',[i]);
    	ch:=CreateFile(PChar(name), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_OVERLAPPED, 0); // Создание файла с именем com-порта
  	  if ch<>INVALID_HANDLE_VALUE then // Если при создании не было ошибок, то
        begin
          Result.Add(name); // Добавляем порт в список портов
          CloseHandle(ch); // Освобождаем порт
        end;
    end;
end;


end.

