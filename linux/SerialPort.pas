unit SerialPort;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , MemoryWork
  , SimpleThread
  , BaseUnix
  , termio
  ;

type
  TOnDataFromPort=procedure(data:PByte; len:Integer) of object;

  { TSerialPortWork }

  TSerialPortWork=class
  private
    FFd:cint;                                       // Идентификатор открытого порта для чтения/записи данных
    FPortOpened:Boolean;                            // Признак того, что порт открыт
    FReadThread:TSimpleThread;                      // Объект для потока чтения данных
    FInBuf:TMemWork;                                // Буфер для входных данных
    FOutBuf:TMemWork;                               // Буфер для выходных данных
    FErrStrings:TStrings;                           // Строки, которые отображают текущую ошибку
    FOnDataFromPort:TOnDataFromPort;                // Переменная обслуживает событие прихода данных
    FOnErrorReadPort:TNotifyEvent;                  // Переменная обслуживает событие ошибки чтения из порта (такой ошибки, при которой поток чтения закрывается)

    procedure ReadFunc(Sender: TObject);                        // Функция, которая будет работать в потоке и ожидать данные от порта
    function PortRead(buf:PByte; len:TSize):Integer;            // Чтение данных из порта
    function PortSettings(br,db,sb,par,fc:cint):Integer;   // Функция выполняет настройку порта
  public
    constructor Create;                             // На вход передаётся имя библиотеки, которая хранит процедуры и функции для работы с последовательным портом)
    destructor Destroy; override;
    function OpenPort(name:string; speed:Integer; sb:Integer):Boolean;         // Открытие порта
    procedure ClosePort;                            // Закрытие порта
    function WritePort(buf:PByte; len:size_t):Integer; // Запись данных в порт
    function WritePort:Integer;                     // Запись данных в порт (выполняется запись в порт буфера OutBuf)

    procedure RTSDown;                              // Выход RTS у последовательного порта подтянуть к земле
    procedure RTSUp;                                // Выход RTS у последовательного порта подтянуть к плюсу питания
    procedure Flush;                                // Вроде как завершение всех отложеных операций

    function GetPortsList: TStrings;                // Функция определяет наличие COM-портов в системе

    property PortOpened:Boolean read FPortOpened;   // Свойство-признак того, что порт открыт
    property InBuf:TMemWork read FInBuf;            // Свойства для доступа к буферам
    property OutBuf:TMemWork read FOutBuf;
    property ErrStrings:TStrings read FErrStrings;
    property OnDataFromPort:TOnDataFromPort read FOnDataFromPort write FOnDataFromPort; // Событие которое запускается всякий раз когда приходят данные с порта
    property OnErrorReadPort:TNotifyEvent read FOnErrorReadPort write FOnErrorReadPort; // Событие которое запускается если дальнейшее чтение из порта невозможно
  end;


implementation

type
  TSpeedConst=record
    stdspd:Integer;
    brconst:Integer;
  end;

const
  BR_CONST_MASS:array [0..21] of TSpeedConst=(
                                              (stdspd:1200    ;   brconst:B1200),
                                              (stdspd:1800    ;   brconst:B1800),
                                              (stdspd:2400    ;   brconst:B2400),
                                              (stdspd:4800    ;   brconst:B4800),
                                              (stdspd:9600    ;   brconst:B9600),     // Эта скорость будет задаваться по умолчанию
                                              (stdspd:19200   ;   brconst:B19200),
                                              (stdspd:38400   ;   brconst:B38400),
                                              (stdspd:57600   ;   brconst:B57600),
                                              (stdspd:115200  ;   brconst:B115200),
                                              (stdspd:230400  ;   brconst:B230400),
                                              (stdspd:460800  ;   brconst:B460800),
                                              (stdspd:500000  ;   brconst:B500000),
                                              (stdspd:576000  ;   brconst:B576000),
                                              (stdspd:921600  ;   brconst:B921600),
                                              (stdspd:1000000 ;   brconst:B1000000),
                                              (stdspd:1152000 ;   brconst:B1152000),
                                              (stdspd:1500000 ;   brconst:B1500000),
                                              (stdspd:2000000 ;   brconst:B2000000),
                                              (stdspd:2500000 ;   brconst:B2500000),
                                              (stdspd:3000000 ;   brconst:B3000000),
                                              (stdspd:3500000 ;   brconst:B3500000),
                                              (stdspd:4000000 ;   brconst:B4000000)
                                            );



{ TSerialPortWork }

procedure TSerialPortWork.ReadFunc(Sender: TObject);
var
  tob:TSimpleThread;
  offset,bhalf:DWord;
  len,cnterr:Integer;
begin
  tob:=Sender as TSimpleThread;

  bhalf:=FInBuf.Size shr 1;
  offset:=0;
  len:=0;
  cnterr:=0;
  repeat
    len:=PortRead(FInBuf.Memory[offset], bhalf); // читаем данные, с порта
    if tob.CheckTerminated then Exit;
    if len=-3 then begin
      Inc(cnterr);
      if cnterr>50 then begin
        FErrStrings.Append('Ошибка при чтении с порта');
        FPortOpened:=False;
        FpClose(FFd);
        FFd:=-1;
        if Assigned(FOnErrorReadPort) then FOnErrorReadPort(Self);
        Exit;                          // Такая ошибка означает, что
      end;
      Sleep(20);
      Continue;
    end;
    cnterr:=0;

    if len>0 then begin
      if Assigned(FOnDataFromPort) then
        FOnDataFromPort(FInBuf.Memory[offset], len);
      if offset=0 then offset:=bhalf else offset:=0;  // Смена буфера
    end;
  until tob.CheckTerminated;
end;

function TSerialPortWork.PortRead(buf: PByte; len: TSize): Integer;
var
  mread:TFDSet;
  tmoutv:TimeVal;
  res,err:Integer;
begin
  fpFD_ZERO(mread);
  fpFD_SET(FFd, mread);

  tmoutv.tv_sec:=0;
  tmoutv.tv_usec:=100000;  // Предел времени ожидания 100 мс

  res:=fpSelect(FFd+1, @mread, nil, nil, @tmoutv);
  if res=0 then Exit(-1);
  if res<0 then Exit(-2);

  Result:=FpRead(FFd, PChar(buf), len);
  if Result<=0 then begin
    err:=fpgeterrno;
    if err=ESysEAGAIN then Exit(0) else Exit(-3);
  end;
end;

function TSerialPortWork.WritePort(buf: PByte; len: size_t): Integer;
var
  err:LongInt;
begin
  Result:=FpWrite(FFd, PChar(buf), len);
  if Result<=0 then begin
    err:=fpgeterrno;
    if err=ESysEAGAIN then Exit(0) else Exit(-1);
  end;
end;

constructor TSerialPortWork.Create;
begin
  FErrStrings:=TStringList.Create;  // Объект для работы со строками
  FPortOpened:=False; // Признак того, что порт открыт
  FInBuf:=TMemWork.Create(4096);   // Буфер для входных данных
  FOutBuf:=TMemWork.Create(2048);  // Буфер для выходных данных
end;

destructor TSerialPortWork.Destroy;
begin
  ClosePort; // Закрытие порта
  FreeAndNil(FInBuf); // Буфер для входных данных
  FreeAndNil(FOutBuf); // Буфер для выходных данных
  FreeAndNil(FErrStrings);  // Удаление списка строк с ошибками

  inherited Destroy;
end;

function TSerialPortWork.OpenPort(name: string; speed: Integer; sb: Integer): Boolean;
var
  res,i,br:Integer;
begin
  // name   - Имя устройства (имя последовательного порта)
  // speed  - Скорость определяется константами вида BR1200...BR4000000;
  // sb     - Кол-во стоп битов

  Result:=True;
  FErrStrings.Clear;

  if FPortOpened then begin
    FErrStrings.Add('Порт уже открыт');
    Exit;
  end;

  FFd:=FPOpen(name, O_RDWR or O_NOCTTY);

  if FFd<0 then begin
    FErrStrings.Add(Format('Ошибка в SerialPortOpen: FPOpen err=%d',[fpgeterrno]));
    Exit(False);
  end;

  if FFd>0 then begin
    if TCFlush(FFd, TCIOFLUSH)<>0 then begin  // Если очистка буфера прошла НЕ успешно, то
      FpClose(FFd);
      FErrStrings.Add(Format('Ошибка в SerialPortOpen: TCFlush err=%d',[fpgeterrno]));
      Exit(False);
    end;
  end;

  // Если порт открыт и очищен, то будут выполняться остальные действия

  // Через переданную скорость определяется наиболее близкая из списка разрещённых
  i:=Length(BR_CONST_MASS)-1;

  // Если скорость не входит в заданный диапазон, то устанавливается скорость по умолчанию 9600
  br:=B9600;
  if (speed>BR_CONST_MASS[i].stdspd)or(speed<BR_CONST_MASS[0].stdspd) then
    br:=BR_CONST_MASS[4].brconst
  else begin
    for i:=0 to i do begin
      if speed>=BR_CONST_MASS[i].stdspd then br:=BR_CONST_MASS[i].brconst;
      if speed<=BR_CONST_MASS[i].stdspd then Break;
    end;
  end;
  speed:=br;

  res:=PortSettings(speed, 8, sb, 0, 0);
  if res<>0 then begin
    FErrStrings.Add(Format('Ошибка в PortSettings err=%d',[fpgeterrno]));
    FpClose(FFd);
    Exit(False);
  end;

  FReadThread:=TSimpleThread.Create(True); // Объект для потока чтения данных
  FReadThread.OnExec:=@ReadFunc; // Назначаем функцию, которая будет работать в потоке
  FReadThread.FreeOnTerminate:=True;
  FReadThread.Start; // Запуск читающего потока

  FPortOpened:=True;
end;

function TSerialPortWork.PortSettings(br, db, sb, par, fc: cint): Integer;
var
  ttyset:Termios;
begin
  // fd   - Файловый дескриптор
  // br   - скорость работы порта (константы B115200, B921600 ...)
  // db   - кол-во бит в одном сообщении (5,6,7,8)
  // sb   - стоп биты 1, 2
  // par  - чётность 0,1,2 (0 - None; 1 - Odd = 1; 2 - PT_Even) бит чётности)
  // fc   - FlowControl 0,1

  FillByte(ttyset, SizeOf(Termios), 0);
  if (TCGetAttr(FFd, ttyset) <> 0) then Exit(-1);

  // Установка скорости работы порта
  CFSetISpeed(ttyset, br);
  CFSetOSpeed(ttyset, br);

  case fc of
    0:  ttyset.c_cflag:=ttyset.c_cflag and not CRTSCTS;
    1:  ttyset.c_cflag:=ttyset.c_cflag or CRTSCTS;
  end;

  case sb of
    1:  ttyset.c_cflag:=ttyset.c_cflag and not CSTOPB;
    2:  ttyset.c_cflag:=ttyset.c_cflag or CSTOPB;
  end;

  case par of
    0:  ttyset.c_cflag:=ttyset.c_cflag and not PARENB;
    1:  begin ttyset.c_cflag:=ttyset.c_cflag or PARENB; ttyset.c_cflag:=ttyset.c_cflag or PARODD; end;
    2:  begin ttyset.c_cflag:=ttyset.c_cflag or PARENB; ttyset.c_cflag:=ttyset.c_cflag and not PARODD; end;
  end;

  ttyset.c_cflag:=ttyset.c_cflag and not CS8; // Сброс битов, которые определяют кол-во бит для передачи (такое состояние соответствует CS5 (5 бит))
  case db of
    6: ttyset.c_cflag:=ttyset.c_cflag or CS6;
    7: ttyset.c_cflag:=ttyset.c_cflag or CS7;
    8: ttyset.c_cflag:=ttyset.c_cflag or CS8;
  end;

  ttyset.c_cflag:=ttyset.c_cflag or CREAD or CLOCAL;

  ttyset.c_cc[VMIN]:=1;   // Минимальное кол-во символов, которое принимается

  CFMakeRaw(ttyset);

  TCFlush(FFd, TCIFLUSH); // Flush Port, then applies attributes

  if TCSetAttr(FFd, TCSANOW, ttyset)<>0 then Exit(-2);

  Result:=0;
end;

procedure TSerialPortWork.ClosePort;
begin
  if not FPortOpened then Exit;

  FPortOpened:=False;
  FReadThread.Terminate; // Остановка работы потока чтения данных
  FReadThread.WaitFor;
  FpClose(FFd);
  FFd:=-1;
end;

function TSerialPortWork.WritePort: Integer;
begin
  Result:=0;
  if not FPortOpened then Exit;
  Result:=WritePort(FOutBuf.Memory[0], FOutBuf.Pos); // Передаём буфер в процедуру, которая записывает данные в порт
end;

procedure TSerialPortWork.RTSDown;        // Подтяжка сигнала RTS к земле
var
  flags:Int32;
begin
  // ВНИМАНИЕ!!! сигнал RTS инвертирован, поэтому тут всё наоборот по отношению к названию процедуры

  if not FPortOpened then Exit;
  FpIOCtl(FFd, TIOCMGET, @flags);         // Получение текущего состояния флагов
  if flags and TIOCM_RTS<>0 then Exit;    // Если бит уже установлен, то и устанавливать нечего
  flags:=flags or TIOCM_RTS;              // Установка бита приведёт к тому, что выход RTS будет притянут к плюсу
  FpIOCtl(FFd, TIOCMSET, @flags);         // Запись нового состояния
end;

procedure TSerialPortWork.RTSUp;          // Подтяжка сигнала RTS к плюсу
var
  flags:Int32;
begin
  // ВНИМАНИЕ!!! сигнал RTS инвертирован, поэтому тут всё наоборот по отношению к названию процедуры

  if not FPortOpened then Exit;
  FpIOCtl(FFd, TIOCMGET, @flags);         // Получение текущего состояния флагов
  if flags and TIOCM_RTS=0 then Exit;     // Если бита нет, то и сбрасывать нечего
  flags:=flags and (not TIOCM_RTS);       // Сброс бита приведёт к тому, что выход RTS будет притянут к земле
  FpIOCtl(FFd, TIOCMSET, @flags);         // Запись нового состояния
end;

procedure TSerialPortWork.Flush;
begin
  if not FPortOpened then Exit;
  TCFlush(FFd, TCIOFLUSH);
end;

function TSerialPortWork.GetPortsList: TStrings;
begin
  // Реализация этого в линуксе затруднительна
  Result:=TStringList.Create;
  if FileExists('/dev/ttyUSB0') then Result.Add('/dev/ttyUSB0');
  if FileExists('/dev/ttyUSB1') then Result.Add('/dev/ttyUSB1');
  if FileExists('/dev/ttyUSB2') then Result.Add('/dev/ttyUSB2');
  if FileExists('/dev/ttyUSB3') then Result.Add('/dev/ttyUSB3');
  if FileExists('/dev/ttyS0') then Result.Add('/dev/ttyS0');
  if FileExists('/dev/ttyS0') then Result.Add('/dev/ttyS0');
  if FileExists('/dev/ttyS1') then Result.Add('/dev/ttyS1');
  if FileExists('/dev/ttyS2') then Result.Add('/dev/ttyS2');
  if FileExists('/dev/ttyS3') then Result.Add('/dev/ttyS3');
end;


end.

