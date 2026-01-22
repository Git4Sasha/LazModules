unit TCPClient;

interface

uses
  Sockets
  , Classes
  , BaseUnix
  , sysutils
  ;

type

  { TTCPClient }

  TTCPClient=class
  private
    FClient:TSocket;                      // Сокет - Клиент
    FConnectNow:Boolean;                  // Признак наличия соединения
    FBlockSize:Integer;                   // Размер элементарного блока данных, на который будут делиться отправляемые и принимаемые данные
    FOnDisconnect:TNotifyEvent;           // Процедура-событие возникает, когда происходит разрыв соединения

    procedure DoDisconnect;               // Функция вызываемая при разрыве соединения
  public
    constructor Create;
    destructor Destroy; override;
    procedure Disconnect;                 // Процедура для разрыва связи с сервером
    function ConnectToServer(ServerName:string; port:Integer; timeoutms:Integer=5000):Boolean;
    function SendNBytes(p:Pointer; len:Integer):Boolean;                                            // Отправка данных серверу ( функция возвращает True если отправились все данные, иначе False )
    function SendNBytes(p:Pointer; len:Integer; var lenout:Integer):Boolean;                        // Отправка данных серверу ( функция возвращает True если отправились все данные, иначе False )
    function ReceiveNBytes(p:Pointer; len:Integer; var lenout:Integer):Boolean;                     // Принятие данных от сервера ( функция возвращает True если принялись все данные, иначе False )
    function ReceiveNBytes(p:Pointer; len:Integer; var lenout:Integer; timeout:Integer):Boolean;    // Принятие данных от сервера ( функция возвращает True если принялись все данные, иначе False )

    property ConnectNow:Boolean read FConnectNow; // Свойство для определения наличия соединения
    property OnDisconnect:TNotifyEvent read FOnDisconnect write FOnDisconnect; // Событие срабатывает в случае разрыва соединения
  end;

implementation

{ TTCPClient }

constructor TTCPClient.Create;
begin
  FConnectNow:=False;
  FBlockSize:=8192;
end;

destructor TTCPClient.Destroy;
begin
  Disconnect; // Закрываем соединение
  inherited;
end;

procedure TTCPClient.Disconnect;
begin
  fpshutdown(FClient, SHUT_RDWR);
  closesocket(FClient);
  FConnectNow:=False;
end;

function TTCPClient.ConnectToServer(ServerName: string; port: Integer; timeoutms: Integer): Boolean;
var
  addr:TSockAddr;
  timeout: TimeVal;
  opt,res:Integer;
begin
  // timeoutms - Предельное время ожидания соединения в миллисекундах

  if not FConnectNow then begin
    FClient:=fpsocket(AF_INET, SOCK_STREAM, 0); // Создаём ТСР/IP сокет

    //opt:=1;
    //res:=fpsetsockopt(FClient, SOL_TCP, TCP_NODELAY, @opt, 4);

    // Установка предельного времени приёма данных для сокета
    timeout.tv_sec:=timeoutms div 1000;         // Секунды предельного времени ожидания
    timeout.tv_usec:=(timeoutms mod 1000)*1000; // Микросекунды предельного времени ожидания
    if fpsetsockopt(FClient, SOL_SOCKET, SO_SNDTIMEO, @timeout, SizeOf(timeout))<>0 then Exit(False);

    // Формируем адрес сокета
    addr.sin_family:=AF_INET; // Семейство протоколов TCP/IP
    addr.sin_port:=htons(Port); // Задаём порт
    addr.sin_addr:=StrToNetAddr(PChar(ServerName)); // Задаём адрес сервера
    if fpconnect(FClient, @addr, SizeOf(addr))<>0 then // Если функция connect не завершилась 0-м, значит возникла ошибка
      closesocket(FClient) // закрываем сокет ( в связи с проблемой соединения с сервером )
    else
      FConnectNow:=True;
  end;
  Result:=FConnectNow;
end;

function TTCPClient.SendNBytes(p: Pointer; len: Integer): Boolean;
var
  l:Integer;
  pb:PByte;
begin
  // p        - адрес из которого берутся данные
  // len      - отправляеме кол-во байт

  pb:=PByte(p);
  Result:=True;                             // Изначально предпологается, что будут отправлены все данные
  while len<>0 do begin
    if len>FBlockSize then
      l:=fpsend(FClient, pb, FBlockSize, 0) // Отправка данных размером FBlockSize ( в l будер размер данных, которое удалось отправить )
    else
      l:=fpsend(FClient, pb, len, 0);       // Отправка данных размером len
    if l=-1 then begin                      // Если произошла ошибка, то
      DoDisconnect;                         // Отсоединяемся от сервера
      Result:=False;                        // Из-за ошибки возвращаем False
      Break;                                // Выходим из цикла
    end;
    len-=l;                                 // Уменшаем размер отправляемых данных на то количество байт, которое удалось отправить
    Inc(pb, l);                             // Сдвигаем указатель на то количество байт, которое удалось отпрвить
  end;
end;

function TTCPClient.ReceiveNBytes(p: Pointer; len: Integer; var lenout:Integer): Boolean;
var
  l:Integer;
  pb:PByte;
begin
  pb:=PByte(p);
  Result:=True; // Изначально предпологается, что будут приняты все данные
  lenout:=0;
  while len<>0 do begin
    if len>FBlockSize then
      l:=fprecv(FClient, pb, FBlockSize, 0) // Приём данных размером FBlockSize ( в l будер размер данных, которое удалось принять )
    else
      l:=fprecv(FClient, pb, len, 0); // Приём данных размером len
    if (l=-1)or(l=0) then begin       // Если произошла ошибка, то
      DoDisconnect;                   // Отсоединяемся от сервера
      Result:=False;                  // Из-за ошибки возвращаем False
      Break;                          // Выходим из цикла
    end;
    len-=l; // Уменшаем размер принимаемых данных на то количество байт, которое удалось принять
    lenout+=l; // Увеличиваем количество принятых байт (это количество есть выходной параметр)
    Inc(pb, l); // Сдвигаем указатель на то количество байт, которое удалось принять
  end;
end;

function TTCPClient.ReceiveNBytes(p: Pointer; len: Integer; var lenout: Integer; timeout:Integer): Boolean;
var
  l,res:Integer;
  pb:PByte;
  mread:TFDSet;
  tmoutv,tmoutc:TimeVal;
begin
  //  p       - адрес для складирования данных
  // len      - ожидаемое кол-во байт
  // lenout   - реально принятое кол-во байт (оно может быть меньше, если по какой-то прочине приём прервался)
  // timeout  - ограничение времени приёма в миллисекундах

  pb:=PByte(p);
  Result:=True;                           // Изначально предпологается, что будут приняты все данные
  lenout:=0;                              // Количество реально отправленных байт
  tmoutc.tv_sec:=timeout div 1000;         // Секунды ограничения ожидания приёма
  tmoutc.tv_usec:=(timeout mod 1000)*1000; // Микросекунды ограничение ожидания приёма

  while len<>0 do begin
    fpFD_ZERO(mread);
    fpFD_SET(FClient, mread);
    tmoutv:=tmoutc;                              // т.к. переменная timeout будет меняться после процедуры select, то её необходимо каждый раз загружать
    res:=fpSelect(FClient+1, @mread, nil, nil, @tmoutv);  // Ожидаем готовности чтения

    case res of
      0:  Exit(False);                            // Вышло время ожидания
     -1:  Exit(False);                            // Какая-то ошибка с сокетом
      1:  if fpFD_ISSET(FClient, mread)<>0 then begin  // Если в максе сокетов для чтения есть наш сокет, то можно считывать данные
            if len>FBlockSize then
              l:=fprecv(FClient, pb, FBlockSize, 0) // Приём данных размером FBlockSize ( в l будер размер данных, которое удалось принять )
            else
              l:=fprecv(FClient, pb, len, 0);       // Приём данных размером len
            if (l=-1)or(l=0) then begin             // Если произошла ошибка, то
              DoDisconnect;                         // Отсоединяемся от сервера
              Result:=False;                        // Из-за ошибки возвращаем False
              Break;                                // Выходим из цикла
            end;
            len-=l;                                 // Уменшаем размер принимаемых данных на то количество байт, которое удалось принять
            lenout+=l;                              // Увеличиваем количество принятых байт (это количество есть выходной параметр)
            Inc(pb, l);                             // Сдвигаем указатель на то количество байт, которое удалось принять
          end;
    end;
  end;
end;

function TTCPClient.SendNBytes(p: Pointer; len: Integer; var lenout:Integer): Boolean;
var
  l:Integer;
  pb:PByte;
begin
  // p        - адрес из которого берутся данные
  // len      - отправляеме кол-во байт
  // lenout   - реально отправленное кол-во байт (оно может быть меньше, если по какой-то причине отправка прервалась)

  pb:=PByte(p);
  Result:=True; // Изначально предпологается, что будут отправлены все данные
  lenout:=0; // Количество реально отправленных байт
  while len<>0 do begin
    if len>FBlockSize then
      l:=fpsend(FClient, pb, FBlockSize, 0) // Отправка данных размером FBlockSize ( в l будер размер данных, которое удалось отправить )
    else
      l:=fpsend(FClient, pb, len, 0); // Отправка данных размером len
    if l=-1 then begin                // Если произошла ошибка, то
      DoDisconnect; // Отсоединяемся от сервера
      Result:=False; // Из-за ошибки возвращаем False
      Break; // Выходим из цикла
    end;
    len-=l; // Уменшаем размер отправляемых данных на то количество байт, которое удалось отправить
    lenout+=l; // Увеличиваем количество переданных байт
    Inc(pb, l); // Сдвигаем указатель на то количество байт, которое удалось отпрвить
  end;
end;

procedure TTCPClient.DoDisconnect;
begin
  Disconnect; // Отсоединяемся
  if Assigned(OnDisconnect) then OnDisconnect(self);
end;

end.
