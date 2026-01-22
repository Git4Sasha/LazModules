unit TCPClient;

interface

uses
  WinSock2, Sockets, Classes;

type

  { TTCPClient }

  TTCPClient=class
  private
    FWSAData:WSAData; // Для инициализации модуля WinSock
    FClient:TSocket; // Сокет - Клиент
    FConnectNow:Boolean; // Признак наличия соединения
    FBlockSize:Integer; // Размер элементарного блока данных, на который будут делиться отправляемые и принимаемые данные
    FOnDisconnect:TNotifyEvent; // Процедура-событие возникает, когда происходит разрыв соединения

    procedure DoDisconnect; // Функция вызываемая при разрыве соединения
  public
    constructor Create;
    destructor Destroy; override;
    procedure Disconnect; // Процедура для разрывя связи с сервером
    function ConnectToServer(ServerName:string; port:Integer; timeoutms:Integer=5000):Boolean;
    function SendNBytes(p:Pointer; len:Integer; var lenout:Integer):Boolean; // Отправка данных серверу ( функция возвращает True если отправились все данные, иначе False )
    function ReceiveNBytes(p:Pointer; len:Integer; var lenout:Integer):Boolean; // Принятие данных от сервера ( функция возвращает True если принялись все данные, иначе False )
    function ReceiveNBytes(p:Pointer; len:Integer; var lenout:Integer; timeout:Integer):Boolean;    // Принятие данных от сервера ( функция возвращает True если принялись все данные, иначе False )

    property ConnectNow:Boolean read FConnectNow; // Свойство для определения наличия соединения
    property OnDisconnect:TNotifyEvent read FOnDisconnect write FOnDisconnect; // Событие срабатывает в случае разрыва соединения
  end;

implementation

{ TTCPClient }

constructor TTCPClient.Create;
begin
  WSAStartup($101, FWSAData);
  FConnectNow:=False;
  FBlockSize:=8192;
end;

destructor TTCPClient.Destroy;
begin
  Disconnect; // Закрываем соединение
  WSACleanup;
  inherited;
end;

procedure TTCPClient.Disconnect;
begin
  fpshutdown(FClient, SD_BOTH);
  closesocket(FClient);
  FConnectNow:=False;
end;

function TTCPClient.ConnectToServer(ServerName: string; port: Integer; timeoutms: Integer): Boolean;
var
  addr:TSockAddr;
  timeout:DWord;
begin
  // timeoutms - Предельное время ожидания соединения в миллисекундах

  if not FConnectNow then
    begin
      FClient:=socket(AF_INET, SOCK_STREAM, 0); // Создаём ТСР/IP сокет

      // ХОТЬ ЭТО И СДЕЛАННО, НО В ВИНДОВС ЭТО НЕ РАБОТАЕТ (В ЛИНУКСЕ РАБОТАЕТ)
      // СДЕЛАННО НА ВСЯКИЙ СЛУЧАЙ, ВДРУГ В ДРУГИХ ВЕРСИЯХ ЗАРАБОТАЕТ
      // Установка предельного времени приёма данных для сокета
      timeout:=timeoutms;
      if fpsetsockopt(FClient, SOL_SOCKET, SO_SNDTIMEO, @timeout, SizeOf(timeout))<>0 then Exit(False);

      // Формируем адрес сокета
      addr.sin_family:=AF_INET; // Семейство протоколов TCP/IP
      addr.sin_port:=htons(Port); // Задаём порт
      addr.sin_addr.S_addr:=inet_addr(PChar(ServerName)); // Задаём адрес сервера
      if fpconnect(FClient, @addr, SizeOf(addr))<>0 then // Если функция connect не завершилась 0-м, значит возникла ошибка
        closesocket(FClient) // закрываем сокет ( в связи с проблемой соединения с сервером )
      else
        FConnectNow:=True;
    end;
  Result:=FConnectNow;
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
      l:=recv(FClient, pb^, FBlockSize, 0) // Принимаем данных размером FBlockSize ( в l будер размер данных, которое удалось принять )
    else
      l:=recv(FClient, pb^, len, 0); // Принимаем данных размером len
    if (l=-1)or(l=0) then begin // Если произошла ошибка, то
      DoDisconnect; // Отсоединяемся от сервера
      Result:=False; // Из-за ошибки возвращаем False
      Break; // Выходим из цикла
    end;
    len-=l; // Уменшаем размер принимаемых данных на то количество байт, которое удалось принять
    lenout+=l; // Увеличиваем количество принятых байт (это количество есть выходной параметр)
    Inc(pb, l); // Сдвигаем указатель на то количество байт, которое удалось принять
  end;
end;

function TTCPClient.ReceiveNBytes(p: Pointer; len: Integer; var lenout: Integer; timeout: Integer): Boolean;
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
    FD_ZERO(mread);
    FD_SET(FClient, mread);
    tmoutv:=tmoutc;                              // т.к. переменная timeout будет меняться после процедуры select, то её необходимо каждый раз загружать
    res:=Select(0, @mread, nil, nil, @tmoutv);  // Ожидаем готовности чтения (в Windows 1-й парамтр функции Select игнорируется)

    case res of
      0:  Exit(False);                            // Вышло время ожидания
     -1:  Exit(False);                            // Какая-то ошибка с сокетом
      1:  if FD_ISSET(FClient, mread) then begin  // Если в максе сокетов для чтения есть наш сокет, то можно считывать данные
            if len>FBlockSize then
              l:=Recv(FClient, pb, FBlockSize, 0) // Приём данных размером FBlockSize ( в l будер размер данных, которое удалось принять )
            else
              l:=Recv(FClient, pb, len, 0);       // Приём данных размером len
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
  pb:=PByte(p);
  Result:=True; // Изначально предпологается, что будут отправлены все данные
  lenout:=0; // Количество реально отправленных байт
  while len<>0 do begin
    if len>FBlockSize then
      l:=send(FClient, pb^, FBlockSize, 0) // Отправка данных размером FBlockSize ( в l будер размер данных, которое удалось отправить )
    else
      l:=send(FClient, pb^, len, 0); // Отправка данных размером len
    if l=-1 then begin // Если произошла ошибка, то
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
  if Assigned(OnDisconnect) then
    OnDisconnect(self);
end;

end.
