unit LocalUDPSocket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WinSock2, Sockets;

type

  { TLocalUDPSocket }

  TLocalUDPSocket=class
  private
    FWSAData:TWSAData;          // Переменная изпользуется в функциях WSAStartup
    FSocket:Tsocket;            // Идентификатор для сокета
    FLastError:Integer;         // Ошибка при последней операции
    FSockAddrSend:TSockAddr;    // Адрес на который будут передаваться данные в функциях, в которых адрес не указывается
    FSockAddRecv:TSockAddr;     // Адрес того, кто отправляет данные к нам (это поле заполняется в функциях RecvData)
    FBackAddrPresent:Boolean;   // Признак того, что есть адрес для обратной отправки пакета (такой адрес появляется если были приняты пакеты с помощью функций RecvData)
    FBindAddr:TSockAddr;        // Адрес к которому привязан сокет с помощью функции bind (формируется в процедуре SetRecvIPAndPort)
    FBindAddrPresent:Boolean;   // Признак того, что сокет привязан к какому-либо адресу и порту

    function GetBindIP: string;
    function GetBindPort: Word;
    function GetRemoteIP: string;
    function GetRemotePort: Word;
  public
    constructor Create;
    destructor Destroy; override;

    function CreateSocket(block:Boolean=True):Integer;     // Создание сокета
    procedure CloseSoc;
    function SetRecvIPAndPort(port:Word; ip:string=''):Integer; // Задание порта, по которому будут ожидаться данные
    procedure SetSendIpAndPort(port:Word; ip:string=''); // Задание порта и IP адреса, на которые будут передаваться данные
    function SetBackIpAndPort:Boolean;                    // Эта функция устанавливает адрес для передачи данных таким с которого данные пришли
    function SendData(buf: Pointer; sz: Integer): Integer;  overload; // Передача данных без указания имени получателя
    function SendData(SockAddr:TSockAddr; buf: Pointer; sz: Integer): Integer; overload; // Передача данных с указанием имени получателя
    function Send32Bit(SockAddr:TSockAddr; val:Integer):Integer; overload; // Передача 32-х битного чила через локальные сокеты
    function Send32Bit(val:Integer):Integer; overload;  // Передача 32-х битного чила через локальные сокеты
    function RecvData(buf:Pointer; sz, tlim:Integer):Integer; overload;
    function RecvData(buf:Pointer; sz:Integer):Integer; overload;
    function SendBroadcast(buf:Pointer; sz, port:Integer):Integer;  // Отправка широковещательного сообщения
    function SetSendBuf(bsize:Integer):Boolean; // Функция задаёт размер буфера сокета для отправкм данных
    function SetRecvBuf(bsize:Integer):Boolean; // Функция задаёт размер буфера сокета для приёма данных

    property LastError:Integer read FLastError;
    property RemoteIP:string read GetRemoteIP;        // Адрес удалённой машины, от которой были приняты данные
    property RemotePort:Word read GetRemotePort;      // Порт с которого были приняты данные
    property BindIP:string read GetBindIP;            // Получение сетевого адреса к которому привязан сокет
    property BindPort:Word read GetBindPort;          // Получение порта к которому привязан сокет
  end;

implementation

{ TLocalUDPSocket }

procedure TLocalUDPSocket.CloseSoc;
begin
  if FSocket<>-1 then begin
    shutdown(FSocket, SHUT_RDWR);  // Вызываетмя для прекращения всех операций на чтение и на запись (потипу sync)
    CloseSocket(FSocket);
    FSocket:=-1;
    FBindAddrPresent:=False;
  end;
end;

function TLocalUDPSocket.GetRemoteIP: string;
begin
  if not FBackAddrPresent then Exit(''); // Если нет адреса для обратной отправки, то возвращается пустая строка
  Result:=WinSock2.inet_ntoa(WinSock2.TInAddr(FSockAddRecv.sin_addr));
end;

function TLocalUDPSocket.GetBindIP: string;
begin
  if not FBindAddrPresent then Exit(''); // Если сокет не привязан, то возвращается пустая строка
  Result:=WinSock2.inet_ntoa(WinSock2.TInAddr(FBindAddr.sin_addr));
end;

function TLocalUDPSocket.GetBindPort: Word;
begin
  if not FBindAddrPresent then Exit(0); // Если сокет не привязан, то возвращается нулевой порт
  Result:=NToHs(FBindAddr.sin_port);
end;

function TLocalUDPSocket.GetRemotePort: Word;
begin
  if not FBackAddrPresent then Exit(0); // Если нет адреса для обратной отправки, то возвращается нулевой порт (скорее всего по такому порту отправить ничего нельзя)
  Result:=NToHs(FSockAddRecv.sin_port);
end;

constructor TLocalUDPSocket.Create;
begin
  WSAStartup($101, FWSAData);
  FSocket:=-1;
end;

destructor TLocalUDPSocket.Destroy;
begin
  CloseSoc;
  WSACleanup;
  inherited Destroy;
end;

function TLocalUDPSocket.CreateSocket(block: Boolean): Integer;
var
  val:u_long;
begin
  Result:=0;

  CloseSoc; // Закрытие сокета, если он был открыт до этого

  FSocket:=socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP); // Создаём cокет для чтения/записи UDP пакетов
  if FSocket=-1 then begin
    FLastError:=socketerror;
    FSocket:=-1;
    Exit(-1);
  end;

  val:=1;
  fpsetsockopt(FSocket, SOL_SOCKET, SO_REUSEADDR, @val, 4);

  // Чтобы сокет был не блокирующим
  if not block then begin
    val:=1;
    ioctlsocket(FSocket, FIONBIO, val);
  end;

  FLastError:=0;
end;

function TLocalUDPSocket.SetRecvIPAndPort(port: Word; ip: string): Integer;
begin
  // port Тут указывается порт, с которым будет связан сокет, по этому порту выполняется ожидание данных
  if FSocket=-1 then Exit(-1); // Если сокет ещё не создавался, то выходим с ошибкой

  // Адрес с которым связан сокет, на этот адрес необходимо передавать данные, чтобы их получить
  FillChar(FBindAddr, SizeOf(FBindAddr), 0);
  FBindAddr.sin_family:=AF_INET;
  FBindAddr.sin_port:=htons(port); // Задаём порт
  if ip='' then
    FBindAddr.sin_addr.s_addr:=INADDR_ANY // IP адрес будет выбран автоматически
  else
    FBindAddr.sin_addr.s_addr:=inet_addr(PChar(ip)); // IP адрес задаётся пользователем

  if bind(FSocket, @FBindAddr, SizeOf(FBindAddr))<>0 then begin
    FLastError:=socketerror;
    FillChar(FBindAddr, SizeOf(FBindAddr), 0);    // Если не удалось привязать сокет к адресу, то поле обнуляется
    Exit(-2);
  end;

  FBindAddrPresent:=True;
  FLastError:=0;
  Result:=0;
end;

procedure TLocalUDPSocket.SetSendIpAndPort(port: Word; ip: string); // Задание порта и IP адреса, на которые будут передаваться данные
begin
  // Формируем адрес и порт
  FillChar(FSockAddrSend.sin_zero, SizeOf(FSockAddrSend.sin_zero), 0);
	FSockAddrSend.sin_family:=AF_INET; // Семейство протоколов TCP
  if ip='' then
    FSockAddrSend.sin_addr.S_addr:=inet_addr('127.0.0.1') // Если адрес для отправки не задан, то задаётся локальный адрес машины
  else
    FSockAddrSend.sin_addr.S_addr:=inet_addr(PChar(ip)); // Если адрес задан, то изпользуем, то что передали
  FSockAddrSend.sin_port:=htons(port); // Задаём порт
end;

function TLocalUDPSocket.SetBackIpAndPort: Boolean; // Эта функция устанавливает адрес для передачи данных таким с которого данные пришли
begin
  Result:=FBackAddrPresent; // Если хоть раз был приём данных, то будет и обратный адрес для отправки
  if not Result then Exit;
  // Адрес для отправки данных делаем равным тому адресу, с которого приходили данные
  FSockAddrSend:=FSockAddRecv;
end;

function TLocalUDPSocket.SendData(buf: Pointer; sz: Integer): Integer;
begin
  Result:=SendData(FSockAddrSend, buf, sz);
end;

function TLocalUDPSocket.SendData(SockAddr: TSockAddr; buf: Pointer; sz: Integer): Integer;
begin
  Result:=sendto(FSocket, buf, sz, 0, @SockAddr, SizeOf(SockAddr)); // Отправка данных
  if Result=-1 then begin
    FLastError:=socketerror;
    Exit;
  end;
  if (Result<>sz) then
    FLastError:=socketerror;
end;

function TLocalUDPSocket.RecvData(buf: Pointer; sz, tlim: Integer): Integer;
var
  mread:TFDSet;
  timeout: TimeVal;
  fromlen:LongInt;
begin
  // tlim - предел времени ожидания задаётся в миллисекундах

  FD_ZERO(mread);
  FD_SET(FSocket, mread);
  timeout.tv_sec:=tlim div 1000;          // Формирование кол-ва секунд из миллисекунд
  timeout.tv_usec:=1000*(tlim mod 1000);  // Формирование кол-ва микросекунд из миллисекунд ( (tlim mod 1000) - Кол-во миллисекунд между секундами )
  Result:=select(FSocket+1, @mread, nil, nil, @timeout); // Ожидаем готовности чтения

  case Result of
    -1: FLastError:=socketerror;
     0: Exit(-2); // Вышло время ожидания
     1: if FD_ISSET(FSocket, mread) then begin
          fromlen:=SizeOf(FSockAddRecv);
          Result:=RecvFrom(FSocket, buf, sz, 0, @FSockAddRecv, @fromlen); // Читаем данные с сокета
          if Result<0 then begin // Если результат отрицательный, значит проверяем ошибку
            FLastError:=socketerror;
            Exit(-3);
          end;
          FBackAddrPresent:=True;  // Установка признака того, что существует обратный адрес для передачи данных
        end;
  else
    Exit(-4);
  end;
end;

function TLocalUDPSocket.RecvData(buf: Pointer; sz: Integer): Integer;
var
  fromlen:LongInt;
begin
  fromlen:=SizeOf(FSockAddRecv);
  Result:=RecvFrom(FSocket, buf, sz, 0, @FSockAddRecv, @fromlen); // Читаем данные с сокета
  if Result<0 then begin // Если результат отрицательный, значит проверяем ошибку
    FLastError:=socketerror;
    Exit(-1);
  end;
  FBackAddrPresent:=True;  // Установка признака того, что существует обратный адрес для передачи данных
end;

function TLocalUDPSocket.Send32Bit(SockAddr: TSockAddr; val: Integer): Integer; // Передача 32-х битного чила через локальные сокеты
begin
  Result:=SendData(SockAddr, @val, 4);
end;

function TLocalUDPSocket.Send32Bit(val:Integer):Integer; // Передача 32-х битного чила через локальные сокеты
begin
  Result:=SendData(FSockAddrSend, @val, 4);
end;

function TLocalUDPSocket.SendBroadcast(buf: Pointer; sz, port: Integer): Integer; // Отправка широковещательного пакета
var
  i:Integer;
  addr:TSockAddr;
begin
  if FSocket=-1 then Exit(-1);

  Result:=0;  // Если результат останется 0-м, значит функция отработала без ошибок

  i:=1; // Это, чтобы задать 1-цу в качестве параметра
  i:=fpsetsockopt(FSocket, SOL_SOCKET, SO_BROADCAST, @i, SizeOf(i)); // Устанавливаем сокет широковещательным

  if i<0 then Exit(-2);

  // Формируем адрес и порт для широковещательного сокета
  FillChar(addr.sin_zero, SizeOf(addr.sin_zero), 0);
  addr.sin_family:=AF_INET;         // Семейство протоколов TCP (AF - adress family)
  addr.sin_addr.s_addr:=$FFFFFFFF;  // Широковещательный адрес
  addr.sin_port:=htons(port);       // Задаём порт

  i:=fpsendto(FSocket, buf, sz, 0, @addr, SizeOf(addr)); // Отправка широковещательного сообщения
  if i<0 then Exit(-3);

  // снятие признака широковещательности с сокета
  i:=0;
  i:=fpsetsockopt(FSocket, SOL_SOCKET, SO_BROADCAST, @i, SizeOf(i)); // Устанавливаем сокет широковещательным
end;

function TLocalUDPSocket.SetSendBuf(bsize: Integer): Boolean;
begin
  if FSocket=-1 then Exit(False);
  if bsize<=0 then Exit(False);
  if setsockopt(FSocket, SOL_SOCKET, SO_SNDBUF, @bsize, 4)<>0 then begin
    FLastError:=socketerror;
    Result:=False;
  end;
  Result:=True;
end;

function TLocalUDPSocket.SetRecvBuf(bsize: Integer): Boolean; // Функция задаёт размер буфера сокета для приёма данных
begin
  if FSocket=-1 then Exit(False);
  if bsize<=0 then Exit(False);
  if setsockopt(FSocket, SOL_SOCKET, SO_RCVBUF, @bsize, 4)<>0 then begin
    FLastError:=socketerror;
    Result:=False;
  end;
  Result:=True;
end;


end.

