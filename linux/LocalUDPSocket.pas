unit LocalUDPSocket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix, Sockets;

type

  { TLocalUDPSocket }

  TLocalUDPSocket=class
  private
    FSocket:Tsocket;          // Идентификатор для сокета
    FLastError:Integer;       // Ошибка при последней операции ( чтобы подробнее узнать о кодах ошибок можно перейти в модуль Sockets, а от туда в errno.inc )
    FSockAddrSend:TSockAddr;  // Адрес на который будут передаваться данные в функциях, в которых адрес не указывается
    FSockAddRecv:TSockAddr;   // Адрес того, кто отправляет данные к нам (это поле заполняется в функциях RecvData)
    FBackAddrPresent:Boolean; // Признак того, что есть адрес для обратной отправки пакета (такой адрес появляется если были приняты пакеты с помощью функций RecvData)
    FBindAddr:TSockAddr;        // Адрес к которому привязан сокет с помощью функции bind (формируется в процедуре SetRecvIPAndPort)
    FBindAddrPresent:Boolean;   // Признак того, что сокет привязан к какому-либо адресу и порту

    function GetBindIP: string;
    function GetBindPort: Word;
    function GetRemoteIP: string;
    function GetRemotePort: Word;
  public
    constructor Create;
    destructor Destroy; override;

    function CreateSocket(block:Boolean=True):Integer;                      // Создание сокета, который будет изпользоваться для приёма и передачи данных
    procedure CloseSoc;
    function SetRecvIpAndPort(port:Word; ip:string=''):Integer;             // Формирование порта, по которому будут ожидаться данные
    procedure SetSendIpAndPort(port:Word; ip:string='');                    // Задание порта и IP адреса, на которые будут передаваться данные
    function SetBackIpAndPort:Boolean;                                      // Эта функция устанавливает адрес для передачи данных тому от кого данные пришли
    function SendData(buf: Pointer; sz: Integer): Integer;  overload;       // Передача данных без указания имени получателя
    function SendData(SockAddr:TSockAddr; buf: Pointer; sz: Integer): Integer; overload; // Передача данных с указанием имени получателя
    function Send32Bit(SockAddr:TSockAddr; val:Integer):Integer; overload;  // Передача 32-х битного чила через локальные сокеты
    function Send32Bit(val:Integer):Integer; overload;                      // Передача 32-х битного чила через локальные сокеты
    function RecvData(buf:Pointer; sz, tlim:Integer):Integer; overload;
    function RecvData(buf:Pointer; sz:Integer):Integer; overload;
    function SendBroadcast(buf:Pointer; sz, port:Integer;ip:string=''):Integer; // Отправка широковещательного сообщения
    function SetSendBuf(bsize:Integer):Boolean;                             // Функция задаёт размер буфера сокета для отправкм данных
    function SetRecvBuf(bsize:Integer):Boolean;                             // Функция задаёт размер буфера сокета для приёма данных
    function GetSockAddr(out port:Word; out ip:string):Integer;             // Процедура возвращает порт и ip-адрес к которым привязан сокет

    property LastError:Integer read FLastError;
    property RemoteIP:string read GetRemoteIP;                              // Адрес удалённой машины, от которой были приняты данные
    property RemotePort:Word read GetRemotePort;                            // Порт с которого были приняты данные
    property BindIP:string read GetBindIP;                                  // Получение сетевого адреса к которому привязан сокет
    property BindPort:Word read GetBindPort;                                // Получение порта к которому привязан сокет
  end;

implementation

{ TLocalUDPSocket }


function TLocalUDPSocket.GetRemoteIP: string;
begin
  if not FBackAddrPresent then Exit(''); // Если нет адреса для обратной отправки, то возвращается пустая строка
  Result:=NetAddrToStr(FSockAddRecv.sin_addr);
end;

function TLocalUDPSocket.GetRemotePort: Word;
begin
  if not FBackAddrPresent then Exit(0); // Если нет адреса для обратной отправки, то возвращается нулевой порт (скорее всего по такому порту отправить ничего нельзя)
  Result:=NToHs(FSockAddRecv.sin_port);
end;

constructor TLocalUDPSocket.Create;
begin
  FSocket:=-1;
end;

function TLocalUDPSocket.GetBindIP: string;
begin
  if not FBindAddrPresent then Exit(''); // Если сокет не привязан, то возвращается пустая строка
  Result:=NetAddrToStr(FBindAddr.sin_addr);
end;

function TLocalUDPSocket.GetBindPort: Word;
begin
  if not FBindAddrPresent then Exit(0); // Если сокет не привязан, то возвращается нулевой порт
  Result:=NToHs(FBindAddr.sin_port);
end;

destructor TLocalUDPSocket.Destroy;
begin
  inherited Destroy;
  CloseSoc;
end;

function TLocalUDPSocket.CreateSocket(block: Boolean): Integer;
var
  flags:cint;
begin
  // SockPort Тут указывается порт, с которым будет связан сокет, по этому порту выполняется ожидание данных

  Result:=0;  // Такой результат будет означать, что функция отработала нормально
  CloseSoc; // Закрытие сокета, если он был открыт до этого

  FSocket:=fpsocket(AF_INET, SOCK_DGRAM, IPPROTO_UDP); // Создаём cокет для чтения/записи UDP пакетов
  if FSocket=-1 then begin
    FLastError:=socketerror;    // чтобы подробнее узнать о кодах ошибок можно перейти в модуль Sockets, а от туда в errno.inc
    FSocket:=-1;
    Exit(-1);
  end;

  // Чтобы сокет был не блокирующим
  if not block then begin
    flags:=FpFcntl(FSocket, F_GETFL, 0);
    if FpFcntl(FSocket, F_SETFL, flags or O_NONBLOCK)=-1 then begin
      FLastError:=socketerror;                                        // чтобы подробнее узнать о кодах ошибок можно перейти в модуль Sockets, а от туда в errno.inc
      Sockets.CloseSocket(FSocket);
      FSocket:=-1;
    end;
  end;


  // fpgetsockname(); - Реализовать эту возможность

  FLastError:=0;
end;

procedure TLocalUDPSocket.CloseSoc;
begin
  if FSocket<>-1 then begin
    //fpshutdown(FSocket, SHUT_RDWR);  // shutdown - это только для сокетов, предполагающих наличие соединения
    CloseSocket(FSocket);
    FSocket:=-1;
    FBindAddrPresent:=False;
  end;
end;

function TLocalUDPSocket.SetRecvIpAndPort(port: Word; ip: string): Integer;  // Формирование порта, по которому будут ожидаться данные
begin
  if FSocket=-1 then Exit(-1);

  // port Тут указывается порт, с которым будет связан сокет, по этому порту выполняется ожидание данных

  // Адрес с которым связан сокет, на этот адрес необходимо передавать данные, чтобы их получить
  FillChar(FBindAddr, SizeOf(FBindAddr), 0);
  FBindAddr.sin_family:=AF_INET;
  FBindAddr.sin_port:=htons(port); // Задаём порт (если порт задаётся нулевым, то система сама выдаст свободный порт для сокета)
  if ip='' then
    FBindAddr.sin_addr.s_addr:=INADDR_ANY  // если задан INADDR_ANY, то сокет будет связан со всеми локальными сетевыми интерфейсами.
  else
    FBindAddr.sin_addr:=StrToNetAddr(ip); // IP адрес задаётся пользователем

  // Любая привязка сокета к порту приводит к тому, что он будет привязан к данному порту до тех пор, пока не будет закрыт
  // Привязать сокет к другому порту можно только через его закрытие.
  // Ошибка будет даже при попытке привязать сокет к тому адресу и порту, к которым он уже привязан
  // Привязка сокета к нулевому порту означает, что порт ему выдаст сама система

  if fpbind(FSocket, @FBindAddr, SizeOf(FBindAddr))<>0 then begin
    FLastError:=socketerror;                      // чтобы подробнее узнать о кодах ошибок можно перейти в модуль Sockets, а от туда в errno.inc
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
    FSockAddrSend.sin_addr:=StrToNetAddr('127.0.0.1') // Если адрес для отправки не задан, то задаётся локальный адрес машины
  else
    FSockAddrSend.sin_addr:=StrToNetAddr(ip); // Если адрес задан, то изпользуем, то что передали
  FSockAddrSend.sin_port:=htons(port);        // Задаём порт
end;

function TLocalUDPSocket.SetBackIpAndPort: Boolean; // Эта функция устанавливает адрес для передачи данных таким с которого данные пришли
begin
  Result:=FBackAddrPresent; // Если хоть раз был приём данных, то будет и обратный адрес для отправки
  if not Result then Exit;
  // Адрес для отправки данных делаем равным тому адресу, с которого приходили данные
  FSockAddrSend:=FSockAddRecv;
end;

function TLocalUDPSocket.SendData(buf: Pointer; sz: Integer): Integer; // передача данных по адресу, который ранее был установлен функцией SetSendIpAndPort
begin
  Result:=SendData(FSockAddrSend, buf, sz); // FSockAddrSend - адрес, который устанавливается функцией FSockAddrSend
end;

function TLocalUDPSocket.SendData(SockAddr: TSockAddr; buf: Pointer; sz: Integer): Integer; // Передача данных на адрес, который сформирова из вне
begin
  // Если сокет не привязан к порту (функцией bind)
  // и через него отправляется что-либо с помощью функции sendto,
  // то система сама привязывает сокет к некоторому свободному порту,
  // после чего его уже нельзя привязать к другому порту, только через закрытие сокета

  Result:=fpsendto(FSocket, buf, sz, 0, @SockAddr, SizeOf(SockAddr)); // Отправка данных
  if Result=-1 then begin
    FLastError:=socketerror;    // чтобы подробнее узнать о кодах ошибок можно перейти в модуль Sockets, а от туда в errno.inc
    Exit(-1);
  end;
  if (Result<>sz) then
    FLastError:=socketerror;  // чтобы подробнее узнать о кодах ошибок можно перейти в модуль Sockets, а от туда в errno.inc
end;

function TLocalUDPSocket.Send32Bit(SockAddr: TSockAddr; val: Integer): Integer;
begin
  Result:=SendData(SockAddr, @val, 4);
end;

function TLocalUDPSocket.Send32Bit(val: Integer): Integer;
begin
  Result:=SendData(FSockAddrSend, @val, 4);
end;

function TLocalUDPSocket.RecvData(buf: Pointer; sz, tlim: Integer): Integer;
var
  mread:TFDSet;
  timeout: TimeVal;
  fromlen:LongInt;
begin
  // tlim - предел времени ожидания задаётся в миллисекундах

  fpFD_ZERO(mread);
  fpFD_SET(FSocket, mread);

  timeout.tv_sec:=tlim div 1000;          // Формирование кол-ва секунд из миллисекунд
  timeout.tv_usec:=1000*(tlim mod 1000);  // Формирование кол-ва микросекунд из миллисекунд ( (tlim mod 1000) - Кол-во миллисекунд между секундами )
  Result:=fpselect(FSocket+1, @mread, nil, nil, @timeout); // Ожидаем готовности чтения
  case Result of
    -1: FLastError:=socketerror;   // чтобы подробнее узнать о кодах ошибок можно перейти в модуль Sockets, а от туда в errno.inc
     0: Exit(-2); // Вышло время ожидания
     1: if fpFD_ISSET(FSocket, mread)<>0 then begin
          fromlen:=SizeOf(FSockAddRecv);
          Result:=fpRecvFrom(FSocket, buf, sz, 0, @FSockAddRecv, @fromlen); // Читаем данные с сокета
          if Result<0 then begin // Если результат отрицательный, значит проверяем ошибку
            FLastError:=socketerror;  // чтобы подробнее узнать о кодах ошибок можно перейти в модуль Sockets, а от туда в errno.inc
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
  Result:=fpRecvFrom(FSocket, buf, sz, 0, @FSockAddRecv, @fromlen); // Читаем данные с сокета
  if Result<0 then begin // Если результат отрицательный, значит проверяем ошибку
    FLastError:=socketerror;  // чтобы подробнее узнать о кодах ошибок можно перейти в модуль Sockets, а от туда в errno.inc
    Exit(-1);
  end;
  FBackAddrPresent:=True;  // Установка признака того, что существует обратный адрес для передачи данных
end;

function TLocalUDPSocket.SendBroadcast(buf: Pointer; sz, port: Integer; ip: string): Integer; // Отправка широковещательного сообщения
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

  if ip='' then
    addr.sin_addr.s_addr:=$FFFFFFFF  // Широковещательный адрес (этот адрес скорее всего работать не будет, его не допустит система, поэтому широковещательные сообщения нужно отправлять как-то по другому)
  else
    addr.sin_addr:=StrToNetAddr(ip);
  // Чтобы система дала отправлять широковещательное сообщение нужно ограничить сетевой адрес (пример показан ниже)
  //addr.sin_addr:=StrToNetAddr('192.168.0.255'); // по такому адресу будут отправляться широковещательные сообщения в нулевой подсети

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
  if fpsetsockopt(FSocket, SOL_SOCKET, SO_SNDBUF, @bsize, 4)<>0 then begin
    FLastError:=socketerror;  // чтобы подробнее узнать о кодах ошибок можно перейти в модуль Sockets, а от туда в errno.inc
    Result:=False;
  end;
  Result:=True;
end;

function TLocalUDPSocket.SetRecvBuf(bsize: Integer): Boolean;
begin
  if FSocket=-1 then Exit(False);
  if bsize<=0 then Exit(False);
  if fpsetsockopt(FSocket, SOL_SOCKET, SO_RCVBUF, @bsize, 4)<>0 then begin
    FLastError:=socketerror;  // чтобы подробнее узнать о кодах ошибок можно перейти в модуль Sockets, а от туда в errno.inc
    Result:=False;
  end;
  Result:=True;
end;

function TLocalUDPSocket.GetSockAddr(out port: Word; out ip: string): Integer;
var
  sadr:TSockAddr;
  alen:TSockLen;
  res:cint;
begin
  port:=0;
  ip:='';
  Result:=-1;

  alen:=SizeOf(sadr); // Перед вызовом fpGetSockName в этот параметр нужно занести размер выделенный под имя сокета (sadr)
  res:=fpGetSockName(FSocket, @sadr, @alen);
  if res=-1 then begin
    FLastError:=socketerror;  // чтобы подробнее узнать о кодах ошибок можно перейти в модуль Sockets, а от туда в errno.inc
    Exit;
  end;

  port:=NToHs(sadr.sin_port);
  ip:=NetAddrToStr(sadr.sin_addr);
  Result:=0;
end;

end.

