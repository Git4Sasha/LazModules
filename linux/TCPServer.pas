// Это болванка для создания на её основе сервера, который будет реализовать тот или иной протокол передачи данных
unit TCPServer;

interface

uses
  Sockets
  , SimpleThread
  , MemoryWork
  , Classes
  , SysUtils
  , BaseUnix


  ;

type
  TClientDisconnectReasons=(cdrCloseServer, cdrHaveToDisconnectNow, cdrTimeOutNoActive, cdrTimeOutConnection, cdrClientCaseInRead, cdrClientCaseInWrite); // константы причин отключения клиентов

  { TSocketClient }

  TSocketClient=class
  private
    Fcsock: Integer;                                // идентификатор сокета клиента
    Fipaddess: string;                              // IP адрес клиента ( для подключённого клиента )
    FTimeConnect:QWord;                             // Время подсоединения клиента к серверу
    FTimeLastActive:QWord;                          // Время последней активности клиента
    FtoNotActive:QWord;                             // Время в миллисекундах, которое задаёт то, сколько сервер будет держать связь с клиентом с момента последней активности клиента
    FtoConnection:QWord;                            // Время в миллисекундах, которое задаёт то, сколько сервер будет держать связь с клиентом с момента подключение ( сервер разорвёт связь по истечении этого времени в любом случае )
    FBufForSend:Pointer;                            // Это буфер для отравления данных клиенту ( клиент не занимается освобождением памяти от буфера после его оправки, это должна делать внешняя функция при обработке события - "отправка завершена" (событие генерирует сервер) )
    FSizeBufSend:Integer;                           // Хранит количество отправляемых данных
    FCloseConnectNow:Boolean;                       // Если это поле равно True, значит сервер должен в ближайшее время разорвать соединение с клиентом
    FDisconnectReason:TClientDisconnectReasons;     // Это поле хранит код причины отключения клиента ( он может быть изпользован в обработчике разрыва связи клиента и сервера )

    function GetDisconnectReasonStr: string;
  public
    constructor Create(csock:Integer; ipaddress:string);
    destructor Destroy; override;

    property csock:Integer read Fcsock;
    property IPAddress:string read Fipaddess;
    property TimeConnect: QWord read FTimeConnect;
    property TimeLasstActive: QWord read FTimeLastActive;
    property toNoActive: QWord read FtoNotActive write FtoNotActive;
    property toConnection: QWord read FtoConnection write FtoConnection;
    property CloseConnectNow:Boolean read FCloseConnectNow write FCloseConnectNow;
    property DisconnectReason:TClientDisconnectReasons read FDisconnectReason;
    property DisconnectReasonStr:string read GetDisconnectReasonStr;
  end;

  TClientConnect = procedure(sender:TObject; client:TSocketClient) of object; // Эта процедура будет обрабатывать собитие возникающее при подключении клиента
  TClientDisconnect = procedure(sender:TObject; client:TSocketClient) of object; // Эта процедура будет обрабатывать собитие возникающее при отключении клиента
  TClientPutData = procedure(clientnum:Integer; mass:TMemWork; size:Integer) of object; // Эта процедура будет анализировать данные, которые поступают от клиента
  TClientAllDataSend = procedure(clientnum:Integer) of object; // Эта процедура сообщающая о том, что все данные отправлены клиенту
  TLogMessageHandler = procedure(mess:string) of object; // процедура приёма текстовых сообщений от сервера

  { TTCPServer }

  TTCPServer=class
  private
    FServerSock           : Integer;                // Сокет для сервера
    FClientSockMass       : TList;                  // Массив в котором хранятся сокеты клиентов и другая информация
    FCurentClientCount    : Integer;                // количество клиентов, подлючённых в данный момент к серверу
    FClientCountMax       : Integer;                // количество клиентов, которое будет обслуживать сервер
    FMemWrk               : TMemWork;               // Буфер для приёма/передачи или ещё для каких-нибудь операций
    FServerIp             : string;                 // Сетевой адрес сервера
    FPort                 : Integer;                // Порт, который слушает сервер
    FServWaitClient       : TSimpleThread;          // FServWaitClient - для создания потока, в котором будет работать процедура приёма клиентов
    FServHandlerClient    : TSimpleThread;          // FServHandlerClient - для создания потока, в котором будут обслуживаться клиенты
    FServerRun            : Boolean;                // переменная информирующая о том, что сервер запущен
    FOnClientConnect      : TClientConnect;         // Эта процедура будет обрабатывать собитие возникающее при подключении клиента
    FOnClientDisconnect   : TClientDisconnect;      // Эта процедура будет обрабатывать собитие возникающее при отключении клиента
    FOnClientPutData      : TClientPutData;         // Эта процедура будет анализировать данные, которые поступают от клиента
    FOnClientAllDataSend  : TClientAllDataSend;     // Эта процедура сообщающая о том, что все данные отправлены клиенту
    FOnLogMessage         : TLogMessageHandler;     // Сюда будет присвоена функция ( пользователем ), которая должна будет как-то обрабатывать сообщения от сервера ( сообщения в виде строки )
    FLastError            : Integer;                // Код ошибки последней операции

    function ReadWithoutLen(socket: TSocket; var len:Integer):Integer;            // Функция читает данные до тех пор, пока буфер не пуст и возвращает ошибку
    function GetDataFromClient(clientnum:Integer):Integer;                        // Функция получает данные от клиента ( возвращает некий результат )
    function SendDataToClient(clientnum:Integer):Integer;                         // Функция отправляет данные клиенту ( возвращает некий результат )
    procedure WaitClientFunc(sender: TObject);                                    // Функция, которая будет выполняться в потоке ожидания клиентов
    procedure HandlerClient(sender: TObject);                                     // Функция, которая будет обрабатывать запросы клиентов ( на всех клиентов один поток )
    procedure DoDisconnectClient(cnum:Integer; reason:TClientDisconnectReasons);  // Процедура выполняет всё необходимое для удаления клиента ( удаление клиента, это не только уделение его из списка клиентов )
    procedure CreateServerSocket;                                                 // Процедура создающая сокет для сервера ( запускается до запуска потоков ожидания клиентов и обработки запросов от клиентов )
    procedure SendLog(mstr:string);                                               // процедура отсылает сообщение вызывая функцию FOnLogMessgae
    function GetClient(index: Integer): TSocketClient;                            // Процедура обслуживает свойство Clients
  public
    constructor Create;
    destructor Destroy; override;
    procedure ServerStart(ClientCount:Integer; Port:Integer; ip:string=''); // Функция запускающая сервер
    procedure StopServer;                                                   // Остановка сервера
    procedure SetSendBuf(cnum:Integer; buf:Pointer; size:Integer);          // Функция отправляет данные переданные через Buf ( после отправки всех данных сервер генерирует событие OnAllDataSend )

    property ServerRun:Boolean read FServerRun;
    property Potr:Integer read FPort; // Порт который слушает сервер
    property ClientCountMax:Integer read FClientCountMax; // Максимальное количество клиентов
    property CurentClientCount:Integer read FCurentClientCount; // Текущее количество подключённых клиентов
    property Clients[index:Integer]:TSocketClient read GetClient;

    property OnLogMessage:TLogMessageHandler read FOnLogMessage write FOnLogMessage;
    property OnPutDataFromClient:TClientPutData read FOnClientPutData write FOnClientPutData;
    property OnAllDataSend:TClientAllDataSend read FOnClientAllDataSend write FOnClientAllDataSend;
    property OnClientConnect:TClientConnect read FOnClientConnect write FOnClientConnect;
    property OnClientDisconnect:TClientDisconnect read FOnClientDisconnect write FOnClientDisconnect;
  end;

implementation

{ TSocketClient }

function TSocketClient.GetDisconnectReasonStr: string;
begin
  case FDisconnectReason of
    cdrCloseServer:           Result:='Клиент отключён по причине закрытия сервера';
    cdrHaveToDisconnectNow:   Result:='Клиент был отключёт по команде отключения клиента';
    cdrTimeOutNoActive:       Result:='Клиент не был активен в течении определённого времени';
    cdrTimeOutConnection:     Result:='Вышло время для сохранения соединения с клиентом';
    cdrClientCaseInRead:      Result:='Соединение закрыто со стороны клиента при чтении данных';
    cdrClientCaseInWrite:     Result:='Соединение закрыто со стороны клиента при передачи данных клиенту';
  end;
end;

constructor TSocketClient.Create(csock: Integer; ipaddress: string);
begin
  Fcsock:=csock;
  Fipaddess:=ipaddress;

  // т.к. предпологается, что клиента создают в момент его подключения, то в конструкторе фиксируем текущее время
  FTimeConnect:=GetTickCount64;
  FTimeLastActive:=FTimeConnect;
  // Обнуляем времена автоматического отключения ( их можно изменить в любой момент )
  FtoNotActive:=0;
  FtoConnection:=0;
  FCloseConnectNow:=False;
end;

destructor TSocketClient.Destroy;
begin
  Fipaddess:='';
  inherited;
end;

{ TSimpleSoketServer }

constructor TTCPServer.Create;
begin
  FServerRun:=False;
  FMemWrk:=TMemWork.Create(65536); // Создаём буфер для приёма данных от клиента
end;

destructor TTCPServer.Destroy;
begin
  StopServer;
  FMemWrk.Free;
  inherited;
end;

function TTCPServer.ReadWithoutLen(socket: TSocket; var len:Integer): Integer; // Эта функция читае данные из входного буфера сокета до тех пор пока в буфере есть данные, если данные закончились, или произошла ошибка, то функция завершается
var
  bz:Integer;
begin
  bz:=65536;                                        // Размер буфера для приём данных
  FMemWrk.Pos:=0;                                   // Ставим позицию в буфере в начало

  len:=fprecv(socket, FMemWrk.Memory[0], bz, 0);    // Читаем bz байт ( данные записываются в начало буфера )
  bz:=socketerror;                                  // Сразу сохраняем код результата последней операции
  if (len=0)or(bz=ESysECONNRESET) then begin        // Если разорвали соединение, то выходим из функции
    Result:=-1;                                     // Выходим с ошибкой - соединение разорвано
    Exit;
  end;
  if len<0 then begin                               // Если возникла ошибка, то считываем её из переменной WSAGetLastError и выходим
    Len:=0;
    if bz<>ESysEWOULDBLOCK then begin               // Если возникла ошибка не WSAEWOULDBLOCK, то передаём её вызвавщей функции
      Result:=bz;
      Exit;
    end;
    // bz=ESysEWOULDBLOCK Наиболее вероятная ситуация при которой возвращается эта ошибка - это чтение из пустого буфера, что может означать, что
    // данные были полностью считаны, поэтому возвращаем ноль и не паримся
  end;

  Result:=0; // Если дошли до сюда, значит все данные считаны - это нормальный выход
end;

procedure TTCPServer.ServerStart(ClientCount: Integer; Port: Integer; ip: string); // Функция запускающая сервер
begin
  if FServerRun then // Если сервер работает, то нельзя его снова запускать
    Exit;
  FClientCountMax:=ClientCount;
  FPort:=Port;
  FServerIp:=ip;

  // Создание классов для запуска по потоков ожидания клиентов и обработки запросов клиентов ( потоки ещё не запускаются, а только создаются инструменты для запуска потоков )
  FServWaitClient:=TSimpleThread.Create(True);
  FServWaitClient.OnExec:=@WaitClientFunc; // Назначаем функцию, которая будет работать в рамках потока
  FServWaitClient.FreeOnTerminate:=True;

  FServHandlerClient:=TSimpleThread.Create(True);
  FServHandlerClient.OnExec:=@HandlerClient; // Назначаем функцию, которая будет работать в рамках потока
  FServHandlerClient.FreeOnTerminate:=True;

  CreateServerSocket;                       // Создаём сокет сервера
  if FServerSock<0 then Exit;               // Выход, т.к. сокет для сервера создать не удалось

  FServWaitClient.Start; // Запускаем поток ожидания подключения клиентов
  FServHandlerClient.Start; // Запускаем поток сервера
  FServerRun:=True; // Установка признака "сервер работает" в true, чтобы определять запущен ли сервер
end;

procedure TTCPServer.StopServer; // Остановка сервера
var
  i:Integer;
begin
  if FServerRun then // если сервер запущен, то
    begin // необходимо остановить поток ожидания клиентов и поток обработки запросов клиентов
      FServHandlerClient.Terminate;
      FServHandlerClient.WaitFor;
      SendLog('Поток обработки сообщений от клиентов остановлен');

      FServWaitClient.Terminate;
      FServWaitClient.WaitFor;
      SendLog('Поток ожидания соединения клиентов остановлен');

      // Закрываем сокет сервера
      closesocket(FServerSock); // Закрываем сокет сервера
      FServerSock:=-1;

      // Отключаем клиентов, если кто-то из клиентов остался подключённым
      while FClientSockMass.Count<>0 do
        DoDisconnectClient(0, cdrCloseServer); // Оключаем нулевого клиента ( при этом в списке клиентов все будут сдвигаться к началу списка )
      FClientSockMass.Clear; // Освобождаем всю память от элементов списка
      FServerRun:=False;        
    end;
end;

procedure TTCPServer.SendLog(mstr: string);
begin
  if Assigned(FOnLogMessage) then
    FOnLogMessage(mstr);
end;

procedure TTCPServer.CreateServerSocket; // Процедура создаёт сокет сервера
var
  addr:TSockAddr;
  flags:cint;
begin
  FServerSock:=fpsocket(AF_INET, SOCK_STREAM, 0); // Создаём ТСР/IP сокет

  flags:=FpFcntl(FServerSock, F_GETFL, 0);
  if FpFcntl(FServerSock, F_SETFL, flags or O_NONBLOCK)=-1 then begin
    FLastError:=socketerror;                                        // чтобы подробнее узнать о кодах ошибок можно перейти в модуль Sockets, а от туда в errno.inc
    SendLog(Format('Ошибка создания сокета %d',[FLastError]));
    Sockets.CloseSocket(FServerSock);
    FServerSock:=-1;
  end;
  SendLog('Сокет для сервера создан');

  // Задаём IP адрес для сервера
  addr.sin_family:=AF_INET;                   // Семейство протоколов TCP/IP
  addr.sin_port:=htons(FPort);                // Задаём порт ( htons - функция формирует нужный для сетей TCP/IP порядок байтов ) ( порядок байтов может отличаться на машине, на которой работает сервер и тот, что изпользуется в сетях TCP/IP )
  if FServerIp='' then                        // Если адрес сервера не указан, то позволяем системе самой определить сетевой адрес сервера
    addr.sin_addr.S_addr:=INADDR_ANY          // Задаём адрес сервера ( IP адрес к которому будет привязан сокет ) ( данная константа означает - любой адрес локального хоста )
  else
    addr.sin_addr:=StrToNetAddr(FServerIp);   // Если сетевой адрес указан, то устанавливает его принудительно

  // Связываем адрес с сокетом ( для сокетов - серверов )
  if fpbind(FServerSock, @addr, SizeOf(addr))<>0 then begin
    FLastError:=socketerror;                        // чтобы подробнее узнать о кодах ошибок можно перейти в модуль Sockets, а от туда в errno.inc
    Sockets.CloseSocket(FServerSock);
    FServerSock:=-1;
    SendLog(Format('Ошибка bind %d',[FLastError]));
    Exit;
  end;
  SendLog('Связь сокета с адресом выполнена');

  FClientSockMass:=TList.Create; // Создаём список клиентов
  FCurentClientCount:=0; // Текущее количество клиентов обнуляем

  // Задаём размер очереди клиентов
  if fplisten(FServerSock, FClientCountMax)=-1 then begin
    FLastError:=socketerror;                        // чтобы подробнее узнать о кодах ошибок можно перейти в модуль Sockets, а от туда в errno.inc
    SendLog(Format('Ошибка listen %d',[FLastError]));
    Exit;
  end;
  SendLog('Размер очереди клиентов = '+IntToStr(FClientCountMax));
end;

procedure TTCPServer.WaitClientFunc(sender: TObject); // Функция, которая будет выполняться в потоке ожидания клиентов
var
  thobj:TSimpleThread;
  addr:TSockAddr;
  csock: TSocket;
  i,flags:Integer;
  newclient:TSocketClient;
begin
  SendLog('Запущен поток ожидания подключения клиентов');
  thobj:=TSimpleThread(sender);
  repeat
    if FServerSock=-1 then Break; // Сокет сервера могут закрыть, тогда делать тут нечего и можно выходить

    if FCurentClientCount=FClientCountMax then begin // Если все места для клиентов заняты, то дальше идти смысла нет, т.к. добавить нового клиента мы всё равно не можем
      Sleep(100); // поспим 50 мс перед тем, как снова запускать функцию ожидания клиента ( иначе будет большая нагрузка на процессор )
      Continue; // Переход в начало, для ожидания соединения с клиентом
    end;

    i:=SizeOf(addr);
    csock:=fpaccept(FServerSock, @addr, @i); // Реальное ожидание подключение клиента
    FLastError:=socketerror;
    // т.к. сокет не блокирующий, то ошибка "ESysEWOULDBLOCK" вполне ожидаема в том случае, если клиент не подключился в течении некоторого времени
    // поэтому попытка ожидания клиента будет повторена

    if csock<0 then begin // если произошла какая-нибудь другая ошибка, кроме ESysEWOULDBLOCK, то сообщаем об этом
      case FLastError of
        EsockEBADF        : SendLog('fpaccept err EsockEBADF');
        EsockENOTSOCK     : SendLog('fpaccept err EsockENOTSOCK');
        EsockEFAULT       : SendLog('fpaccept err EsockEFAULT');
        EsockEWOULDBLOCK  : begin end; // SendLog('fpaccept err EsockEWOULDBLOCK'); // Об этой ошибке сообщать не смыла т.к. это нормально при небокирующем сокете (просто вышло время ожидания)
        EsockEINVAL       : SendLog('fpaccept err EsockEINVAL');   // Скорее всего неправильно настроен сокет сервера
      else
        SendLog(Format('fpaccept err %d',[FLastError]));
      end;

      Sleep(100);
      Continue;
    end;

    if csock>0 then begin
      flags:=FpFcntl(csock, F_GETFL, 0);
      if FpFcntl(csock, F_SETFL, flags or O_NONBLOCK)=-1 then begin
        FLastError:=socketerror;                                        // чтобы подробнее узнать о кодах ошибок можно перейти в модуль Sockets, а от туда в errno.inc
        SendLog(Format('Ошибка создания сокета %d',[FLastError]));
        Sockets.CloseSocket(csock);
        FServerSock:=-1;
      end else begin
        SendLog(Format('Сокет склиента настроен sock=%d',[csock]));

        newclient:=TSocketClient.Create(csock, NetAddrToStr(addr.sin_addr));  // Создаём нового клиента
        i:=FClientSockMass.Add(newclient);                                    // Добалвяем клиента в список клиентов
        FCurentClientCount:=FClientSockMass.Count;                            // Текущее количество клиентов
        if Assigned(FOnClientConnect) then                                    // Если назначен обработчик собития подключения клиента, то
          FOnClientConnect(Self, newclient);                                  // запускаем этот обработчик ( передаём номер клиента во 2-м параметре )
      end;
    end;
  until thobj.CheckTerminated;
end;

procedure TTCPServer.HandlerClient(sender: TObject); // Функция, которая будет обрабатывать запросы клиентов ( на всех клиентов один поток )
var
  thobj:TSimpleThread;
  i,res,maxid,csock:Integer;
  mread,mwrite:TFDSet;
  cl:TSocketClient;
begin
  SendLog('Запущена основная функция сервера');
  thobj:=TSimpleThread(sender);
  repeat
    if thobj.CheckTerminated then Break;
    if FCurentClientCount=0 then begin Sleep(100); Continue; end;

    maxid:=0;
    // Если есть клиенты, то необходимо с помощью оператора Select, определить, какой клиент готов общяться с сервером
    fpFD_ZERO(mread); // Обнуляем множество клиентов, которое будем проверять на готовность чтения данных от них
    fpFD_ZERO(mwrite); // Обнуляем множество, для определения того, кто из клиентов готов к чтению данных
    for i:=0 to FCurentClientCount-1 do begin // Записываем клиентов в множество
      csock:=TSocketClient(FClientSockMass.Items[i]).csock;
      if maxid<csock then maxid:=csock;

      fpFD_SET(csock, mread);                                           // Добавление сокета в множество для чтения

      if TSocketClient(FClientSockMass[i]).FSizeBufSend<>0 then         // если для клиента есть данные на отправку, то
        fpFD_SET(csock, mwrite);                                        // можно добавлять сокет в множество
    end;

    res:=fpselect(maxid+1, @mread, @mwrite, nil, cint(100));            // 100мс - время ожидания ответа от оператора select
    FLastError:=socketerror;                                            // Сразу сохраняем ошибку

    WriteLn(Format('Handle client select=%d  FLastError=%d',[res, FLastError]));

    if res>0 then begin                           // Если select вернул кол-во клиентов, которое готово к работе, то можно выполнять обработку
      // Тут необходимо проверить есть ли клиенты которые сервер может отключить автоматически
      i:=0;
      repeat
        cl:=TSocketClient(FClientSockMass[i]);
        if cl.CloseConnectNow then DoDisconnectClient(i, cdrHaveToDisconnectNow) // удаляем клиента ( Отключание по команде из вне )
        else begin
          if cl.FtoConnection<>0 then begin
            if cl.FTimeConnect+cl.FtoConnection<=GetTickCount64 then  // Если проходит проверка удаления клиента (по истечении некоторого времени от момента подлючения) в "любом случае", то
              DoDisconnectClient(i, cdrTimeOutNoActive);              // удаляем клиента (клиент удаляется т.к. установлено предельное время соединения с ним не зависимо от активности клиента)
          end else begin
            if cl.FtoNotActive<>0 then begin
              if cl.FTimeLastActive+cl.FtoNotActive<=GetTickCount64 then  // Если проходит проверка удаления клиента по неактивности клиента, то
                DoDisconnectClient(i, cdrTimeOutNoActive);                // удаляем клиента ( по причине его неактивности )
            end;
          end;
        end;
        Inc(i);
      until i>=FCurentClientCount;
    end;

    WriteLn(Format('Handle client 2',[]));

    if res<0 then begin
      SendLog(Format('select выдал ошибку SOCKET_ERROR, socketerror=%d',[FLastError]));
      Continue; // вышло, так что будем пытаться снова опрашивать сокеты с помощью оператора select
    end;

    i:=-1;
    repeat
      Inc(i);
      if i>=FCurentClientCount then Break;
      cl:=TSocketClient(FClientSockMass[i]);

      if fpFD_ISSET(cl.csock, mread)<>0 then begin // если данный клиент готов передать данные на сервер, то
        res:=GetDataFromClient(i);

        cl.FTimeLastActive:=GetTickCount64; // Обновляем время последней активности ( это время изпользуется для автоматического отключения клиента, если задано поле toNoActive )
        // Далее нужно обработать результат
        if res=-1 then begin  // если res=-1, значит клиент закрыл соединение
          DoDisconnectClient(i, cdrClientCaseInRead); // Удаляем клиента ( соединение закрыто со стороны клиента во время чтения данных от клиента )
          Continue;
        end;
      end;

      if fpFD_ISSET(cl.csock, mwrite)<>0 then // Если данный клиент готов принимать данные от сервера, то
        begin
          res:=SendDataToClient(i);
          // Далее нужно обработать результат
          if res=0 then begin                       // Если результат говорит о том, что переданный буфер отправлен, то сообщаем об этом через событие
            cl.FTimeLastActive:=GetTickCount64;     // Обновляем время последней активности ( это время изпользуется для автоматического отключения клиента, если задано поле toNoActive )
            if Assigned(FOnClientAllDataSend) then  // Если назначен обработчик на это событие, то
              FOnClientAllDataSend(i);              // вызываем этот обработчик
            Continue;
          end;
          if res=-1 then begin                            // если res=-1, значит клиент закрыл соединение
            DoDisconnectClient(i, cdrClientCaseInWrite);  // Удаляем клиента ( соединение закрыто со стороны клиента во время передачи клиенту данных )
            Continue;
          end;
        end;
    until False;

    WriteLn(Format('Handle client 3',[]));


  until False;
end;

function TTCPServer.GetDataFromClient(clientnum: Integer):Integer;
var
  l,cb:Integer;
begin
  l:=ReadWithoutLen(TSocketClient(FClientSockMass[clientnum]).csock, cb); // Эта процедура заполняет данными буфер FMemWrk с нулевой позиции
  if l<>0 then begin                                                      // Если возникла ошибка, то выходим с передачей ошибки выше
    Result:=l;
    Exit;
  end;
  if cb=0 then begin                                                      // Если количество считанных байт равно нулю, то выходим
    Result:=0;
    Exit;
  end;
  if Assigned(FOnClientPutData) then          // Если назначен обработчик прихода данных от клиента, то
    FOnClientPutData(clientnum, FMemWrk, cb); // запускаем назначенный обработчик

  Result:=0;
end;

function TTCPServer.SendDataToClient(clientnum: Integer): Integer; // Процедура отправки данных клиенту будет отправлять все данные, которые задали для отправки в функции SetSendBuf до тех пор, пока не отправит или до тех пор, пока не возникнет ошибка
var
  cl:TSocketClient;
  l,ek:Integer;
begin
  cl:=TSocketClient(FClientSockMass[clientnum]);
  l:=fpsend(cl.csock, cl.FBufForSend, cl.FSizeBufSend, 0); // Пытаемся отправить cl.FSizeBufSend байт
  FLastError:=socketerror;                       // Сразу сохраняем ошибку
  ek:=FLastError;
  if l<>0 then begin
    if (ek=ESysENETRESET) or (ek=ESysECONNABORTED) or (ek=ESysECONNRESET) then begin // При такой ошибке нужно отключать клиента, т.к. соединение разорвано
      Result:=-1;
      Exit;
    end;
    if (ek=ESysENOBUFS)or(ek=ESysEWOULDBLOCK) then ek:=1; // Эта ошибка означает, что отправлены не все данные, поэтому их нужно доотправить при следующем запуске функции
  end;
  // Если ошибок нет, то попадём сюда
  cl.FSizeBufSend:=cl.FSizeBufSend-l; // Вычитаем количество отправленных байт
  if cl.FSizeBufSend=0 then begin     // Если были отправлены все данные, то
    cl.FBufForSend:=nil;              // Это как признак того, что буфер больше не нужен, т.к. данные отправлены
    Result:=0;                        // Возвращаем код - отправлены все данные
    Exit;
  end;
  Inc(PByte(cl.FBufForSend), l);      // Сдвигаем указатель на переданное количество байт
  Result:=ek;                         // Возвращаем код, говорящий о, не полностью переданных данных
end;

function TTCPServer.GetClient(index: Integer): TSocketClient;
begin
  if (index<0)or(index>=FCurentClientCount) then
    raise Exception.Create('В функции GetClient номер вне допустимого диапазона');
  Result:=TSocketClient(FClientSockMass[index]);
end;

procedure TTCPServer.DoDisconnectClient(cnum: Integer;  reason:TClientDisconnectReasons);
var
  clff:TSocketClient;
  flags:Integer;
begin
  clff:=TSocketClient(FClientSockMass[cnum]);
  clff.FDisconnectReason:=reason; // Задаём причину отключения клиента
  if Assigned(FOnClientDisconnect) then // Если назначен обработчик события отключения клиента, то
    FOnClientDisconnect(Self, clff); // запускаем этот обработчик

  // Выполняем все необходимые действия для закрытия сокета и удаляем клиента из списка клиентов
  flags:=FpFcntl(clff.csock, F_GETFL, 0);
  flags:=flags and not O_NONBLOCK;
  FpFcntl(clff.csock, F_SETFL, flags);

  closesocket(clff.csock);
  clff.Free;
  FClientSockMass.Delete(cnum); // Удаляем клиента с номером nc
  FCurentClientCount:=FClientSockMass.Count; // Обновляем текущее значение клиентов
end;

procedure TTCPServer.SetSendBuf(cnum: Integer; buf: Pointer; size: Integer);
begin
  TSocketClient(FClientSockMass[cnum]).FBufForSend:=buf; // Задаём адрес где лежат данные
  TSocketClient(FClientSockMass[cnum]).FSizeBufSend:=size; // Задаём размер данных
end;

end.
