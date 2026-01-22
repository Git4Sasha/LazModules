// Это болванка для создания на её основе сервера, который будет реализовать тот или иной протокол передачи данных
unit SimpleSocketServer;

interface

uses
  WinSock, SimpleThread, MemoryWork, Classes, SysUtils, Windows;

type
  TClientDisconnectReasons=(cdrCloseServer, cdrHaveToDisconnectNow, cdrTimeOutNoActive, cdrTimeOutConnection, cdrClientCaseInRead, cdrClientCaseInWrite); // константы причин отключения клиентов

  { TSocketClient }

  TSocketClient=class
  private
    Fcsock: Integer; // идентификатор сокета клиента
    Fipaddess: string; // IP адрес клиента ( для подключённого клиента )
    FTimeConnect:Cardinal; // Время подсоединения клиента к серверу
    FTimeLastActive:Cardinal; // Время последней активности клиента
    FtoNotActive:Cardinal; // Время в миллисекундах, которое задаёт то, сколько сервер будет держать связь с клиентом с момента последней активности клиента
    FtoConnection:Cardinal; // Время в миллисекундах, которое задаёт то, сколько сервер будет держать связь с клиентом с момента подключение ( сервер разорвёт связь по истечении этого времени в любом случае )
    FBufForSend:Pointer; // Это буфер для отравления данных клиенту ( клиент не занимается освобождением памяти от буфера после его оправки, это должна делать внешняя функция при обработке события - "отправка завершена" (событие генерирует сервер) )
    FSizeBufSend:Integer; // Хранит количество отправляемых данных
    FCloseConnectNow:Boolean; // Если это поле равно True, значит сервер должен в ближайшее время разорвать соединение с клиентом
    FDisconnectReason:TClientDisconnectReasons; // Это поле хранит код причины отключения клиента ( он может быть изпользован в обработчике разрыва связи клиента и сервера )

    function GetDisconnectReasonStr: string;
  public
    constructor Create(csock:Integer; ipaddress:string);
    destructor Destroy; override;

    property csock:Integer read Fcsock;
    property IPAddress:string read Fipaddess;
    property TimeConnect: Cardinal read FTimeConnect;
    property TimeLasstActive: Cardinal read FTimeLastActive;
    property toNoActive: Cardinal read FtoNotActive write FtoNotActive;
    property toConnection: Cardinal read FtoConnection write FtoConnection;
    property CloseConnectNow:Boolean read FCloseConnectNow write FCloseConnectNow;
    property DisconnectReason:TClientDisconnectReasons read FDisconnectReason;
    property DisconnectReasonStr:string read GetDisconnectReasonStr;
  end;

  TClientConnect = procedure(sender:TObject; client:TSocketClient) of object; // Эта процедура будет обрабатывать собитие возникающее при подключении клиента
  TClientDisconnect = procedure(sender:TObject; client:TSocketClient) of object; // Эта процедура будет обрабатывать собитие возникающее при отключении клиента
  TClientPutData = procedure(clientnum:Integer; mass:TMemWork; size:Integer) of object; // Эта процедура будет анализировать данные, которые поступают от клиента
  TClientAllDataSend = procedure(clientnum:Integer) of object; // Эта процедура сообщающая о том, что все данные отправлены клиенту
  TLogMessageHandler = procedure(mess:string) of object; // процедура приёма текстовых сообщений от сервера

  TSimpleSocketServer=class
  private
    FWSData:TWSAData; // Для работы функции WSAStartup ( используется для хранения информации о версии сокета )
    FServerSock: Integer;
    FClientSockMass: TList; // Массив в котором хранятся сокеты клиентов и другая информация
    FCurentClientCount: Integer; // количество клиентов, подлючённых в данный момент к серверу
    FClientCountMax: Integer; // количество клиентов, которое будет обслуживать сервер
    FMemWrk:TMemWork; // Буфер для приёма/передачи или ещё для каких-нибудь операций
    FPort: Integer;
    FServWaitClient: TSimpleThread; // FServWaitClient - для создания потока, в котором будет работать процедура приёма клиентов
    FServHandlerClient: TSimpleThread; // FServHandlerClient - для создания потока, в котором будут обслуживаться клиенты
    FServerRun: Boolean; // переменная информирующая о том, что сервер запущен
    FOnClientConnect:TClientConnect; // Эта процедура будет обрабатывать собитие возникающее при подключении клиента
    FOnClientDisconnect:TClientDisconnect; // Эта процедура будет обрабатывать собитие возникающее при отключении клиента
    FOnClientPutData:TClientPutData; // Эта процедура будет анализировать данные, которые поступают от клиента
    FOnClientAllDataSend:TClientAllDataSend; // Эта процедура сообщающая о том, что все данные отправлены клиенту
    FOnLogMessage:TLogMessageHandler; // Сюда будет присвоена функция ( пользователем ), которая должна будет как-то обрабатывать сообщения от сервера ( сообщения в виде строки )

    function ReadWithoutLen(socket: TSocket; var len:Integer):Integer; // Функция читает данные до тех пор, пока буфер не пуст и возвращает ошибку
    function GetDataFromClient(clientnum:Integer):Integer; // Функция получает данные от клиента ( возвращает некий результат )
    function SendDataToClient(clientnum:Integer):Integer; // Функция отправляет данные клиенту ( возвращает некий результат )
    procedure WaitClientFunc(sender: TObject); // Функция, которая будет выполняться в потоке ожидания клиентов
    procedure HandlerClient(sender: TObject); // Функция, которая будет обрабатывать запросы клиентов ( на всех клиентов один поток )
    procedure DoDisconnectClient(cnum:Integer; reason:TClientDisconnectReasons); // Процедура выполняет всё необходимое для удаления клиента ( удаление клиента, это не только уделение его из списка клиентов )
    procedure CreateServerSocket; // Процедура создающая сокет для сервера ( запускается до запуска потоков ожидания клиентов и обработки запросов от клиентов )
    procedure SendLog(mstr:string); // процедура отсылает сообщение вызывая функцию FOnLogMessgae
    function GetClient(index: Integer): TSocketClient; // Процедура обслуживает свойство Clients
  public
    constructor Create;
    destructor Destroy; override;
    procedure ServerStart(ClientCount:Integer; Port:Integer); // Функция запускающая сервер
    procedure StopServer; // Остановка сервера
    procedure SetSendBuf(cnum:Integer; buf:Pointer; size:Integer); // Функция отправляет данные переданные через Buf ( после отправки всех данных сервер генерирует событие OnAllDataSend )

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
  FTimeConnect:=GetTickCount;
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

constructor TSimpleSocketServer.Create;
begin
  FServerRun:=False;

  WSAStartup($101, FWSData); // Указываем версию сокетов, с которой будем работать

  FMemWrk:=TMemWork.Create; // Создаём буфер приёма, передачи и т.д.
end;

destructor TSimpleSocketServer.Destroy;
begin
  StopServer;
  WSACleanup;
  FMemWrk.Free;
  inherited;
end;

function TSimpleSocketServer.ReadWithoutLen(socket: TSocket; var len:Integer): Integer; // Эта функция читае данные из входного буфера сокета до тех пор пока в буфере есть данные, если данные закончились, или произошла ошибка, то функция завершается
var
  bz:Integer;
begin
  bz:=65536; // Размер буфера для приём данных
  FMemWrk.Pos:=0; // Ставим позицию в буфере в начало
  if FMemWrk.Size<bz then // Если размер буфера на данный момент меньше размера данных, которые мы будем принимать, то
    FMemWrk.Size:=bz; // Увеличиваем размер буфера до bz

  len:=recv(socket, FMemWrk.Memory[0]^, bz, 0); // Читаем bz байт ( данные записываются в начало буфера )
  bz:=WSAGetLastError; // Сразу сохраняем код результата последней операции
  if (len=0)or(bz=WSAECONNRESET) then // Если разорвали соединение, то выходим из функции
    begin
      Result:=-1; // Выходим с ошибкой - соединение разорвано
      Exit;
    end;
  if len<0 then // Если возникла ошибка, то считываем её из переменной WSAGetLastError и выходим
    begin
      Len:=0;
      if bz<>WSAEWOULDBLOCK then // Если возникла ошибка не WSAEWOULDBLOCK, то передаём её вызвавщей функции
        begin
          Result:=bz;
          Exit;
        end;
      // bz=WSAEWOULDBLOCK Наиболее вероятная ситуация при которой возвращается эта ошибка - это чтение из пустого буфера, что может означать, что
      // данные были полностью считаны, поэтому возвращаем ноль и не паримся
    end;

  Result:=0; // Если дошли до сюда, значит все данные считаны - это нормальный выход
end;

procedure TSimpleSocketServer.ServerStart(ClientCount, Port: Integer); // Функция запускающая сервер
begin
  if FServerRun then // Если сервер работает, то нельзя его снова запускать
    Exit;
  FClientCountMax:=ClientCount;
  FPort:=Port;

  // Создание классов для запуска по потоков ожидания клиентов и обработки запросов клиентов ( потоки ещё не запускаются, а только создаются инструменты для запуска потоков )
  FServWaitClient:=TSimpleThread.Create(True);
  FServWaitClient.OnExec:=@WaitClientFunc; // Назначаем функцию, которая будет работать в рамках потока
  FServWaitClient.FreeOnTerminate:=True;

  FServHandlerClient:=TSimpleThread.Create(True);
  FServHandlerClient.OnExec:=@HandlerClient; // Назначаем функцию, которая будет работать в рамках потока
  FServHandlerClient.FreeOnTerminate:=True;

  CreateServerSocket; // Создаём сокет сервера
  FServWaitClient.Start; // Запускаем поток ожидания подключения клиентов
  FServHandlerClient.Start; // Запускаем поток сервера
  FServerRun:=True; // Установка признака "сервер работает" в true, чтобы определять запущен ли сервер
end;

procedure TSimpleSocketServer.StopServer; // Остановка сервера
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
      i:=0;
      ioctlsocket(FServerSock, FIONBIO, i); // Делаем так, чтобы сокет был блокирующим ( не знаю почему, но перед закрытием сокетка его нужно переводить в блокирующий режим ( конечно, если он был не блокирующим ))
      closesocket(FServerSock); // Закрываем сокет сервера
      FServerSock:=-1;

      // Отключаем клиентов, если кто-то из клиентов остался подключённым
      while FClientSockMass.Count<>0 do
        DoDisconnectClient(0, cdrCloseServer); // Оключаем нулевого клиента ( при этом в списке клиентов все будут сдвигаться к началу списка )
      FClientSockMass.Clear; // Освобождаем всю память от элементов списка
      FServerRun:=False;        
    end;
end;

procedure TSimpleSocketServer.SendLog(mstr: string);
begin
  if Assigned(FOnLogMessage) then
    FOnLogMessage(mstr);
end;

procedure TSimpleSocketServer.CreateServerSocket; // Процедура создаёт сокет сервера
var
  addr:TSockAddr;
  i:Integer;
begin
  FServerSock:=socket(AF_INET, SOCK_STREAM, 0); // Создаём ТСР/IP сокет
  i:=1;
  ioctlsocket(FServerSock, FIONBIO, i); // Делаем так, чтобы сокет был не блокирующим
  if FServerSock=-1 then
    begin
      SendLog(Format('Ошибка создания сокета %d',[WSAGetLastError]));
      Exit;
    end;
  SendLog('Сокет для сервера создан');

  // Задаём IP адрес для сервера
  addr.sin_family:=AF_INET; // Семейство протоколов TCP/IP
  addr.sin_port:=htons(FPort); // Задаём порт ( htons - функция формирует нужный для сетей TCP/IP порядок байтов ) ( порядок байтов может отличаться на машине, на которой работает сервер и тот, что изпользуется в сетях TCP/IP )
  addr.sin_addr.S_addr:=INADDR_ANY; // Задаём адрес сервера ( IP адрес к которому будет привязан сокет ) ( данная константа означает - любой адрес локального хоста )

  // Связываем адрес с сокетом ( для сокетов - серверов )
  if bind(FServerSock, addr, SizeOf(addr))=-1 then
    begin
      SendLog(Format('Ошибка bind %d',[WSAGetLastError]));
      Exit;
    end;
  SendLog('Связь сокета с адресом выполнена');

  FClientSockMass:=TList.Create; // Создаём список клиентов
  FCurentClientCount:=0; // Текущее количество клиентов обнуляем

  // Задаём размер очереди клиентов
  if listen(FServerSock, FClientCountMax)=-1 then
    begin
      SendLog(Format('Ошибка listen %d',[WSAGetLastError]));
      Exit;
    end;
  SendLog('Размер очереди клиентов = '+IntToStr(FClientCountMax));
end;

procedure TSimpleSocketServer.WaitClientFunc(sender: TObject); // Функция, которая будет выполняться в потоке ожидания клиентов
var
  thobj:TSimpleThread;
  addr:TSockAddr;
  csock: TSocket;
  i:Integer;
  newclient:TSocketClient;
begin
  SendLog('Запущен поток ожидания подключения клиентов');
  thobj:=TSimpleThread(sender);
  repeat
    if thobj.CheckTerminated then Break; // прерываем цикл, если необходимо завершить работу потока, то выходим
    if FServerSock=-1 then Break; // Сокет сервера могут закрыть, тогда делать тут нечего и можно выходить

    if FCurentClientCount=FClientCountMax then // Если все места для клиентов заняты, то дальше идти смысла нет, т.к. добавить нового клиента мы всё равно не можем
      begin
        Sleep(50); // поспим 50 мс перед тем, как снова запускать функцию ожидания клиента ( иначе будет большая нагрузка на процессор )
        Continue; // Переход в начало, для ожидания соединения с клиентом
      end;

    i:=SizeOf(addr);
    csock:=accept(FServerSock, @addr, @i); // Реальное ожидание подключение клиента
    i:=WSAGetLastError;
    // т.к. сокет не блокирующий, то ошибка "WSAEWOULDBLOCK" вполне ожидаема в том случае, если клиент не подключился в течении некоторого промежутка времени
    // поэтому попытка ожидания клиента будет повторена

    if i=WSAEWOULDBLOCK then begin // Не клиентов ожидающих подключения ( т.к. сокет не блокирующи, то выход из функции происходит без задержки, но при этом формируется проверяемая ошибка )
      Sleep(10);
      Continue;
    end;

    if csock=INVALID_SOCKET then begin // если произошла какая-нибудь другая ошибка, кроме WSAEWOULDBLOCK, то сообщаем об этом
      Sleep(10);
      Continue;
    end;

    if csock<>INVALID_SOCKET then begin
      i:=1;
      ioctlsocket(csock, FIONBIO, i); // Делаем так, чтобы сокет был не блокирующим

      newclient:=TSocketClient.Create(csock, inet_ntoa(addr.sin_addr)); // Создаём нового клиента
      i:=FClientSockMass.Add(newclient); // Добалвяем клиента в список клиентов
      FCurentClientCount:=FClientSockMass.Count; // Текущее количество клиентов
      if Assigned(FOnClientConnect) then // Если назначен обработчик собития подключения клиента, то
        FOnClientConnect(Self, newclient); // запускаем этот обработчик ( передаём номер клиента во 2-м параметре )
    end;
  until False;
end;

procedure TSimpleSocketServer.HandlerClient(sender: TObject); // Функция, которая будет обрабатывать запросы клиентов ( на всех клиентов один поток )
var
  thobj:TSimpleThread;
  i,res:Integer;
  mread,mwrite:TFDSet;
  tval:timeval;
  cl:TSocketClient;
begin
  SendLog('Запущена основная функция сервера');
  thobj:=TSimpleThread(sender);
  repeat
    if thobj.CheckTerminated then Break;
    if FCurentClientCount=0 then begin Sleep(10); Continue; end;

    // Если есть клиенты, то необходимо с помощью оператора Select, определить, какой клиент готов общяться с сервером
    FD_ZERO(mread); // Обнуляем множество клиентов, которое будем проверять на готовность чтения данных от них
    for i:=0 to FCurentClientCount-1 do // Записываем клиентов в множество
      FD_SET(TSocketClient(FClientSockMass.Items[i]).csock, mread); // можно добавлять сокет в множество

    FD_ZERO(mwrite); // Обнуляем множество, для определения того, кто из клиентов готов к чтению данных
    for i:=0 to FCurentClientCount-1 do // Записываем клиентов в множество для определения того, кто из них готов принимать данные от сервера
      if TSocketClient(FClientSockMass[i]).FSizeBufSend<>0 then // если для клиента есть данные на отправку, то
        FD_SET(TSocketClient(FClientSockMass[i]).csock, mwrite); // можно добавлять сокет в множество

    tval.tv_sec:=0;
    tval.tv_usec:=10000; // 10 мс
    res:=select(0, @mread, @mwrite, nil, @tval); // tval - время ожидания ответа от оператора select
    i:=WSAGetLastError; // Сразу сохраняем ошибку

    if res=0 then // Если select вернул 0, значит вышло время ожидания
      begin
        // Тут необходимо проверить есть ли клиенты которые сервер может отключить автоматически
        i:=0;
        repeat
          cl:=TSocketClient(FClientSockMass.Items[i]);
          if cl.CloseConnectNow then
            DoDisconnectClient(i, cdrHaveToDisconnectNow) // удаляем клиента ( Отключание по команде из вне )
          else
            begin
              if cl.FtoConnection<>0 then
                begin
                  if cl.FTimeConnect+cl.FtoConnection<=GetTickCount then // Если проходит проверка удаления клиента в "любом случае", то
                    DoDisconnectClient(i, cdrTimeOutNoActive); // удаляем клиента
                end
              else
                if cl.FtoNotActive<>0 then
                  begin
                    if cl.FTimeLastActive+cl.FtoNotActive<=GetTickCount then // Если проходит проверка удаления клиента по неактивности клиента, то
                      DoDisconnectClient(i, cdrTimeOutNoActive); // удаляем клиента ( по причине его неактивности )
                  end;
            end;
          Inc(i);
        until i>=FCurentClientCount;
        Continue; // вышло, так что будем пытаться снова опрашивать сокеты с помощью оператора select
      end;

    if res=SOCKET_ERROR then
      begin
        res:=i;
        SendLog(Format('select выдал ошибку SOCKET_ERROR, WSAGetLastError=%d',[res]));
        Continue; // вышло, так что будем пытаться снова опрашивать сокеты с помощью оператора select
      end;

    i:=-1;
    repeat
      Inc(i);
      if i>=FCurentClientCount then
        Break;
      cl:=TSocketClient(FClientSockMass[i]);

      if FD_ISSET(cl.csock, mread) then // если данный клиент готов передать данные на сервера, то
        begin
          res:=GetDataFromClient(i);
          cl.FTimeLastActive:=GetTickCount; // Обновляем время последней активности ( это время изпользуется для автоматического отключения клиента, если задано поле toNoActive )
          // Далее нужно обработать результат
          if res=-1 then // если res=-1, значит клиент закрыл соединение
            begin
              DoDisconnectClient(i, cdrClientCaseInRead); // Удаляем клиента ( соединение закрыто со стороны клиента во время чтения данных от клиента )
              Continue;
            end;
        end;

      if FD_ISSET(cl.csock, mwrite) then // Если данный клиент готов принимать данные от сервера, то
        begin
          res:=SendDataToClient(i);
          // Далее нужно обработать результат
          if res=0 then // Если результат говорит о том, что переданный буфер отправлен, то сообщаем об этом через событие
            begin
              cl.FTimeLastActive:=GetTickCount; // Обновляем время последней активности ( это время изпользуется для автоматического отключения клиента, если задано поле toNoActive )
              if Assigned(FOnClientAllDataSend) then // Если назначен обработчик на это событие, то
                FOnClientAllDataSend(i); // вызываем этот обработчик
              Continue;
            end;
          if res=-1 then // если res=-1, значит клиент закрыл соединение
            begin
              DoDisconnectClient(i, cdrClientCaseInWrite); // Удаляем клиента ( соединение закрыто со стороны клиента во время передачи клиенту данных )
              Continue;
            end;
        end;
    until False;
  until False;
end;

function TSimpleSocketServer.GetDataFromClient(clientnum: Integer):Integer;
var
  l,cb:Integer;
begin
  l:=ReadWithoutLen(TSocketClient(FClientSockMass[clientnum]).csock, cb); // Эта процедура заполняет данными буфера FMemWrk с нулевой позиции ( размер буфера может измениться только в большую сторону )
  if l<>0 then // Если возникла ошибка, то выходим с передачей ошибки выше
    begin
      Result:=l;
      Exit;
    end;
  if cb=0 then // Если количество считанных байт равно нулю, то выходим
    begin
      Result:=0;
      Exit;
    end;
  if Assigned(FOnClientPutData) then // Если назначен обработчик прихода данных от клиента, то
    FOnClientPutData(clientnum, FMemWrk, cb); // запускаем назначенный обработчик

  Result:=0;
end;

function TSimpleSocketServer.SendDataToClient(clientnum: Integer): Integer; // Процедура отправки данных клиенту будет отправлять все данные, которые задали для отправки в функции SetSendBuf до тех пор, пока не отправит или до тех пор, пока не возникнет ошибка
var
  cl:TSocketClient;
  l,ek:Integer;
begin
  cl:=TSocketClient(FClientSockMass[clientnum]);
  l:=send(cl.csock, cl.FBufForSend^, cl.FSizeBufSend, 0); // Пытаемся отправить cl.FSizeBufSend байт
  ek:=WSAGetLastError; // Сразу сохраняем код результата последней операции
  if l=SOCKET_ERROR then
    begin
      if (ek=WSAENETRESET) or (ek=WSAECONNABORTED) or (ek=WSAECONNRESET) then // При такой ошибке нужно отключать клиента, т.к. соединение разорвано
        begin
          Result:=-1;
          Exit;
        end;
      if (ek=WSAENOBUFS)or(ek=WSAEWOULDBLOCK) then // Эта ошибка означает, что отправлены не все данные, поэтому их нужно доотправить при следующем запуске функции
        ek:=1;
    end;
  // Если ошибок нет, то попадём сюда
  cl.FSizeBufSend:=cl.FSizeBufSend-l; // Вычитаем количество отправленных байт
  if cl.FSizeBufSend=0 then // Если были отправлены все данные, то
    begin
      cl.FBufForSend:=nil; // Это как признак того, что буфер больше не нужен, т.к. данные отправлены
      Result:=0; // Возвращаем код - отправлены все данные
      Exit;
    end;
  Inc(PByte(cl.FBufForSend), l); // Сдвигаем указатель на переданное количество байт
  Result:=ek; // Возвращаем код, говорящий о, не полностью переданных данных
end;

function TSimpleSocketServer.GetClient(index: Integer): TSocketClient;
begin
  if (index<0)or(index>=FCurentClientCount) then
    raise Exception.Create('В функции GetClient номер вне допустимого диапазона');
  Result:=TSocketClient(FClientSockMass[index]);
end;

procedure TSimpleSocketServer.DoDisconnectClient(cnum: Integer;  reason:TClientDisconnectReasons);
var
  clff:TSocketClient;
  v:Integer;
begin
  clff:=TSocketClient(FClientSockMass[cnum]);
  clff.FDisconnectReason:=reason; // Задаём причину отключения клиента
  if Assigned(FOnClientDisconnect) then // Если назначен обработчик события отключения клиента, то
    FOnClientDisconnect(Self, clff); // запускаем этот обработчик

  // Выполняем все необходимые действия для закрытия сокета и удаляем клиента из списка клиентов
  v:=0;
  ioctlsocket(clff.csock, FIONBIO, v); // Перез закрытием сокета делаем его блокирующим ( не знаю зачем, но так надо )

  closesocket(clff.csock);
  clff.Free;
  FClientSockMass.Delete(cnum); // Удаляем клиента с номером nc
  FCurentClientCount:=FClientSockMass.Count; // Обновляем текущее значение клиентов
end;

procedure TSimpleSocketServer.SetSendBuf(cnum: Integer; buf: Pointer; size: Integer);
begin
  TSocketClient(FClientSockMass[cnum]).FBufForSend:=buf; // Задаём адрес где лежат данные
  TSocketClient(FClientSockMass[cnum]).FSizeBufSend:=size; // Задаём размер данных
end;

end.
