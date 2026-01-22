unit USB_HID_ReadWrite;

// Этот модуль работает через библиотеку libUSB-1.0.dll
// в этом случае необходима библиотека libUSB-1.0.dll для 32/64 битной системы библиотека разная. Так же необходимо заменить
// стандартный драйыер Windows для HID устройств на драйвер WinUSB, т.к. через этот драйвер работает библиотека libUSB-1.0
// для замены стандартного драйвера изпользуется программа zadig, которая для выбранного USB устройства может заменить
// драйвер, который Windows будет изпользовать при работе с этим устройством. Windows по PID и VID привязывает драйвер,
// который был указан в zadig, после чего вынимая и вставляя устройство будет подключаться выбранный драйвер.
// Чтобы вернуть родной драйвер, нужно удалить устройство из диспетчера устройств, далее вытащить и вставить устройство,
// после чего Windows привяжет к нему свой стандартный драйвер.

interface

uses
  Classes
  , SysUtils
  , SimpleThread
  , LibUSB
  , ArrayOfTypes
  , RingBufHelper
  ;

type
  TDataIn=procedure(buf:PByte; cnt, epnum:Integer) of object;

  TEndPointInfo=record      // Структура для хранения информации о конечной точке
    sizein:Uint16;          // Размер пакета если это конечная точка типа IN
    sizeout:UInt16;         // Размер пакета если это конечная точка типа IN
    attr:UInt32;            // Тут будут храниться параметры конечной точки (тип использования, тип синхронизации,  тип конечной точки) это поле равно bmAttributes в дескрипторе конечной точки
  end;
  TArrayOfEndPointInfo=array of TEndPointInfo;

  THIDReportData=record
    epn:Integer;
    len:Integer;
    buf:array [0..128] of Byte;
  end;
  TArrayOfHIDReportData=array of THIDReportData;

  { TUSBHIDReadWrite }

  TUSBHIDReadWrite=class
  private
    FLibUSBCtx:Pointer;                     // Контекст для работы библиотеки lIbUSB
    FDevHandle:Pointer;                     // Идентификатор устройства для чтения
    FOpenedDevName:string;                  // Имя открытого устройства
    FManufactureString:string;              // Имя производителя
    FProductString:string;                  // Имя продукта
    FSerialNumberString:string;             // Строка с серийным номером
    FReadThreads:array of TSimpleThread;    // Массив для хранения потоков чтения данных (на каждую конечную точку, которая может передавать данные хосту создаётся свой поток чтения)
    FReadThreadOn:Boolean;                  // Признак того, что поток для чтения данных запущен (можно и без него обойтись (особенно если USB устройство ничего не возвращает))
    FOnDataIn:TDataIn;                      // Событие, которое возникает при приходе данных от USB устройства
    FOnNoConnect:TNotifyEvent;              // Событие, которое возникает при разрыве соединения с USB устройством
    FUSBDeviceOpened:Boolean;               // Признак того, что HID устройство открыто
    FEndPointINCount:Integer;               // Кол-во конечных точек типа "IN"
    FEndPointOUTCount:Integer;              // Кол-во конечных точек типа "OUT"
    FEndPointInfo:TArrayOfEndPointInfo;     // Массив, который хранит информацию о конечных точках устройства
    FUseRecvRingBuf:Boolean;                // Признак того, что будет изпользоваться кольцевой буфер для входящих сообщений
    FRBHelper:TRingBufHelper;               // Помошник для кольцевого буфера
    FDataRingBuf:TArrayOfHIDReportData;     // Массив, который будет изпользоваться для приёма входящих сообщений для кольцевого буфера

    function GetStringDesc(pdh:Pointer; StrIndex:Word):string;                // Получение строки по индексу из описателя устройства
    procedure FReadFuncEP(Sender:TObject);                                    // Функция реализует поток чтения данных с USB устройства для конечной точки
    function OpenUSBDByHandle(RunReadThread:Boolean=True):Boolean;            // Открытие устройства по его адресу
    procedure StopReadThreads;                                                // Процедура для остановки потоков чтения данных
    function CreateUSBDevName(devhandle:plibusb_device_handle; desc:libusb_device_descriptor):string; // Функция формирует имя USB устройства
    function CreateUSBDevNameShort(devhandle:plibusb_device_handle):string;                 // Функция формирует короткое имя USB устройства

  public
    Log:TStrings; // журнал

    constructor Create;
    destructor Destroy; override;
    function GetUSBDeviceNames(nshort:Boolean=False; pid:Word=0; vid:Word=0):TStrings;          // Процедура возвращает список имён устройств
    function OpenUSBDByPID_VID(PID,VID:Word; RunReadThread:Boolean=True):Boolean;               // Открывает устройство по идентификаторам устройства VID и PID
    function OpenUSBDByName(name:string; shortname:Boolean; RunReadThread:Boolean):Boolean;     // Функция открывает устройство по его имения, которое можно получить изпользуя функцию GetUSBDeviceNames
    procedure CloseUSBDevice;                                                                   // Закрытие USB устройства
    function RecvData(epnum:Integer; buf:PByte; tout:Integer=MaxInt):Integer;                   // приём данных от USB устройства
    function SendData(epnum:Integer; buf:PByte; tout:Integer=MaxInt):Integer;                   // передача данных USB устройству
    procedure SetUseRingBuf(rblen:Integer);                                                     // Процедура устанавливает размер кольцевого буфера для входящих сообщений
    function ReadRingBuf(out mes:THIDReportData):Boolean;                                       // Функция считывет данные из кольцевого буфера

    property ManufactureString:string read FManufactureString;   // Имя производителя
    property ProductString:string read FProductString;           // Имя продукта
    property SerialNumberString:string read FSerialNumberString; // Строка с серийным номером
    property OnDataIn:TDataIn read FOnDataIn write FOnDataIn; // Событие, которое возникает при приходе данных от USB устройства
    property OnNoConnect:TNotifyEvent read FOnNoConnect write FOnNoConnect;
    property DeviceOpened:Boolean read FUSBDeviceOpened; // Признак того что устройство открыто
    property EndPointINCount:Integer read FEndPointINCount;
    property EndPointOUTCount:Integer read FEndPointOUTCount;
    property OpenedDevName:string read FOpenedDevName;
  end;

implementation

{ TUSBHIDReadWrite }

constructor TUSBHIDReadWrite.Create;
begin
  Log:=TStringList.Create;            // Для журнала
  FUSBDeviceOpened:=False;            // HID устройство не открыто
  libusb_init(FLibUSBCtx);            // Инициализация библиотеки libUSB
//  libusb_set_debug(FLibUSBCtx, LIBUSB_LOG_LEVEL_DEBUG);    // Можно установить уровень вывода отладочных сообщений
//  libusb_set_log_cb                   // Можно установить функцию обратного вызова для отлавливания отладочных сообщений
  FUSBDeviceOpened:=False;

  SetLength(FEndPointInfo, 15);      // Выделение памяти на максимально возможное кол-во конечных точек (потом каждая ячейка будет заполнена при открытии устройства)
end;

destructor TUSBHIDReadWrite.Destroy;
begin
  CloseUSBDevice; // Закрываем HID устройство
  Log.Free;
  libusb_exit(FLibUSBCtx); // Прекращаем работу с библиотекой
  FEndPointInfo:=nil;

  if FUseRecvRingBuf then begin
    FRBHelper.Free;
    FDataRingBuf:=nil;
  end;

  inherited;
end;

function TUSBHIDReadWrite.GetStringDesc(pdh: Pointer; StrIndex: Word): string;
var
  buf:array [0..255] of Byte;
  buflen,v:Integer;
begin
  Result:='';

  buflen:=SizeOf(buf);
  FillByte(buf[0], buflen, 0);
  if StrIndex<>0 then begin
    v:=libusb_get_string_descriptor_ascii(pdh, StrIndex, @buf[0], buflen);
    if v<=0 then Exit;
  end;

  Result:=PAnsiChar(@buf[0]);
end;

procedure TUSBHIDReadWrite.FReadFuncEP(Sender: TObject);  // Функция потока для чтения, приходящих данных от USB устройства
var
  tob:TSimpleThread;
  uhrw:TUSBHIDReadWrite;
  res, i,epnum,ep:Integer;
  bread,epps:Integer;
  buf:TArrayOfByte;
  pbuf:PByte;
begin
  tob:=Sender as TSimpleThread;
  uhrw:=TUSBHIDReadWrite(tob.UserPar);
  epnum:=tob.UserId;                                      // Номер конечной точки
  ep:=epnum or $80;

  epps:=FEndPointInfo[epnum].sizein;                      // Максимальный размер пакета конечной точки (конечной точки типа IN)
  buf:=nil;
  SetLength(buf, epps);
  pbuf:=@buf[0];

  repeat
    res:=-1;

    res:=libusb_interrupt_transfer(FDevHandle, ep, pbuf, epps, bread, 300);

    if (res=0)and(bread<>0) then begin                                         // Если данные приняты, то

      if FUseRecvRingBuf then begin                        // Если изпользуется кольцевой буфе, то данные помещаются в кольцевой буфер
        if FRBHelper.CanWrite then begin                   // Если в кольцевом буфере есть место, то можно заполнять данные
          i:=FRBHelper.Index4Write;
          FDataRingBuf[i].epn:=epnum;
          FDataRingBuf[i].len:=bread;
          Move(pbuf^, FDataRingBuf[i].buf[0], bread);
          FRBHelper.EndWrite;                              // Обязательно сообщаем, что данные записаны
        end;
      end;

      if Assigned(FOnDataIn) then FOnDataIn(pbuf, bread, epnum);  // Если назначен обработчик этого события, то запускаем этот обработчик
    end;

    if res=LIBUSB_ERROR_NO_DEVICE then Break;              // Если устройство было отключено, то прерываем поток
  until tob.CheckTerminated;                              // Проверка того, что поток необходимо остановить

  buf:=nil;

  i:=Length(FReadThreads)-1;
  if tob=FReadThreads[i] then begin                       // Если завершается последний поток из тех, которые считывают данные, то выполняются действия для оповещения основного потока об этом событии
    FReadThreadOn:=False;                                 // Потоки чтения не работают
    if res=LIBUSB_ERROR_NO_DEVICE then begin              // Если выход из потока произошёл из-за отключения устройства, то его нужно закрыть
      if Assigned(FOnNoConnect) then FOnNoConnect(uhrw);  // Вызов обработчика разрыва соединения, если он задан
      CloseUSBDevice;                                     // Ножно закрыть устройство т.к. произошла ошибка чтения данных
    end;
  end;
end;

function TUSBHIDReadWrite.OpenUSBDByHandle(RunReadThread: Boolean): Boolean;
var
  i,res,epnum:Integer;
  dev:Pointer;
  desc:libusb_device_descriptor;
  conf:plibusb_config_descriptor;
  pepd:plibusb_endpoint_descriptor;
  str:string;
  inepnums:array [0..15] of Byte;
  devname:string;
begin
  Result:=False;
  FOpenedDevName:='';

  // Теперь захватываем интерфейс
  res:=libusb_claim_interface(FDevHandle, 0);
  Log.Append(Format('libusb_claim_interface %d',[res]));
  if res<0 then begin
    libusb_close(FDevHandle);
    Exit;
  end;

  dev:=libusb_get_device(FDevHandle);

  Log.Append('Устройство открыто');

  res:=libusb_get_device_descriptor(dev, desc);
  if res<0 then
    Log.Append('Не удалось получить информацию об устройстве')
  else begin
    FManufactureString:=GetStringDesc(FDevHandle, desc.iManufacturer);
    FProductString:=GetStringDesc(FDevHandle, desc.iProduct);
    FSerialNumberString:=GetStringDesc(FDevHandle, desc.iSerialNumber);
  end;

  // Нужно определить кол-во конечных точек
  // Кол-во конечных точек будет определяться только для 1-й конфигурации (для начала будет так)
  conf:=nil;
  res:=libusb_get_config_descriptor_by_value(dev, 1, conf);
  if res<>0 then begin
    Log.Append('libusb_get_config_descriptor_by_value <> 0');
    libusb_close(FDevHandle);
    Exit;
  end;

  FEndPointINCount:=0;        // Кол-во конечных точек типа "IN"
  FEndPointOUTCount:=0;       // Кол-во конечных точек типа "OUT"
  FillByte(FEndPointInfo[0], SizeOf(TEndPointInfo)*Length(FEndPointInfo), 0);          // Обнуление массива, который хранит параметры конечных точек

  Log.Append(Format('End point count = %d',[conf^.interface_^.altsetting^.bNumEndpoints]));

  FEndPointINCount:=0;        // Кол-во конечных точек типа "IN"
  FEndPointOUTCount:=0;       // Кол-во конечных точек типа "OUT"
  FillByte(FEndPointInfo[0], SizeOf(TEndPointInfo)*Length(FEndPointInfo), 0);          // Обнуление массива, который хранит параметры конечных точек

  Log.Append(Format('End point count = %d',[conf^.interface_^.altsetting^.bNumEndpoints]));

  pepd:=conf^.interface_^.altsetting^.endpoint;                       // Получен адрес на массив конечных точек
  for i:=0 to conf^.interface_^.altsetting^.bNumEndpoints-1 do begin  // Цикл по конечным точкам, чтобы определить их параметры
    epnum:=pepd[i].bEndpointAddress and $0F;
    str:=Format('%d  EndpointAddress=%d', [i, epnum]);

    FEndPointInfo[epnum].attr:=pepd[i].bmAttributes;                // Сохранение атрибутов конечной точки

    if pepd[i].bEndpointAddress and $80<>0 then begin
      inepnums[FEndPointINCount]:=epnum;                              // Сохраняются номера конечных точек типа IN, чуть ниже этот массив понадобится при запуске потоков, которые будут ожидать данные от устройства
      Inc(FEndPointINCount);
      FEndPointInfo[epnum].sizein:=pepd[i].wMaxPacketSize;            // Максимальный размер пакета для конечной точки epnum если она работает как IN

      str:=str + ' IN ';
      case pepd[i].bmAttributes and %11 of
        LIBUSB_TRANSFER_TYPE_CONTROL      :  str:=str + ' Ctrl ';
        LIBUSB_TRANSFER_TYPE_ISOCHRONOUS  :  str:=str + ' Iso ';
        LIBUSB_TRANSFER_TYPE_BULK         :  str:=str + ' bulk ';
        LIBUSB_TRANSFER_TYPE_INTERRUPT    :  str:=str + ' Int ';
      end;
    end else begin
      Inc(FEndPointOUTCount);
      FEndPointInfo[epnum].sizeout:=pepd[i].wMaxPacketSize;           // Максимальный размер пакета для конечной точки epnum если она работает как OUT
      str:=str + ' OUT ';
      case pepd[i].bmAttributes and %11 of
        LIBUSB_TRANSFER_TYPE_CONTROL      :  str:=str + ' Ctrl ';
        LIBUSB_TRANSFER_TYPE_ISOCHRONOUS  :  str:=str + ' Iso ';
        LIBUSB_TRANSFER_TYPE_BULK         :  str:=str + ' bulk ';
        LIBUSB_TRANSFER_TYPE_INTERRUPT    :  str:=str + ' Int ';
      end;
    end;

    str:=str + Format('Pack size = %d', [pepd[i].wMaxPacketSize]);
    Log.Append(str);
  end;

  devname:=CreateUSBDevName(FDevHandle, desc);

  libusb_free_config_descriptor(conf);

  if RunReadThread then begin
    // Запуск потока, который будет принимать данные от USB-HID устройства
    // !!! ВНИМАНИЕ !!! не забудь, что в линуксе при работе с потоками необходимо подключить модуль cthreads самым первым в проекте (см. файл проекта *.lpr)
    // На каждую конечную точку типа IN создаётся свой поток чтения данных
    SetLength(FReadThreads, FEndPointINCount);      // Выделение памяти для массива, который будет хранить объекты для работы с потоками, которые ожидают данные от USB устройства
    for i:=0 to FEndPointINCount-1 do begin
      FReadThreads[i]:=TSimpleThread.CreateSimple;
      // В качестве пользовательского параметра изпользуется сам объект
      // В качестве параметра uid, передаваемого в поточную функцию будет передаваться номер конечной точки
      FReadThreads[i].StartThread(@FReadFuncEP, inepnums[i], Self);
    end;

    FReadThreadOn:=True;  // По этому признаку будет определяться нужно ли останавливать поток когда устройство закрывается
  end else
    FReadThreadOn:=False;  // Поток для чтения данных запущен не был, значит и останавливать его нет необходимости

  FUSBDeviceOpened:=True;
  FOpenedDevName:=devname;      // Сохранение имени открытого устройства

  Result:=True;
end;

procedure TUSBHIDReadWrite.StopReadThreads;   // Процедура для остановки потоков чтения данных
var
  i:Integer;
begin
  if FReadThreadOn then begin                       // Если потоки для чтения данных были запущены, то они останавливаются
    if FReadThreadOn then begin                     // Если поток для чтения запускался, то его нужно остановить
      for i:=0 to Length(FReadThreads)-1 do begin   // Остановка всех потоков, которые ожидают данные от USB устройства
        FReadThreads[i].Terminate;
        FReadThreads[i].WaitFor;
      end;
      FReadThreads:=nil;
      FReadThreadOn:=False;
    end;
  end;
end;

function TUSBHIDReadWrite.CreateUSBDevName(devhandle: plibusb_device_handle; desc: libusb_device_descriptor): string;
var
  res,i:Integer;
  ports:array [0..31] of Byte;
  dev:Pointer;
begin
  Result:='';

  dev:=libusb_get_device(devhandle);

  res:=libusb_get_port_numbers(dev, @ports[0], 32);
  if res>0 then begin
    Result:='usb port: ';
    for i:=0 to res-1 do Result:=Result + Format('%d-',[ports[i]]);
    Delete(Result, Length(Result), 1);
  end;

  Result:=Result + ' ';
  Result:=Result + Format('VID=%.4x  PID=%.4x',[desc.idVendor, desc.idProduct]);
  Result:=Result + ' '+GetStringDesc(devhandle, desc.iManufacturer);
  Result:=Result + ' '+GetStringDesc(devhandle, desc.iProduct);
  Result:=Result + ' '+GetStringDesc(devhandle, desc.iSerialNumber);
  Result:=Result + Format(' %.2d',[i]);
end;

function TUSBHIDReadWrite.CreateUSBDevNameShort(devhandle: plibusb_device_handle): string;
var
  res,i:Integer;
  ports:array [0..31] of Byte;
  dev:Pointer;
begin
  // Функция формирует короткое имя USB устройства, которое состоит только из строки вида: usb port: x.y - где x - номер шины, y - номер устройства на шине

  Result:='';

  dev:=libusb_get_device(devhandle);

  res:=libusb_get_port_numbers(dev, @ports[0], 32);
  if res>0 then begin
    Result:='usb port: ';
    for i:=0 to res-1 do Result:=Result + Format('%d-',[ports[i]]);
    Delete(Result, Length(Result), 1);
  end;
end;

procedure TUSBHIDReadWrite.CloseUSBDevice;
begin
  if not FUSBDeviceOpened then Exit;          // Если устройство не отрыто, то и делать тут нечего

  StopReadThreads;                            // Остановка потоков чтения данных

  libusb_release_interface(FDevHandle, 0);
  libusb_close(FDevHandle);

  // Очистка всех остальных параметров
  FManufactureString:='';
  FProductString:='';
  FSerialNumberString:='';
  FOpenedDevName:='';

  FEndPointINCount:=0;
  FEndPointOUTCount:=0;
  FillByte(FEndPointInfo[0], SizeOf(TEndPointInfo)*Length(FEndPointInfo), 0);          // Обнуление массива, который хранит размеры для конечных точек

  FUSBDeviceOpened:=False; // Выставляем признак того, что устройство не открыто
end;

function TUSBHIDReadWrite.GetUSBDeviceNames(nshort: Boolean; pid: Word; vid: Word): TStrings; // Получаем список имён USB устройств
var
  devs:pplibusb_device; // Это будет массив идентификаторов устройств USB (каждое устройство это указатель)
  i:Integer;
  v:Integer;
  dev_handle:plibusb_device_handle;
  desc:libusb_device_descriptor;
begin
  // nshort - Признак необходимости формирования коротких имён устройств
  // pid    - Если параметр не равен нулю, то будут выводиться имена только тех устройств PID которых равен указанному
  // vid    - Если параметр не равен нулю, то будут выводиться имена только тех устройств VID которых равен указанному

  Result:=TStringList.Create;

  devs:=nil;
  i:=libusb_get_device_list(FLibUSBCtx, @devs);
  if i<0 then Exit; // USB устройства не обнаружены

  for i:=0 to i-1 do begin // Цикл по списку устройств
    v:=libusb_get_device_descriptor(devs[i], desc);
    if (pid<>0)and(vid<>0) then begin // Если входные параметры НЕ равны нулю, значит задан фильтр по идентификаторам
      if (pid<>desc.idProduct)or(vid<>desc.idVendor) then Continue;// Если номера идентификаторов не соответствуют тем, что заданы, то переходим к следующему устройству
    end;

    if v<0 then Continue; // Если не удалось получить описатель устройства, то переключаемся на следующее устройство

    dev_handle:=nil;
    v:=libusb_open(devs[i], dev_handle); // Получение значение через которое можно определять различные параметры устройства
    if v<>0 then Continue;  // Если произошла ошибка, то переключаемя на следующее устройство

    if nshort then                                          // Если нужно формировать короткое имя, то
      Result.Append(CreateUSBDevNameShort(dev_handle))      // формирование короткого имени
    else
      Result.Append(CreateUSBDevName(dev_handle, desc));    // формирование длинного имени

    libusb_close(dev_handle); // Закрываем устройство
  end;

  libusb_free_device_list(devs, 1); // Освобождение памяти от списка идентификаторов устройств (кол-во ссылок на устройства уменьшается на 1)
end;

function TUSBHIDReadWrite.OpenUSBDByPID_VID(PID, VID: Word; RunReadThread: Boolean): Boolean;
begin
  Log.Clear;
  Result:=False;

  if FUSBDeviceOpened then begin
    Log.Append(Format('Уже открыто устройство:  %s', [FOpenedDevName]));
    Exit;
  end;

  FDevHandle:=libusb_open_device_with_vid_pid(FLibUSBCtx, vid, pid);
  if FDevHandle=nil then Exit;

  Result:=OpenUSBDByHandle(RunReadThread);
end;

function TUSBHIDReadWrite.OpenUSBDByName(name: string; shortname: Boolean; RunReadThread: Boolean): Boolean;
var
  devs:pplibusb_device; // Это будет массив идентификаторов устройств USB (каждое устройство это указатель)
  i:Integer;
  v:Integer;
  dev_handle:plibusb_device_handle;
  desc:libusb_device_descriptor;
  str:string;
begin
  // name           - Имя USB устройства
  // shortname      - Признак того, что имя USB устройства короткое, а значит для формирования имени нужно изпользовать функцию CreateUSBDevNameShort вместо CreateUSBDevName
  // RunReadThread  - Признак для запуска потока(ов), которые выполняют чтение данных из конечных точек USB устройства

  Result:=False;
  Log.Clear;

  if FUSBDeviceOpened then begin
    Log.Append(Format('Уже открыто устройство:  %s', [FOpenedDevName]));
    Exit;
  end;

  devs:=nil;
  i:=libusb_get_device_list(FLibUSBCtx, @devs);
  if i<0 then Exit; // USB устройства не обнаружены

  for i:=0 to i-1 do begin // Цикл по списку устройств
    str:='';
    v:=libusb_get_device_descriptor(devs[i], desc);

    if v<0 then Continue; // Если не удалось получить описатель устройства, то переключаемся на следующее устройство

    dev_handle:=nil;
    v:=libusb_open(devs[i], dev_handle); // Получение значение через которое можно определять различные параметры устройства
    if v<>0 then Continue;  // Если произошла ошибка, то переключаемя на следующее устройство

    if shortname then                               // Если для открытия USB устройства изпользуется короткое имя, то
      str:=CreateUSBDevNameShort(dev_handle)         // формируется короткое имя
    else
      str:=CreateUSBDevName(dev_handle, desc);      // иначе формируется длинное имя

    if str=name then begin
      FDevHandle:=dev_handle;
      Result:=OpenUSBDByHandle(RunReadThread);
      Break;
    end;
    libusb_close(dev_handle); // Закрываем устройство
  end;

  libusb_free_device_list(devs, 1); // Освобождение памяти от списка идентификаторов устройств (кол-во ссылок на устройства уменьшается на 1)
end;

function TUSBHIDReadWrite.RecvData(epnum: Integer; buf: PByte; tout: Integer): Integer;
var
  bread:Integer;
  res,sz:Integer;
begin
  if not FUSBDeviceOpened then Exit(-1);      // Если устройство не открыто, то делать тут нечего

  sz:=FEndPointInfo[epnum].sizeout;         // Размер пакета, который можно прогнать через конечную точку за раз

  res:=libusb_interrupt_transfer(FDevHandle, epnum or $80, buf, sz, bread, tout);
  if res=0 then Result:=bread else Result:=res;
end;

function TUSBHIDReadWrite.SendData(epnum: Integer; buf: PByte; tout: Integer): Integer;
var
  bwrite:Integer;
  res,sz:Integer;
begin
  if not FUSBDeviceOpened then Exit(-1);      // Если устройство не открыто, то делать тут нечего

  sz:=FEndPointInfo[epnum].sizein;
  bwrite:=0;
  res:=libusb_interrupt_transfer(FDevHandle, epnum, buf, sz, bwrite, tout);

  if res=0 then Result:=bwrite else Result:=res;
end;

procedure TUSBHIDReadWrite.SetUseRingBuf(rblen: Integer); // Процедура устанавливает размер кольцевого буфера для входящих сообщений
begin
  if rblen<=0 then Exit;

  FUseRecvRingBuf:=True;                    // Признак того, что будет изпользоваться кольцевой буфер для входящих сообщений
  FRBHelper:=TRingBufHelper.Create(rblen);  // Помошник для кольцевого буфера
  SetLength(FDataRingBuf, rblen);           // Массив, который будет изпользоваться для приёма входящих сообщений для кольцевого буфера
end;

function TUSBHIDReadWrite.ReadRingBuf(out mes: THIDReportData): Boolean;    // Функция считывет данные из кольцевого буфера
var
  i:Integer;
begin
  Result:=FRBHelper.CanRead;
  if not Result then Exit(False);
  mes:=FDataRingBuf[FRBHelper.Index4Read];
  FRBHelper.EndRead;
end;


end.
