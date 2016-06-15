unit UMetadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TField }

  TField = class(TObject)
    public
      FDBName, FAppName: String;
      FieldWidth: Integer;
      Visible: Boolean;
      constructor Create(ADBName, AAppName: String; AVisible: Boolean = False; AFieldWidth: Integer = 0); overload;
  end;

  { TLink }

  TLink = class(TField)
    public
      RefTable, RefField: Integer;
      constructor Create(ADBName, AAppName: String; ARefTable, ARefField: Integer;
         AVisible: Boolean = False; AFieldWidth: Integer = 0); overload;
  end;

  { TTable }

  TTable = class(TObject)
    public
      TDBName, TAppName: String;
      Fields: array of TField;
      class function AddTable(ADBName, AAppName: String): TTable;
      function AddField(AField: TField): TTable;
  end;

  {Records for TimeTable}

  TRecord = record
    Rec: array of String;
    ID: Integer;
  end;

  TCell = array of TRecord;

  TTimeTable = array of array of TCell;

  TCap = record
    Value: String;
    ID: Integer;
    isEmpty: Boolean;
  end;

  TCaps = array of TCap;
  TFieldsName = array of String;

  { TConflictType }

  TConflict = class(TObject)
    Name: String;
    SameColumns: array of String;

    constructor Create(AName: String; ASameColumns: array of String);
  end;

  { TCompareConf }

  TCompareConf = class(TConflict)
    DifferentColumns: array of String;

    constructor Create(AName: String; ASameColumns, ADifferentColumns: array of String);
  end;

  { TOverflowConf }

  TOverflowConf = class(TConflict)
    CapacityColumn, CountColumn: String;

    constructor Create(AName: String; ACapacityColumn, ACountColumn: String; ASameColumns: array of String);
  end;

var
  ConflictTypes: array of TConflict;
  Tables: array of TTable;

implementation

{ TTable }

class function TTable.AddTable(ADBName, AAppName: String): TTable;
begin
  SetLength(Tables, Length(Tables) + 1);
  Tables[High(Tables)]:= TTable.Create;

  with Tables[High(Tables)] do begin
    TDBName:= ADBName;
    TAppName:= AAppName;
  end;
  Result:= Tables[High(Tables)];
end;

function TTable.AddField(AField: TField): TTable;
begin
  SetLength(Fields, Length(Fields) + 1);
  Fields[High(Fields)]:= AField;
  Result:= Self;
end;

{ TLink }

constructor TLink.Create(ADBName, AAppName: String; ARefTable, ARefField: Integer;
  AVisible: Boolean = False; AFieldWidth: Integer = 0);
begin
  inherited Create(ADBName, AAppName, AVisible, AFieldWidth);

  RefTable:= ARefTable;
  RefField:= ARefField;
end;

{ TField }

constructor TField.Create(ADBName, AAppName: String; AVisible: Boolean = False; AFieldWidth: Integer = 0);
begin
  Self.Create;

  FDBName:= ADBName;
  FAppName:= AAppName;
  FieldWidth:= AFieldWidth;
  Visible:= AVisible;
end;

{ TConflictType }

procedure AddConflictType(AConflict: TConflict);
begin
  SetLength(ConflictTypes, Length(ConflictTypes) + 1);
  ConflictTypes[High(ConflictTypes)]:= AConflict;
end;

{ TConflict }

constructor TConflict.Create(AName: String; ASameColumns: array of String);
var
  i: Integer;
begin
  inherited Create;

  Name:= AName;
  SetLength(SameColumns, Length(ASameColumns));
  for i:= 0 to High(SameColumns) do
    SameColumns[i]:= ASameColumns[i];
end;

{ TOverflowConf }

constructor TOverflowConf.Create(AName: String; ACapacityColumn, ACountColumn: String;
  ASameColumns: array of String);
begin
  inherited Create(AName, ASameColumns);

  CapacityColumn:= ACapacityColumn;
  CountColumn:= ACountColumn;
end;

{ TCompareConf }

constructor TCompareConf.Create(AName: String; ASameColumns, ADifferentColumns: array of String);
var
  i: Integer;
begin
  inherited Create(AName, ASameColumns);

  SetLength(DifferentColumns, Length(ADifferentColumns));
  for i:= 0 to High(DifferentColumns) do
    DifferentColumns[i]:= ADifferentColumns[i];
end;

initialization

TTable.AddTable('CLASSROOMS', 'Аудитории')
 .AddField(TField.Create('ID', 'ID', True, 0))
 .AddField(TField.Create('NAME', 'Аудитория', True, 100))
 .AddField(TField.Create('CAPACITY', 'Вместимость', True, 80));

TTable.AddTable('GROUPS', 'Группы')
 .AddField(TField.Create('ID', 'ID', True, 0))
 .AddField(TField.Create('NAME', 'Группа', True, 100))
 .AddField(TField.Create('STUDENTS', 'Студентов', True, 70));

TTable.AddTable('LESSONS', 'Предметы')
   .AddField(TField.Create('ID', 'ID', True, 0))
   .AddField(TField.Create('NAME', 'Предмет', True, 280));

TTable.AddTable('LESSONS_TIMES', 'Время')
   .AddField(TField.Create('ID', 'ID', True, 0))
   .AddField(TField.Create('BEGIN_', 'Начало', True, 100))
   .AddField(TField.Create('END_', 'Конец', True, 100));

TTable.AddTable('LESSONS_TYPES', 'Типы лекций')
   .AddField(TField.Create('ID', 'ID', True, 0))
   .AddField(TField.Create('NAME', 'Тип', True, 180));

TTable.AddTable('TEACHERS', 'Преподаватели')
   .AddField(TField.Create('ID', 'ID', True, 0))
   .AddField(TField.Create('LAST_NAME', 'Фамилия', True, 150))
   .AddField(TField.Create('FIRST_NAME', 'Имя', True, 150))
   .AddField(TField.Create('MIDDLE_NAME', 'Отчество', True, 150));

TTable.AddTable('WEEKDAYS', 'Дни недели')
   .AddField(TField.Create('ID', 'ID', True, 0))
   .AddField(TField.Create('NAME', 'День недели', True, 120));


TTable.AddTable('TIMETABLE', 'Расписание')
   .AddField(TField.Create('ID', 'ID', True, 0))
   .AddField(TField.Create('START_PERIOD', 'Начало действия', True, 120))
   .AddField(TField.Create('END_PERIOD', 'Конец действия', True, 120))
   .AddField(TLink.Create('LESSON_ID', 'Предмет', 2, 0, True, 200))
   .AddField(TLink.Create('LESSON_TYPE_ID', 'Тип', 4, 0, True, 200))
   .AddField(TLink.Create('TEACHER_ID', 'Преподаватель', 5, 0, True, 200))
   .AddField(TLink.Create('GROUP_ID', 'Группа', 1, 0, True, 200))
   .AddField(TLink.Create('CLASSROOM_ID', 'Аудитория', 0, 0, True, 200))
   .AddField(TLink.Create('WEEKDAY_ID', 'День недели', 6, 0, True, 200))
   .AddField(TLink.Create('LESSON_TIME_ID', 'Время', 3, 0, True, 200));

AddConflictType(TCompareConf.Create('В одной аудитории одновременно ведут два перподавателя',
  ['CLASSROOM_ID', 'WEEKDAY_ID', 'LESSON_TIME_ID'], ['TEACHER_ID']));
AddConflictType(TCompareConf.Create('Преподаватель одновременно ведет пары в двух разных аудиториях',
  ['TEACHER_ID', 'WEEKDAY_ID', 'LESSON_TIME_ID'], ['CLASSROOM_ID']));
AddConflictType(TCompareConf.Create('У одной группы одновременно проходят разные пары',
  ['GROUP_ID', 'WEEKDAY_ID', 'LESSON_TIME_ID'], ['LESSON_ID']));
AddConflictType(TCompareConf.Create('У одной группы одновременно проходят пары в разных аудиториях',
  ['GROUP_ID', 'WEEKDAY_ID', 'LESSON_TIME_ID'], ['CLASSROOM_ID']));
AddConflictType(TCompareConf.Create('Одинаковые записи',
  ['LESSON_ID', 'LESSON_TYPE_ID', 'TEACHER_ID', 'GROUP_ID', 'CLASSROOM_ID', 'WEEKDAY_ID', 'LESSON_TIME_ID'], []));
AddConflictType(TOverflowConf.Create('Аудитория переполнена', 'CAPACITY', 'STUDENTS',
  ['CLASSROOM_ID', 'WEEKDAY_ID', 'LESSON_TIME_ID']));

end.

