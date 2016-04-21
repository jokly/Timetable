unit UEditCard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DbCtrls, UMetadata, USQL, UDBConnection, UNotification;

type

  TEditor = record
    FLabel: TLabel;
    Control: TWinControl;
    IDs: array of Integer;
    TableIndex, CountFields: Integer;
  end;

  { TEditCard }

  TEditCard = class(TForm)
    DataSource: TDataSource;
    ScrollBox: TScrollBox;
    SQLQuery: TSQLQuery;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
    TableIndex, FieldID, CurrentY: Integer;
    FBSQL: TSQL;
    Editors: array of TEditor;
    procedure GetRecord();
    function AddLable(ACaption: String): TLabel;
    procedure AddEdit(ALabelText: String; AWidth: Integer; ACaption: String = '');
    procedure AddComboBox(ATableIndex: Integer; ACaption: String);
    procedure AddButClick(Sender: TObject);
    procedure EditButClick(Sender: TObject);
  public
    SetCursorPosition: TNotifyEvent;
    constructor Create(ATableIndex: Integer; ASetCursorPos: TNotifyEvent; AFieldID: Integer = -1); overload;
  end;

var
  EditCard: TEditCard;

implementation

const
     MarginTop = 10;
     MarginLeft = 10;
     LabelHeight = 23;

{$R *.lfm}

{ TEditCard }

procedure TEditCard.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
begin
  TNotification.UpdateDirectoryForms;
  TNotification.DeleteFromProtect(TableIndex, FieldID);

  for i:= 0 to High(Editors) do
    if Editors[i].Control is TComboBox then
        TNotification.DeleteFromProtect(Editors[i].TableIndex);

  SetCursorPosition(Sender);
end;

procedure TEditCard.GetRecord();
var
  Cond: TCondition;
begin
  Cond.Field:= Tables[TableIndex].TDBName + '.ID';
  Cond.Operation:= '=';
  SQLQuery.Close;
  SQLQuery.SQL.Text:= FBSQL.SelectAllFrom(TableIndex).InnerJoin().Where([Cond]).Query;
  SQLQuery.Params[0].AsString:= IntToStr(FieldID);
  SQLQuery.Open;
end;

constructor TEditCard.Create(ATableIndex: Integer; ASetCursorPos: TNotifyEvent; AFieldID: Integer = -1);
var
  i, j, k: Integer;
  Field: TField;
  Link: TLink;
  Cap: String;
  ExecuteBtn: TButton;

begin
  inherited Create(Application);

  TableIndex:= ATableIndex;
  FieldID:= AFieldID;
  FBSQL:= TSQL.Create;
  SetCursorPosition:= ASetCursorPos;

  CurrentY:= 0;
  k:= 1;
  for i:= 0 to High(Tables[ATableIndex].Fields) do begin
    GetRecord();
    Field:= Tables[ATableIndex].Fields[i];
    if Field.FDBName = 'ID' then
       Continue;
    if Field is TLink then begin
      Link:= Field as TLink;
      Cap:= '';
      for j:= 0 to High(Tables[Link.RefTable].Fields) do begin
        Field:= Tables[Link.RefTable].Fields[j];
        if Field.FDBName = 'ID' then
           Continue;
        Cap+= DataSource.DataSet.Fields.Fields[k].AsString + ' ';
        Inc(k);
      end;
      Delete(Cap, High(Cap), 1);
      AddComboBox(Link.RefTable, Cap);
    end
    else begin
      AddEdit(Field.FAppName, Field.FieldWidth, DataSource.DataSet.Fields.Fields[k].AsString);
      Inc(k);
    end;
  end;

  ExecuteBtn:= TButton.Create(ScrollBox);
  with ExecuteBtn do begin
    Left:= MarginLeft;
    Top:= CurrentY + MarginTop;
    Width:= 100;
    Caption:= 'Применить';
    if AFieldID = -1 then
      OnClick:= @AddButClick
    else begin
      TNotification.AddToProtect(ATableIndex, AFieldID);
      OnClick:= @EditButClick;
    end;
    Parent:= ScrollBox;
  end;
end;

procedure TEditCard.AddButClick(Sender: TObject);
var
  i: Integer;
  Control: TWinControl;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text:= FBSQL.InsertRecord(TableIndex, Length(Editors)).Query;
  SQLQuery.Prepare;
  for i:= 0 to High(Editors) do begin
    Control:= Editors[i].Control;
    if Control is TComboBox then
       SQLQuery.Params[i].AsInteger:= Editors[i].IDs[(Control as TComboBox).ItemIndex]
    else
       SQLQuery.Params[i].AsString:= (Control as TEdit).Caption;
  end;
  SQLQuery.ExecSQL;
  DBConnection.SQLTransaction.Commit;
  TNotification.UpdateDirectoryForms;
  Self.Close;
end;

procedure TEditCard.EditButClick(Sender: TObject);
var
  i: Integer;
  FieldsName: array of String;
  Cond: TCondition;
  Control: TWinControl;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text:= FBSQL.SelectAllFrom(TableIndex).Query;
  SQLQuery.Open;

  Cond.Operation:= '=';
  Cond.Field:= 'ID';
  SetLength(FieldsName, SQLQuery.FieldDefs.Count - 1);
  for i:= 0 to SQLQuery.FieldDefs.Count - 2 do
    FieldsName[i]:= SQLQuery.FieldDefs.Items[i + 1].DisplayName;
  SQLQuery.Close;
  SQLQuery.SQL.Text:= FBSQL.UpdateRecord(TableIndex, FieldsName, Cond).Query;
  SQLQuery.Prepare;
  for i:=0 to High(Editors) do begin
    Control:= Editors[i].Control;
    if Control is TComboBox then
      SQLQuery.Params[i].AsInteger:= Editors[i].IDs[(Control as TComboBox).ItemIndex]
    else
       SQLQuery.Params[i].AsString:= (Control as TEdit).Caption;
  end;
  SQLQuery.Params[i + 1].AsInteger:= FieldID;
  SQLQuery.ExecSQL;
  DBConnection.SQLTransaction.Commit;
  Self.Close;
end;

function TEditCard.AddLable(ACaption: String): TLabel;
var
  FLabel: TLabel;
begin
  FLabel:= TLabel.Create(ScrollBox);
  with FLabel do begin
    Top:= CurrentY + MarginTop;
    CurrentY:= Top + LabelHeight;
    Left:= MarginLeft;
    Caption:= ACaption;
    Height:= LabelHeight;
    Parent:= ScrollBox;
  end;
  Result:= FLabel;
end;

procedure TEditCard.AddEdit(ALabelText: String; AWidth: Integer; ACaption: String = '');
var
  FEditor: TEditor;
begin
  FEditor.FLabel:= AddLable(ALabelText);
  FEditor.Control:= TEdit.Create(ScrollBox);
  with FEditor.Control as TEdit do begin
    Width:= AWidth;
    Left:= MarginLeft;
    Top:= CurrentY;
    CurrentY:= Top + Height;
    Caption:= ACaption;
    Parent:= ScrollBox;
  end;
  SetLength(Editors, Length(Editors) + 1);
  Editors[High(Editors)]:= FEditor;
end;

procedure TEditCard.AddComboBox(ATableIndex: Integer; ACaption: String);
var
  i, k: Integer;
  FEditor: TEditor;
  Field: TField;
  CurText, LabelText: String;
begin
  TNotification.AddToProtect(ATableIndex);
  for i:= 1 to High(Tables[ATableIndex].Fields) do
    LabelText+= Tables[ATableIndex].Fields[i].FAppName + ' ';

  FEditor.FLabel:= AddLable(LabelText);

  SQLQuery.Close;
  SQLQuery.SQL.Text:= FBSQL.SelectAllFrom(ATableIndex).
     OrderBy([Tables[ATableIndex].TDBName + '.ID']).Query;
  SQLQuery.Open;

  FEditor.Control:= TComboBox.Create(ScrollBox);
  FEditor.CountFields:= Length(Tables[ATableIndex].Fields) - 1;
  FEditor.TableIndex:= ATableIndex;
  for i:= 1 to High(Tables[ATableIndex].Fields) do begin
    Field:= Tables[ATableIndex].Fields[i];
    with FEditor.Control as TComboBox do begin
      if i = 1 then
         Width:= Field.FieldWidth
      else
         Width:= Width + Field.FieldWidth;
      Left:= MarginLeft;
      Top:= CurrentY;
      ReadOnly:= True;
      Tag:= Length(Editors);
      Parent:= ScrollBox;
      SQLQuery.First;
      k:= 0;
      while(not SQLQuery.EOF) do begin
        CurText:= SQLQuery.FieldByName(SQLQuery.FieldDefs.
           Items[i].DisplayName).Text;
        if i = 1 then begin
          SetLength(FEditor.IDs, Length(FEditor.IDs) + 1);
          FEditor.IDs[High(FEditor.IDs)]:= DataSource.DataSet.FieldByName('ID').AsInteger;
          Items.Add(CurText);
        end
        else begin
          Items[k]:= Items[k] + ' ' + CurText;
        end;
        if Items[k] = ACaption then
           ItemIndex:= k;
        SQLQuery.Next;
        Inc(k);
      end;
      if ItemIndex = -1 then
         ItemIndex:= 0;
    end;
  end;

  with FEditor.Control do
    CurrentY:= Top + Height;

  SetLength(Editors, Length(Editors) + 1);
  Editors[High(Editors)]:= FEditor;
end;

end.

