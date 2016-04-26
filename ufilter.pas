unit UFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, Buttons, UMetadata, USQL;

type

  TFilter = record
    Fields: TComboBox;
    Operations: TComboBox;
    Constant: TEdit;
    DeleteFilter: TBitBtn;
  end;

  TSystemCondition = record
    Condition: TCondition;
    Value: String;
  end;

  TFilterField = record
    AppName, DBName: String;
  end;

  TConditions = array of TCondition;

  { TFilters }

  TFilters = class(TObject)
    public
      SystemConditions: array of TSystemCondition;
      Filters: array of TFilter;
      FFields: array of TFilterField;
      constructor Create(ATableIndex: Integer);
      procedure SetSystemConditions(AConditions: array of TSystemCondition);
      procedure SetFields(ATableIndex: Integer);
      procedure AddFilter(AOwner: TWinControl; AOnChangeFilter: TNotifyEvent; LeftAddition: Integer);
      procedure DeleteFilter(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
      function ToConditions(): TConditions;
    private
      CurrentFilterY: Integer;
  end;

implementation

const
  MarginTopFilters = 10;
  MarginLeftFilters = 10;

var
  Operations: array[0..6] of String = ('>', '<', '=', '>=', '<=' , '<>', 'LIKE');

{ TFilters }

constructor TFilters.Create(ATableIndex: Integer);
begin
  inherited Create();
  CurrentFilterY:= 0;
  SetFields(ATableIndex);
end;

procedure TFilters.SetSystemConditions(AConditions: array of TSystemCondition);
var
  i: Integer;
begin
  SetLength(SystemConditions, Length(AConditions));
  for i:= 0 to High(SystemConditions) do begin
    SystemConditions[i].Condition:= AConditions[i].Condition;
    SystemConditions[i].Value:= AConditions[i].Value;
  end;
end;

procedure TFilters.SetFields(ATableIndex: Integer);
var
  i, j: Integer;
  LField: TLink;
begin
  for i:= 1 to High(Tables[ATableIndex].Fields) do begin
    if Tables[ATableIndex].Fields[i] is TLink then begin
      LField:= Tables[ATableIndex].Fields[i] as TLink;
      for j:= 1 to High(Tables[LField.RefTable].Fields) do begin
        SetLength(FFields, Length(FFields) + 1);
        FFields[High(FFields)].AppName:= Tables[LField.RefTable].Fields[j].FAppName;
        FFields[High(FFields)].DBName:= Tables[LField.RefTable].TDBName + '.'
          + Tables[LField.RefTable].Fields[j].FDBName;
      end;
    end
    else begin
      SetLength(FFields, Length(FFields) + 1);
      FFields[High(FFields)].AppName:= Tables[ATableIndex].Fields[i].FAppName;
      FFields[High(FFields)].DBName:= Tables[ATableIndex].TDBName + '.' + Tables[ATableIndex].Fields[i].FDBName;
    end;
  end;
end;

procedure TFilters.AddFilter(AOwner: TWinControl; AOnChangeFilter: TNotifyEvent; LeftAddition: Integer);
var
  Filter: TFilter;
  i: Integer;
begin
  Filter.Fields:= TComboBox.Create(AOwner);
  with Filter.Fields do begin
    Left:= LeftAddition + MarginLeftFilters;
    Top:= CurrentFilterY + MarginTopFilters;
    for i:= 0 to High(FFields) do
      Items.Add(FFields[i].AppName);
    ItemIndex:= 0;
    ReadOnly:= True;
    Parent:= AOwner;
    OnChange:= AOnChangeFilter;
  end;

  Filter.Operations:= TComboBox.Create(AOwner);
  with Filter.Operations do begin
    Left:= Filter.Fields.Left + Filter.Fields.Width + MarginLeftFilters;
    Top:= Filter.Fields.Top;
    for i:= 0 to High(Operations) do
      Items.Add(Operations[i]);
    ItemIndex:= 0;
    ReadOnly:= True;
    Parent:= AOwner;
    OnChange:= AOnChangeFilter;
  end;

  Filter.Constant:= TEdit.Create(AOwner);
  with Filter.Constant do begin
    Left:= Filter.Operations.Left + Filter.Operations.Width + MarginLeftFilters;
    Top:= Filter.Fields.Top;
    Parent:= AOwner;
    OnChange:= AOnChangeFilter;
  end;

  Filter.DeleteFilter:= TBitBtn.Create(AOwner);
  with Filter.DeleteFilter do begin
    Left:= Filter.Constant.Left + Filter.Constant.Width + MarginLeftFilters;
    Top:= Filter.Fields.Top;
    Glyph.LoadFromFile('img/delete.bmp');
    Spacing:= 0;
    Height:= 25;
    Width:= 25;
    Parent:= AOwner;
    Tag:= Length(Filters);
    OnMouseUp:= @DeleteFilter;
  end;

  SetLength(Filters, Length(Filters) + 1);
  Filters[High(Filters)]:= Filter;
  CurrentFilterY:= Filter.Fields.Top + Filter.Fields.Height;
  Filter.Fields.OnChange(Filter.Fields);
end;

procedure TFilters.DeleteFilter(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  Filters[(Sender as TBitBtn).Tag].Fields.OnChange(Sender);
  for i:= (Sender as TBitBtn).Tag to High(Filters) - 1 do begin
    Filters[i].Fields.ItemIndex:= Filters[i + 1].Fields.ItemIndex;
    Filters[i].Operations.ItemIndex:= Filters[i + 1].Operations.ItemIndex;
    Filters[i].Constant.Text:= Filters[i + 1].Constant.Text;
    Filters[i].DeleteFilter.Tag:= i;
  end;

  with Filters[High(Filters)] do begin
    Fields.Free;
    Operations.Free;
    Constant.Free;
    DeleteFilter.Free;
  end;

  SetLength(Filters, Length(Filters) - 1);

  if Length(Filters) = 0 then
    CurrentFilterY:= 0
  else
    CurrentFilterY:= Filters[High(Filters)].Fields.Top + Filters[High(Filters)].Fields.Height;
end;

function TFilters.ToConditions: TConditions;
var
  i: Integer;
  Conds: TConditions;
begin
  SetLength(Conds, Length(Filters) + Length(SystemConditions));
  for i:= 0 to High(Filters) do begin
    Conds[i].Field:= FFields[Filters[i].Fields.ItemIndex].DBName;
    Conds[i].Operation:= Filters[i].Operations.Text;
  end;
  for i:= 0 to High(SystemConditions) do begin
    Conds[i + Length(Filters)].Field:= SystemConditions[i].Condition.Field;
    Conds[i + Length(Filters)].Operation:= SystemConditions[i].Condition.Operation;
  end;

  Result:= Conds;
end;

end.

