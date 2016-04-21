unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  UMetadata, UDirectoryForm, UTimeTable;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MTimetable: TMenuItem;
    MTables: TMenuItem;
    MAbout: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure MAboutClick(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    procedure MTimetableClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
  Item: TMenuItem;
begin
  for i:= 0 to High(Tables) do begin
    Item:= TMenuItem.Create(MTables);
    with Item do begin
      Caption:= Tables[i].TAppName;
      Tag:= i;
      OnClick:= @MenuItemClick;
    end;
    MTables.Add(Item);
  end;
end;

procedure TMainForm.MAboutClick(Sender: TObject);
begin
  ShowMessage('Сластен Тихон Дмитриевич (с) ДВФУ Б8103а');
end;

procedure TMainForm.MenuItemClick(Sender: TObject);
begin
  TDirectoryForm.Create((Sender as TMenuItem).Tag);
end;

procedure TMainForm.MTimetableClick(Sender: TObject);
begin
  TTimetableForm.Create(Self);
end;

end.

