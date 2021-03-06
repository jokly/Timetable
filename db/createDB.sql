CREATE DATABASE 'localhost:C:\Users\slast\Documents\Projects\Timetable\db\FEFU.FDB' USER 'SYSDBA' PASSWORD 'masterkey' 
	DEFAULT CHARACTER SET UTF8;
CREATE GENERATOR GroupsIdGenerator;
CREATE GENERATOR LessonsIdGenerator;
CREATE GENERATOR TeachersIdGenerator;
CREATE GENERATOR ClassRoomsIdGenerator;
CREATE GENERATOR LessonsTimesIdGenerator;
CREATE GENERATOR WeekDaysIdGenerator;
CREATE GENERATOR TimeTableIdGenerator;
CREATE GENERATOR LessonsTypesIdGenerator;

CREATE TABLE Groups(
	id INTEGER NOT NULL PRIMARY KEY,
	name VARCHAR(10),
	students INTEGER
);

CREATE TABLE Lessons(
	id INTEGER NOT NULL PRIMARY KEY,
	name VARCHAR(100)
);

CREATE TABLE Teachers(
	id INTEGER NOT NULL PRIMARY KEY,
	last_name VARCHAR(20),
	first_name VARCHAR(20), 
    middle_name VARCHAR(20)
);

CREATE TABLE Classrooms(
	id INTEGER NOT NULL PRIMARY KEY,
	name VARCHAR(10),
	capacity INTEGER
);

CREATE TABLE Lessons_Times(
	id INTEGER NOT NULL PRIMARY KEY,
	begin_ TIME,
	end_ TIME
);

CREATE TABLE Weekdays(
	id INTEGER NOT NULL PRIMARY KEY,
	name VARCHAR(20)
);

CREATE TABLE Lessons_Types(
	id INTEGER NOT NULL PRIMARY KEY,
	name VARCHAR(50)
);

CREATE TABLE Timetable(
	id INTEGER NOT NULL PRIMARY KEY,
	start_period DATE,
	end_period DATE,
	lesson_id INTEGER REFERENCES Lessons(id) ON UPDATE CASCADE ON DELETE CASCADE,
	lesson_type_id INTEGER REFERENCES Lessons_Types(id) ON UPDATE CASCADE ON DELETE CASCADE,
	teacher_id INTEGER REFERENCES Teachers(id) ON UPDATE CASCADE ON DELETE CASCADE,
	group_id INTEGER REFERENCES Groups(id) ON UPDATE CASCADE ON DELETE CASCADE,
	classroom_id INTEGER REFERENCES Classrooms(id) ON UPDATE CASCADE ON DELETE CASCADE,
	weekday_id INTEGER REFERENCES Weekdays(id) ON UPDATE CASCADE ON DELETE CASCADE,
	lesson_time_id INTEGER REFERENCES Lessons_Times(id) ON UPDATE CASCADE ON DELETE CASCADE
);

SET TERM ^ ;

CREATE TRIGGER GroupsIdTrigger FOR Groups ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR GroupsIdGenerator;
END^

CREATE TRIGGER TeachersIdTrigger FOR Teachers ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR TeachersIdGenerator;
END^

CREATE TRIGGER LessonsIdTrigger FOR Lessons ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR LessonsIdGenerator;
END^

CREATE TRIGGER LessonsTimesIdTrigger FOR Lessons_Times ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR LessonsTimesIdGenerator;
END^

CREATE TRIGGER WeekDaysIdTrigger FOR Weekdays ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR WeekDaysIdGenerator;
END^

CREATE TRIGGER ClassRoomsIdTrigger FOR Classrooms ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR ClassRoomsIdGenerator;
END^

CREATE TRIGGER LessonsTypesIdTrigger FOR Lessons_Types ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR LessonsTypesIdGenerator;
END^

CREATE TRIGGER TimeTableIdTrigger FOR Timetable ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR TimeTableIdGenerator;
END^

SET TERM ; ^

COMMIT;

INSERT INTO Groups VALUES(1, 'Б8103а1', 30);
INSERT INTO Groups VALUES(2, 'Б8103а2', 30);
INSERT INTO Groups VALUES(3, 'Б8103б1', 30);
INSERT INTO Groups VALUES(4, 'Б8103б2', 30);
INSERT INTO Groups VALUES(5, 'Б8102-1', 30);
INSERT INTO Groups VALUES(6, 'Б8102-2', 30);
INSERT INTO Groups VALUES(7, 'С3107в', 30);

INSERT INTO Lessons VALUES(1, 'Математический анализ');
INSERT INTO Lessons VALUES(2, 'Дискретная математика и математическая логика');
INSERT INTO Lessons VALUES(3, 'Алгебра и геометрия');
INSERT INTO Lessons VALUES(4, 'Иностранный язык');
INSERT INTO Lessons VALUES(5, 'Практикум на ЭВМ');
INSERT INTO Lessons VALUES(6, 'Языки и методы программирования');
INSERT INTO Lessons VALUES(7, 'Базы данных');
INSERT INTO Lessons VALUES(8, 'Архитектура компьютеров');
INSERT INTO Lessons VALUES(9, 'Элективные курсы по физической культуре');
INSERT INTO Lessons VALUES(10, 'Software engineering');
INSERT INTO Lessons VALUES(11, 'Теоретическая математика и физика');
INSERT INTO Lessons VALUES(12, 'Русский язык и культура речи');
INSERT INTO Lessons VALUES(13, 'Алгебра');
INSERT INTO Lessons VALUES(14, 'История');
INSERT INTO Lessons VALUES(15, 'Теоретическая механика');
INSERT INTO Lessons VALUES(16, 'Начертательная геометрия и инженерная графика');
INSERT INTO Lessons VALUES(17, 'Физика');
INSERT INTO Lessons VALUES(18, 'Информационные технологии в строительстве');
INSERT INTO Lessons VALUES(19, 'Инженерная геология');

INSERT INTO Teachers VALUES(1, 'Кленин', 'Александр', 'Сергеевич');
INSERT INTO Teachers VALUES(2, 'Клевчихин', 'Юрий', 'Александрович');
INSERT INTO Teachers VALUES(3, 'Прилепкина', 'Елена', 'Гумаровна');
INSERT INTO Teachers VALUES(4, 'Спорышев', 'Максим', 'Сергеевич');
INSERT INTO Teachers VALUES(5, 'Машенцев', 'Владимир', 'Юрьевич');
INSERT INTO Teachers VALUES(6, 'Пак', 'Геннадий', 'Константинович');
INSERT INTO Teachers VALUES(7, 'Трикашная', 'Наталья', 'Вячеславовна');
INSERT INTO Teachers VALUES(8, 'Кириленко', 'Лариса', 'Михайловна');
INSERT INTO Teachers VALUES(9, 'Баранов', 'Андрей', 'Александрович');
INSERT INTO Teachers VALUES(10, 'Алексанин', 'Григорий', 'Анатольевич');
INSERT INTO Teachers VALUES(11, 'Веремеева', 'Ирина', 'Феликсовна');
INSERT INTO Teachers VALUES(12, 'Абстрактный', 'Учитель', 'Физкультуры');
INSERT INTO Teachers VALUES(13, 'Пак', 'С.', 'Б.');
INSERT INTO Teachers VALUES(14, 'Сущенко', 'А.', 'А.');
INSERT INTO Teachers VALUES(15, 'Малыкина', 'Ирина', 'Анатольевна');
INSERT INTO Teachers VALUES(16, 'Чеканов', 'Сергей', 'Геннадьевич');
INSERT INTO Teachers VALUES(17, 'Мишаков', 'Александр', 'Владиславович');
INSERT INTO Teachers VALUES(18, 'Сторожок', 'Евгений', 'Анатольевич');
INSERT INTO Teachers VALUES(19, 'Перцевая', 'Ксения', 'Александровна');
INSERT INTO Teachers VALUES(20, 'Степанова', 'Алена', 'Андреевна');
INSERT INTO Teachers VALUES(21, 'Сакун', 'Р.', 'А.');
INSERT INTO Teachers VALUES(22, 'Крикунова', 'Юлия', 'Анатольевна');
INSERT INTO Teachers VALUES(23, 'Исхакова', 'Ольга', 'Дмитриевна');
INSERT INTO Teachers VALUES(24, 'Бойко', 'Людмила', 'Александровна');
INSERT INTO Teachers VALUES(25, 'Шустикова', 'Татьяна', 'Валентиновна');
INSERT INTO Teachers VALUES(26, 'Мацуцин', 'Артур', 'Алексеевич');
INSERT INTO Teachers VALUES(27, 'Фоменко', 'Елена', 'Львовна');
INSERT INTO Teachers VALUES(28, 'Проценко', 'Виктория', 'Владимировна');
INSERT INTO Teachers VALUES(29, 'Василенко', 'Геннадий', 'Петрович');
INSERT INTO Teachers VALUES(30, 'Аверкова', 'Галина', 'Владимировны');
INSERT INTO Teachers VALUES(31, 'Руковишникова', 'Валентина', 'Ивановна');

INSERT INTO Lessons_Times VALUES(1, '08:30:00.0000', '10:00:00.0000');
INSERT INTO Lessons_Times VALUES(2, '10:10:00.0000', '11:40:00.0000');
INSERT INTO Lessons_Times VALUES(3, '11:50:00.0000', '13:20:00.0000');
INSERT INTO Lessons_Times VALUES(4, '13:30:00.0000', '15:00:00.0000');
INSERT INTO Lessons_Times VALUES(5, '15:10:00.0000', '16:40:00.0000');
INSERT INTO Lessons_Times VALUES(6, '16:50:00.0000', '18:20:00.0000');
INSERT INTO Lessons_Times VALUES(7, '18:30:00.0000', '20:00:00.0000');
INSERT INTO Lessons_Times VALUES(8, '20:10:00.0000', '21:40:00.0000');

INSERT INTO Weekdays VALUES(1, 'Понедельник');
INSERT INTO Weekdays VALUES(2, 'Вторник');
INSERT INTO Weekdays VALUES(3, 'Среда');
INSERT INTO Weekdays VALUES(4, 'Четверг');
INSERT INTO Weekdays VALUES(5, 'Пятница');
INSERT INTO Weekdays VALUES(6, 'Суббота');
INSERT INTO Weekdays VALUES(7, 'Воскресенье');

INSERT INTO Classrooms VALUES(1, 'D733', 30);
INSERT INTO Classrooms VALUES(2, 'D733a', 30);
INSERT INTO Classrooms VALUES(3, 'D654/752', 80);
INSERT INTO Classrooms VALUES(4, 'D412/542', 80);
INSERT INTO Classrooms VALUES(5, 'S', 200);
INSERT INTO Classrooms VALUES(6, 'D547', 30);
INSERT INTO Classrooms VALUES(7, 'D741', 30);
INSERT INTO Classrooms VALUES(8, 'D743', 80);
INSERT INTO Classrooms VALUES(9, 'D549a', 80);
INSERT INTO Classrooms VALUES(10, 'D746', 30);
INSERT INTO Classrooms VALUES(11, 'D546', 80);
INSERT INTO Classrooms VALUES(12, 'D548', 80);
INSERT INTO Classrooms VALUES(13, 'D820', 30);
INSERT INTO Classrooms VALUES(14, 'D546а', 20);
INSERT INTO Classrooms VALUES(15, 'D734а', 30);
INSERT INTO Classrooms VALUES(16, 'D945', 30);
INSERT INTO Classrooms VALUES(17, 'D734', 60);
INSERT INTO Classrooms VALUES(18, 'C406', 30);
INSERT INTO Classrooms VALUES(19, 'C408', 80);
INSERT INTO Classrooms VALUES(20, 'E425', 30);
INSERT INTO Classrooms VALUES(21, 'E320/224', 30);
INSERT INTO Classrooms VALUES(22, 'D817', 30);
INSERT INTO Classrooms VALUES(23, 'E720', 30);
INSERT INTO Classrooms VALUES(24, 'E323/236', 60);
INSERT INTO Classrooms VALUES(25, 'E702/802', 30);
INSERT INTO Classrooms VALUES(26, 'E809', 30);
INSERT INTO Classrooms VALUES(27, 'E706', 30);
INSERT INTO Classrooms VALUES(28, 'E809', 60);
INSERT INTO Classrooms VALUES(29, 'E541/442', 30);

INSERT INTO Lessons_Types VALUES(1, 'Практическое занятие');
INSERT INTO Lessons_Types VALUES(2, 'Лекция');
INSERT INTO Lessons_Types VALUES(3, 'Лабораторное занятие');

INSERT INTO Timetable VALUES(1, '1.09.2015', '1.09.2016',1, 2, 3, 1, 3, 1, 2);
INSERT INTO Timetable VALUES(2, '1.09.2015', '1.09.2016',1, 2, 3, 2, 3, 1, 2);
INSERT INTO Timetable VALUES(3, '1.09.2015', '1.09.2016',1, 2, 3, 3, 3, 1, 2);
INSERT INTO Timetable VALUES(4, '1.09.2015', '1.09.2016',1, 2, 3, 4, 3, 1, 2);
INSERT INTO Timetable VALUES(5, '1.09.2015', '1.09.2016',2, 2, 6, 1, 4, 1, 3);
INSERT INTO Timetable VALUES(6, '1.09.2015', '1.09.2016',2, 2, 6, 2, 4, 1, 3);
INSERT INTO Timetable VALUES(7, '1.09.2015', '1.09.2016',2, 2, 6, 3, 4, 1, 3);
INSERT INTO Timetable VALUES(8, '1.09.2015', '1.09.2016',2, 2, 6, 4, 4, 1, 3);
INSERT INTO Timetable VALUES(9, '1.09.2015', '1.09.2016',3, 2, 6, 1, 4, 1, 4);
INSERT INTO Timetable VALUES(10, '1.09.2015', '1.09.2016', 3, 2, 6, 2, 4, 1, 4);
INSERT INTO Timetable VALUES(11, '1.09.2015', '1.09.2016', 3, 2, 6, 3, 4, 1, 4);
INSERT INTO Timetable VALUES(12, '1.09.2015', '1.09.2016', 3, 2, 6, 4, 4, 1, 4);
INSERT INTO Timetable VALUES(13, '1.09.2015', '1.09.2016', 9, 1, 12, 1, 5, 1, 5);
INSERT INTO Timetable VALUES(14, '1.09.2015', '1.09.2016', 9, 1, 12, 2, 5, 1, 5);
INSERT INTO Timetable VALUES(15, '1.09.2015', '1.09.2016', 9, 1, 12, 3, 5, 1, 5);
INSERT INTO Timetable VALUES(16, '1.09.2015', '1.09.2016', 9, 1, 12, 4, 5, 1, 5);
INSERT INTO Timetable VALUES(17, '1.09.2015', '1.09.2016', 1, 1, 2, 1, 6, 2, 1);
INSERT INTO Timetable VALUES(18, '1.09.2015', '1.09.2016', 1, 1, 2, 2, 6, 2, 1);
INSERT INTO Timetable VALUES(19, '1.09.2015', '1.09.2016', 4, 1, 8, 1, 8, 2, 2);
INSERT INTO Timetable VALUES(20, '1.09.2015', '1.09.2016', 4, 1, 8, 2, 8, 2, 2);
INSERT INTO Timetable VALUES(21, '1.09.2015', '1.09.2016', 4, 1, 11, 1, 7, 2, 2);
INSERT INTO Timetable VALUES(22, '1.09.2015', '1.09.2016', 4, 1, 11, 2, 7, 2, 2);
INSERT INTO Timetable VALUES(23, '1.09.2015', '1.09.2016', 5, 3, 4, 1, 1, 2, 3);
INSERT INTO Timetable VALUES(24, '1.09.2015', '1.09.2016', 5, 3, 4, 1, 1, 2, 4);
INSERT INTO Timetable VALUES(25, '1.09.2015', '1.09.2016', 6, 3, 9, 2, 2, 2, 3);
INSERT INTO Timetable VALUES(26, '1.09.2015', '1.09.2016', 2, 1, 6, 1, 6, 3, 1);
INSERT INTO Timetable VALUES(27, '1.09.2015', '1.09.2016', 2, 1, 6, 2, 6, 3, 1);
INSERT INTO Timetable VALUES(28, '1.09.2015', '1.09.2016', 3, 1, 7, 1, 7, 3, 2);
INSERT INTO Timetable VALUES(29, '1.09.2015', '1.09.2016', 3, 1, 7, 2, 7, 3, 2);
INSERT INTO Timetable VALUES(30, '1.09.2015', '1.09.2016', 3, 1, 7, 1, 7, 3, 3);
INSERT INTO Timetable VALUES(31, '1.09.2015', '1.09.2016', 3, 1, 7, 2, 7, 3, 3);
INSERT INTO Timetable VALUES(32, '1.09.2015', '1.09.2016', 5, 3, 4, 2, 1, 4, 3);
INSERT INTO Timetable VALUES(33, '1.09.2015', '1.09.2016', 5, 3, 4, 2, 1, 4, 4);
INSERT INTO Timetable VALUES(34, '1.09.2015', '1.09.2016', 9, 1, 12, 1, 5, 4, 5);
INSERT INTO Timetable VALUES(35, '1.09.2015', '1.09.2016', 9, 1, 12, 2, 5, 4, 5);
INSERT INTO Timetable VALUES(36, '1.09.2015', '1.09.2016', 9, 1, 12, 3, 5, 4, 5);
INSERT INTO Timetable VALUES(37, '1.09.2015', '1.09.2016', 9, 1, 12, 4, 5, 4, 5);
INSERT INTO Timetable VALUES(38, '1.09.2015', '1.09.2016', 7, 3, 1, 1, 1, 5, 1);
INSERT INTO Timetable VALUES(39, '1.09.2015', '1.09.2016', 7, 3, 1, 1, 1, 5, 2);
INSERT INTO Timetable VALUES(40, '1.09.2015', '1.09.2016', 8, 3, 5, 1, 2, 5, 2);
INSERT INTO Timetable VALUES(41, '1.09.2015', '1.09.2016', 8, 3, 5, 1, 2, 5, 3);
INSERT INTO Timetable VALUES(42, '1.09.2015', '1.09.2016', 8, 3, 5, 2, 2, 5, 4);
INSERT INTO Timetable VALUES(43, '1.09.2015', '1.09.2016', 8, 3, 5, 2, 2, 5, 5);
INSERT INTO Timetable VALUES(44, '1.09.2015', '1.09.2016', 7, 2, 1, 1, 3, 6, 1);
INSERT INTO Timetable VALUES(45, '1.09.2015', '1.09.2016', 6, 2, 1, 1, 3, 6, 2);
INSERT INTO Timetable VALUES(46, '1.09.2015', '1.09.2016', 8, 2, 1, 1, 3, 6, 2);
INSERT INTO Timetable VALUES(47, '1.09.2015', '1.09.2016', 6, 1, 9, 1, 11, 6, 3);
INSERT INTO Timetable VALUES(48, '1.09.2015', '1.09.2016', 7, 3, 10, 2, 9, 6, 3);
INSERT INTO Timetable VALUES(49, '1.09.2015', '1.09.2016', 7, 3, 10, 2, 9, 6, 4);
INSERT INTO Timetable VALUES(50, '1.09.2015', '1.09.2016', 1, 1, 2, 3, 6, 2, 2);
INSERT INTO Timetable VALUES(51, '1.09.2015', '1.09.2016', 1, 1, 2, 4, 6, 2, 2);
INSERT INTO Timetable VALUES(52, '1.09.2015', '1.09.2016', 4, 1, 8, 3, 8, 2, 3);
INSERT INTO Timetable VALUES(53, '1.09.2015', '1.09.2016', 4, 1, 8, 4, 8, 2, 3);
INSERT INTO Timetable VALUES(54, '1.09.2015', '1.09.2016', 5, 3, 13, 3, 11, 2, 4);
INSERT INTO Timetable VALUES(55, '1.09.2015', '1.09.2016', 5, 3, 13, 3, 11, 2, 5);
INSERT INTO Timetable VALUES(56, '1.09.2015', '1.09.2016', 5, 3, 9, 4, 2, 2, 4);
INSERT INTO Timetable VALUES(57, '1.09.2015', '1.09.2016', 5, 3, 9, 4, 2, 2, 5);
INSERT INTO Timetable VALUES(58, '1.09.2015', '1.09.2016', 3, 1, 7, 3, 7, 3, 1);
INSERT INTO Timetable VALUES(59, '1.09.2015', '1.09.2016', 3, 1, 7, 4, 7, 3, 1);
INSERT INTO Timetable VALUES(60, '1.09.2015', '1.09.2016', 2, 1, 6, 3, 6, 3, 2);
INSERT INTO Timetable VALUES(61, '1.09.2015', '1.09.2016', 2, 1, 6, 4, 6, 3, 2);
INSERT INTO Timetable VALUES(62, '1.09.2015', '1.09.2016', 3, 1, 7, 3, 7, 3, 3);
INSERT INTO Timetable VALUES(63, '1.09.2015', '1.09.2016', 3, 1, 7, 4, 7, 3, 3);
INSERT INTO Timetable VALUES(64, '1.09.2015', '1.09.2016', 7, 1, 18, 3, 14, 3, 4);
INSERT INTO Timetable VALUES(65, '1.09.2015', '1.09.2016', 7, 1, 18, 4, 14, 3, 4);
INSERT INTO Timetable VALUES(66, '1.09.2015', '1.09.2016', 6, 3, 13, 3, 2, 4, 1);
INSERT INTO Timetable VALUES(67, '1.09.2015', '1.09.2016', 8, 3, 21, 4, 14, 4, 1);
INSERT INTO Timetable VALUES(68, '1.09.2015', '1.09.2016', 8, 3, 21, 3, 14, 4, 2);
INSERT INTO Timetable VALUES(69, '1.09.2015', '1.09.2016', 7, 3, 18, 3, 14, 4, 2);
INSERT INTO Timetable VALUES(70, '1.09.2015', '1.09.2016', 7, 3, 18, 4, 14, 4, 2);
INSERT INTO Timetable VALUES(71, '1.09.2015', '1.09.2016', 8, 3, 21, 4, 14, 4, 2);
INSERT INTO Timetable VALUES(72, '1.09.2015', '1.09.2016', 8, 3, 21, 3, 14, 4, 3);
INSERT INTO Timetable VALUES(73, '1.09.2015', '1.09.2016', 6, 3, 9, 4, 2, 4, 3);
INSERT INTO Timetable VALUES(74, '1.09.2015', '1.09.2016', 7, 3,  18, 3, 14, 5, 1);
INSERT INTO Timetable VALUES(75, '1.09.2015', '1.09.2016', 7, 3,  18, 3, 14, 5, 2);
INSERT INTO Timetable VALUES(76, '1.09.2015', '1.09.2016', 7, 3,  18, 4, 14, 5, 2);
INSERT INTO Timetable VALUES(77, '1.09.2015', '1.09.2016', 7, 3,  18, 4, 14, 5, 3);
INSERT INTO Timetable VALUES(78, '1.09.2015', '1.09.2016', 7, 2, 1, 2, 3, 6, 1);
INSERT INTO Timetable VALUES(79, '1.09.2015', '1.09.2016', 6, 2, 1, 2, 3, 6, 2);
INSERT INTO Timetable VALUES(80, '1.09.2015', '1.09.2016', 8, 2, 1, 2, 3, 6, 2);
INSERT INTO Timetable VALUES(81, '1.09.2015', '1.09.2016', 7, 2, 1, 3, 3, 6, 1);
INSERT INTO Timetable VALUES(82, '1.09.2015', '1.09.2016', 6, 2, 1, 3, 3, 6, 2);
INSERT INTO Timetable VALUES(83, '1.09.2015', '1.09.2016', 8, 2, 1, 3, 3, 6, 2);
INSERT INTO Timetable VALUES(84, '1.09.2015', '1.09.2016', 7, 2, 1, 4, 3, 6, 1);
INSERT INTO Timetable VALUES(85, '1.09.2015', '1.09.2016', 6, 2, 1, 4, 3, 6, 2);
INSERT INTO Timetable VALUES(86, '1.09.2015', '1.09.2016', 8, 2, 1, 4, 3, 6, 2);
INSERT INTO Timetable VALUES(87, '1.09.2015', '1.09.2016', 1, 2, 3, 5, 3, 1, 2);
INSERT INTO Timetable VALUES(88, '1.09.2015', '1.09.2016', 1, 2, 3, 6, 3, 1, 2);
INSERT INTO Timetable VALUES(89, '1.09.2015', '1.09.2016', 10, 1, 14, 5, 1, 1, 3);
INSERT INTO Timetable VALUES(90, '1.09.2015', '1.09.2016', 10, 1, 14, 6, 1, 1, 3);
INSERT INTO Timetable VALUES(91, '1.09.2015', '1.09.2016', 5, 3, 15, 5, 17, 1, 3);
INSERT INTO Timetable VALUES(92, '1.09.2015', '1.09.2016', 5, 3, 14, 6, 1, 1, 3);
INSERT INTO Timetable VALUES(93, '1.09.2015', '1.09.2016', 5, 3, 15, 5, 17, 1, 4);
INSERT INTO Timetable VALUES(94, '1.09.2015', '1.09.2016', 5, 3, 14, 6, 1, 1, 4);
INSERT INTO Timetable VALUES(95, '1.09.2015', '1.09.2016', 9, 1, 12, 5, 5, 1, 5);
INSERT INTO Timetable VALUES(96, '1.09.2015', '1.09.2016', 9, 1, 12, 6, 5, 1, 5);
INSERT INTO Timetable VALUES(97, '1.09.2015', '1.09.2016', 2, 1, 16, 5, 12, 2, 2);
INSERT INTO Timetable VALUES(98, '1.09.2015', '1.09.2016', 2, 1, 16, 6, 12, 2, 2);
INSERT INTO Timetable VALUES(99, '1.09.2015', '1.09.2016', 1, 1, 3, 5, 12, 2, 3);
INSERT INTO Timetable VALUES(100, '1.09.2015', '1.09.2016', 1, 1, 3, 6, 12, 2, 3);
INSERT INTO Timetable VALUES(101, '1.09.2015', '1.09.2016', 4, 1, 8, 5, 8, 2, 4);
INSERT INTO Timetable VALUES(102, '1.09.2015', '1.09.2016', 4, 1, 8, 6, 8, 2, 4);
INSERT INTO Timetable VALUES(103, '1.09.2015', '1.09.2016', 11, 2, 17, 5, 13, 3, 2);
INSERT INTO Timetable VALUES(104, '1.09.2015', '1.09.2016', 11, 2, 17, 6, 13, 3, 2);
INSERT INTO Timetable VALUES(105, '1.09.2015', '1.09.2016', 11, 1, 17, 5, 13, 3, 3);
INSERT INTO Timetable VALUES(106, '1.09.2015', '1.09.2016', 11, 1, 17, 6, 13, 3, 3);
INSERT INTO Timetable VALUES(107, '1.09.2015', '1.09.2016', 11, 3, 17, 5, 13, 3, 3);
INSERT INTO Timetable VALUES(108, '1.09.2015', '1.09.2016', 11, 3, 17, 6, 13, 3, 3);
INSERT INTO Timetable VALUES(109, '1.09.2015', '1.09.2016', 7, 2, 18, 5, 14, 3, 4);
INSERT INTO Timetable VALUES(110, '1.09.2015', '1.09.2016', 7, 2, 18, 6, 14, 3, 4);
INSERT INTO Timetable VALUES(112, '1.09.2015', '1.09.2016', 7, 3, 18, 5, 14, 3, 5);
INSERT INTO Timetable VALUES(113, '1.09.2015', '1.09.2016', 7, 3, 18, 6, 14, 3, 5);
INSERT INTO Timetable VALUES(114, '1.09.2015', '1.09.2016', 6, 3, 14, 5, 1, 4, 2);
INSERT INTO Timetable VALUES(115, '1.09.2015', '1.09.2016', 12, 1, 19, 5, 7, 4, 2);
INSERT INTO Timetable VALUES(116, '1.09.2015', '1.09.2016', 12, 1, 19, 6, 7, 4, 2);
INSERT INTO Timetable VALUES(117, '1.09.2015', '1.09.2016', 2, 2, 16, 5, 13, 4, 3);
INSERT INTO Timetable VALUES(118, '1.09.2015', '1.09.2016', 2, 2, 16, 6, 13, 4, 3);
INSERT INTO Timetable VALUES(119, '1.09.2015', '1.09.2016', 7, 1, 18, 5, 15, 4, 4);
INSERT INTO Timetable VALUES(120, '1.09.2015', '1.09.2016', 7, 1, 18, 6, 15, 4, 4);
INSERT INTO Timetable VALUES(121, '1.09.2015', '1.09.2016', 9, 1, 12, 5, 5, 4, 5);
INSERT INTO Timetable VALUES(122, '1.09.2015', '1.09.2016', 9, 1, 12, 6, 5, 4, 5);
INSERT INTO Timetable VALUES(123, '1.09.2015', '1.09.2016', 13, 2, 20, 5, 16, 5, 1);
INSERT INTO Timetable VALUES(124, '1.09.2015', '1.09.2016', 13, 2, 20, 6, 16, 5, 1);
INSERT INTO Timetable VALUES(125, '1.09.2015', '1.09.2016', 13, 1, 20, 5, 16, 5, 2);
INSERT INTO Timetable VALUES(126, '1.09.2015', '1.09.2016', 13, 1, 20, 6, 16, 5, 2);
INSERT INTO Timetable VALUES(127, '1.09.2015', '1.09.2016', 13, 3, 20, 5, 16, 5, 2);
INSERT INTO Timetable VALUES(128, '1.09.2015', '1.09.2016', 13, 3, 20, 6, 16, 5, 2);
INSERT INTO Timetable VALUES(129, '1.09.2015', '1.09.2016', 6, 3, 21, 6, 1, 5, 3);
INSERT INTO Timetable VALUES(130, '1.09.2015', '1.09.2016', 6, 2, 1, 5, 3, 6, 2);
INSERT INTO Timetable VALUES(131, '1.09.2015', '1.09.2016', 6, 2, 1, 6, 3, 6, 2);
INSERT INTO Timetable VALUES(132, '1.09.2015', '1.09.2016', 1, 2, 31, 7, 21, 1, 2);
INSERT INTO Timetable VALUES(133, '1.09.2015', '1.09.2016', 4, 1, 22, 7, 19, 1, 4);
INSERT INTO Timetable VALUES(134, '1.09.2015', '1.09.2016', 14, 1, 23, 7, 19, 2, 1);
INSERT INTO Timetable VALUES(135, '1.09.2015', '1.09.2016', 15, 1, 24, 7, 19, 2, 2);
INSERT INTO Timetable VALUES(136, '1.09.2015', '1.09.2016', 9, 1, 12, 7, 5, 2, 3);
INSERT INTO Timetable VALUES(137, '1.09.2015', '1.09.2016', 16, 1, 25, 7, 18, 3, 1);
INSERT INTO Timetable VALUES(138, '1.09.2015', '1.09.2016', 16, 1, 25, 7, 18, 3, 2);
INSERT INTO Timetable VALUES(139, '1.09.2015', '1.09.2016', 17, 1, 26, 7, 20, 3, 3);
INSERT INTO Timetable VALUES(140, '1.09.2015', '1.09.2016', 17, 2, 26, 7, 21, 4, 2);
INSERT INTO Timetable VALUES(141, '1.09.2015', '1.09.2016', 3, 2, 31, 7, 21, 4, 2);
INSERT INTO Timetable VALUES(142, '1.09.2015', '1.09.2016', 17, 3, 27, 7, 22, 4, 3);
INSERT INTO Timetable VALUES(143, '1.09.2015', '1.09.2016', 18, 3, 28, 7, 23, 4, 3);
INSERT INTO Timetable VALUES(144, '1.09.2015', '1.09.2016', 17, 3, 27, 7, 22, 4, 4);
INSERT INTO Timetable VALUES(145, '1.09.2015', '1.09.2016', 18, 3, 28, 7, 23, 4, 4);
INSERT INTO Timetable VALUES(146, '1.09.2015', '1.09.2016', 14, 2, 23, 7, 24, 5, 2);
INSERT INTO Timetable VALUES(147, '1.09.2015', '1.09.2016', 9, 1, 12, 7, 5, 5, 3);
INSERT INTO Timetable VALUES(148, '1.09.2015', '1.09.2016', 19, 2, 29, 7, 25, 5, 4);
INSERT INTO Timetable VALUES(149, '1.09.2015', '1.09.2016', 19, 1, 29, 7, 27, 5, 5);
INSERT INTO Timetable VALUES(150, '1.09.2015', '1.09.2016', 3, 1, 30, 7, 26, 6, 1);
INSERT INTO Timetable VALUES(151, '1.09.2015', '1.09.2016', 15, 2, 24, 7, 29, 6, 1);
INSERT INTO Timetable VALUES(152, '1.09.2015', '1.09.2016', 1, 1, 30, 7, 26, 6, 2);

COMMIT;
