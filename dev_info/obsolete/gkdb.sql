drop table if exists `Individuals`;
create table `Individuals` (
	id integer auto_increment not null,
	primary key (id)
);

drop table if exists `Families`;
create table `Families` (
	id integer auto_increment not null,
	primary key (id)
);

drop table if exists `Notes`;
create table `Notes` (
	id integer auto_increment not null,
	note text,
	primary key (id)
);

drop table if exists `Multimedia`;
create table `Multimedia` (
	id integer auto_increment not null,
	primary key (id)
);

drop table if exists `Sources`;
create table `Sources` (
	id integer auto_increment not null,
	primary key (id)
);

drop table if exists `Repositories`;
create table `Repositories` (
	id integer auto_increment not null,
	primary key (id)
);

drop table if exists `Groups`;
create table `Groups` (
	id integer auto_increment not null,
	name nvarchar(100) not null,
	primary key (id)
);

drop table if exists `Researches`;
create table `Researches` (
	id integer auto_increment not null,
	primary key (id)
);

drop table if exists `Tasks`;
create table `Tasks` (
	id integer auto_increment not null,
	primary key (id)
);

drop table if exists `Communications`;
create table `Communications` (
	id integer auto_increment not null,
	primary key (id)
);

drop table if exists `Locations`;
create table `Locations` (
	id integer auto_increment not null,
	primary key (id)
);

drop table if exists `Submissions`;
create table `Submissions` (
	id integer auto_increment not null,
	primary key (id)
);

drop table if exists `Submitters`;
create table `Submitters` (
	id integer auto_increment not null,
	primary key (id)
);
