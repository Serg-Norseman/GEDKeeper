# C# Coding Standards and Naming Conventions

## Namespace

Namespaces are named using Pascal Case (also called `UpperCamelCase`) with no underscores. 
This means the first letter of every word in the name is capitalized. For example: `MyNewNamespace`. 
However, are allowed capitalized acronyms (`MyXMLNamespace`).

## Assemblies

If an assembly contains only one namespace, they should use the same name. 
Otherwise, Assemblies should follow the normal Pascal Case format.

## Classes and Structures

Pascal Case, no underscores or leading C, cls, or I. Classes should not have the same name as the namespace 
in which they reside. Allowed capitalized acronyms. Try to avoid abbreviations, and try to always use nouns.

## Exception Classes

Follow class naming conventions, but add Exception to the end of the name.

## Interfaces

Follow class naming conventions, but start the name with I and capitalize the letter following the I. 
Example: `IFoo` The I prefix helps to differentiate between Interfaces and classes and also to avoid name collisions.

## Methods

Pascal Case, no underscores except in the event handlers. Try to avoid abbreviations. 
Many programmers have a nasty habit of overly abbreviating everything. This should be discouraged.

## Properties and Public Fields

Pascal Case, no underscores. Try to avoid abbreviations.

## Method arguments and local variables

Camel Case (or `lowerCamelCase`). Try to avoid abbreviations. 
Camel Case is the same as Pascal case, but the first letter of the first word is lowercased.

## Class-level Private and Protected fields

Pascal Case with a leading `f`-char. Always indicate protected or private in the declaration. 
The leading character helps to prevent name collisions in constructors (a parameter and a private field 
having the same name).

## Constants and readonly fields

ALL_CAPS.

## Controls on Forms

Pascal Case with a prefix that identifies it as being part of the UI instead of a purely coded control 
(example a temporary variable). Many developers use ui as the prefix followed by a descriptive name such as 
txtUserName or lblUserNickName ("txt" stands for TextBox control and "lbl" for Label control)

Some samples:
```
Control 	Prefix 	Example
Label 	lbl 	lblSurname
TextBox 	txt 	txtSurname
DataGrid 	grid 	gridResults
Button 	btn 	btnSave
ListBox 	lst 	lstCompany
Checkbox 	chk 	chkMailList
CheckBoxList 	lst 	lstAddress
RadioButton 	rad 	radSex
Image 	img 	imgLogo
Panel 	pnl 	pnlSevtion
Table 	tbl 	tblResults
```
