# Common requirements

Platform for all projects should be strictly .NET 3.0 - to ensure maximum coverage of users,
without forcing inexperienced or conservative to do the platform upgrade on both old
and not updated versions of Windows. The exception is the sub-project GKTests, which allowed
more high platform.

Using Linq in the project is prohibited.

On the levels of implementation below the UI is not advisable to use the foreach operator.

Features available in only one specific platform (Windows/Linux) and unavailable to another,
can be developed only in agreement with the project's lead.

# C# Coding Standards and Naming Conventions

## Namespace

Namespaces are named using Pascal Case (also called `UpperCamelCase`) with no underscores. 
This means the first letter of every word in the name is capitalized. For example: `MyNewNamespace`. 
However, are allowed capitalized acronyms (`MyXMLNamespace`).

## Assemblies

If an assembly contains only one namespace, they should use the same name. 
Otherwise, assemblies should follow the normal Pascal Case format.

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

## Declare member variables of classes

Do declare all member variables at the top of a class, with static variables at the very top.

## Properties and Public Fields, Constants and readonly fields

Pascal Case, no underscores. Try to avoid abbreviations.

## Method arguments and local variables

Camel Case (or `lowerCamelCase`). Try to avoid abbreviations. 
Camel Case is the same as Pascal case, but the first letter of the first word is lowercased.

## Class-level Private and Protected fields

Pascal Case with a leading `f`-char. Always indicate protected or private in the declaration. 
The leading character helps to prevent name collisions in constructors (a parameter and a private field 
having the same name).

The use of "m_" and "_" as prefixes for instance members is highly discouraged.

## Class members order

Declare all member variables at the top of a class, with static variables at the very top:

- private static fields;
- protected and public static fields;
- private and public consts;
- private member fields;
- protected member fields (private and protected may be mixed);
- public member fields;
- properties;
- instance's control (constructor, destructor, static create methods if needs);
- private methods and public methods usually are mixed grouped by meaning and functions.

In old and very old classes, the order of the sections may be absent, for example constructors can be at the end of class.
But this is valid only before the refactoring. Not supposed to do in the new classes.

## Indentation

Use spaces (and configure your IDE to show a size of 4 spaces for them) for writing your code. 
If you are modifying someone else’s code, try to keep the coding style similar.

Switch statements have the "case" at the additional indentation.

```
switch (x) {
    case 'a':
       ...
    case 'b':
       ...
}
```

## Blank lines

Blank lines improve readability. They set off blocks of code which are in themselves logically related.

Two blank lines should always be used between:

- Logical sections of a source file
- Class and interface definitions (try one class/interface per file to prevent this case)

One blank line should always be used between:

- Methods
- Properties
- Local variables in a method and its first statement
- Logical sections inside a method to improve readability.

## Wrapping Lines

Preferred line length is 80 characters, taking into consideration the recommendations below:

- Break after a comma.
- Break after an operator.
- Prefer higher-level breaks to lower-level breaks.
- Align the new line with the beginning of the expression at the same level on the previous line.

## Where to put spaces

Don't use a space before an opening parenthesis when calling functions, or indexing, like this:

bad:
```
method (a);
b [10];
```

Don't put a space after the opening parenthesis and the closing one, ie:

bad:
```
method ( a );
array[ 10 ];
```

Don't put a space between the generic types, ie:

bad:
```
var list = new List <int> ();
```

good:
```
var list = new List<int>();
```


## Where to put braces

Inside a code block, put the opening brace on the same line as the statement:

good:
```
if (a) {
    code();
    code();
}
```
bad:
```
if (a)
{
    code();
    code();
}
```
Avoid using unnecessary open/close braces, vertical space is usually limited:

good:
```
if (a)
    code();
```
bad:
```
if (a) {
    code();
}
```
Unless there are either multiple hierarchical conditions being used or that the condition cannot fit into a single line.

good:﻿
```
if (a) {
    if (b)
        code();
}
```
bad:
```
if (a)
    if (b)
        code();
```

If statements with else clauses are formatted like this:

good:
```
if (dingus) {
        ...
} else {
        ...
}
```
bad:
```
if (dingus)
{
        ...
}
else
{
        ...
}
```
bad:
```
if (dingus) {
        ...
}
else {
        ...
}
```

When defining namespaces and classes, a method (including ctor), properties and indexers, use a new line for the brace.
For very small properties, you can compress things:

passable:
```
int Property
{
    get { return value; }
    set { x = value; }
}
```

Empty methods: they should have the body of code using two lines (if it's temporary stub), or all in one line (if it will never be implemented).

## Use whitespace for clarity

Use white space in expressions liberally, except in the presence of parenthesis.

good:

```
if (a + 5 > method(blah() + 4))
```

bad:

```
if (a + 5 > method (blah ()+4))
```

bad:

```
if (a+5>method(blah()+4))
```

## Use of var

You can use "var" on the left-hand side of an assignment when the type name is repeated on the right hand side.
Otherwise, the use of "var" is prohibited. Exception: primitive types (int, string, double, etc) use predefined names.

## Use flags and directives

Use flags and directives in code, so that you can come back later and work on it.

You can use the #warning and #error directives,

```
#warning This is dirty code...
#error Fix this before everything explodes!
```

Also you can mark it with "//TODO:" or "//FIXME:" comments that show up in the task pane in IDE or for fast search.

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
