/*
 * INI files
 * 
 * Copyright by Gajatko a.d. 2007.
 * All rights reserved.
 * 
 * In this file there are classes representing various elements in an INI file.
 * The main are IniFileValue and IniFileSectionStart. The IniFileElement is
 * a base class for all the rest here.
 */

using System;
using System.Text;

namespace Externals.IniFiles
{
    /// <summary>Base class for all Config File elements.</summary>
    public class IniFileElement
    {
        private string fLine;

        /// <summary>Same as Formatting</summary>
        protected string fFormatting = "";

        /// <summary>Initializes a new, empty instance IniFileElement</summary>
        protected IniFileElement()
        {
            fLine = "";
        }

        /// <summary>Initializes a new instance IniFileElement</summary>
        /// <param name="content">Actual content of a line in a INI file.</param>
        public IniFileElement(string content)
        {
            fLine = content.TrimEnd();
        }

        /// <summary>Gets or sets a formatting string of this INI file element, spicific to it's type.
        /// See DefaultFormatting property in IniFileEx for more info.</summary>
        public string Formatting
        {
            get { return fFormatting; }
            set { fFormatting = value; }
        }

        /// <summary>Gets or sets a string of white characters which precedes any meaningful content of a line.</summary>
        public string Intendation
        {
            get
            {
                StringBuilder intend = new StringBuilder();
                for (int i = 0; i < fFormatting.Length; i++) {
                    if (!char.IsWhiteSpace(fFormatting[i])) break;
                    intend.Append(fFormatting[i]);
                }
                return intend.ToString();
            }
            set
            {
                if (value.TrimStart().Length > 0)
                    throw new ArgumentException("Intendation property cannot contain any characters which are not condsidered as white ones.");
                if (IniFileEx.TabReplacement != null)
                    value = value.Replace("\t", IniFileEx.TabReplacement);
                fFormatting = value + fFormatting.TrimStart();
                fLine = value + fLine.TrimStart();
            }
        }

        /// <summary>Gets full text representation of a config file element, excluding intendation.</summary>
        public string Content
        {
            get { return fLine.TrimStart(); }
            protected set { fLine = value; }
        }

        /// <summary>Gets full text representation of a config file element, including intendation.</summary>
        public string Line
        {
            get
            {
                string intendation = Intendation;
                if (fLine.Contains(Environment.NewLine)) {
                    string[] lines = fLine.Split(new string[] { Environment.NewLine }, StringSplitOptions.None);
                    StringBuilder ret = new StringBuilder();
                    ret.Append(lines[0]);
                    for (int i = 1; i < lines.Length; i++)
                        ret.Append(Environment.NewLine + intendation + lines[i]);
                    
                    return ret.ToString();
                }
                
                return fLine;
            }
        }

        /// <summary>Gets a string representation of this IniFileElement object.</summary>
        public override string ToString()
        {
            return "Line: \"" + fLine + "\"";
        }

        /// <summary>Formats this config element</summary>
        public virtual void FormatDefault()
        {
            Intendation = "";
        }


        protected static string StartsWith(string line, string[] array)
        {
            if (array == null) return null;
            for (int i = 0; i < array.Length; i++)
                if (line.StartsWith(array[i]))
                    return array[i];
            return null;
        }

        protected struct IndexOfAnyResult
        {
            public int Index;
            public string Any;

            public IndexOfAnyResult(int i, string any)
            {
                Any = any;
                Index = i;
            }
        }

        protected static IndexOfAnyResult IndexOfAny(string text, string[] array)
        {
            for (int i = 0; i < array.Length; i++)
                if (text.Contains(array[i]))
                    return new IndexOfAnyResult(text.IndexOf(array[i]), array[i]);
            return new IndexOfAnyResult(-1, null);
        }

        protected static string OfAny(int index, string text, string[] array)
        {
            for (int i = 0; i < array.Length; i++)
                if (text.Length - index >= array[i].Length && text.Substring(index, array[i].Length) == array[i])
                    return array[i];
            return null;
        }
    }
}
