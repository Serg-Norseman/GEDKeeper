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
        private string line;
        /// <summary>Same as Formatting</summary>
        protected string formatting = "";


        /// <summary>Initializes a new, empty instance IniFileElement</summary>
        protected IniFileElement()
        {
            line = "";
        }

        /// <summary>Initializes a new instance IniFileElement</summary>
        /// <param name="content">Actual content of a line in a INI file.</param>
        public IniFileElement(string content)
        {
            line = content.TrimEnd();
        }

        /// <summary>Gets or sets a formatting string of this INI file element, spicific to it's type.
        /// See DefaultFormatting property in IniFileSettings for more info.</summary>
        public string Formatting
        {
            get { return formatting; }
            set { formatting = value; }
        }

        /// <summary>Gets or sets a string of white characters which precedes any meaningful content of a line.</summary>
        public string Intendation
        {
            get
            {
                StringBuilder intend = new StringBuilder();
                for (int i = 0; i < formatting.Length; i++) {
                    if (!char.IsWhiteSpace(formatting[i])) break;
                    intend.Append(formatting[i]);
                }
                return intend.ToString();
            }
            set
            {
                if (value.TrimStart().Length > 0)
                    throw new ArgumentException("Intendation property cannot contain any characters which are not condsidered as white ones.");
                if (IniFileSettings.TabReplacement != null)
                    value = value.Replace("\t", IniFileSettings.TabReplacement);
                formatting = value + formatting.TrimStart();
                line = value + line.TrimStart();
            }
        }
        /// <summary>Gets full text representation of a config file element, excluding intendation.</summary>
        public string Content
        {
            get { return line.TrimStart(); }
            protected set { line = value; }
        }
        /// <summary>Gets full text representation of a config file element, including intendation.</summary>
        public string Line
        {
            get
            {
                string intendation = Intendation;
                if (line.Contains(Environment.NewLine)) {
                    string[] lines = line.Split(new string[] { Environment.NewLine }, StringSplitOptions.None);
                    StringBuilder ret = new StringBuilder();
                    ret.Append(lines[0]);
                    for (int i = 1; i < lines.Length; i++)
                        ret.Append(Environment.NewLine + intendation + lines[i]);
                    
                    return ret.ToString();
                }
                
                return line;
            }
        }
        /// <summary>Gets a string representation of this IniFileElement object.</summary>
        public override string ToString()
        {
            return "Line: \"" + line + "\"";
        }
        /// <summary>Formats this config element</summary>
        public virtual void FormatDefault()
        {
            Intendation = "";
        }
    }
}
