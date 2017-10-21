/*
 * INI files.
 * 
 * Copyright by Gajatko a.d. 2007.
 * All rights reserved.
 * 
 * In this file there are all needed classes to parse, manage and write old-style
 * INI [=initialization] files, like "win.ini" in Windows folder.
 * However, they use classes contained in the "ConfigFileElement.cs" source file.
 */

using System;
using System.Collections.Generic;
using System.IO;

namespace Externals.IniFiles
{
    /// <summary>Object model for INI file, which stores a whole structure in memory.</summary>
    public class IniFileEx
    {
        #region Settings

        /// <summary>Gets or sets array of strings which start a comment line.
        /// Default is {"#" (hash), ";" (semicolon)}. If empty or null, commentaries
        /// will not be allowed.</summary>
        internal static readonly string[] CommentChars = { ";", "#" };

        /// <summary>Gets or sets a character which is used as quote. Default null (not using quotation marks).</summary>
        internal static readonly char? QuoteChar = null;

        /// <summary>A string which determines default formatting of values used in Format() method. '?' (question mark) means a key,
        /// '$' (dollar) means a value and '=' (equality sign) means EqualsString; optionally, ';' is an inline comment.
        /// If QouteChar is not null, '$' will be automatically surrounded with qouetes. Default "?=$  ;" (e.g. "Key=Value  ;comment".</summary>
        internal static readonly string DefaultValueFormatting = "?=$   ;";

        /// <summary>A string which determines default formatting of section headers used in Format() method.
        /// '$' (dollar) means a section's name; '[' and ']' mean brackets; optionally, ';' is an inline comment. Default is "[$]  ;" (e.g. "[Section]  ;comment")</summary>
        internal static readonly string DefaultSectionFormatting = "[$]   ;";

        /// <summary>The string which all tabs in intendentation will be replaced with. If null, tabs will not be replaced. Default "    " (four spaces).</summary>
        internal static readonly string TabReplacement = "    ";

        internal const string EqualsString = "=";
        internal const string SectionCloseBracket = "]";
        internal const string SectionOpenBracket = "[";

        /// <summary>If true, blank lines will be written to a file. Otherwise, they will ignored.</summary>
        internal static readonly bool AllowBlankLines = true;

        /// <summary>If true empty keys will not be removed. Default TRUE.</summary>
        internal static readonly bool AllowEmptyValues = true;

        /// <summary>If true, blank lines will be written to a file. Otherwise, they will ignored.</summary>
        internal static readonly bool AllowInlineComments = true;

        /// <summary>If Quotes are on, then it in such situation: |KEY = "VALUE" blabla|, 'blabla' is
        /// a "text on the right". If this field is set to False, then such string will be ignored.</summary>
        internal static readonly bool AllowTextOnTheRight = true;

        /// <summary>Determines whether all searching/testing operation are case-sensitive. Default TRUE.</summary>
        internal static readonly bool CaseSensitive = true;

        /// <summary>Indicates whether comments and blank lines should be grouped
        /// (if true then multiple line comment will be parsed to the one single IniFileComment object).
        /// Otherwise, one IniFileElement will be always representing one single line in the file. Default TRUE.</summary>
        internal static readonly bool GroupElements = true;

        /// <summary>Inficates whether parser should preserve formatting. Default TRUE.</summary>
        internal static readonly bool PreserveFormatting = true;

        /// <summary>Determines whether a header comment of an INI file is separate from a comment of first section.
        /// If false, comment at the beginning of file may be considered both as header and commentary of the first section. Default TRUE.</summary>
        internal static readonly bool SeparateHeader = true;

        #endregion

        internal List<IniFileSection> sections = new List<IniFileSection>();
        internal List<IniFileElement> elements = new List<IniFileElement>();

        /// <summary>Creates new instance of IniFile.</summary>
        public IniFileEx()
        {
        }

        /// <summary>Gets a IniFileSection object from it's name</summary>
        /// <param name="sectionName">Name of section to search for. If not found, new one is created.</param>
        public IniFileSection this[string sectionName]
        {
            get
            {
                IniFileSection sect = GetSection(sectionName);
                if (sect != null)
                    return sect;

                IniFileSectionStart start;
                if (sections.Count > 0) {
                    IniFileSectionStart prev = sections[sections.Count - 1].sectionStart;
                    start = prev.CreateNew(sectionName);
                }
                else
                    start = IniFileSectionStart.FromName(sectionName);
                elements.Add(start);
                sect = new IniFileSection(this, start);
                sections.Add(sect);
                return sect;
            }
        }

        private IniFileSection GetSection(string name)
        {
            string lower = name.ToLowerInvariant();
            for (int i = 0; i < sections.Count; i++)
                if (sections[i].Name == name || (!IniFileEx.CaseSensitive && sections[i].Name.ToLowerInvariant() == lower))
                    return sections[i];
            return null;
        }

        /// <summary>Reads a INI file from a file or creates one.</summary>
        public static IniFileEx FromFile(string path)
        {
            if (!File.Exists(path)) {
                File.Create(path).Close();
                return new IniFileEx();
            }

            using (StreamReader reader = new StreamReader(path)) {
                IniFileEx ret = FromStream(reader);
                return ret;
            }
        }

        /// <summary>Reads a INI file from a stream.</summary>
        public static IniFileEx FromStream(StreamReader reader)
        {
            IEnumerable<IniFileElement> elemes = ParseText(reader);

            IniFileEx ret = new IniFileEx();
            ret.elements.AddRange(elemes);
            if (ret.elements.Count > 0) {
                IniFileSection section = null;

                if (ret.elements[ret.elements.Count - 1] is IniFileBlankLine)
                    ret.elements.RemoveAt(ret.elements.Count - 1);

                for (int i = 0; i < ret.elements.Count; i++)
                {
                    IniFileElement el = ret.elements[i];
                    if (el is IniFileSectionStart) {
                        section = new IniFileSection(ret, (IniFileSectionStart)el);
                        ret.sections.Add(section);
                    }
                    else if (section != null)
                        section.elements.Add(el);
                    else if (ret.sections.Exists(delegate(IniFileSection a) { return a.Name == ""; }))
                        ret.sections[0].elements.Add(el);
                    else if (el is IniFileValue) {
                        section = new IniFileSection(ret, IniFileSectionStart.FromName(""));
                        section.elements.Add(el);
                        ret.sections.Add(section);
                    }
                }
            }
            return ret;
        }

        /// <summary>Writes a INI file to a disc, using options in IniFileEx class</summary>
        public void Save(string path)
        {
            using (StreamWriter writer = new StreamWriter(path)) {
                Save(writer);
            }
        }

        /// <summary>Writes a INI file to a stream, using options in IniFileEx class</summary>
        public void Save(StreamWriter writer)
        {
            if (writer == null)
                throw new ArgumentNullException("writer");

            foreach (IniFileElement el in elements)
                WriteElement(writer, el);
        }

        /// <summary>Parses a single line.</summary>
        /// <param name="line">Text to parse.</param>
        private static IniFileElement ParseLine(string line)
        {
            if (line == null)
                return null;
            if (line.Contains("\n"))
                throw new ArgumentException("String passed to the ParseLine method cannot contain more than one line.");
            string trim = line.Trim();
            IniFileElement elem = null;
            if (IniFileBlankLine.IsLineValid(trim))
                elem = new IniFileBlankLine(1);
            else if (IniFileCommentary.IsLineValid(line))
                elem = new IniFileCommentary(line);
            else if (IniFileSectionStart.IsLineValid(trim))
                elem = new IniFileSectionStart(line);
            else if (IniFileValue.IsLineValid(trim))
                elem = new IniFileValue(line);
            return elem ?? new IniFileElement(line);
        }

        /// <summary>Parses given text.</summary>
        private static List<IniFileElement> ParseText(StreamReader reader)
        {
            var ret = new List<IniFileElement>();

            if (reader == null)
                return ret;

            IniFileElement lastEl = null;
            while (reader.Peek() != -1)
            {
                IniFileElement currEl = ParseLine(reader.ReadLine());

                if (IniFileEx.GroupElements) {
                    if (lastEl != null)
                    {
                        if (currEl is IniFileBlankLine && lastEl is IniFileBlankLine) {
                            ((IniFileBlankLine)lastEl).Amount++;
                            continue;
                        }

                        if (currEl is IniFileCommentary && lastEl is IniFileCommentary) {
                            ((IniFileCommentary)lastEl).Comment += Environment.NewLine + ((IniFileCommentary)currEl).Comment;
                            continue;
                        }
                    }
                    else
                        lastEl = currEl;
                }
                lastEl = currEl;
                ret.Add(currEl);
            }
            return ret;
        }

        /// <summary>Writes INI file element to the file.</summary>
        /// <param name="element">Element to write.</param>
        private static void WriteElement(StreamWriter writer, IniFileElement element)
        {
            if (!IniFileEx.PreserveFormatting)
                element.FormatDefault();

            // do not write if:
            if (!( // 1) element is a blank line AND blank lines are not allowed
                  (element is IniFileBlankLine && !IniFileEx.AllowBlankLines)
                  // 2) element is an empty value AND empty values are not allowed
                  || (!IniFileEx.AllowEmptyValues && element is IniFileValue && ((IniFileValue)element).Value == "")))
                writer.WriteLine(element.Line);
        }

        /// <summary>Deletes a section and all it's values and comments. No exception is thrown if there is no section of requested name.</summary>
        /// <param name="name">Name of section to delete.</param>
        public void DeleteSection(string name)
        {
            IniFileSection section = GetSection(name);
            if (section == null)
                return;
            IniFileSectionStart sect = section.sectionStart;
            elements.Remove(sect);
            for (int i = elements.IndexOf(sect) + 1; i < elements.Count; i++) {
                if (elements[i] is IniFileSectionStart)
                    break;
                elements.RemoveAt(i);
            }
        }

        /// <summary>Formats whole INI file.</summary>
        /// <param name="preserveIntendation">If true, old intendation will be standarized but not removed.</param>
        public void Format(bool preserveIntendation)
        {
            string lastSectIntend = "";
            string lastValIntend = "";
            for (int i = 0; i < elements.Count; i++) {
                IniFileElement el = elements[i];
                if (preserveIntendation) {
                    if (el is IniFileSectionStart)
                        lastValIntend = lastSectIntend = el.Intendation;
                    else if (el is IniFileValue)
                        lastValIntend = el.Intendation;
                }
                el.FormatDefault();
                if (preserveIntendation) {
                    if (el is IniFileSectionStart)
                        el.Intendation = lastSectIntend;
                    else if (el is IniFileCommentary && i != elements.Count - 1 && !(elements[i + 1] is IniFileBlankLine))
                        el.Intendation = elements[i + 1].Intendation;
                    else
                        el.Intendation = lastValIntend;
                }
            }
        }

        /// <summary>Joins sections which are definied more than one time.</summary>
        public void UnifySections()
        {
            var dict = new Dictionary<string, int>();
            for (int i = 0; i < sections.Count; i++)
            {
                IniFileSection sect = sections[i];
                if (dict.ContainsKey(sect.Name)) {
                    int index = dict[sect.Name] + 1;
                    elements.Remove(sect.sectionStart);
                    sections.Remove(sect);
                    for (int j = sect.elements.Count - 1; j >= 0; j--) {
                        IniFileElement el = sect.elements[j];
                        if (!(j == sect.elements.Count - 1 && el is IniFileCommentary))
                            elements.Remove(el);
                        if (!(el is IniFileBlankLine)) {
                            elements.Insert(index, el);
                            IniFileValue val = this[sect.Name].FirstValue();
                            el.Intendation = (val != null) ? val.Intendation : this[sect.Name].sectionStart.Intendation;
                        }
                    }
                }
                else
                    dict.Add(sect.Name, elements.IndexOf(sect.sectionStart));
            }
        }

        /// <summary>Gets or sets a header commentary of an INI file. Header comment must if separate from
        /// comment of a first section except when IniFileSetting.SeparateHeader is set to false.</summary>
        public string Header
        {
            get
            {
                if (elements.Count > 0)
                    if (elements[0] is IniFileCommentary && !(!IniFileEx.SeparateHeader
                                                              && elements.Count > 1 && !(elements[1] is IniFileBlankLine)))
                        return ((IniFileCommentary)elements[0]).Comment;
                return "";
            }
            set
            {
                if (elements.Count > 0 && elements[0] is IniFileCommentary && !(!IniFileEx.SeparateHeader
                                                                                && elements.Count > 1 && !(elements[1] is IniFileBlankLine))) {
                    if (value == "") {
                        elements.RemoveAt(0);
                        if (IniFileEx.SeparateHeader && elements.Count > 0 && elements[0] is IniFileBlankLine)
                            elements.RemoveAt(0);
                    }
                    else
                        ((IniFileCommentary)elements[0]).Comment = value;
                }
                else if (value != "") {
                    if ((elements.Count == 0 || !(elements[0] is IniFileBlankLine)) && IniFileEx.SeparateHeader)
                        elements.Insert(0, new IniFileBlankLine(1));
                    elements.Insert(0, IniFileCommentary.FromComment(value));
                }
            }
        }

        /// <summary>Gets or sets a commentary at the end of an INI file.</summary>
        public string Footer
        {
            get
            {
                if (elements.Count > 0)
                {
                    IniFileCommentary commentary = elements[elements.Count - 1] as IniFileCommentary;
                    if (commentary != null)
                        return commentary.Comment;
                }
                return "";
            }
            set
            {
                if (value == "") {
                    if (elements.Count > 0 && elements[elements.Count - 1] is IniFileCommentary) {
                        elements.RemoveAt(elements.Count - 1);
                        if (elements.Count > 0 && elements[elements.Count - 1] is IniFileBlankLine)
                            elements.RemoveAt(elements.Count - 1);
                    }
                }
                else {
                    if (elements.Count > 0) {
                        if (elements[elements.Count - 1] is IniFileCommentary)
                            ((IniFileCommentary)elements[elements.Count - 1]).Comment = value;
                        else
                            elements.Add(IniFileCommentary.FromComment(value));
                        if (elements.Count > 2) {
                            if (!(elements[elements.Count - 2] is IniFileBlankLine) && IniFileEx.SeparateHeader)
                                elements.Insert(elements.Count - 1, new IniFileBlankLine(1));
                            else if (value == "")
                                elements.RemoveAt(elements.Count - 2);
                        }
                    }
                    else
                        elements.Add(IniFileCommentary.FromComment(value));
                }
            }
        }
    }
}
