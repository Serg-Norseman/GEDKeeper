using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Externals.IniFiles
{
    /// <summary>StreamReader implementation, which read from an INI file.
    /// IniFileReader DOES NOT override any StreamReader methods. New ones are added.</summary>
    public class IniFileReader : StreamReader
    {
        /// <summary>Initializes a new instance of IniFileReader from specified stream.</summary>
        public IniFileReader(Stream str) : base(str)
        {
        }
        /// <summary>Initializes a new instance of IniFileReader from specified stream and encoding.</summary>
        public IniFileReader(Stream str, Encoding enc) : base(str, enc)
        {
        }
        /// <summary>Initializes a new instance of IniFileReader from specified path.</summary>
        public IniFileReader(string path) : base(path)
        {
        }
        /// <summary>Initializes a new instance of IniFileReader from specified path and encoding.</summary>
        public IniFileReader(string path, Encoding enc) : base(path, enc)
        {
        }

        IniFileElement current = null;

        /// <summary>Parses a single line.</summary>
        /// <param name="line">Text to parse.</param>
        public static IniFileElement ParseLine(string line)
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
        /// <param name="text">Text to parse.</param>
        public static List<IniFileElement> ParseText(string text)
        {
            if (text == null)
                return null;
            List<IniFileElement> ret = new List<IniFileElement>();
            IniFileElement lastEl = null;
            string[] lines = text.Split(new string[] { Environment.NewLine }, StringSplitOptions.None);
            for (int i = 0; i < lines.Length; i++) {
                IniFileElement currEl = ParseLine(lines[i]);
                if (IniFileSettings.GroupElements) {
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

        /// <summary>Reads and parses next line from the config file.</summary>
        /// <returns>Created ConfigFileElement.</returns>
        public IniFileElement ReadElement()
        {
            current = ParseLine(base.ReadLine());
            return current;
        }

        /// <summary>Reads all files</summary>
        /// <returns>All new elements which was added.</returns>
        public List<IniFileElement> ReadElementsToEnd()
        {
            List<IniFileElement> ret = ParseText(base.ReadToEnd());
            return ret;
        }

        /// <summary>Seeks to the section of specified name. If such section is not found,
        /// the function returns NULL and leaves the stream at the end of file.</summary>
        /// <param name="sectionName">Name of section to find.</param>
        public IniFileSectionStart GotoSection(string sectionName)
        {
            while (true) {
                string str = ReadLine();
                if (str == null) {
                    current = null;
                    return null;
                }

                if (IniFileSectionStart.IsLineValid(str))
                {
                    IniFileSectionStart sect = ParseLine(str) as IniFileSectionStart;
                    if (sect != null && (sect.SectionName == sectionName || (!IniFileSettings.CaseSensitive && sect.SectionName.ToLowerInvariant() == sectionName))) {
                        current = sect;
                        return sect;
                    }
                }
            }
        }
        /// <summary>Returns a list of IniFileElement object in the currect section. The first element of
        /// returned collection will be a IniFileSectionStart.</summary>
        /// <exception cref="System.InvalidOperationException">A stream is not currently at the IniFileSectionStart.</exception>
        public List<IniFileElement> ReadSection()
        {
            if (current == null || !(current is IniFileSectionStart))
                throw new InvalidOperationException("The current position of the reader must be at IniFileSectionStart. Use GotoSection method");
            List<IniFileElement> ret = new List<IniFileElement>();
            IniFileElement theCurrent = current;
            ret.Add(theCurrent);
            string text = "", temp;
            while ((temp = base.ReadLine()) != null) {
                if (IniFileSectionStart.IsLineValid(temp.Trim())) {
                    current = new IniFileSectionStart(temp);
                    break;
                }
                text += temp + Environment.NewLine;
            }
            if (text.EndsWith(Environment.NewLine) && text != Environment.NewLine)
                text = text.Substring(0, text.Length - Environment.NewLine.Length);
            ret.AddRange(ParseText(text));
            return ret;
        }
        /// <summary>Gets a recently parsed IniFileElement.</summary>
        public IniFileElement Current
        {
            get { return current; }
        }
        /// <summary>Gets values of the current section.</summary>
        /// <exception cref="System.InvalidOperationException">A stream is not currently at the IniFileSectionStart.</exception>
        public List<IniFileValue> ReadSectionValues()
        {
            List<IniFileElement> elements = ReadSection();
            List<IniFileValue> ret = new List<IniFileValue>();
            for (int i = 0; i < elements.Count; i++)
            {
                IniFileValue value = elements[i] as IniFileValue;
                if (value != null)
                    ret.Add(value);
            }
            return ret;
        }
        /// <summary>Searches the current section for a value of specified key. If such key is not found,
        /// the function returns NULL and leaves the stream at next section.</summary>
        /// <param name="key">Key to find.</param>
        public IniFileValue GotoValue(string key)
        {
            return GotoValue(key, false);
        }
        /// <summary>Searches for a value of specified key. If such key is not found,
        /// the function returns NULL and leaves the stream at next section.</summary>
        /// <param name="key">Key to find.</param>
        /// <param name="searchWholeFile">Sets a search scope. If true, function will not stop at the next IniFileSectionStart.</param>
        public IniFileValue GotoValue(string key, bool searchWholeFile)
        {
            while (true) {
                string str = ReadLine();
                if (str == null)
                    return null;

                if (IniFileValue.IsLineValid(str.Trim()))
                {
                    IniFileValue val = ParseLine(str) as IniFileValue;
                    if (val != null && (val.Key == key || (!IniFileSettings.CaseSensitive && val.Key.ToLowerInvariant() == key.ToLowerInvariant())))
                        return val;
                }

                if (!searchWholeFile && IniFileSectionStart.IsLineValid(str.Trim()))
                    return null;
                
            }
        }
    }
}
