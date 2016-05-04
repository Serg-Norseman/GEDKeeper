using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Externals.IniFiles
{
    /// <summary>StreamWriter implementation which writes an INI file.
    /// IniFileWriter DOES NOT override any StreamReader methods. New ones are added.</summary>
    public class IniFileWriter : StreamWriter
    {
        /// <summary>Initializes a new instance of IniFileReader from specified stream.</summary>
        public IniFileWriter(Stream str) : base(str)
        {
        }
        /// <summary>Initializes a new instance of IniFileReader from specified path.</summary>
        public IniFileWriter(string str) : base(str)
        {
        }
        /// <summary>Initializes a new instance of IniFileReader from specified stream and encoding.</summary>
        public IniFileWriter(Stream str, Encoding enc) : base(str, enc)
        {
        }
        /// <summary>Initializes a new instance of IniFileReader from specified path and encoding.</summary>
        public IniFileWriter(string str, bool append) : base(str, append)
        {
        }
        /// <summary>Writes INI file element to the file.</summary>
        /// <param name="element">Element to write.</param>
        public void WriteElement(IniFileElement element)
        {
            if (!IniFileSettings.PreserveFormatting)
                element.FormatDefault();
            // do not write if:
            if (!( // 1) element is a blank line AND blank lines are not allowed
                  (element is IniFileBlankLine && !IniFileSettings.AllowBlankLines)
                  // 2) element is an empty value AND empty values are not allowed
                  || (!IniFileSettings.AllowEmptyValues && element is IniFileValue && ((IniFileValue)element).Value == "")))
                base.WriteLine(element.Line);
        }

        /// <summary>Writes collection of INI file elements to the file.</summary>
        /// <param name="elements">Elements collection to write.</param>
        public void WriteElements(IEnumerable<IniFileElement> elements)
        {
            foreach (IniFileElement el in elements)
                WriteElement(el);
        }

        /// <summary>Writes a whole INI to a file</summary>
        /// <param name="file">Section to write.</param>
        public void WriteIniFile(IniFileEx file)
        {
            WriteElements(file.elements);
        }
        /// <summary>Writes a section to a file</summary>
        /// <param name="section">Section to write.</param>
        public void WriteSection(IniFileSection section)
        {
            WriteElement(section.sectionStart);
            for (int i = section.parent.elements.IndexOf(section.sectionStart) + 1; i < section.parent.elements.Count; i++) {
                if (section.parent.elements[i] is IniFileSectionStart)
                    break;
                WriteElement(section.parent.elements[i]);
            }
        }

    }
}
