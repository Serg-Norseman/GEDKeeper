using System;

namespace Externals.IniFiles
{
    /// <summary>Static class containing format settings for INI files.</summary>
    public static class IniFileSettings
    {
        private static string[] fCommentChars = { ";", "#" };
        private static char? fQuoteChar = null;
        private static string fDefaultValueFormatting = "?=$   ;";
        private static string fDefaultSectionFormatting = "[$]   ;";
        private static string fTabReplacement = "    ";

        public const string EqualsString = "=";
        public const string SectionCloseBracket = "]";
        public const string SectionOpenBracket = "[";

        /// <summary>If true, blank lines will be written to a file. Otherwise, they will ignored.</summary>
        public static readonly bool AllowBlankLines = true;

        /// <summary>If true empty keys will not be removed. Default TRUE.</summary>
        public static readonly bool AllowEmptyValues = true;

        /// <summary>If true, blank lines will be written to a file. Otherwise, they will ignored.</summary>
        public static readonly bool AllowInlineComments = true;

        /// <summary>If Quotes are on, then it in such situation: |KEY = "VALUE" blabla|, 'blabla' is
        /// a "text on the right". If this field is set to False, then such string will be ignored.</summary>
        public static readonly bool AllowTextOnTheRight = true;

        /// <summary>Determines whether all searching/testing operation are case-sensitive. Default TRUE.</summary>
        public static readonly bool CaseSensitive = true;

        /// <summary>Indicates whether comments and blank lines should be grouped
        /// (if true then multiple line comment will be parsed to the one single IniFileComment object).
        /// Otherwise, one IniFileElement will be always representing one single line in the file. Default TRUE.</summary>
        public static readonly bool GroupElements = true;

        /// <summary>Inficates whether parser should preserve formatting. Default TRUE.</summary>
        public static readonly bool PreserveFormatting = true;

        /// <summary>Determines whether a header comment of an INI file is separate from a comment of first section.
        /// If false, comment at the beginning of file may be considered both as header and commentary of the first section. Default TRUE.</summary>
        public static readonly bool SeparateHeader = true;

        /// <summary>Gets or sets array of strings which start a comment line.
        /// Default is {"#" (hash), ";" (semicolon)}. If empty or null, commentaries
        /// will not be allowed.</summary>
        public static string[] CommentChars
        {
            get { return fCommentChars; }
            set
            {
                if (value == null)
                    throw new ArgumentNullException("value", @"Use empty array to disable comments instead of null");
                fCommentChars = value;
            }
        }

        /// <summary>Gets or sets a character which is used as quote. Default null (not using quotation marks).</summary>
        public static char? QuoteChar
        {
            get { return fQuoteChar; }
            set { fQuoteChar = value; }
        }

        /// <summary>A string which determines default formatting of section headers used in Format() method.
        /// '$' (dollar) means a section's name; '[' and ']' mean brackets; optionally, ';' is an inline comment. Default is "[$]  ;" (e.g. "[Section]  ;comment")</summary>
        public static string DefaultSectionFormatting
        {
            get { return fDefaultSectionFormatting; }
            set
            {
                if (value == null)
                    throw new ArgumentNullException("value", @"DefaultSectionFormatting");
                string test = value.Replace("$", "").Replace("[", "").Replace("]", "").Replace(";", "");
                if (test.TrimStart().Length > 0)
                    throw new ArgumentException("DefaultValueFormatting property cannot contain other characters than [,$,] and white spaces.");
                if (!(value.IndexOf('[') < value.IndexOf('$') && value.IndexOf('$') < value.IndexOf(']')
                      && (value.IndexOf(';') == -1 || value.IndexOf(']') < value.IndexOf(';'))))
                    throw new ArgumentException("Special charcters in the formatting strings are in the incorrect order. The valid is: [, $, ].");
                fDefaultSectionFormatting = value;
            }
        }

        /// <summary>A string which determines default formatting of values used in Format() method. '?' (question mark) means a key,
        /// '$' (dollar) means a value and '=' (equality sign) means EqualsString; optionally, ';' is an inline comment.
        /// If QouteChar is not null, '$' will be automatically surrounded with qouetes. Default "?=$  ;" (e.g. "Key=Value  ;comment".</summary>
        public static string DefaultValueFormatting
        {
            get { return fDefaultValueFormatting; }
            set
            {
                if (value == null)
                    throw new ArgumentNullException("value", @"DefaultValueFormatting");
                string test = value.Replace("?", "").Replace("$", "").Replace("=", "").Replace(";", "");
                if (test.TrimStart().Length > 0)
                    throw new ArgumentException("DefaultValueFormatting property cannot contain other characters than ?,$,= and white spaces.");
                if (!(((value.IndexOf('?') < value.IndexOf('=') && value.IndexOf('=') < value.IndexOf('$'))
                       || (value.IndexOf('=') == -1 && test.IndexOf('?') < value.IndexOf('$')))
                      && (value.IndexOf(';') == -1 || value.IndexOf('$') < value.IndexOf(';'))))
                    throw new ArgumentException("Special charcters in the formatting strings are in the incorrect order. The valid is: ?, =, $.");
                fDefaultValueFormatting = value;
            }
        }

        /// <summary>The string which all tabs in intendentation will be replaced with. If null, tabs will not be replaced. Default "    " (four spaces).</summary>
        public static string TabReplacement
        {
            get { return fTabReplacement; }
            set { fTabReplacement = value; }
        }
    }
}
