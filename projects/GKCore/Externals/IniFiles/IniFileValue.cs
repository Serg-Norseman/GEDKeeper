using System;
using System.Text;

namespace Externals.IniFiles
{
    /// <summary>Represents one key-value pair.</summary>
    public class IniFileValue : IniFileElement
    {
        private string fInlineComment;
        private string fInlineCommentChar;
        private string fKey;
        private string fValue;
        private readonly string fTextOnTheRight; // only if qoutes are on, e.g. "Name = 'Jack' text-on-the-right"

        private IniFileValue() : base()
        {
        }

        private IniFileValue(string key, string value) : base()
        {
            fKey = key; 
            fValue = value;
        }

        /// <summary>Initializes a new instance IniFileValue.</summary>
        /// <param name="content">Actual content of a line in an INI file. Initializer assumes that it is valid.</param>
        public IniFileValue(string content) : base(content)
        {
            string[] split = Content.Split(new string[] { IniFileEx.EqualsString }, StringSplitOptions.None);
            fFormatting = ExtractFormat(content);
            string split0 = split[0].Trim();
            string split1 = split.Length >= 1 ? split[1].Trim() : "";
            
            if (split0.Length > 0) {
                if (IniFileEx.AllowInlineComments) {
                    IndexOfAnyResult result = IndexOfAny(split1, IniFileEx.CommentChars);
                    if (result.Index != -1) {
                        fInlineComment = split1.Substring(result.Index + result.Any.Length);
                        split1 = split1.Substring(0, result.Index).TrimEnd();
                        fInlineCommentChar = result.Any;
                    }
                }
                if (IniFileEx.QuoteChar != null && split1.Length >= 2) {
                    char quoteChar = (char)IniFileEx.QuoteChar;
                    if (split1[0] == quoteChar) {
                        int lastQuotePos;
                        if (IniFileEx.AllowTextOnTheRight) {
                            lastQuotePos = split1.LastIndexOf(quoteChar);
                            if (lastQuotePos != split1.Length - 1)
                                fTextOnTheRight = split1.Substring(lastQuotePos + 1);
                        }
                        else
                            lastQuotePos = split1.Length - 1;

                        if (lastQuotePos > 0) {
                            split1 = (split1.Length == 2) ? "" : split1.Substring(1, lastQuotePos - 1);
                        }
                    }
                }
                fKey = split0;
                fValue = split1;
            }
            Format();
        }

        /// <summary>Gets or sets a name of value.</summary>
        public string Key
        {
            get { return fKey; }
            set { fKey = value; Format(); }
        }

        /// <summary>Gets or sets a value.</summary>
        public string Value
        {
            get { return fValue; }
            set { fValue = value; Format(); }
        }

        /// <summary>Gets or sets an inline comment, which appear after the value.</summary>
        public string InlineComment
        {
            get { return fInlineComment; }
            set
            {
                if (!IniFileEx.AllowInlineComments || IniFileEx.CommentChars.Length == 0)
                    throw new NotSupportedException("Inline comments are disabled.");
                if (fInlineCommentChar == null)
                    fInlineCommentChar = IniFileEx.CommentChars[0];
                fInlineComment = value; Format();
            }
        }

        private enum FEState // stare of format extractor (ExtractFormat method)
        {
            BeforeEvery, AfterKey, BeforeVal, AfterVal
        }

        /// <summary>Creates a formatting string basing on an actual content of a line.</summary>
        public string ExtractFormat(string content)
        {
            //bool afterKey = false; bool beforeVal = false; bool beforeEvery = true; bool afterVal = false;
            //return IniFileEx.DefaultValueFormatting;
            FEState pos = FEState.BeforeEvery;
            string insideWhiteChars = "";

            StringBuilder form = new StringBuilder();
            for (int i = 0; i < content.Length; i++) {
                char currC = content[i];
                if (char.IsLetterOrDigit(currC)) {
                    if (pos == FEState.BeforeEvery) {
                        form.Append('?');
                        pos = FEState.AfterKey;
                        //afterKey = true; beforeEvery = false; ;
                    }
                    else if (pos == FEState.BeforeVal) {
                        form.Append('$');
                        pos = FEState.AfterVal;
                    }
                }

                else if (pos == FEState.AfterKey && content.Length - i >= IniFileEx.EqualsString.Length && content.Substring(i, IniFileEx.EqualsString.Length) == IniFileEx.EqualsString) {
                    form.Append(insideWhiteChars);
                    pos = FEState.BeforeVal;
                    //afterKey = false; beforeVal = true;
                    form.Append('=');
                }
                else if ((OfAny(i, content, IniFileEx.CommentChars)) != null) {
                    form.Append(insideWhiteChars);
                    form.Append(';');
                }
                else if (char.IsWhiteSpace(currC)) {
                    string theWhiteChar;
                    if (currC == '\t' && IniFileEx.TabReplacement != null)
                        theWhiteChar = IniFileEx.TabReplacement;
                    else
                        theWhiteChar = currC.ToString();
                    
                    if (pos == FEState.AfterKey || pos == FEState.AfterVal) {
                        insideWhiteChars += theWhiteChar;
                        continue;
                    }
                    
                    form.Append(theWhiteChar);
                }
                insideWhiteChars = "";
            }
            if (pos == FEState.BeforeVal) {
                form.Append('$');
                pos = FEState.AfterVal;
            }
            string ret = form.ToString();
            if (ret.IndexOf(';') == -1)
                ret += "   ;";
            return ret;
        }

        /// <summary>Formats this element using the format string in Formatting property.</summary>
        public void Format()
        {
            Format(fFormatting);
        }

        /// <summary>Formats this element using given formatting string</summary>
        /// <param name="pFormatting">Formatting template, where '?'-key, '='-equality sign, '$'-value, ';'-inline comments.</param>
        public void Format(string pFormatting)
        {
            StringBuilder build = new StringBuilder();
            for (int i = 0; i < pFormatting.Length; i++)
            {
                char currC = pFormatting[i];
                if (currC == '?')
                    build.Append(fKey);
                else if (currC == '$') {
                    if (IniFileEx.QuoteChar != null) {
                        char quoteChar = (char)IniFileEx.QuoteChar;
                        build.Append(quoteChar).Append(fValue).Append(quoteChar);
                    }
                    else
                        build.Append(fValue);
                }
                else if (currC == '=')
                    build.Append(IniFileEx.EqualsString);
                else if (currC == ';')
                    build.Append(fInlineCommentChar + fInlineComment);
                else if (char.IsWhiteSpace(pFormatting[i]))
                    build.Append(currC);
            }
            Content = build.ToString().TrimEnd() + (IniFileEx.AllowTextOnTheRight ? fTextOnTheRight : "");
        }

        /// <summary>Formats content using a scheme specified in IniFileEx.DefaultValueFormatting.</summary>
        public override void FormatDefault()
        {
            Formatting = IniFileEx.DefaultValueFormatting;
            Format();
        }

        /// <summary>Creates a new IniFileValue object basing on a key and a value and the formatting  of this IniFileValue.</summary>
        /// <param name="pKey">Name of value</param>
        /// <param name="pValue">Value</param>
        public IniFileValue CreateNew(string pKey, string pValue)
        {
            IniFileValue ret = new IniFileValue(pKey, pValue);

            if (IniFileEx.PreserveFormatting) {
                ret.fFormatting = fFormatting;
                if (IniFileEx.AllowInlineComments)
                    ret.fInlineCommentChar = fInlineCommentChar;
                ret.Format();
            }
            else
                ret.FormatDefault();

            return ret;
        }

        /// <summary>Determines whether specified string is a representation of particular IniFileElement object.</summary>
        /// <param name="testLine">Trimmed test string.</param>
        public static bool IsLineValid(string testLine)
        {
            int index = testLine.IndexOf(IniFileEx.EqualsString);
            return index > 0;
        }

        /// <summary>Gets a string representation of this IniFileValue object.</summary>
        public override string ToString()
        {
            return "Value: \"" + fKey + " = " + fValue + "\"";
        }

        /// <summary>Crates a IniFileValue object from it's data.</summary>
        /// <param name="key">Value name.</param>
        /// <param name="value">Associated value.</param>
        public static IniFileValue FromData(string key, string value)
        {
            IniFileValue ret = new IniFileValue(key, value);
            ret.FormatDefault();
            return ret;
        }
    }
}
