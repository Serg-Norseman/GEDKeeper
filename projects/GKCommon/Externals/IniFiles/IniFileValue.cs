using System;
using System.Text;

namespace Externals.IniFiles
{
    /// <summary>Represents one key-value pair.</summary>
    public class IniFileValue : IniFileElement
    {
        private string key;
        private string value;
        private string textOnTheRight; // only if qoutes are on, e.g. "Name = 'Jack' text-on-the-right"
        private string inlineComment, inlineCommentChar;

        private IniFileValue() : base()
        {
        }
        /// <summary>Initializes a new instance IniFileValue.</summary>
        /// <param name="content">Actual content of a line in an INI file. Initializer assumes that it is valid.</param>
        public IniFileValue(string content)
            : base(content)
        {
            string[] split = Content.Split(new string[] { IniFileSettings.EqualsString }, StringSplitOptions.None);
            formatting = ExtractFormat(content);
            string split0 = split[0].Trim();
            string split1 = split.Length >= 1 ?
                split[1].Trim()
                : "";
            
            if (split0.Length > 0) {
                if (IniFileSettings.AllowInlineComments) {
                    IniFileSettings.IndexOfAnyResult result = IniFileSettings.IndexOfAny(split1, IniFileSettings.CommentChars);
                    if (result.Index != -1) {
                        inlineComment = split1.Substring(result.Index + result.Any.Length);
                        split1 = split1.Substring(0, result.Index).TrimEnd();
                        inlineCommentChar = result.Any;
                    }
                }
                if (IniFileSettings.QuoteChar != null && split1.Length >= 2) {
                    char quoteChar = (char)IniFileSettings.QuoteChar;
                    if (split1[0] == quoteChar) {
                        int lastQuotePos;
                        if (IniFileSettings.AllowTextOnTheRight) {
                            lastQuotePos = split1.LastIndexOf(quoteChar);
                            if (lastQuotePos != split1.Length - 1)
                                textOnTheRight = split1.Substring(lastQuotePos + 1);
                        }
                        else
                            lastQuotePos = split1.Length - 1;

                        if (lastQuotePos > 0) {
                            split1 = (split1.Length == 2) ? "" : split1.Substring(1, lastQuotePos - 1);
                        }
                    }
                }
                key = split0;
                value = split1;
            }
            Format();
        }
        /// <summary>Gets or sets a name of value.</summary>
        public string Key
        {
            get { return key; }
            set { key = value; Format(); }
        }
        /// <summary>Gets or sets a value.</summary>
        public string Value
        {
            get { return value; }
            set { this.value = value; Format(); }
        }
        /// <summary>Gets or sets an inline comment, which appear after the value.</summary>
        public string InlineComment
        {
            get { return inlineComment; }
            set
            {
                if (!IniFileSettings.AllowInlineComments || IniFileSettings.CommentChars.Length == 0)
                    throw new NotSupportedException("Inline comments are disabled.");
                if (inlineCommentChar == null)
                    inlineCommentChar = IniFileSettings.CommentChars[0];
                inlineComment = value; Format();
            }
        }

        enum FEState // stare of format extractor (ExtractFormat method)
        {
            BeforeEvery, AfterKey, BeforeVal, AfterVal
        }

        /// <summary>Creates a formatting string basing on an actual content of a line.</summary>
        public string ExtractFormat(string content)
        {
            //bool afterKey = false; bool beforeVal = false; bool beforeEvery = true; bool afterVal = false;
            //return IniFileSettings.DefaultValueFormatting;
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

                else if (pos == FEState.AfterKey && content.Length - i >= IniFileSettings.EqualsString.Length && content.Substring(i, IniFileSettings.EqualsString.Length) == IniFileSettings.EqualsString) {
                    form.Append(insideWhiteChars);
                    pos = FEState.BeforeVal;
                    //afterKey = false; beforeVal = true;
                    form.Append('=');
                }
                else if ((IniFileSettings.OfAny(i, content, IniFileSettings.CommentChars)) != null) {
                    form.Append(insideWhiteChars);
                    form.Append(';');
                }
                else if (char.IsWhiteSpace(currC)) {
                    string theWhiteChar;
                    if (currC == '\t' && IniFileSettings.TabReplacement != null)
                        theWhiteChar = IniFileSettings.TabReplacement;
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
            Format(formatting);
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
                    build.Append(key);
                else if (currC == '$') {
                    if (IniFileSettings.QuoteChar != null) {
                        char quoteChar = (char)IniFileSettings.QuoteChar;
                        build.Append(quoteChar).Append(value).Append(quoteChar);
                    }
                    else
                        build.Append(value);
                }
                else if (currC == '=')
                    build.Append(IniFileSettings.EqualsString);
                else if (currC == ';')
                    build.Append(inlineCommentChar + inlineComment);
                else if (char.IsWhiteSpace(pFormatting[i]))
                    build.Append(currC);
            }
            Content = build.ToString().TrimEnd() + (IniFileSettings.AllowTextOnTheRight ? textOnTheRight : "");
        }

        /// <summary>Formats content using a scheme specified in IniFileSettings.DefaultValueFormatting.</summary>
        public override void FormatDefault()
        {
            Formatting = IniFileSettings.DefaultValueFormatting;
            Format();
        }

        /// <summary>Creates a new IniFileValue object basing on a key and a value and the formatting  of this IniFileValue.</summary>
        /// <param name="pKey">Name of value</param>
        /// <param name="pValue">Value</param>
        public IniFileValue CreateNew(string pKey, string pValue)
        {
            IniFileValue ret = new IniFileValue();
            ret.key = pKey; ret.value = pValue;
            if (IniFileSettings.PreserveFormatting) {
                ret.formatting = formatting;
                if (IniFileSettings.AllowInlineComments)
                    ret.inlineCommentChar = inlineCommentChar;
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
            int index = testLine.IndexOf(IniFileSettings.EqualsString);
            return index > 0;
        }

        /// <summary>Sets both key and values. Recommended when both properties have to be changed.</summary>
        public void Set(string pKey, string pValue)
        {
            this.key = pKey; this.value = pValue;
            Format();
        }

        /// <summary>Gets a string representation of this IniFileValue object.</summary>
        public override string ToString()
        {
            return "Value: \"" + key + " = " + value + "\"";
        }

        /// <summary>Crates a IniFileValue object from it's data.</summary>
        /// <param name="key">Value name.</param>
        /// <param name="value">Associated value.</param>
        public static IniFileValue FromData(string key, string value)
        {
            IniFileValue ret = new IniFileValue();
            ret.key = key; ret.value = value;
            ret.FormatDefault();
            return ret;
        }
    }
}
