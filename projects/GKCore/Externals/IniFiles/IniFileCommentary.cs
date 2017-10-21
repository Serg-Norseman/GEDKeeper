using System;
using System.Text;

namespace Externals.IniFiles
{
    /// <summary>Represents one or more comment lines in a config file.</summary>
    public class IniFileCommentary : IniFileElement
    {
        private string fComment;
        private string fCommentChar;

        private IniFileCommentary()
        {
        }

        /// <summary>Initializes a new instance IniFileCommentary</summary>
        /// <param name="content">Actual content of a line in a INI file.</param>
        public IniFileCommentary(string content) : base(content)
        {
            if (IniFileEx.CommentChars.Length == 0)
                throw new NotSupportedException("Comments are disabled. Set the IniFileEx.CommentChars property to turn them on.");

            fCommentChar = StartsWith(Content, IniFileEx.CommentChars);

            if (fCommentChar != null && Content.Length > fCommentChar.Length)
                fComment = Content.Substring(fCommentChar.Length);
            else
                fComment = "";
        }

        /// <summary>Gets or sets comment char used in the config file for this comment.</summary>
        public string CommentChar
        {
            get { return fCommentChar; }
            set {
                if (fCommentChar == value)
                    return;
                fCommentChar = value;
                Rewrite();
            }
        }

        /// <summary>Gets or sets a commentary string.</summary>
        public string Comment
        {
            get { return fComment; }
            set {
                if (fComment == value)
                    return;
                fComment = value;
                Rewrite();
            }
        }

        private void Rewrite()
        {
            StringBuilder newContent = new StringBuilder();
            string[] lines = fComment.Split(new string[] { Environment.NewLine }, StringSplitOptions.None);
            newContent.Append(fCommentChar + lines[0]);
            for (int i = 1; i < lines.Length; i++)
                newContent.Append(Environment.NewLine + fCommentChar + lines[i]);
            Content = newContent.ToString();
        }

        /// <summary>Determines whether specified string is a representation of particular IniFileElement object.</summary>
        /// <param name="testLine">Trimmed test string.</param>
        public static bool IsLineValid(string testLine)
        {
            return StartsWith(testLine.TrimStart(), IniFileEx.CommentChars) != null;
        }

        /// <summary>Gets a string representation of this IniFileCommentary object.</summary>
        public override string ToString()
        {
            return "Comment: \"" + fComment + "\"";
        }

        /// <summary>Gets an IniFileCommentary object from commentary text.</summary>
        /// <param name="comment">Commentary text.</param>
        public static IniFileCommentary FromComment(string comment)
        {
            if (IniFileEx.CommentChars.Length == 0)
                throw new NotSupportedException("Comments are disabled. Set the IniFileEx.CommentChars property to turn them on.");

            IniFileCommentary ret = new IniFileCommentary();
            ret.fComment = comment;
            ret.CommentChar = IniFileEx.CommentChars[0];
            return ret;
        }

        /// <summary>Formats IniFileCommentary object to default appearance.</summary>
        public override void FormatDefault()
        {
            base.FormatDefault();
            CommentChar = IniFileEx.CommentChars[0];
            Rewrite();
        }
    }
}
