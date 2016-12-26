using System;
using System.Text;

namespace Externals.IniFiles
{
    /// <summary>Represents one or more comment lines in a config file.</summary>
    public class IniFileCommentary : IniFileElement
    {
        private string comment;
        private string commentChar;

        private IniFileCommentary()
        {
        }

        /// <summary>Initializes a new instance IniFileCommentary</summary>
        /// <param name="content">Actual content of a line in a INI file.</param>
        public IniFileCommentary(string content) : base(content)
        {
            if (IniFileSettings.CommentChars.Length == 0)
                throw new NotSupportedException("Comments are disabled. Set the IniFileSettings.CommentChars property to turn them on.");

            commentChar = IniFileSettings.StartsWith(Content, IniFileSettings.CommentChars);

            if (commentChar != null && Content.Length > commentChar.Length)
                comment = Content.Substring(commentChar.Length);
            else
                comment = "";
        }

        /// <summary>Gets or sets comment char used in the config file for this comment.</summary>
        public string CommentChar
        {
            get { return commentChar; }
            set
            {
                if (commentChar == value) return;
                commentChar = value;
                Rewrite();
            }
        }

        /// <summary>Gets or sets a commentary string.</summary>
        public string Comment
        {
            get { return comment; }
            set
            {
                if (comment == value) return;
                comment = value;
                Rewrite();
            }
        }

        void Rewrite()
        {
            StringBuilder newContent = new StringBuilder();
            string[] lines = comment.Split(new string[] { Environment.NewLine }, StringSplitOptions.None);
            newContent.Append(commentChar + lines[0]);
            for (int i = 1; i < lines.Length; i++)
                newContent.Append(Environment.NewLine + commentChar + lines[i]);
            Content = newContent.ToString();
        }

        /// <summary>Determines whether specified string is a representation of particular IniFileElement object.</summary>
        /// <param name="testLine">Trimmed test string.</param>
        public static bool IsLineValid(string testLine)
        {
            return IniFileSettings.StartsWith(testLine.TrimStart(), IniFileSettings.CommentChars) != null;
        }

        /// <summary>Gets a string representation of this IniFileCommentary object.</summary>
        public override string ToString()
        {
            return "Comment: \"" + comment + "\"";
        }

        /// <summary>Gets an IniFileCommentary object from commentary text.</summary>
        /// <param name="comment">Commentary text.</param>
        public static IniFileCommentary FromComment(string comment)
        {
            if (IniFileSettings.CommentChars.Length == 0)
                throw new NotSupportedException("Comments are disabled. Set the IniFileSettings.CommentChars property to turn them on.");

            IniFileCommentary ret = new IniFileCommentary();
            ret.comment = comment;
            ret.CommentChar = IniFileSettings.CommentChars[0];
            return ret;
        }

        /// <summary>Formats IniFileCommentary object to default appearance.</summary>
        public override void FormatDefault()
        {
            base.FormatDefault();
            CommentChar = IniFileSettings.CommentChars[0];
            Rewrite();
        }
    }
}
