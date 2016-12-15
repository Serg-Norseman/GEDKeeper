using System;
using System.Collections.Generic;

namespace Externals.IniFiles
{
    /// <summary>Object model for a section in an INI file, which stores a all values in memory.</summary>
    public class IniFileSection
    {
        internal List<IniFileElement> elements = new List<IniFileElement>();
        internal IniFileSectionStart sectionStart;
        internal IniFileEx parent;

        internal IniFileSection(IniFileEx _parent, IniFileSectionStart sect)
        {
            sectionStart = sect;
            parent = _parent;
        }

        /// <summary>Gets or sets the name of the section</summary>
        public string Name
        {
            get { return sectionStart.SectionName; }
            set { sectionStart.SectionName = value; }
        }

        /// <summary>Gets or sets comment associated with this section. In the file a comment must appear exactly
        /// above section's declaration. Returns "" if no comment is provided.</summary>
        public string Comment
        {
            get {
                return Name == "" ? "" : getComment(sectionStart);
            }
            set
            {
                if (Name != "")
                    setComment(sectionStart, value);
            }
        }

        private void setComment(IniFileElement el, string comment)
        {
            int index = parent.elements.IndexOf(el);
            if (IniFileSettings.CommentChars.Length == 0)
                throw new NotSupportedException("Comments are currently disabled. Setup ConfigFileSettings.CommentChars property to enable them.");
            IniFileCommentary com;
            if (index > 0 && parent.elements[index - 1] is IniFileCommentary) {
                com = ((IniFileCommentary)parent.elements[index - 1]);
                if (comment == "")
                    parent.elements.Remove(com);
                else {
                    com.Comment = comment;
                    com.Intendation = el.Intendation;
                }
            }
            else if (comment != "") {
                com = IniFileCommentary.FromComment(comment);
                com.Intendation = el.Intendation;
                parent.elements.Insert(index, com);
            }
        }

        private string getComment(IniFileElement el)
        {
            int index = parent.elements.IndexOf(el);
            if (index != 0 && parent.elements[index - 1] is IniFileCommentary)
                return ((IniFileCommentary)parent.elements[index - 1]).Comment;
            else return "";
        }
        
        private IniFileValue GetValue(string key)
        {
            string lower = key.ToLowerInvariant();

            for (int i = 0; i < elements.Count; i++)
            {
                IniFileValue value = elements[i] as IniFileValue;
                if (value != null)
                {
                    if (value.Key == key || (!IniFileSettings.CaseSensitive && value.Key.ToLowerInvariant() == lower))
                        return value;
                }
            }

            return null;
        }

        /// <summary>Sets the comment for given key.</summary>
        public void SetComment(string key, string comment)
        {
            IniFileValue val = GetValue(key);
            if (val == null) return;
            setComment(val, comment);
        }
        /// <summary>Sets the inline comment for given key.</summary>
        public void SetInlineComment(string key, string comment)
        {
            IniFileValue val = GetValue(key);
            if (val == null) return;
            val.InlineComment = comment;
        }

        /// <summary>Gets the inline comment for given key.</summary>
        public string GetInlineComment(string key)
        {
            IniFileValue val = GetValue(key);
            return val == null ? null : val.InlineComment;
        }

        /// <summary>Gets or sets the inline for this section.</summary>
        public string InlineComment
        {
            get { return sectionStart.InlineComment; }
            set { sectionStart.InlineComment = value; }
        }

        /// <summary>Gets the comment associated to given key. If there is no comment, empty string is returned.
        /// If the key does not exist, NULL is returned.</summary>
        public string GetComment(string key)
        {
            IniFileValue val = GetValue(key);
            return val == null ? null : getComment(val);
        }

        /// <summary>Renames a key.</summary>
        public void RenameKey(string key, string newName)
        {
            IniFileValue val = GetValue(key);
            if (val == null) return;

            val.Key = newName;
        }

        /// <summary>Deletes a key.</summary>
        public void DeleteKey(string key)
        {
            IniFileValue val = GetValue(key);
            if (val == null) return;

            parent.elements.Remove(val);
            elements.Remove(val);
        }

        /// <summary>Gets or sets value of the key</summary>
        /// <param name="key">Name of key.</param>
        public string this[string key]
        {
            get
            {
                IniFileValue v = GetValue(key);
                return v == null ? null : v.Value;
            }
            set
            {
                IniFileValue v = GetValue(key);
                //if (!IniFileSettings.AllowEmptyValues && value == "") {
                //    if (v != null) {
                //        elements.Remove(v);
                //        parent.elements.Remove(v);
                //        return;
                //    }
                //}
                if (v != null) {
                    v.Value = value;
                    return;
                }
                SetValue(key, value);
            }
        }

        /// <summary>Gets or sets value of a key.</summary>
        /// <param name="key">Name of the key.</param>
        /// <param name="defaultValue">A value to return if the requested key was not found.</param>
        public string this[string key, string defaultValue]
        {
            get
            {
                string val = this[key];
                return string.IsNullOrEmpty(val) ? defaultValue : val;
            }
            set { this[key] = value; }
        }

        private void SetValue(string key, string value)
        {
            IniFileValue ret = null;
            IniFileValue prev = LastValue();
            
            if (IniFileSettings.PreserveFormatting) {
                if (prev != null && prev.Intendation.Length >= sectionStart.Intendation.Length)
                    ret = prev.CreateNew(key, value);
                else {
                    bool valFound = false;
                    for (int i = parent.elements.IndexOf(sectionStart) - 1; i >= 0; i--)
                    {
                        IniFileElement el = parent.elements[i];
                        if (el is IniFileValue) {
                            ret = ((IniFileValue)el).CreateNew(key, value);
                            valFound = true;
                            break;
                        }
                    }

                    if (!valFound)
                        ret = IniFileValue.FromData(key, value);
                    if (ret.Intendation.Length < sectionStart.Intendation.Length)
                        ret.Intendation = sectionStart.Intendation;
                }
            }
            else
                ret = IniFileValue.FromData(key, value);
            if (prev == null) {
                elements.Insert(elements.IndexOf(sectionStart) + 1, ret);
                parent.elements.Insert(parent.elements.IndexOf(sectionStart) + 1, ret);
            }
            else {
                elements.Insert(elements.IndexOf(prev) + 1, ret);
                parent.elements.Insert(parent.elements.IndexOf(prev) + 1, ret);
            }
        }

        internal IniFileValue LastValue()
        {
            for (int i = elements.Count - 1; i >= 0; i--)
            {
                IniFileValue value = elements[i] as IniFileValue;
                if (value != null)
                    return value;
            }
            return null;
        }

        internal IniFileValue FirstValue()
        {
            for (int i = 0; i < elements.Count; i++)
            {
                IniFileValue value = elements[i] as IniFileValue;
                if (value != null)
                    return value;
            }
            return null;
        }

        /// <summary>Gets an array of names of values in this section.</summary>
        public System.Collections.ObjectModel.ReadOnlyCollection<string> GetKeys()
        {
            List<string> list = new List<string>(elements.Count);
            for (int i = 0; i < elements.Count; i++)
            {
                IniFileValue value = elements[i] as IniFileValue;
                if (value != null)
                    list.Add(value.Key);
            }

            return new System.Collections.ObjectModel.ReadOnlyCollection<string>(list);
        }

        /// <summary>Gets a string representation of this IniFileSectionReader object.</summary>
        public override string ToString()
        {
            return sectionStart.ToString() + " (" + elements.Count.ToString() + " elements)";
        }

        /// <summary>Formats whole section.</summary>
        /// <param name="preserveIntendation">Determines whether intendation should be preserved.</param>
        public void Format(bool preserveIntendation)
        {
            for (int i = 0; i < elements.Count; i++) {
                IniFileElement el = elements[i];
                string lastIntend = el.Intendation;
                el.FormatDefault();
                if (preserveIntendation)
                    el.Intendation = lastIntend;
            }
        }
    }
}
